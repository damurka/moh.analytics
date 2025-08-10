#' score_card UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_score_card_ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    layout_columns(
      width = 1 / 4,
      fill = FALSE,
      height = '150px',
      value_box(
        "Full immunized Children (%)",
        uiOutput(ns("fic"), container = h2),
        p('Source: KHIS'),
        showcase = bs_icon("shield-check"),
        showcase_layout = "top right",
        theme = 'teal',
        class = "border-0 shadow-sm"
      ),
      value_box(
        "Skilled Birth Attendance (%)",
        uiOutput(ns("sba"), container = h2),
        p('Source: KHIS'),
        showcase = bs_icon("person-heart"),
        showcase_layout = "top right",
        theme = 'teal',
        class = "border-0 shadow-sm"
      ),
      value_box(
        "Institutional Maternal Mortality Rate (per 100,000)",
        uiOutput(ns("mmr_inst"), container = h2),
        p('Source: KHIS'),
        showcase = bs_icon("hospital"),
        showcase_layout = "top right",
        theme = 'pink',
        class = "border-0 shadow-sm"
      ),
      value_box(
        "Maternal mortality Rate (per 100,000)",
        uiOutput(ns("mmr"), container = h2),
        p('Source: KDHS'),
        showcase = bs_icon("heart-pulse"),
        showcase_layout = "top right",
        theme = 'pink',
        class = "border-0 shadow-sm"
      )
    ),
    layout_columns(
      # width = 1 / 4,
      fill = FALSE,
      # fixed_width = TRUE,
      value_box(
        "Infant mortality Rate (per 1,000 Live Births)",
        uiOutput(ns("imr"), container = h2),
        p('Source: KDHS'),
        showcase = bs_icon("emoji-frown"),
        showcase_layout = "top right",
        theme = 'pink',
        class = "border-0 shadow-sm"
      ),
      value_box(
        "Stunting in Children Under 5 (%)",
        uiOutput(ns("stunt"), container = h2),
        p('Source: KDHS'),
        showcase = bs_icon("bar-chart"),
        showcase_layout = "top right",
        theme = 'pink',
        class = "border-0 shadow-sm"
      ),
      value_box(
        "Teenage Pregnancy Rate (%)",
        uiOutput(ns("tpr"), container = h2),
        p('Source: KDHS'),
        showcase = bs_icon("person-fill-up"),
        showcase_layout = "top right",
        theme = 'pink',
        class = "border-0 shadow-sm"
      ),
      value_box(
        "Under 5 Mortality Rate (per 1,000 Live Births)",
        uiOutput(ns("umr"), container = h2),
        p('Source: KDHS'),
        showcase = bs_icon("emoji-dizzy"),
        showcase_layout = "top right",
        theme = 'pink',
        class = "border-0 shadow-sm"
      )
    )
  )
}

#' score_card Server Functions
#'
#' @noRd
mod_score_card_server <- function(id, cache){
  stopifnot(is.reactive(cache))

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    open_indicator_data <- reactive({
      get_indicator_data(cache()$county, cache()$year_type, cache()$aggregation_level)
    })

    summarised_data <- reactive({
      req(cache()$year, cache()$aggregation_level)

      year_col <- cache()$year_type
      year_val <- resolve_year_value(year_col, cache()$year)

      req(year_val)

      open_indicator_data() %>%
        filter_by_year_county(cov_fic_adj, cov_sba_adj, inst_mmr_adj,
                              year_col = year_col,
                              year_val = year_val,
                              selected_county = cache()$county,
                              agg_unit = cache()$aggregation_unit,
                              agg_val = cache()$aggregation_level)
    })

    kdhs_adjusted_data <- reactive({
      req(cache()$county)
      kdhs_data %>%
        filter(county == cache()$county)
    })

    output$fic <- renderUI({
      if (is.null(summarised_data()) || nrow(summarised_data()) == 0) {
        return(HTML("&ndash;"))
      }
      paste0(scales::number(summarised_data()$cov_fic_adj), '%')
    })

    output$sba <- renderUI({
      if (is.null(summarised_data()) || nrow(summarised_data()) == 0) {
        return(HTML("&ndash;"))
      }
      paste0(scales::number(summarised_data()$cov_sba_adj), '%')
    })

    output$mmr_inst <- renderUI({
      if (is.null(summarised_data()) || nrow(summarised_data()) == 0) {
        return(HTML("&ndash;"))
      }
      scales::number(summarised_data()$inst_mmr_adj)
    })

    output$mmr <- renderUI({
      if (nrow(kdhs_adjusted_data()) == 0) {
        return(HTML("&ndash;"))
      }
      if (is.na(kdhs_adjusted_data()$mmr)) {
        return(HTML("&ndash;"))
      }
      scales::number(kdhs_adjusted_data()$mmr)
    })

    output$imr <- renderUI({
      if (nrow(kdhs_adjusted_data()) == 0) {
        return(HTML("&ndash;"))
      }
      if (is.na(kdhs_adjusted_data()$imr)) {
        return(HTML("&ndash;"))
      }
      scales::number(kdhs_adjusted_data()$imr)
    })

    output$stunt <- renderUI({
      if (nrow(kdhs_adjusted_data()) == 0) {
        return(HTML("&ndash;"))
      }
      if (is.na(kdhs_adjusted_data()$stunting)) {
        return(HTML("&ndash;"))
      }
      paste0(scales::number(kdhs_adjusted_data()$stunting), '%')
    })

    output$tpr <- renderUI({
      if (nrow(kdhs_adjusted_data()) == 0) {
        return(HTML("&ndash;"))
      }
      if (is.na(kdhs_adjusted_data()$tpr)) {
        return(HTML("&ndash;"))
      }
      paste0(scales::number(kdhs_adjusted_data()$tpr), '%')
    })

    output$umr <- renderUI({
      if (nrow(kdhs_adjusted_data()) == 0) {
        return(HTML("&ndash;"))
      }
      if (is.na(kdhs_adjusted_data()$under5_deaths)) {
        return(HTML("&ndash;"))
      }
      scales::number(kdhs_adjusted_data()$under5_deaths)
    })

    output$trend <- renderPlot({
      req(trend_data())
      trend_data() %>%
        ggplot(aes(x = period)) +
        geom_line(aes(y = fic_per, color = "FIC %")) +
        geom_line(aes(y = sba_per, color = "SBA %")) +
        geom_line(aes(y = mmr_inst, color = "MMR Inst")) +
        labs(x = NULL, y = NULL, color = "Indicator") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

  })
}
