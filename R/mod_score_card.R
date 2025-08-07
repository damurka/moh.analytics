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
    layout_column_wrap(
      width = 1 / 4,
      fill = FALSE,
      value_box(
        "Full immunized Children (%)",
        uiOutput(ns("fic"), container = h2),
        p('Source: KHIS'),
        showcase = bs_icon("shield-check"),
        showcase_layout = "top right",
        theme = 'teal'
      ),
      value_box(
        "Skilled Birth Attendance (%)",
        uiOutput(ns("sba"), container = h2),
        p('Source: KHIS'),
        showcase = bs_icon("person-heart"),
        showcase_layout = "top right",
        theme = 'teal'
      ),
      value_box(
        "Institutional Maternal Mortality Rate (per 100,000)",
        uiOutput(ns("mmr_inst"), container = h2),
        p('Source: KHIS'),
        showcase = bs_icon("hospital"),
        showcase_layout = "top right",
        theme = 'pink'
      ),
      value_box(
        "Maternal mortality Rate (per 100,000)",
        uiOutput(ns("mmr"), container = h2),
        p('Source: KDHS'),
        showcase = bs_icon("heart-pulse"),
        showcase_layout = "top right",
        theme = 'pink'
      )
    ),
    layout_column_wrap(
      width = 1 / 4,
      fill = FALSE,
      value_box(
        "Infant mortality Rate (per 1,000 Live Births)",
        uiOutput(ns("imr"), container = h2),
        p('Source: KDHS'),
        showcase = bs_icon("emoji-frown"),
        showcase_layout = "top right",
        theme = 'pink'
      ),
      value_box(
        "Stunting in Children Under 5 (%)",
        uiOutput(ns("stunt"), container = h2),
        p('Source: KDHS'),
        showcase = bs_icon("bar-chart"),
        showcase_layout = "top right",
        theme = 'pink'
      ),
      value_box(
        "Teenage Pregnancy Rate (%)",
        uiOutput(ns("tpr"), container = h2),
        p('Source: KDHS'),
        showcase = bs_icon("person-fill-up"),
        showcase_layout = "top right",
        theme = 'pink'
      ),
      value_box(
        "Under 5 Mortality Rate (per 1,000 Live Births)",
        uiOutput(ns("umr"), container = h2),
        p('Source: KDHS'),
        showcase = bs_icon("emoji-dizzy"),
        showcase_layout = "top right",
        theme = 'pink'
      )
    ),
    layout_column_wrap(
      width = 1,
      class = "mt-3",
      card(
        full_screen = TRUE,
        card_header(
          "Trends",
          class = "d-flex justify-content-between align-items-center"
        ),
        plotOutput("trend")
      )
    )
  )
}

#' score_card Server Functions
#'
#' @noRd
mod_score_card_server <- function(id, khis, kdhs, county, year_type, year, agg_unit, agg_level){
  stopifnot(is.reactive(khis))
  stopifnot(is.reactive(kdhs))
  stopifnot(is.reactive(county))
  stopifnot(is.reactive(year_type))
  stopifnot(is.reactive(year))
  stopifnot(is.reactive(agg_unit))
  stopifnot(is.reactive(agg_level))

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    summarised_data <- reactive({
      req(county(), year_type(), year(), agg_unit(), agg_level())

      year_col <- year_type()
      year_val <- year()
      year_val <- if (year_col == "year") as.integer(year_val) else as.character(year_val)
      agg_val <- agg_level()
      agg_unit_val <- agg_unit()

      agg_unit_col <- switch(
        agg_val,
        "month" = "month",
        "quarter" = if (year_col == "fiscal_year") "quarter" else "calendar_quarter",
        NULL
      )

      req(year_val, agg_val)

      df <- khis() %>%
        filter(if (county() == 'Kenya') TRUE else county == county(), !!sym(year_col) == year_val)

      if (!is.null(agg_unit_col) && !is.null(agg_unit_val)) {
        df <- df %>%
          filter(!!sym(agg_unit_col) == agg_unit_val)
      }

      df %>%
        summarise(
          across(-any_of(c('county', 'year', 'fiscal_year', 'quarter', 'calendar_quarter', 'month')), sum, na.rm = TRUE)
        ) %>%
        mutate(
          under1 = under1/12,
          estimated_births = estimated_births/12,
          fic_per = fic/under1,
          sba_per = sba/estimated_births,
          mmr_inst = 100000 * maternal_death/sba
        )
    })

    trend_data <- reactive({
      req(county())

      khis() %>%
        filter(if (county() == "Kenya") TRUE else county == county()) %>%
        mutate(period = ym(paste0(year, ' ', month))) %>%
        summarise(
          under1 = sum(under1, na.rm = TRUE) / 12,
          estimated_births = sum(estimated_births, na.rm = TRUE) / 12,
          fic = sum(fic, na.rm = TRUE),
          sba = sum(sba, na.rm = TRUE),
          maternal_death = sum(maternal_death, na.rm = TRUE),
          .by = period
        ) %>%
        mutate(
          fic_per = fic / under1 * 100,
          sba_per = sba / estimated_births * 100,
          mmr_inst = 100000 * maternal_death / sba
        ) %>%
        arrange(period)
    })

    output$fic <- renderUI({
      if (nrow(summarised_data()) == 0) {
        return(HTML("&ndash;"))
      }
      scales::percent(summarised_data()$fic_per)
    })

    output$sba <- renderUI({
      if (nrow(summarised_data()) == 0) {
        return(HTML("&ndash;"))
      }
      scales::percent(summarised_data()$sba_per)
    })

    output$mmr_inst <- renderUI({
      if (nrow(summarised_data()) == 0) {
        return(HTML("&ndash;"))
      }
      scales::number(summarised_data()$mmr_inst)
    })

    output$mmr <- renderUI({
      if (nrow(kdhs()) == 0) {
        return(HTML("&ndash;"))
      }
      if (is.na(kdhs()$mmr)) {
        return(HTML("&ndash;"))
      }
      scales::number(kdhs()$mmr)
    })

    output$imr <- renderUI({
      if (nrow(kdhs()) == 0) {
        return(HTML("&ndash;"))
      }
      if (is.na(kdhs()$imr)) {
        return(HTML("&ndash;"))
      }
      scales::number(kdhs()$imr)
    })

    output$stunt <- renderUI({
      if (nrow(kdhs()) == 0) {
        return(HTML("&ndash;"))
      }
      if (is.na(kdhs()$stunting)) {
        return(HTML("&ndash;"))
      }
      paste0(scales::number(kdhs()$stunting), '%')
    })

    output$tpr <- renderUI({
      if (nrow(kdhs()) == 0) {
        return(HTML("&ndash;"))
      }
      if (is.na(kdhs()$tpr)) {
        return(HTML("&ndash;"))
      }
      paste0(scales::number(kdhs()$tpr), '%')
    })

    output$umr <- renderUI({
      if (nrow(kdhs()) == 0) {
        return(HTML("&ndash;"))
      }
      if (is.na(kdhs()$under5_deaths)) {
        return(HTML("&ndash;"))
      }
      scales::number(kdhs()$under5_deaths)
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
