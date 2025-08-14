#' generic_indicator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_generic_indicator_ui <- function(id, var_name, icon, target_val) {
  ns <- NS(id)
  tagList(
    layout_columns(
      fill = FALSE,

      value_box(
        str_glue("{var_name} Coverage (%)"),
        uiOutput(ns('value'), container = h2),
        showcase = bs_icon(icon),
        showcase_layout = "top right",
        theme = 'primary',
        class = "border-0 shadow-sm"
      ),
      value_box(
        title = str_glue("{var_name} Target (%)"),
        value = h2(target_val),
        showcase = bs_icon("bullseye"),
        showcase_layout = "top right",
        theme = 'success',
        class = "border-0 shadow-sm"
      ),
      value_box(
        title = "Target Achievement",
        value = uiOutput(ns("trend")),
        showcase = bs_icon("graph-up"),
        showcase_layout = "top right",
        theme = "info",
        class = "border-0 shadow-sm"
      )
    ),
    layout_columns(

      card(
        full_screen = TRUE,
        card_header(
          class = "bg-secondary text-white",
          card_title("Month to Month Trends", class = "mb-0")
        ),
        card_body(
          class = "p-2",
          withSpinner(echarts4rOutput(ns("plot")))
        )
      ),

      card(
        full_screen = TRUE,
        card_header(
          class = "bg-primary text-white",
          card_title("Geographic Distribution", class = "mb-0")
        ),
        card_body(
          class = "p-2",
          echarts4rOutput(ns("map"))
        )
      )
    )
  )
}

#' generic_indicator Server Functions
#'
#' @noRd
mod_generic_indicator_server <- function(id, cache, var_cols, target_val, indicator_title){
  stopifnot(is.reactive(cache))

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    target <- reactiveVal(target_val)

    monthly_data <- reactive({
      req(cache()$year)

      year_col <- cache()$year_type
      year_val <- resolve_year_value(year_col, cache()$year)

      req(year_val)

      get_indicator_data(cache()$county, cache()$year_type, 'month') %>%
        filter_by_year_county(year_col = year_col,
                              year_val = year_val,
                              filter = FALSE,
                              selected_county = cache()$county,
                              agg_val = 'month',
                              !!!var_cols)
    })

    county_data <- reactive({
      req(cache()$year)

      year_col <- cache()$year_type
      year_val <- resolve_year_value(year_col, cache()$year)

      req(year_val)

      get_khis_data(level = 'county', period = cache()$year_type) %>%
        filter_by_year_county(year_col = year_col,
                              year_val = year_val,
                              !!!var_cols)
    })

    summarised_data <- reactive({
      req(cache()$year, cache()$aggregation_level)

      year_col <- cache()$year_type
      year_val <- resolve_year_value(year_col, cache()$year)

      req(year_val)

      get_indicator_data(cache()$county, cache()$year_type, cache()$aggregation_level) %>%
        filter_by_year_county(year_col = year_col,
                              year_val = year_val,
                              selected_county = cache()$county,
                              starts_with('cov_fic'),
                              agg_unit = cache()$aggregation_unit,
                              agg_val = cache()$aggregation_level,
                              !!!var_cols)
    })

    output$value <- renderUI({
      if (is.null(summarised_data()) || nrow(summarised_data()) == 0) {
        return(HTML("&ndash;"))
      }
      val_col <- var_cols[grep('_adj', var_cols)]
      paste0(scales::number(summarised_data()[[val_col]]), '%')
    })

    output$trend <- renderUI({
      if (is.null(summarised_data()) || nrow(summarised_data()) == 0) {
        return(HTML("&ndash;"))
      }
      val_col <- var_cols[grep('_adj', var_cols)]
      val <- summarised_data()[[val_col]] - target()
      paste0(scales::number(val), '%')
    })

    output$map <- renderEcharts4r({
      req(county_data())

      val_col <- var_cols[grep('_adj', var_cols)]

      title <- str_glue('Distribution of {indicator_title} in Kenya by County, {cache()$year}')

      chart <- county_data() %>%
        e_charts(county) %>%
        e_map_register("Kenya", shapefile()) %>%
        e_map_(val_col, map = "Kenya", nameProperty = 'county', zoom = 1, center = c(37, 0), aspectScale = 1) %>%
        e_visual_map_(val_col, inRange = list(color = c('#d73027', '#fdae61', '#31a354'))) %>%
        e_title(title, textStyle = list(fontSize = 16), left = 10, right = 60) %>%
        e_toolbox() %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_toolbox_feature(feature = "dataView")

      if (!is_national(cache()$county)) {
        chart <- chart %>% e_map_select(name = cache()$county)
      }

      chart
    })

    output$plot <- renderEcharts4r({
      req(monthly_data())

      adj_col <- var_cols[grep('_adj', var_cols)]
      unadj_col <- var_cols[!grepl('_adj', var_cols)]

      ymax <- max(monthly_data()[[unadj_col]], monthly_data()[[adj_col]], target(), na.rm = TRUE) * 1.05

      title <- if (is_national(cache()$county)) {
        str_glue('National Monthly Trend in {indicator_title} - {cache()$year}')
      } else {
        str_glue('{cache()$county}: Monthly {indicator_title} Trend - {cache()$year}')
      }

      monthly_data() %>%
        e_charts(month) %>%
        e_line_(unadj_col, name = 'Unadjusted') %>%
        e_line_(adj_col, name = 'Adjusted') %>%
        e_y_axis(min = 0, max = round(ceiling(ymax / 10) * 10, 0)) %>%
        e_mark_line(data = list(yAxis = target()),
                    lineStyle = list(color = "red", type = "dashed"),
                    label = list(formatter = str_glue("Target: {target()}%"))) %>%
        e_labels() %>%
        e_theme("macarons") %>%
        e_title(title, textStyle = list(fontSize = 16), left = 10, right = 60) %>%
        e_legend(orient = "horizontal", bottom = 0, left = "center", selected = list('Unadjusted' = FALSE, 'Adjusted' = TRUE)) %>%
        e_tooltip(trigger = "axis") %>%
        e_toolbox() %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_toolbox_feature(feature = "dataView") %>%
        e_toolbox_feature(
          feature = "magicType",
          type = list("line", "bar")
        )
    })

  })
}
