#' vaccines UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_vaccines_ui <- function(id) {
  ns <- NS(id)
  navset_card_tab(
    title = 'Vaccine Indicators',
    nav_panel(
      title = 'FIC',
      icon = bs_icon("shield-check"),

      layout_columns(
        fill = FALSE,
        # width = 1/3,
        # height = '200px',

        value_box(
          "Coverage (%)",
          uiOutput(ns("fic"), container = h2),
          showcase = bs_icon("shield-check"),
          showcase_layout = "top right",
          theme = 'primary',
          class = "border-0 shadow-sm"
        ),
        value_box(
          title = "Target (%)",
          value = h2('85%'),
          showcase = bs_icon("bullseye"),
          showcase_layout = "top right",
          theme = 'success',
          class = "border-0 shadow-sm"
        ),
        value_box(
          title = "Trend",
          value = uiOutput(ns("trend")),
          showcase = bs_icon("graph-up"),
          showcase_layout = "top right",
          theme = "info",
          class = "border-0 shadow-sm"
        )
      ),
      layout_columns(
        # width = 1/2,
        # height = '450px',

        card(
          full_screen = TRUE,
          card_header(
            class = "bg-secondary text-white",
            card_title("Month to Month Trends", class = "mb-0")
          ),
          card_body(
            class = "p-2",
            echarts4rOutput(ns("plot"), height = "400px")
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
            echarts4rOutput(ns("map"), height = "400px")
          )
        )
      )
    )
  )
}

#' vaccines Server Functions
#'
#' @noRd
mod_vaccines_server <- function(id, cache){
  stopifnot(is.reactive(cache))

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    target <- reactiveVal(85)

    data <- reactive({
      req(cache())
      level <- if (cache()$county == 'Kenya') 'national' else 'county'
      year_col <- cache()$year_type
      year_val <- cache()$year
      year_val <- suppressWarnings(if (year_col == "year") as.integer(year_val) else as.character(year_val))

      req(year_val)

      period <- if (year_col == 'year') 'month' else 'fiscal_month'

      cache()$khis_data %>%
        cached_generate_indicators(level = level, period = period) %>%
        filter(!!sym(year_col) == year_val,
               if (level == 'national') TRUE else county == cache()$county) %>%
        select(any_of(c('county', year_col)), month, cov_fic, cov_fic_adj) %>%
        mutate(
          across(starts_with('cov_fic'), round, digits = 1)
        )
    })

    summarised_data <- reactive({
      req(cache()$year, cache()$aggregation_level)

      year_col <- cache()$year_type
      year_val <- cache()$year
      year_val <- suppressWarnings(if (year_col == "year") as.integer(year_val) else as.character(year_val))
      agg_val <- cache()$aggregation_level
      agg_unit_val <- cache()$aggregation_unit


      df <- cache()$summarised_data %>%
        filter(if (cache()$county == 'Kenya') TRUE else county == cache()$county,
               !!sym(year_col) == year_val)

      if (agg_val != 'year') {
        req(agg_unit_val)
        agg_unit_col <- switch(
          agg_val,
          "month" = "month",
          "quarter" = if (year_col == "fiscal_year") "fiscal_quarter" else "quarter",
          NULL
        )

        df <- df %>%
          filter(if(is.null(agg_unit_col)) TRUE else !!sym(agg_unit_col) == agg_unit_val)
      }
      return(df)
    })

    shapefile <- reactive({
      shape <- st_read('inst/county.gpkg', 'county')
      sf_geojson(shape)
    })

    county_data <- reactive({
      req(cache())
      year_col <- cache()$year_type
      year_val <- cache()$year
      year_val <- suppressWarnings(if (year_col == "year") as.integer(year_val) else as.character(year_val))

      req(year_val)

      cache()$khis_data %>%
        cached_generate_indicators(level = 'county', period = year_col) %>%
        filter(!!sym(year_col) == year_val) %>%
        select(county, any_of(year_col), cov_fic, cov_fic_adj) %>%
        mutate(
          across(starts_with('cov_fic'), round, digits = 1)
        )
    })

    output$fic <- renderUI({
      if (is.null(summarised_data()) || nrow(summarised_data()) == 0) {
        return(HTML("&ndash;"))
      }
      paste0(scales::number(summarised_data()$cov_fic_adj), '%')
    })

    output$trend <- renderUI({
      if (is.null(summarised_data()) || nrow(summarised_data()) == 0) {
        return(HTML("&ndash;"))
      }
      val <- summarised_data()$cov_fic_adj - target()
      paste0(scales::number(val), '%')
    })

    output$map <- renderEcharts4r({
      req(county_data())

      county_data() %>%
        e_charts(county) %>%
        e_map_register("Kenya", shapefile()) %>%
        e_map(cov_fic_adj, map = "Kenya", nameProperty = 'county') %>%
        e_visual_map(cov_fic_adj, inRange = list(
          color = c('#d73027', '#fdae61', '#31a354')  # green, orange, red,
        )) %>%
        e_show_loading()
    })

    output$plot <- renderEcharts4r({
      req(data())

      title <- if (cache()$county == 'Kenya') {
        str_glue('National Monthly Trend in Full Child Immunisation - {cache()$year}')
      } else {
        str_glue('{cache()$county}: Monthly Full Child Immunsation Trend - {cache()$year}')
      }

      data() %>%
        e_charts(month) %>%
        e_line(cov_fic, name = 'Unadjusted') %>%
        e_line(cov_fic_adj, name = 'Adjusted') %>%
        e_mark_line(data = list(yAxis = target()),
                    lineStyle = list(color = "red", type = "dashed"),
                    label = list(formatter = str_glue("Target: {target()}%"))) %>%
        e_labels() %>%
        e_theme("macarons") %>%
        e_title(title) %>%
        e_legend(orient = "horizontal", bottom = 0, left = "center", selected = list('Unadjusted' = FALSE, 'Adjusted' = TRUE)) %>%
        e_tooltip(trigger = "axis") %>%
        e_toolbox() %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_toolbox_feature(feature = "dataView") %>%
        e_toolbox_feature(
          feature = "magicType",
          type = list("line", "bar")
        ) %>%
        e_show_loading()
    })
  })
}
