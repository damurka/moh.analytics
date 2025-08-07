#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  counties <- reactive({
    county <- khis_data %>%
      select(county) %>%
      pull(county)

    c('Kenya', county)
  })

  years <- reactive({
    req(input$year_type)
    khis_data %>%
      distinct(!!sym(input$year_type)) %>%
      arrange(desc(!!sym(input$year_type))) %>%
      pull(!!sym(input$year_type))
  })

  month_choices <- reactive({
    req(input$year_type, input$year)

    year_col <- input$year_type
    year_val <- input$year
    year_val <- if (year_col == "year") as.integer(year_val) else as.character(year_val)

    khis_data %>%
      filter(!!sym(year_col) == year_val) %>%
      distinct(month, year) %>%
      mutate(label = paste0(month, " ", year)) %>%
      arrange(desc(year), desc(match(month, month.name))) %>%
      transmute(label, value = month)
  })

  kdhs_adjusted_data <- reactive({
    req(input$county)
    kdhs_data %>%
      filter(county == input$county)
  })

  khis_adjusted_data <- reactive({

    khis_data %>%
      adjust_data(k_factors) %>%
      mutate(
        calendar_quarter = case_when(
          month %in% c('January', 'February', 'March') ~ 'Q1',
          month %in% c('April', 'May', 'June') ~ 'Q2',
          month %in% c('July', 'August', 'September') ~ 'Q3',
          month %in% c('October', 'November', 'December') ~ 'Q4',
          .default = NA
        )
      )
  })

  observe({
    req(counties())
    updateSelectizeInput(session, 'county', choices = counties())
  })

  observe({
    req(years())
    updateSelectizeInput(session, 'year', choices = years())
  })

  output$agg_unit_ui <- renderUI({
    req(input$agg_level, month_choices())

    if (input$agg_level == 'month') {
      selectInput(
        inputId = "agg_unit",
        label = "Select Month",
        choices = setNames(month_choices()$value, month_choices()$label)
      )
    } else if (input$agg_level == "quarter") {
      selectInput(
        inputId = "agg_unit",
        label = "Select Quarter",
        choices = paste0("Q", 1:4)
      )
    }
  })

  mod_score_card_server("score_card_1", khis_adjusted_data, kdhs_adjusted_data,
                        reactive(input$county), reactive(input$year_type), reactive(input$year),
                        reactive(input$agg_unit), reactive(input$agg_level))
  mod_health_status_server("health_status_1")

}
