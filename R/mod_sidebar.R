#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sidebar_ui <- function(id) {
  ns <- NS(id)
  sidebar(
    selectizeInput(
      ns('county'),
      label = 'County',
      choices = NULL,
      selected = NULL
    ),
    radioButtons(
      inputId = ns("year_type"),
      label = "Year Type",
      choices = c("Fiscal Year" = "fiscal_year", "Calendar Year" = "year"),
      inline = TRUE
    ),
    selectInput(
      inputId = ns("year"),
      label = "Year",
      choices = NULL
    ),
    selectInput(
      inputId = ns("agg_level"),
      label = "Aggregation Level",
      choices = c("Month" = "month", "Quarter" = "quarter", "Year" = "year"),
      selected = 'year'
    ),
    uiOutput(ns("agg_unit_ui"))
  )
}

#' sidebar Server Functions
#'
#' @noRd
mod_sidebar_server <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    counties <- reactive({
      req(khis_data)
      county <- khis_data %>%
        select(county) %>%
        pull(county)

      c('Kenya', county)
    })

    years <- reactive({
      req(khis_data)
      khis_data %>%
        relocate(county, year, fiscal_year) %>%
        distinct(!!sym(input$year_type)) %>%
        arrange(desc(!!sym(input$year_type))) %>%
        pull(!!sym(input$year_type))
    })

    month_choices <- reactive({
      req(khis_data, input$year_type, input$year)

      year_col <- input$year_type
      year_val <- resolve_year_value(year_col, cache()$year)

      req(year_val)

      memoised_month_choices(year_col, year_val)
    })

    observeEvent(TRUE, {
      req(counties())
      updateSelectizeInput(session, 'county', choices = counties(), selected = 'Kenya')
    }, once = TRUE)

    observeEvent(input$county, {
      req(cache(), input$county)
      cache()$set_county(input$county)
    })

    observeEvent(input$year_type, {
      req(cache(), input$year_type)
      # freezeReactiveValue(input, "year")
      updateSelectInput(session, "year", choices = years())
      cache()$set_year_type(input$year_type)
    })

    observeEvent(input$year, {
      req(cache(), input$year)
      cache()$set_year(input$year)
    })

    observeEvent(input$agg_level, {
      req(cache(), input$agg_level)
      cache()$set_aggregation_level(input$agg_level)
    })

    observeEvent({
      input$agg_unit
      input$agg_level
    }, {
      req(input$agg_unit, input$agg_level)
      cache()$set_aggregation_unit(input$agg_unit)
    }, ignoreInit = TRUE)

    output$agg_unit_ui <- renderUI({
      req(input$agg_level, month_choices())

      if (input$agg_level == 'month') {
        selectInput(
          inputId = ns("agg_unit"),
          label = "Select Month",
          choices = setNames(month_choices()$value, month_choices()$label)
        )
      } else if (input$agg_level == "quarter") {
        selectInput(
          inputId = ns("agg_unit"),
          label = "Select Quarter",
          choices = paste0("Q", 1:4)
        )
      }
    })

  })
}
