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
    selectizeInput(
      inputId = ns("year"),
      label = "Year",
      choices = NULL
    ),
    selectizeInput(
      inputId = ns("agg_level"),
      label = "Aggregation Level",
      choices = c("Month" = "month", "Quarter" = "quarter", "Year" = "year"),
      selected = 'year'
    ),
    uiOutput(ns("agg_unit_ui")),
    selectizeInput(
      inputId = ns("analysis_type"),
      label = "Analysis Type",
      choices = c("Adjusted" = "adjusted", "Unadjusted" = "unadjusted"),
      selected = 'year'
    )
  )
}

#' sidebar Server Functions
#'
#' @noRd
mod_sidebar_server <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Reactive value to store the previous aggregation unit
    # This helps in preventing the observer from firing on initial render
    previous_agg_unit <- reactiveVal(NULL)

    counties <- reactive({
      county <- get_khis_data('county', 'fiscal_year') %>%
        select(county) %>%
        pull(county)

      c('Kenya', county)
    })

    years <- reactive({
      months_data %>%
        distinct(!!sym(input$year_type)) %>%
        arrange(desc(!!sym(input$year_type))) %>%
        pull(!!sym(input$year_type))
    })

    month_choices <- reactive({
      req(input$year_type, input$year)

      year_col <- input$year_type
      year_val <- resolve_year_value(year_col, cache()$year)

      req(year_val)

      months_data %>%
        filter(!!sym(year_col) == year_val) %>%
        distinct(month, year) %>%
        mutate(label = paste0(month, " ", year)) %>%
        arrange(desc(year), desc(match(month, month.name))) %>%
        transmute(label, value = month)
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
      cache()$set_year_type(input$year_type)
      updateSelectizeInput(session, "year", choices = years())
    })

    observeEvent(input$year, {
      req(cache(), input$year)
      cache()$set_year(input$year)
    })

    observeEvent(input$agg_level, {
      req(cache(), input$agg_level)
      cache()$set_aggregation_level(input$agg_level)
    })

    observeEvent(input$agg_unit, {
      req(cache(), input$agg_unit)
      # Check if the new value is different from the previous on

      # Get the previous value from the reactiveVal
      prev_value <- previous_agg_unit()

      if (is.null(prev_value) || isTRUE(input$agg_unit != prev_value)) {
        cache()$set_aggregation_unit(input$agg_unit)
        # Update the reactive value with the current value
        previous_agg_unit(input$agg_unit)
      }
    })

    observeEvent(input$analysis_type, {
      req(cache(), input$analysis_type)
      cache()$set_analysis_type(input$analysis_type)
    })

    output$agg_unit_ui <- renderUI({
      req(input$agg_level, month_choices())

      # Reset the previous_agg_unit when the aggregation level changes
      # This ensures the new input's value will be processed
      previous_agg_unit(NULL)
      level <- input$agg_level

      if (input$agg_level %in% c('month', 'quarter')) {
        selectizeInput(
          inputId = ns("agg_unit"),
          label = if (level == 'month') "Select Month" else "Select Quarter",
          choices = if (level == 'month') {
            setNames(month_choices()$value, month_choices()$label)
          } else {
            paste0("Q", 1:4)
          }
        )
      }
    })

  })
}
