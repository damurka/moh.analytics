#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  cache <- init_CacheConnection()$reactive()

  mod_sidebar_server("sidebar_1", cache)
  mod_score_card_server("score_card_1", cache)
  mod_health_status_server("health_status_1")
  mod_vaccines_server("vaccines_1", cache)

  observe({
    session$sendCustomMessage("resizeCharts", list())
  })
}
