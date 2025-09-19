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

  iwalk(get_indicators(), function(items, cat_label) {
    iwalk(items, function(sp, item_label) {
      mod_generic_indicator_server(
        id = sp$id,
        cache = cache,
        var_cols = c(sp$id, str_glue('{sp$id}_adj')),
        target_val = sp$target,
        indicator_title = sp$short,
        unit = if (is.na(sp$unit)) '' else sp$unit
      )
    })
  })

  observe({
    session$sendCustomMessage("resizeCharts", list())
  })
}
