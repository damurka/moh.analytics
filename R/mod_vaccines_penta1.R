#' vaccines_penta1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_vaccines_penta1_ui <- function(id) {
  mod_generic_indicator_ui(
    id = id,
    var_name = 'Penta 1',
    icon = 'shield-check',
    target_val = '95%'
  )
}

#' vaccines_penta1 Server Functions
#'
#' @noRd
mod_vaccines_penta1_server <- function(id, cache){
  mod_generic_indicator_server(
    id = id,
    cache = cache,
    var_cols = c('cov_penta1', 'cov_penta1_adj'),
    target_val = 95,
    indicator_title = 'Penta 1 Coverage'
  )
}
