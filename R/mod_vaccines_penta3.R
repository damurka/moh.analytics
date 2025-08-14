#' vaccines_penta3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_vaccines_penta3_ui <- function(id) {
  mod_generic_indicator_ui(
    id = id,
    var_name = 'Penta 3',
    icon = 'shield-check',
    target_val = '95%'
  )
}

#' vaccines_penta3 Server Functions
#'
#' @noRd
mod_vaccines_penta3_server <- function(id, cache){
  mod_generic_indicator_server(
    id = id,
    cache = cache,
    var_cols = c('cov_penta3', 'cov_penta3_adj'),
    target_val = 95,
    indicator_title = 'Penta 3 Coverage'
  )
}
