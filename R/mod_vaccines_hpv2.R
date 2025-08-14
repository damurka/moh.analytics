#' vaccines_hpv2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_vaccines_hpv2_ui <- function(id) {
  mod_generic_indicator_ui(
    id = id,
    var_name = 'HPV 2',
    icon = 'shield-check',
    target_val = '90%'
  )
}

#' vaccines_hpv2 Server Functions
#'
#' @noRd
mod_vaccines_hpv2_server <- function(id, cache){
  mod_generic_indicator_server(
    id = id,
    cache = cache,
    var_cols = c('cov_hpv2', 'cov_hpv2_adj'),
    target_val = 90,
    indicator_title = 'HPV 2 Coverage'
  )
}
