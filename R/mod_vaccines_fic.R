#' fic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_vaccines_fic_ui <- function(id) {
  mod_generic_indicator_ui(
    id = id,
    var_name = 'FIC',
    icon = 'shield-check',
    target_val = '85%'
  )
}

#' fic Server Functions
#'
#' @noRd
mod_vaccines_fic_server <- function(id, cache){
  mod_generic_indicator_server(
    id = id,
    cache = cache,
    var_cols = c('cov_fic', 'cov_fic_adj'),
    target_val = 85,
    indicator_title = 'FIC Coverage'
  )
}
