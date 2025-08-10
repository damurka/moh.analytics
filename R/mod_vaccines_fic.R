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
    titles = list(
      map = str_glue('Distribution of FIC Coverage in Kenya by County, {cache()$year}'),
      plot = if (is_national(cache()$county)) {
        str_glue('National Monthly Trend in Full Child Immunisation - {cache()$year}')
      } else {
        str_glue('{cache()$county}: Monthly Full Child Immunsation Trend - {cache()$year}')
      }
    )
  )
}
