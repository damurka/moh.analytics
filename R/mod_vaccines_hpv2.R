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
    titles = list(
      map = str_glue('Distribution of HPV 2 Coverage in Kenya by County, {cache()$year}'),
      plot = if (is_national(cache()$county)) {
        str_glue('National Monthly Trend in HPV 2 Coverage - {cache()$year}')
      } else {
        str_glue('{cache()$county}: Monthly HPV 2 Coverage Trend - {cache()$year}')
      }
    )
  )
}
