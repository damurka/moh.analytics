#' vaccines UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_vaccines_ui <- function(id) {
  ns <- NS(id)
  navset_card_tab(
    title = 'Vaccine Indicators',

    nav_panel(title = 'FIC', mod_vaccines_fic_ui(ns("fic_1"))),
    nav_panel(title = 'Penta 1', mod_vaccines_penta1_ui(ns("penta1_1"))),
    nav_panel(title = 'Penta 3', mod_vaccines_penta3_ui(ns("penta3_1"))),
    nav_panel(title = 'HPV 2', mod_vaccines_hpv2_ui(ns("hpv2_1")))
  )
}

#' vaccines Server Functions
#'
#' @noRd
mod_vaccines_server <- function(id, cache){
  stopifnot(is.reactive(cache))

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    mod_vaccines_fic_server("fic_1", cache)
    mod_vaccines_penta1_server("penta1_1", cache)
    mod_vaccines_penta3_server("penta3_1", cache)
    mod_vaccines_hpv2_server("hpv2_1", cache)
  })
}
