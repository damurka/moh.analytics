#' health_status UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_health_status_ui <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

#' health_status Server Functions
#'
#' @noRd
mod_health_status_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}
