#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    page_navbar(
      theme = bs_theme(
        version = 5,
        preset = 'bootstrap',
        # bootswatch = "flatly",
        primary = "#2c3e50",
        secondary = "#3498db",
        font_scale = 0.8
      ),
      title = 'Analytics Dashboard',
      sidebar = mod_sidebar_ui("sidebar_1"),

      nav_panel("Score Card", mod_score_card_ui("score_card_1")),
      nav_panel("Health Status", mod_health_status_ui("health_status_1")),
      nav_panel("Vaccines",  mod_vaccines_ui("vaccines_1"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "moh.analytics"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
