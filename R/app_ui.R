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
    # Your application UI logic
    page_navbar(
      theme = bs_theme(
        bootswatch = "minty",
        base_font = font_google("Inter")
      ),
      title = 'Analytics Dashboard',
      collapsible = TRUE,
      nav_panel(
        "Dashboard",
        layout_sidebar(
          sidebar = sidebar(
            title = tooltip(
              span(
                "Controls",
                bs_icon("info-circle-fill"),
                class = "sidebar-title"
              ),
              "Controls used to select data in the dashboard"
            ),
            selectizeInput(
              'county',
              label = 'County',
              choices = NULL,
              selected = NULL
            ),
            radioButtons(
              inputId = "year_type",
              label = "Year Type",
              choices = c("Fiscal Year" = "fiscal_year", "Calendar Year" = "year"),
              inline = TRUE
            ),
            selectInput(
              inputId = "year",
              label = "Year",
              choices = NULL
            ),
            selectInput(
              inputId = "agg_level",
              label = "Aggregation Level",
              choices = c("Month" = "month", "Quarter" = "quarter", "Year" = "year")
            ),
            uiOutput("agg_unit_ui")
          ),
          layout_column_wrap(
            width = 1 / 4,
            fill = FALSE,
            value_box(
              "Full immunized Children (%)",
              uiOutput("fic", container = h2),
              p('Source: KHIS'),
              showcase = bs_icon("shield-check"),
              showcase_layout = "top right",
              theme = 'teal'
            ),
            value_box(
              "Skilled Birth Attendance (%)",
              uiOutput("sba", container = h2),
              p('Source: KHIS'),
              showcase = bs_icon("person-heart"),
              showcase_layout = "top right",
              theme = 'teal'
            ),
            value_box(
              "Institutional Maternal Mortality Rate (per 100,000)",
              uiOutput("mmr_inst", container = h2),
              p('Source: KHIS'),
              showcase = bs_icon("hospital"),
              showcase_layout = "top right",
              theme = 'pink'
            ),
            value_box(
              "Maternal mortality Rate (per 100,000)",
              uiOutput("mmr", container = h2),
              p('Source: KDHS'),
              showcase = bs_icon("heart-pulse"),
              showcase_layout = "top right",
              theme = 'pink'
            )
          ),
          layout_column_wrap(
            width = 1 / 4,
            fill = FALSE,
            value_box(
              "Infant mortality Rate (per 1,000 Live Births)",
              uiOutput("imr", container = h2),
              p('Source: KDHS'),
              showcase = bs_icon("emoji-frown"),
              showcase_layout = "top right",
              theme = 'pink'
            ),
            value_box(
              "Stunting in Children Under 5 (%)",
              uiOutput("stunt", container = h2),
              p('Source: KDHS'),
              showcase = bs_icon("bar-chart"),
              showcase_layout = "top right",
              theme = 'pink'
            ),
            value_box(
              "Teenage Pregnancy Rate (%)",
              uiOutput("tpr", container = h2),
              p('Source: KDHS'),
              showcase = bs_icon("person-fill-up"),
              showcase_layout = "top right",
              theme = 'pink'
            ),
            value_box(
              "Under 5 Mortality Rate (per 1,000 Live Births)",
              uiOutput("umr", container = h2),
              p('Source: KDHS'),
              showcase = bs_icon("emoji-dizzy"),
              showcase_layout = "top right",
              theme = 'pink'
            )
          ),
          layout_column_wrap(
            width = 1,
            class = "mt-3",
            card(
              full_screen = TRUE,
              card_header(
                "Trends",
                class = "d-flex justify-content-between align-items-center"
              ),
              plotOutput("trend")
            )
          )
        )
      )
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
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
