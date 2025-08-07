#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  counties <- reactive({
    county <- khis_data %>%
      select(county) %>%
      pull(county)

    c('Kenya', county)
  })

  years <- reactive({
    req(input$year_type)
    khis_data %>%
      distinct(!!sym(input$year_type)) %>%
      arrange(desc(!!sym(input$year_type))) %>%
      pull(!!sym(input$year_type))
  })

  month_choices <- reactive({
    req(input$year_type, input$year)

    year_col <- input$year_type
    year_val <- input$year
    year_val <- if (year_col == "year") as.integer(year_val) else as.character(year_val)

    khis_data %>%
      filter(!!sym(year_col) == year_val) %>%
      distinct(month, year) %>%
      mutate(label = paste0(month, " ", year)) %>%
      arrange(desc(year), desc(match(month, month.name))) %>%
      transmute(label, value = month)
  })

  kdhs_adjusted_data <- reactive({
    req(input$county)
    kdhs_data %>%
      filter(county == input$county)
  })

  khis_adjusted_data <- reactive({

    indicator_groups <- list(
      anc = c('sba'),
      vacc = c('fic')
    )

    all_indicators <- list_c(indicator_groups)
    k_defaults <- c(anc = 0.25, vacc = 0.25)
    last_year <- robust_max(khis_data$year)

    khis_data %>%
      mutate(
        across(
          all_of(all_indicators),
          ~ get(paste0(names(keep(indicator_groups, ~ cur_column() %in% .x)), "_rr")),
          .names = "{.col}_rr"
        ),
      ) %>%
      mutate(
        across(
          all_of(paste0(all_indicators, "_rr")),
          ~ if_else(. < 75 | is.na(.), median(.[. >= 75 & . <= 100], na.rm = TRUE), .)
        ),
        .by = county
      ) %>%
      mutate(
        across(
          all_of(all_indicators),
          ~ {
            # Identify the main indicator group for the current sub-indicator
            group <- names(keep(indicator_groups, ~ cur_column() %in% .x))

            # Retrieve the rate column for the current group directly within cur_data()
            rate <- get(paste0(cur_column(), "_rr"))

            # Retrieve the k-value from the k_defaults list based on the group
            k_value <- k_defaults[[group]]

            # Apply the adjustment formula if the rate is not missing or zero
            if_else(
              !is.na(rate) & rate != 0,
              round(. * (1 + (1 / (rate / 100) - 1) * k_value), 1),
              .
            )
          }
        )
      ) %>%
      mutate(
        across(
          all_of(all_indicators),
          list(
            med = ~ {
              values <- if_else(year < last_year, ., NA_real_)
              med <- median(values, na.rm = TRUE)
              # med <- median(., na.rm = TRUE)
              if_else(is.na(med), robust_max(.), med) |> round(1)
            },
            mad = ~ {
              values <- if_else(year < last_year, ., NA_real_)
              mad_val <- mad(values, na.rm = TRUE)
              if_else(is.na(mad_val), robust_max(.), mad_val) |> round(1)
            }
          ),
          .names = "{.col}_{.fn}"
        ),
        .by = c(county)
      ) %>%
      mutate(
        # Step 2: Calculate outlier flags based on bounds
        across(
          all_of(all_indicators),
          ~ {
            med <- get(paste0(cur_column(), "_med"))
            mad <- get(paste0(cur_column(), "_mad"))

            lower_bound <- round(med - 5 * mad, 1)
            upper_bound <- round(med + 5 * mad, 1)

            if_else(!is.na(.) & (. < lower_bound | . > upper_bound), 1, 0)
          },
          .names = "{.col}_outlier5std"
        ),
        .by = county
      ) %>%
      mutate(
        across(
          all_of(all_indicators),
          ~ {
            outlier <- get(paste0(cur_column(), "_outlier5std"))
            med <- round(median(if_else(outlier != 1, ., NA_real_), na.rm = TRUE), 0)

            if_else(outlier == 1, robust_max(med), .)
          }
        ),
        across(
          all_of(all_indicators),
          ~ {
            med <- round(median(if_else(!is.na(.), ., NA_real_), na.rm = TRUE), 0)
            max_med <- robust_max(med)
            if_else(
              is.na(.) & !is.na(max_med),
              max_med,
              .
            )
          }
        ),
        .by = c(county, year)
      ) %>%
      mutate(
        calendar_quarter = case_when(
          month %in% c('January', 'February', 'March') ~ 'Q1',
          month %in% c('April', 'May', 'June') ~ 'Q2',
          month %in% c('July', 'August', 'September') ~ 'Q3',
          month %in% c('October', 'November', 'December') ~ 'Q4',
          .default = NA
        )
      ) %>%
      select(-any_of(paste0(all_indicators, "_rr")))
  })

  summarised_data <- reactive({
    req(input$county, input$year_type, input$year, input$agg_unit, input$agg_level)

    year_col <- input$year_type
    year_val <- input$year
    year_val <- if (year_col == "year") as.integer(year_val) else as.character(year_val)
    agg_val <- input$agg_level
    agg_unit_val <- input$agg_unit

    agg_unit_col <- switch(
      agg_val,
      "month" = "month",
      "quarter" = if (year_col == "fiscal_year") "quarter" else "calendar_quarter",
      NULL
    )

    req(year_val, agg_val)

    df <- khis_adjusted_data() %>%
      filter(if (input$county == 'Kenya') TRUE else county == input$county, !!sym(year_col) == year_val)

    if (!is.null(agg_unit_col) && !is.null(agg_unit_val)) {
      df <- df %>%
        filter(!!sym(agg_unit_col) == agg_unit_val)
    }

    df %>%
      summarise(
        across(-any_of(c('county', 'year', 'fiscal_year', 'quarter', 'calendar_quarter', 'month')), sum, na.rm = TRUE)
      ) %>%
      mutate(
        under1 = under1/12,
        estimated_births = estimated_births/12,
        fic_per = fic/under1,
        sba_per = sba/estimated_births,
        mmr_inst = 100000 * maternal_death/sba
      )
  })

  trend_data <- reactive({
    req(input$county, input$agg_level, input$year_type)

    khis_adjusted_data() %>%
      filter(if (input$county == "Kenya") TRUE else county == input$county) %>%
      mutate(period = ym(paste0(year, ' ', month))) %>%
      summarise(
        under1 = sum(under1, na.rm = TRUE) / 12,
        estimated_births = sum(estimated_births, na.rm = TRUE) / 12,
        fic = sum(fic, na.rm = TRUE),
        sba = sum(sba, na.rm = TRUE),
        maternal_death = sum(maternal_death, na.rm = TRUE),
        .by = period
      ) %>%
      mutate(
        fic_per = fic / under1 * 100,
        sba_per = sba / estimated_births * 100,
        mmr_inst = 100000 * maternal_death / sba
      ) %>%
      arrange(period)
  })

  observe({
    req(counties())
    updateSelectizeInput(session, 'county', choices = counties())
  })

  observe({
    req(years())
    updateSelectizeInput(session, 'year', choices = years())
  })

  output$fic <- renderUI({
    if (nrow(summarised_data()) == 0) {
      return(HTML("&ndash;"))
    }
    scales::percent(summarised_data()$fic_per)
  })

  output$sba <- renderUI({
    if (nrow(summarised_data()) == 0) {
      return(HTML("&ndash;"))
    }
    scales::percent(summarised_data()$sba_per)
  })

  output$mmr_inst <- renderUI({
    if (nrow(summarised_data()) == 0) {
      return(HTML("&ndash;"))
    }
    scales::number(summarised_data()$mmr_inst)
  })

  output$mmr <- renderUI({
    if (nrow(kdhs_adjusted_data()) == 0) {
      return(HTML("&ndash;"))
    }
    if (is.na(kdhs_adjusted_data()$mmr)) {
      return(HTML("&ndash;"))
    }
    scales::number(kdhs_adjusted_data()$mmr)
  })

  output$imr <- renderUI({
    if (nrow(kdhs_adjusted_data()) == 0) {
      return(HTML("&ndash;"))
    }
    if (is.na(kdhs_adjusted_data()$imr)) {
      return(HTML("&ndash;"))
    }
    scales::number(kdhs_adjusted_data()$mmr)
  })

  output$stunt <- renderUI({
    if (nrow(kdhs_adjusted_data()) == 0) {
      return(HTML("&ndash;"))
    }
    if (is.na(kdhs_adjusted_data()$stunting)) {
      return(HTML("&ndash;"))
    }
    paste0(scales::number(kdhs_adjusted_data()$stunting), '%')
  })

  output$tpr <- renderUI({
    if (nrow(kdhs_adjusted_data()) == 0) {
      return(HTML("&ndash;"))
    }
    if (is.na(kdhs_adjusted_data()$tpr)) {
      return(HTML("&ndash;"))
    }
    paste0(scales::number(kdhs_adjusted_data()$tpr), '%')
  })

  output$umr <- renderUI({
    if (nrow(kdhs_adjusted_data()) == 0) {
      return(HTML("&ndash;"))
    }
    if (is.na(kdhs_adjusted_data()$under5_deaths)) {
      return(HTML("&ndash;"))
    }
    scales::number(kdhs_adjusted_data()$under5_deaths)
  })

  output$agg_unit_ui <- renderUI({
    req(input$agg_level, month_choices())

    if (input$agg_level == 'month') {
      selectInput(
        inputId = "agg_unit",
        label = "Select Month",
        choices = setNames(month_choices()$value, month_choices()$label)
      )
    } else if (input$agg_level == "quarter") {
      selectInput(
        inputId = "agg_unit",
        label = "Select Quarter",
        choices = paste0("Q", 1:4)
      )
    }
  })

  output$trend <- renderPlot({
    req(trend_data())
    trend_data() %>%
      ggplot(aes(x = period)) +
        geom_line(aes(y = fic_per, color = "FIC %")) +
        geom_line(aes(y = sba_per, color = "SBA %")) +
        geom_line(aes(y = mmr_inst, color = "MMR Inst")) +
        labs(x = NULL, y = NULL, color = "Indicator") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

robust_max <- function(x, fallback = NA) {
  finite_values <- x[is.finite(x)]

  if (length(finite_values) == 0) {
    return(fallback)
  } else {
    max(finite_values)
  }
}
