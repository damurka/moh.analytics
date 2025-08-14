resolve_period <- function(year_type, aggregation_level) {
  year_type <- arg_match(year_type, c('fiscal_year', 'year'))
  aggregation_level <- arg_match(aggregation_level, c('year', 'quarter', 'month'))

  # extended logic for summarised_data
  if (aggregation_level == 'year') return(year_type)
  if (aggregation_level == 'quarter' && year_type == 'year') return('quarter')
  if (aggregation_level == 'quarter' && year_type == 'fiscal_year') return('fiscal_quarter')
  if (aggregation_level == 'month' && year_type == 'fiscal_year') return('fiscal_month')
  return('month')
}

resolve_period_col <- function(period = NULL, year_type = NULL, aggregation_level = NULL) {
  if (is.null(period) && is.null(year_type)) {
    cli::cli_abort(c('x' = '{.arg period} amd {.arg year_type} cannot be null'))
  }
  if (!is.null(period) && !is.null(year_type)) {
    cli::cli_abort(c('x' = 'choose between {.arg period} amd {.arg year_type}'))
  }
  if (is.null(period) && !is.null(year_type)) {
    aggregation_level <- if (is.null(aggregation_level)) 'year' else aggregation_level
    period <- resolve_period(year_type, aggregation_level)
  }

  period_col <- switch(
    period,
    fiscal_quarter = c('fiscal_year', 'fiscal_quarter'),
    quarter = c('year', 'quarter'),
    fiscal_month = c('fiscal_year', 'fiscal_quarter', 'month'),
    month = c('year', 'quarter', 'month'),
    period
  )
}

resolve_year_value <- function(year_type, year) {
  year_type <- arg_match(year_type, c('fiscal_year', 'year'))
  year_int <- suppressWarnings(as.integer(year))

  if (year_type == 'year' && !is.na(year_int)) {
    return(year_int)
  } else if (year_type == 'fiscal_year' && is.na(year_int)) {
    return(as.character(year))
  } else {
    return(NULL)
  }
}

is_national <- function(county) county == 'Kenya'

resolve_level <- function(county) {
  if (is_national(county)) 'national' else 'county'
}


# helper: get indicator data from cache
get_indicator_data <- function(county, year_type, aggregation_level) {
  level <- resolve_level(county)
  period <- resolve_period(year_type, aggregation_level)
  get_khis_data(level, period)
}

get_khis_data <- function(level, period) {
  khis_data[[str_glue('{level}_{period}')]]
}

# helper: filter by year and county
filter_by_year_county <- function(.data, ..., year_col, year_val, filter = TRUE, selected_county = NULL, agg_val = NULL, agg_unit = NULL) {
  col_exprs <- rlang::enquos(...)
  month_col <- resolve_period_col(year_type = year_col, aggregation_level = agg_val)

  df <- .data %>%
    filter(
      !!sym(year_col) == year_val,
      if (is.null(selected_county) || is_national(selected_county)) TRUE else county == selected_county
    ) %>%
    select(any_of(c('county', year_col, month_col)), !!!col_exprs) %>%
    mutate(across(c(!!!col_exprs), round, digits = 1))

  if (filter && !is.null(agg_val) && agg_val != 'year') {
    req(agg_unit)
    agg_unit_col <- resolve_period(year_col, agg_val)
    if (agg_unit_col == 'fiscal_month') {
      agg_unit_col = 'month'
    }
    df <- df %>%
      filter(!!sym(agg_unit_col) == agg_unit)
  }

  return(df)
}
