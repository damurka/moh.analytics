resolve_period <- function(year_type, aggregation_level) {
  # extended logic for summarised_data
  if (aggregation_level == 'year') return(year_type)
  if (aggregation_level == 'quarter' && year_type == 'year') return('quarter')
  if (aggregation_level == 'quarter' && year_type == 'fiscal_year') return('fiscal_quarter')
  if (aggregation_level == 'month' && year_type == 'fiscal_year') return('fiscal_month')
  return('month')
}

resolve_year_value <- function(year_type, year) {
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
  cached_generate_indicators(level = level, period = period)
}

# helper: filter by year and county
filter_by_year_county <- function(.data, ..., year_col, year_val, include_month = FALSE, selected_county = NULL, agg_val = NULL, agg_unit = NULL) {
  col_exprs <- rlang::enquos(...)
  month_col <- if (include_month) 'month' else NULL

  df <- .data %>%
    filter(
      !!sym(year_col) == year_val,
      if (is.null(selected_county) || is_national(selected_county)) TRUE else county == selected_county
    ) %>%
    select(any_of(c('county', year_col, month_col)), !!!col_exprs) %>%
    mutate(across(c(!!!col_exprs), round, digits = 1))

  if (!is.null(agg_val) && agg_val != 'year') {
    req(agg_unit)
    agg_unit_col <- resolve_period(year_col, agg_val)
    df <- df %>%
      filter(!!sym(agg_unit_col) == agg_unit)
  }

  return(df)
}
