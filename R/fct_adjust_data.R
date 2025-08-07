#' Adjust Indicator Data Using Reporting Rates and K-Factors
#'
#' Applies adjustment to health indicators using their associated reporting rates (RR)
#' and group-specific k-factors to account for underreporting or incompleteness.
#'
#' @details
#' This function:
#' - Validates presence of `_rr` columns (reporting rates)
#' - Identifies indicators by group (e.g., ANC, malaria, NCD)
#' - Calculates adjusted values using the formula:
#'
#'   `adjusted_value = raw_value * (1 + (1 / (rr / 100) - 1) * k)`
#'
#' - Replaces low or missing RRs with group-level medians when below 75%
#' - Produces columns with `_adj` suffix for each adjusted indicator
#'
#' K-factors should be provided as a data.frame with `column_name` and `k_factor`
#'
#' @param .data A data.frame with indicator values and reporting rate columns (e.g., `sba`, `sba_rr`)
#' @param k_factors A data.frame with columns: `column_name` (e.g. `sba`) and `k_factor` (numeric)
#'
#' @return A data.frame with new columns suffixed `_adj` for each adjusted indicator
#'
#' @keywords internal
#' @noRd
adjust_data <- function(.data, k_factors) {
  check_required(.data)
  check_required(k_factors)

  columns <- colnames(.data)

  if (sum(str_ends(columns, '_rr')) == 0) {
    cli::cli_abort(c('x' = 'Reporting rate columns required'))
  }

  indicator_groups <- list(
    malaria = c('bs_positive'),
    screening = c('cacx_screened', 'cbe'),
    ncd = c('hba1c', 'hypertension', 'cva', 'heart_failure', 'hypertension', 'total_hypertension'),
    anc = c('anc1', 'sba', 'maternal_death', 'fresh_stillbirth', 'total_sgbv', 'sgbv_72h', 'macerated_stillbirth', 'anc8', 'low_bweigth', 'kmc', 'cord_care', 'contraceptive', 'delayed_milestone', 'total_underweight', 'livebirths', 'total_sgbv', 'preterm'),
    opd_o5 = c('opd_over5', 'confirmed_malaria', 'mental_disorder', 'rta', 'first_attendance', 'reattendance'),
    opd_u5 = c('opd_under5', 'pnuemonia_treated', 'diarrhea_treated', 'total_pehumonia', 'total_dehydration'),
    vacc = c('penta1', 'fic', 'penta3', 'hpv2', 'vitamina'),
    eye = c('cataract'),
    lab = c('total_bs_test', 'rdt_test_positive', 'rdt_test_negative'),
    cancer = c('chemo', 'radio'),
    inpatient = c('medical_bed_days', 'total_bed_days', 'rta_death', 'maternal_death_audit', 'total_bed', 'total_medical_discharges')
  )

  all_indicators <- list_c(indicator_groups)
  last_year <- robust_max(.data$year)

  .data %>%
    mutate(
      across(
        any_of(all_indicators),
        ~ get(paste0(names(keep(indicator_groups, ~ cur_column() %in% .x)), "_rr")),
        .names = "{.col}_rr"
      ),
    ) %>%
    mutate(
      across(
        any_of(paste0(all_indicators, "_rr")),
        ~ if_else(. < 75 | is.na(.), median(.[. >= 75 & . <= 100], na.rm = TRUE), .)
      ),
      .by = county
    ) %>%
    mutate(
      across(
        any_of(all_indicators),
        ~ {
          # Identify the main indicator group for the current sub-indicator
          group <- names(keep(indicator_groups, ~ cur_column() %in% .x))

          # Retrieve the rate column for the current group directly within cur_data()
          rate <- get(paste0(cur_column(), "_rr"))

          # Retrieve the k-value from the k_defaults list based on the group
          # k_value <- k_defaults[[group]]
          k_value <- k_factors %>%
            filter(column_name == cur_column()) %>%
            pull(k_factor)

          # Apply the adjustment formula if the rate is not missing or zero
          if_else(
            !is.na(rate) & rate != 0,
            round(. * (1 + (1 / (rate / 100) - 1) * k_value), 1),
            .
          )
        },
        .names = '{.col}_adj'
      ),
    ) %>%
    select(-ends_with('_rr')) %>%
    mutate(
      across(
        any_of(all_indicators),
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
        any_of(all_indicators),
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
        any_of(all_indicators),
        ~ {
          outlier <- get(paste0(cur_column(), "_outlier5std"))
          med <- round(median(if_else(outlier != 1, ., NA_real_), na.rm = TRUE), 0)

          if_else(outlier == 1, robust_max(med), .)
        }
      ),
      across(
        any_of(all_indicators),
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
    )
}
