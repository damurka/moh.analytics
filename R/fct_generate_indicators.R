#' Generate Core Health Indicators
#'
#' This function calculates core indicators such as service coverage,
#' utilization, and ratios. It supports both raw and adjusted values,
#' grouped by year, month, and administrative level (e.g., county).
#'
#' @param .data A data.frame containing cleaned DHIS2 routine data.
#' @param level Aggregation level: "national" or "county".
#' @param period Time level: "fiscal_year", "year", or "month".
#'
#' @return A data.frame of aggregated and computed health indicators.
#'
#' @noRd
generate_indicators <- function(.data, level = c('national', 'county'), period = c('fiscal_year', 'year', 'fiscal_quarter', 'quarter', 'fiscal_month', 'month')) {

  check_required(.data)
  level <- arg_match(level)
  level_col <- switch(
    level,
    national = NULL,
    county = 'county'
  )

  period <- arg_match(period)
  period_col <- resolve_period_col(period = period)

  .data %>%
    summarise(
      across(-any_of(c('county', 'year', 'fiscal_year', 'quarter', 'fiscal_quarter', 'month', 'pe')), sum, na.rm = TRUE),
      .by = all_of(c(level_col, period_col))
    ) %>%
    mutate(
      # Unadjusted Indicators
      cov_sba = sba/est_deliveries * 100,
      csection_rate = csection/instdelivery * 100,
      inst_mmr = maternal_death/est_livebirths * 100000,
      stillbirth_rate = stillbirth/total_birth * 1000,
      fsb_rate = fresh_stillbirth/total_birth * 1000,
      maternal_death_audited = maternal_death_audit/maternal_death * 100,
      cov_low_bweigth = low_bweigth/total_birth * 100,
      cov_kmc = kmc / sum(preterm, low_bweigth) * 100,
      cov_cord_care = cord_care/livebirths * 100,
      cov_treated_pnuemonia = pnuemonia_treated / total_pehumonia * 100,
      cov_treated_diarhoea = diarrhea_treated / total_dehydration * 100,
      cov_vitamina = vitamina/pop_6_59 * 100,
      cov_underweight = total_underweight / pop_under5 * 100,
      cov_delayed_milestone = delayed_milestone /pop_under5 * 100,
      cov_anc1 = anc1/est_pregnancies * 100,
      cov_anc8 = anc8/est_pregnancies * 100,
      cov_fic = fic/pop_under1 * 100,
      cov_penta3 = penta3/pop_under1 * 100,
      cov_hpv2 = hpv2/pop_10_14 * 100,
      cov_cervical = cacx_screened/pop_24_49 * 100,
      cov_htn = hypertension/total_hypertension * 100,
      cov_sgbv = sgbv_72h/total_sgbv * 100,
      cov_viral_suppressed = viral_suppressed/total_vl * 100,
      rta = rta/total_opd * 100,
      opd_utilization_rate = total_opd/pop_total,
      bed_occupancy_rate = total_bed_days/total_actual_bed_days * 100,
      cov_penta1 = penta1/pop_under1 * 100,
      malaria_test_positivity = malaria_positive / total_malaria_test * 100,
      alos = medical_bed_days/total_medical_discharges,
      confirmed_malaria_rate = confirmed_malaria/pop_total * 1000,
      cov_mental_disorder = mental_disorder/pop_total * 100,
      cov_breastfed_ex = breasfed_ex/total_breastfed * 100,
      cov_stunted = stunted/pop_under5 * 100,

      # Adjusted Indicators
      cov_sba_adj = sba_adj/est_deliveries * 100,
      csection_rate_adj = csection_adj/instdelivery_adj * 100,
      inst_mmr_adj = maternal_death_adj/est_livebirths * 100000,
      stillbirth_rate_adj = stillbirth_adj/total_birth_adj * 1000,
      fsb_rate_adj = fresh_stillbirth_adj/total_birth_adj * 1000,
      maternal_death_audited_adj = maternal_death_audit_adj/maternal_death_adj * 100,
      cov_low_bweigth_adj = low_bweigth_adj/total_birth_adj * 100,
      cov_kmc_adj = kmc_adj / sum(preterm_adj, low_bweigth_adj) * 100,
      cov_cord_care_adj = cord_care_adj/livebirths_adj * 100,
      cov_treated_pnuemonia_adj = pnuemonia_treated_adj / total_pehumonia_adj * 100,
      cov_treated_diarhoea_adj = diarrhea_treated_adj / total_dehydration_adj * 100,
      cov_vitamina_adj = vitamina_adj/pop_6_59 * 100,
      cov_underweight_adj = total_underweight_adj / pop_under5 * 100,
      cov_delayed_milestone_adj = delayed_milestone_adj /pop_under5 * 100,
      cov_anc1_adj = anc1_adj/est_pregnancies * 100,
      cov_anc8_adj = anc8_adj/est_pregnancies * 100,
      cov_fic_adj = fic_adj/pop_under1 * 100,
      cov_penta3_adj = penta3_adj/pop_under1 * 100,
      cov_hpv2_adj = hpv2_adj/pop_10_14 * 100,
      cov_cervical_adj = cacx_screened_adj/pop_24_49 * 100,
      cov_htn_adj = hypertension_adj/total_hypertension_adj * 100,
      cov_sgbv_adj = sgbv_72h_adj/total_sgbv_adj * 100,
      cov_viral_suppressed_adj = viral_suppressed_adj/total_vl_adj * 100,
      rta_adj = rta_adj/total_opd_adj * 100,
      opd_utilization_rate_adj = total_opd_adj/pop_total,
      cov_penta1_adj = penta1_adj/pop_under1 * 100,
      malaria_test_positivity_adj = malaria_positive_adj / total_malaria_test_adj * 100,
      bed_occupancy_rate_adj = total_bed_days_adj/total_actual_bed_days_adj * 100,
      alos_adj = medical_bed_days_adj/total_medical_discharges_adj,
      confirmed_malaria_rate_adj = confirmed_malaria_adj/pop_total * 1000,
      cov_mental_disorder_adj = mental_disorder_adj/pop_total * 100,
      cov_breastfed_ex_adj = breasfed_ex_adj/total_breastfed_adj * 100,
      cov_stunted_adj = stunted_adj/pop_under5 * 100
    )
}
