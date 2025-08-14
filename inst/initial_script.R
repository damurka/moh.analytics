library(khisr)
library(dplyr)
library(tidyr)
library(readxl)
library(purrr)
library(lubridate)
library(stringr)

khis_cred(config_path = '../khisr/inst/secret/secret.json')

# elements <- read_csv('indicators.csv')

elements <- read_excel('inst/indicators.xlsx', 'service') %>%
  separate_wider_delim(element_id, delim = '.', names = c('element_id', 'category_id'), too_few = 'align_start')
pop_elements <- read_excel('inst/indicators.xlsx', 'population')
datasets_els <- read_excel('inst/indicators.xlsx', 'datasets')
orgs <- get_organisation_units(level %.eq% 2)
k_factors <-  elements %>%
  distinct(column_name, k_factor)

pop_cols <- c('wra', 'est_deliveries', 'pop_under5', 'pop_under1', 'est_livebirths',
              'pop_6_59', 'pop_total', 'est_pregnancies', 'pop_10_14')

pop_data <- get_analytics(
  dx %.d% pop_elements$element_id,
  pe %.d% c(2023:2025),
  ou %.d% 'LEVEL-2',
  timeout = 300
) %>%
  left_join(pop_elements, join_by(dx == element_id), relationship = 'many-to-many') %>%
  left_join(orgs, join_by(ou == id)) %>%
  mutate(
    year = as.integer(pe)
  ) %>%
  rename(county = name) %>%
  select(county, year, column_name, value) %>%
  pivot_wider(names_from = column_name, values_from = value)

datasets_data <- get_data_sets_by_level(datasets_els$element_id, '2023-07-01', '2025-06-30', level = 2) %>%
  mutate(dataset = str_trim(dataset)) %>%
  left_join(datasets_els, join_by(dataset == elenent_name)) %>%
  mutate(reporting_rate = actual_reports/expected_reports * 100) %>%
  select(county, year, month, reporting_rate, column_name) %>%
  pivot_wider(names_from = column_name, values_from = reporting_rate)


el_nocat <- elements %>%
  filter(is.na(category_id))

data <- get_analytics(
  dx %.d% el_nocat$element_id,
  # pe %.d% c('2023July','2024July'),
  pe %.d% c(202307:202312, 202401:202412, 202501:202506),
  ou %.d% 'LEVEL-2',
  # co %.d% 'all',
  # startDate = '2023-07-01',
  # endDate = '2025-06-30',
  timeout = 300
)

data <- data %>%
  left_join(el_nocat, join_by(dx == element_id), relationship = 'many-to-many') %>%
  left_join(orgs, join_by(ou == id)) %>%
  mutate(
    pe = ym(pe),
    month = month(pe, abbr = FALSE, label = TRUE),
    year = year(pe)
  ) %>%
  rename(county = name) %>%
  summarise(
    total = sum(value),
    .by = c(county, year, month, pe, column_name)
  ) %>%
  pivot_wider(
    names_from = column_name,
    values_from = total
  ) %>%
  arrange(county, year, month)

el_cat <- elements %>%
  # separate_wider_delim(element_id, delim = '.', names = c('element_id', 'category_id'), too_few = 'align_start') %>%
  drop_na(category_id) %>%
  distinct(element_id, category_id, .keep_all = TRUE)

data1 <- get_analytics(
  dx %.d% el_cat$element_id,
  pe %.d% c(202307:202312, 202401:202412, 202501:202506),
  ou %.d% 'LEVEL-2',
  co %.d% 'all',
  timeout = 300
)

data1 <- data1 %>%
  right_join(el_cat, join_by(dx == element_id, co == category_id)) %>%
  left_join(orgs, join_by(ou == id)) %>%
  mutate(
    pe = ym(pe),
    month = month(pe, abbr = FALSE, label = TRUE),
    year = year(pe)
  ) %>%
  rename(county = name) %>%
  summarise(
    total = sum(value),
    .by = c(county, year, month, pe, column_name)
  ) %>%
  pivot_wider(
    names_from = column_name,
    values_from = total
  ) %>%
  arrange(county, year, month)

# Combine both parts
final_df <- data %>%
  left_join(data1, join_by(county, year, month, pe)) %>%
  left_join(datasets_data, join_by(county, year, month)) %>%
  left_join(pop_data, join_by(county, year)) %>%
  mutate(across(all_of(pop_cols), ~ .x /12)) %>%
  mutate(
    fiscal_year = as.integer(quarter(pe, fiscal_start = 7, type = "year.quarter")),
    fiscal_year = factor(str_glue("{fiscal_year-1}/{fiscal_year}")),
    fiscal_quarter = str_glue('Q{quarter(pe, fiscal_start = 7)}'),
    quarter = case_when(
      month %in% c('January', 'February', 'March') ~ 'Q1',
      month %in% c('April', 'May', 'June') ~ 'Q2',
      month %in% c('July', 'August', 'September') ~ 'Q3',
      month %in% c('October', 'November', 'December') ~ 'Q4',
      .default = NA
    )
  ) %>%
  relocate(county, year, fiscal_year, quarter, fiscal_quarter, month, pe) %>%
  # summarise(total = sum(chemo, na.rm = TRUE), .by = fiscal_year)
  adjust_data(k_factors)

levels <- c("national", "county")
periods <- c("fiscal_year", "year", "fiscal_quarter", "quarter", "fiscal_month", "month")

combos <- expand.grid(level = levels, period = periods, stringsAsFactors = FALSE)

khis_data <- map2(combos$level, combos$period, ~final_df %>% generate_indicators(level = .x, period = .y)) %>%
  set_names(paste(combos$level, combos$period, sep = "_"))

months_data <- final_df %>%
  distinct(year, fiscal_year, quarter, fiscal_quarter, month, pe)



# 11,12
county_12 <- final_df %>%
  generate_indicators(level = 'county') %>%
  select(any_of('county'), fiscal_year, opd_utilization_rate, bed_occupancy_rate, cov_penta1, cov_anc1, cov_sba, chemo, radio, cataract, csection_rate, ortho_assist, malaria_test_positivity, maternal_death_audited,
         inst_mmr, fsb_rate, alos, surgial_emergency, rta_death, opd_utilization_rate_adj, bed_occupancy_rate_adj, cov_penta1_adj, cov_anc1_adj, malaria_test_positivity_adj,
         cov_sba_adj, chemo_adj, radio_adj, cataract_adj, csection_rate_adj, ortho_assist_adj, maternal_death_audited_adj, inst_mmr_adj,
         fsb_rate_adj, alos_adj, surgial_emergency_adj, rta_death_adj) %>%
  pivot_longer(-any_of(c('county', 'fiscal_year')), names_to = 'indicator', values_to = 'values') %>%
  pivot_wider(names_from = fiscal_year, values_from = values)

# 13,14,15
kenya_12 <- final_df %>%
  generate_indicators(level = 'national') %>%
  select(any_of('county'), fiscal_year, cov_fic, cov_penta3, confirmed_malaria, confirmed_malaria_rate, cov_cervical, cov_hpv2, hba1c, cov_htn, cva, heart_failure, cbe, sgbv_72h, total_sgbv, cov_sgbv, rta, cov_viral_suppressed, cov_mental_disorder, snake_bites, dog_bites,
         cov_fic_adj, cov_penta3_adj, confirmed_malaria_adj, confirmed_malaria_rate_adj, cov_cervical_adj, cov_hpv2_adj, hba1c_adj, cov_htn_adj, cva_adj, heart_failure_adj, cbe_adj, sgbv_72h_adj, total_sgbv_adj, cov_sgbv_adj, rta_adj, cov_viral_suppressed_adj, cov_mental_disorder_adj, snake_bites_adj, dog_bites_adj) %>%
  pivot_longer(-any_of(c('county', 'fiscal_year')), names_to = 'indicator', values_to = 'values') %>%
  pivot_wider(names_from = fiscal_year, values_from = values) %>% print(n = 50)

# 16,17,18
kenya_12 <- final_df %>%
  generate_indicators(level = 'national') %>%
  select(any_of('county'), fiscal_year, cov_sba, instdelivery, inst_mmr, stillbirth_rate, fsb_rate, cov_anc8, maternal_death_audited, cov_low_bweigth, cov_kmc, cov_cord_care, cov_treated_pnuemonia, cov_treated_diarhoea, contraceptive, cov_vitamina, cov_underweight, cov_delayed_milestone, cov_breastfed_ex,
         cov_sba_adj, instdelivery_adj, inst_mmr_adj, stillbirth_rate_adj, fsb_rate_adj, cov_anc8_adj, maternal_death_audited_adj, cov_low_bweigth_adj, cov_kmc_adj, cov_cord_care_adj, cov_treated_pnuemonia_adj, cov_treated_diarhoea_adj, contraceptive_adj, cov_vitamina_adj, cov_underweight_adj, cov_delayed_milestone_adj, cov_breastfed_ex_adj) %>%
  pivot_longer(-any_of(c('county', 'fiscal_year')), names_to = 'indicator', values_to = 'values') %>%
  pivot_wider(names_from = fiscal_year, values_from = values)

kenya_12 %>%
  mutate(
    county = 'Kenya'
  ) %>%
  bind_rows(county_12) %>%
  relocate(county) %>%
  write_csv('16-18.csv')

