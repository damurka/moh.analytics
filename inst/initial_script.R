library(khisr)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)

khis_cred(config_path = 'khisr/inst/secret/secret.json')

# HTstlNrwDAH - MOH 717 Revised 2020 _Maternal deaths
# KDFtI6rqYwI - MOH 711 Rev 2020_Maternal deaths 10-14Years
# BYMSIbnUzXQ - MOH 711 Rev 2020_Maternal deaths 15-19Years
# AC1Iorxdijc - MOH 711 Rev 2020_Maternal deaths 20-24 Years
# dPRCstLVkZu - MOH 711 Rev 2020_Maternal deaths 25+ Years
# 
# HNl64GqRCd5 - MOH 717 Rev2020_Live births
# Ye28rnsRe3q - MOH 717 Revised 2020 - Still births
# UqKC1DJnymn - MOH 711 Live birth
# rYn8RCcHJg1 - MOH 711 Fresh Still Birth
# wZtDYVBa41h - MOH 711 Macerated still Birth
# 
# BA4dSFQINJq - MOH 711 Rev 2020_Children under five deaths
# 
# Fz0LzxMT1vV - MOH 711 Pregnant women completing 4 ANC visits
# 
# cV4qoKSYiBs - MOH 711 Client receiving Male condoms
# 
get_data_elements(id %.in% c('BQmcVE8fex4', 'uHM6lzLXDBd', 'yQFyyQBhXQf', 'DuBH6qPPdaO', 'otgQMOXuyIn', 'PggNwT09D3U', 'cV4qoKSYiBs', 'OiuBpHJw4kf', 'Fxb4iVJdw2g', 'paDQStynGGD', 'PgQIx7Hq1kp', 'NMCIxSeGpS3', 'Wv02gixbRpT', 'hRktPfPEegP', 'CJdFYcZ1zOq', 'TUHzoPGLM3t')) %>% 
  print(n = 600)

fp <- get_indicators(name %.like% 'family planning', fields = 'name, numerator') %>% 
  print(n = 100)

# tBPGQFhWB7M - MOH710 Fully Immunized Children(FIC) under 1 year
# pMbuvKvH4rg - Population surving infants (under 1 year)
get_data_elements(id %.in% c('tBPGQFhWB7M', 'pMbuvKvH4rg'))

data_elements <- tribble(
  ~element_id, ~element_name, ~element_type, ~category_id, ~ category_name, ~column_name,
  'tBPGQFhWB7M', 'MOH710 Fully Immunized Children(FIC) under 1 year', 'data_element', 'bCfwecaPx2W', '>1  Years', 'fic',
  'tBPGQFhWB7M', 'MOH710 Fully Immunized Children(FIC) under 1 year', 'data_element', 'ieuodyY5ybC', '<1 Years', 'fic',
  'pMbuvKvH4rg', 'Population surving infants (under 1 year)', 'population', 'NhSoXUMPK2K', 'default', 'under1',
  'Kx64gGqaFVq', 'MOH 711 Assisted vaginal delivery', 'data_element', 'NhSoXUMPK2K', 'default', 'sba',
  'sMqM8DwiAaj', 'MOH 711 Breach Delivery', 'data_element', 'NhSoXUMPK2K', 'default', 'sba',
  'rAZBTMa7Jy3', 'MOH 711 Caesarian Sections', 'data_element', 'NhSoXUMPK2K', 'default', 'sba',
  'jaPrPmor6WV', 'MOH 711 Normal Deliveries', 'data_element',	'NhSoXUMPK2K', 'default', 'sba',
  'tmlMpIqHimf', 'Estimated Deliveries', 'population', 'NhSoXUMPK2K', 'default', 'estimated_births',
  'KDFtI6rqYwI',' MOH 711 Rev 2020_Maternal deaths 10-14Years', 'data_element', 'NhSoXUMPK2K', 'default', 'maternal_death',
  'BYMSIbnUzXQ', 'MOH 711 Rev 2020_Maternal deaths 15-19Years', 'data_element', 'NhSoXUMPK2K', 'default', 'maternal_death',
  'AC1Iorxdijc', 'MOH 711 Rev 2020_Maternal deaths 20-24 Years', 'data_element', 'NhSoXUMPK2K', 'default', 'maternal_death',
  'dPRCstLVkZu', 'MOH 711 Rev 2020_Maternal deaths 25+ Years', 'data_element', 'NhSoXUMPK2K', 'default', 'maternal_death',
  'XoHnrLBL1qB', 'MOH 710 Vaccines and Immunisation Rev 2020', 'dataset', NA, NA, 'vacc_rr',
  'UpS2bTVcClZ', 'MOH 711 Integrated Summary Report: Reproductive & Child Health, Medical & Rehabilitation Services Rev 2020', 'dataset', NA, NA, 'anc_rr'
)

counties <- get_organisation_units(level %.eq% 2) %>% 
  rename(county = name)

population_elements <- data_elements %>% 
  filter(element_type == 'population')

population_analytics <- get_analytics(
  dx %.d% unique(population_elements$element_id),
  pe %.d% c(2020:2025),
  ou %.d% 'LEVEL-2'
) %>% 
  right_join(counties, join_by(ou == id)) %>% 
  left_join(population_elements, join_by(dx == element_id)) %>% 
  mutate(
    year = as.integer(pe),
  ) %>% 
  arrange(county, pe) %>% 
  select(county, column_name, year, value) %>% 
  pivot_wider(names_from = column_name, values_from = value)

d_elements <- data_elements %>% 
  filter(element_type == 'data_element')

elements_analytics <- get_analytics(
  dx %.d% unique(d_elements$element_id),
  pe %.d% c(202001:202012, 202101:202112, 202201:202212, 202301:202312, 202401:202412, 202501:202506),
  co %.d% unique(d_elements$category_id),
  ou %.d% 'LEVEL-2',
  timeout = 300
) %>% 
  right_join(counties, join_by(ou == id)) %>% 
  left_join(d_elements, join_by(dx == element_id, co == category_id)) %>% 
  summarise(
    value = sum(value, na.rm = TRUE),
    .by = c(county, pe, column_name)
  ) %>% 
  mutate(
    pe = ym(pe),
    year = year(pe),
    month = month(pe, label = TRUE, abbr = FALSE),
    quarter = as.integer(quarter(pe, fiscal_start = 7, type = "quarter")), 
    quarter = factor(str_glue("Q{quarter}")), 
    fiscal_year = as.integer(quarter(pe, fiscal_start = 7, type = "year.quarter")), 
    fiscal_year = factor(str_glue("{fiscal_year-1}/{fiscal_year}"))
  ) %>% 
  arrange(county, pe) %>%
  select(-pe) %>% 
  pivot_wider(names_from = column_name, values_from = value)

dataset_elements <- population_elements <- data_elements %>% 
  filter(element_type == 'dataset')

dataset <- get_analytics(
  dx %.d% paste0(dataset_elements$element_id, '.REPORTING_RATE'),
  pe %.d% c(202001:202012, 202101:202112, 202201:202212, 202301:202312, 202401:202412, 202501:202506),
  ou %.d% 'LEVEL-2'
) %>% 
  separate_wider_delim(dx, '.', names = c('dataset_id', NA)) %>% 
  left_join(counties, join_by(ou == id)) %>% 
  left_join(dataset_elements, join_by(dataset_id == element_id)) %>% 
  mutate(
    pe = ym(pe),
    month = month(pe, label = TRUE, abbr = FALSE),
    year = year(pe),
    quarter = as.integer(quarter(pe, fiscal_start = 7, type = "quarter")), 
    quarter = factor(str_glue("Q{quarter}")), 
    fiscal_year = as.integer(quarter(pe, fiscal_start = 7, type = "year.quarter")), 
    fiscal_year = factor(str_glue("{fiscal_year-1}/{fiscal_year}"))
  ) %>% 
  arrange(county, pe) %>% 
  select(county, year, month, fiscal_year, quarter, column_name, value) %>% 
  pivot_wider(names_from = column_name, values_from = value)

combined_data <- elements_analytics %>% 
  left_join(population_analytics, join_by(county, year)) %>% 
  left_join(dataset, join_by(county, year, month, fiscal_year, quarter))

readr::write_csv(combined_data, 'moh.analytics/data.csv')

indicator_groups <- list(
  anc = c('sba'),
  vacc = c('fic')
)

all_indicators <- list_c(indicator_groups)
k_defaults <- c(anc = 0.25, vacc = 0.25)
last_year <- 2025

combined_data <- combined_data %>% 
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
  select(-any_of(paste0(all_indicators, "_rr")))

combined_data %>% 
  summarise(
    across(-any_of(c('county', 'fiscal_year', 'quarter', 'year', 'month')), sum, na.rm = TRUE),
    .by = c(county, year)
  ) %>%
  mutate(
    # instlivebirths = sba,
    under1 = under1/12,
    estimated_births = estimated_births/12,
    fic_per = fic/under1 * 100,
    sba_per = sba/estimated_births * 100,
    mmr_inst = 100000 * maternal_death/sba
  ) %>% 
  select(-ends_with('med'), -ends_with('mad'), -ends_with('outlier5std')) %>% 
  print(n = 500)

robust_max <- function(x, fallback = NA) {
  finite_values <- x[is.finite(x)]
  
  if (length(finite_values) == 0) {
    return(fallback)
  } else {
    max(finite_values)
  }
}

