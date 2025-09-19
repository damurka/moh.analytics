#' indicators
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
get_indicators <- function() {
  list(
    'Service Utilization' = list(
      'OPD per capita utilization rate' = list(
        id = 'opd_utilization_rate',
        short = 'OPD Utilization Rate',
        icon = 'activity',
        target = 2,
        units = NA
      ),
      '% Bed occupancy rate' = list(
        id = 'bed_occupancy_rate',
        short = 'Bed occupancy',
        icon = 'hospital',
        target = 75,
        unit = '%'
      ),
      'Coverage of ANC 1' = list(
        id = 'cov_anc1',
        short = 'ANC1 Coverage',
        icon = 'person',
        target = NA,
        unit = '%'
      ),
      '(%) of Deliveries conducted by Skilled Birth Attendants' =
        list(
          id = 'cov_sba',
          short = 'SBA',
          icon = 'person-check',
          target = 76,
          unit = '%'
        ),
      'Number of patients undergoing chemotherapy' =
        list(
          id = 'chemo',
          short = 'Patients undergoing Chemotherapy',
          icon = 'capsule-pill',
          target = NA,
          unit = NA
        ),
      'Number of patients undergoing radiotherapy' =
        list(
          id = 'radio',
          short = 'Patients undergoing Radiotherapy',
          icon = 'radioactive',
          target = NA,
          unit = NA
        ),
      'Number of Orthopedic Cases assisted in theatre' =
        list(
          id = 'ortho_assist',
          short = 'Orthopaedic Cases Assisted',
          icon = 'bandaid',
          target = NA,
          unit = NA
        ),
      'Number of patients undergoing cataract surgeries' =
        list(
          id = 'cataract',
          short = 'Cataracts Surgeries',
          icon = 'eye',
          target = NA,
          unit = NA
        )
    ),

    'QOC Indicators' = list(
      'Proportion of malaria test positivity rate' =
        list(
          id = 'malaria_test_positivity',
          short = 'Malaria TPR',
          icon = 'virus',
          target = 28,
          unit = '%'
        ),
      'Fresh Stillbirth rate per 1,000 births in institutions' =
        list(
          id = 'fsb_rate',
          short = 'FSB rate',
          icon = 'exclamation-triangle',
          target = 9.2,
          unit = 'per 1,000 births'
        ),
      'Average Length of stay (ALOS) Medical Ward (Days)' =
        list(
          id = 'alos',
          short = 'ALOS',
          icon = 'clock',
          target = 3,
          unit = 'Days'
        )
    ),

    'NCDs' = list(
      'Percentage of women aged 25−49 years screened for cervical cancer' =
        list(
          id = 'cov_cervical',
          short = 'Cervical Coverage',
          icon = 'clipboard-check',
          target = 45,
          unit = '%'
        ),
      'Number of diabetes patients tested for HBA1c on target (<7%)' =
        list(
          id = 'cov_hba1c',
          short = 'HBA1c on target',
          icon = 'droplet',
          target = NA,
          unit = NA
        ),
      'Proportion of hypertension clients not meeting blood pressure target (>140/90mmHg)' =
        list(
          id = 'cov_htn',
          short = 'Hypertension not Controlled',
          icon = 'heart-pulse',
          target = 34,
          unit = '%'
        )
    ),

    'Violence and Injuries' = list(
      'SGBV total survivors seen' =
        list(
          id = 'total_sgbv',
          short = 'SGBV total',
          icon = 'shield',
          target = 3200,
          unit = NA
        ),
      'Proportion of SGBV clients seen within 72 hours' =
        list(
          id = 'cov_sgbv',
          short = 'SGBV seen within 72h',
          icon = 'clock',
          target = 64,
          unit = '%'
        ),
      'Road traffic injuries per 1,000 OPD visits' =
        list(
          id = 'rta',
          short = 'Road Traffic Injuries',
          icon = 'car-front',
          target = 2.1,
          unit = 'per 1,000 OPD Visits')
    ),

    'RMNCAH' = list(
      'Number of maternal deaths in health facilities per 100,000 live births (FMMR)' =
        list(
          id = 'inst_mmr',
          short = 'Institutional MMR',
          icon = 'hospital',
          target = 84,
          unit = 'per 100,000 deliveries'
        ),
      'Proportion of maternal deaths audited' =
        list(
          id = 'maternal_death_audited',
          short = 'Maternal audit',
          icon = 'clipboard-check',
          target = 95.8,
          unit = '%'
        ),
      'Percentage of Low birth weight in health facilities' =
        list(
          id = 'cov_low_bweigth',
          short = 'Low Birth Weight',
          icon = 'person',
          target = 5,
          unit = '%'
        ),
      'Proportion of LBW put on KMC (Proportion of babies initiated on KMC)' =
        list(
          id = 'cov_kmc',
          short = 'LBW on Kangaroo Care',
          icon = 'heart',
          target = 68,
          unit = '%'
        ),
      'Proportion of newborns who received chlorhexidine for cord care' =
        list(
          id = 'cov_cord_care',
          short = 'Cord care',
          icon = 'bandaid',
          target = 74,
          unit = '%'
        ),
      'Proportion of children under 5 years with pneumonia treated with amoxicillin DT(Facility)' =
        list(
          id = 'cov_treated_pnuemonia',
          short = 'Pneumonia treated',
          icon = 'lungs',
          target = 65,
          unit = '%'
        ),
      'Proportion of under-five treated for diarrhea with combination ZN-ORS' =
        list(
          id = 'cov_treated_diarhoea',
          short = 'Diarrhea treated',
          icon = 'capsule-pill',
          target = 40,
          unit = '%'
        )
    ),

    'Health Risk Factors' = list(
      'Proportion of children 6-59 months supplemented with Vitamin A' =
        list(
          id = 'cov_vitamina',
          short = 'Vitamin A',
          icon = 'capsule-pill',
          target = 84,
          unit = '%'
        ),
      'Percentage of children 0-5 (<6 months) months who were exclusively breastfed' =
        list(
          id = 'cov_breastfed_ex',
          short = 'Exclusive BF',
          icon = 'heart',
          target = 65,
          unit = '%'
        )
    ),

    'Collaboration with health-related sectors' = list(
      'Proportion of children under five with delayed developmental milestones' =
        list(
          id = 'cov_delayed_milestone',
          short = 'Delayed milestones',
          icon = 'puzzle',
          target = 20,
          unit = '%'
        ),
      'Percentage of children under 5yrs who are underweight' =
        list(
          id = 'cov_underweight',
          short = 'Underweight',
          icon = 'person',
          target = 8.5,
          unit = '%'
        )
    )
  )
}
