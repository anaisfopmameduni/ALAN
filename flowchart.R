# ============================================================
# Participant flowchart
# ============================================================

library(dplyr)
library(haven)
library(DiagrammeR)

# ------------------------------------------------------------
# Read source data for baseline exclusions
# ------------------------------------------------------------

requested <- haven::read_dta(
  "data/original_database/MCC_ALAN_obesity_analysis_requested_variables_20240918.dta",
  encoding = "latin1"
)

# Convert labelled Stata variables to their underlying numeric codes
raw <- requested %>%
  mutate(
    tipocancer_code = as.numeric(haven::zap_labels(tipocancer)),
    nightever_code = as.numeric(haven::zap_labels(nightever))
  )

# Check participant IDs are unique
stopifnot(!anyDuplicated(raw$id_study))

# Baseline source counts
n_source <- n_distinct(raw$id_study)

# The existing script defines cancer-free controls as tipocancer == 0
cancer_free <- raw %>%
  filter(tipocancer_code == 0)

n_cancer_free <- n_distinct(cancer_free$id_study)
n_cancer_cases_removed <- n_source - n_cancer_free

# Exclude known current/past night workers.
# Missing nightever values are retained, consistent with your current script.
eligible <- cancer_free %>%
  filter(is.na(nightever_code) | nightever_code != 1)

n_eligible <- n_distinct(eligible$id_study)
n_night_workers_removed <- n_cancer_free - n_eligible

# ------------------------------------------------------------
# Read prepared analysis datasets
# ------------------------------------------------------------

indoor_dat <- readRDS("data/cleaned_analysis_data_indoor.rds")
outdoor_dat <- readRDS("data/cleaned_analysis_data_outdoor.rds")

# Check IDs are present and unique
stopifnot("participant_id" %in% names(indoor_dat))
stopifnot("participant_id" %in% names(outdoor_dat))
stopifnot(!anyDuplicated(indoor_dat$participant_id))
stopifnot(!anyDuplicated(outdoor_dat$participant_id))

# Confirm required analysis variables exist
required_indoor_vars <- c(
  "participant_id",
  "bmi",
  "diabetes_original",
  "indoor_alan",
  "simplified_healthy_sleep_score",
  "age_years",
  "sex",
  "education"
)

required_outdoor_vars <- c(
  "participant_id",
  "bmi",
  "diabetes_original",
  "ms_gr_longest",
  "age_years",
  "sex",
  "education"
)


# ------------------------------------------------------------
# Count missingness by analysis population
# ------------------------------------------------------------

count_missing <- function(dat, outcome_diabetes, exposure_alan,
                          include_sleep = FALSE) {
  result <- dat %>%
    summarise(
      n = n_distinct(participant_id),
      bmi_missing = sum(is.na(bmi)),
      diabetes_missing = sum(is.na(.data[[outcome_diabetes]])),
      either_outcome_missing = sum(
        is.na(bmi) | is.na(.data[[outcome_diabetes]])
      ),
      alan_missing = sum(is.na(.data[[exposure_alan]])),
      age_missing = sum(is.na(age_years)),
      sex_missing = sum(is.na(sex)),
      education_missing = sum(is.na(education))
    )
  
  if (include_sleep) {
    result$sleep_missing <- sum(
      is.na(dat$simplified_healthy_sleep_score)
    )
  }
  
  result
}

indoor_counts <- count_missing(
  dat = indoor_dat,
  outcome_diabetes = "diabetes_original",
  exposure_alan = "indoor_alan",
  include_sleep = TRUE
)

outdoor_counts <- count_missing(
  dat = outdoor_dat,
  outcome_diabetes = "diabetes_original",
  exposure_alan = "ms_gr_longest",
  include_sleep = FALSE
)

n_indoor <- indoor_counts$n
n_outdoor <- outdoor_counts$n

# ------------------------------------------------------------
# Build flowchart
# ------------------------------------------------------------

flow <- DiagrammeR::grViz(sprintf("
digraph participant_flow {
  graph [layout = dot, rankdir = TB, nodesep = 0.35, ranksep = 0.45]
  node [
    shape = box,
    style = 'rounded,filled',
    fillcolor = '#F4F7FB',
    color = '#52677D',
    fontname = Arial,
    fontsize = 10,
    margin = '0.16,0.10'
  ]
  edge [color = '#52677D', arrowsize = 0.7]

  source [
    label = 'Requested source dataset\\nN = %s'
  ]

  cancer_free [
    label = 'Cancer cases excluded\\nRemoved: %s\\nCancer-free: n = %s'
  ]

  eligible [
    label = 'Current/past night workers excluded\\nRemoved: %s\\nEligible baseline population: n = %s'
  ]

  source -> cancer_free -> eligible

  indoor [
    label = 'Study population 1: indoor ALAN\\nAll regions\\nn = %s'
  ]

  outdoor [
    label = 'Study population 2: outdoor ALAN\\nBarcelona and Madrid\\nn = %s'
  ]

  eligible -> indoor
  eligible -> outdoor

  indoor_analysis [
    label = 'Indoor analysis variables\\n
BMI missing: %s\\n
Type 2 diabetes missing: %s\\n
Missing either outcome: %s\\n
Indoor ALAN missing: %s\\n
Baseline sleep (SHSS) missing: %s\\n
Age missing: %s\\n
Sex missing: %s\\n
Education missing: %s\\n
Other confounders: missing category used'
  ]

  outdoor_analysis [
    label = 'Outdoor analysis variables\\n
BMI missing: %s\\n
Type 2 diabetes missing: %s\\n
Missing either outcome: %s\\n
Outdoor ALAN missing: %s\\n
Age missing: %s\\n
Sex missing: %s\\n
Education missing: %s\\n
Other confounders: missing category used'
  ]

  indoor -> indoor_analysis
  outdoor -> outdoor_analysis
}
",
n_source,
n_cancer_cases_removed,
n_cancer_free,
n_night_workers_removed,
n_eligible,
n_indoor,
n_outdoor,
indoor_counts$bmi_missing,
indoor_counts$diabetes_missing,
indoor_counts$either_outcome_missing,
indoor_counts$alan_missing,
indoor_counts$sleep_missing,
indoor_counts$age_missing,
indoor_counts$sex_missing,
indoor_counts$education_missing,
outdoor_counts$bmi_missing,
outdoor_counts$diabetes_missing,
outdoor_counts$either_outcome_missing,
outdoor_counts$alan_missing,
outdoor_counts$age_missing,
outdoor_counts$sex_missing,
outdoor_counts$education_missing
))

flow

# Optional: save the interactive flowchart as HTML
dir.create("output", showWarnings = FALSE, recursive = TRUE)

htmlwidgets::saveWidget(
  flow,
  "output/participant_flow.html",
  selfcontained = TRUE
)