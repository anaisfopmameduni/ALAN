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
  "data/raw/base_cohort.dta",
  encoding = "latin1"
)

# Convert labelled Stata variables to their underlying numeric codes
raw <- requested %>%
  mutate(
    tipocancer_code = as.numeric(haven::zap_labels(tipocancer)),
    nightever_code = as.numeric(haven::zap_labels(nightever))
  )

# Check participant IDs are present and unique
stopifnot(!anyNA(raw$id_study))
stopifnot(!anyDuplicated(raw$id_study))

n_source <- n_distinct(raw$id_study)

# Cancer-free controls
cancer_free <- raw %>%
  filter(tipocancer_code == 0)

n_cancer_free <- n_distinct(cancer_free$id_study)
n_cancer_cases_removed <- n_source - n_cancer_free

# Exclude known current/past night workers.
# Participants with missing night-work status are retained.
eligible <- cancer_free %>%
  filter(is.na(nightever_code) | nightever_code != 1)

n_eligible <- n_distinct(eligible$id_study)
n_night_workers_removed <- n_cancer_free - n_eligible

# ------------------------------------------------------------
# Read prepared analysis datasets
# ------------------------------------------------------------

indoor_dat <- readRDS("data/cleaned_analysis_data_indoor.rds")
outdoor_dat <- readRDS("data/cleaned_analysis_data_outdoor.rds")

# Check required variables
stopifnot(
  all(
    c("participant_id", "bmi", "diabetes_original", "indoor_alan") %in%
      names(indoor_dat)
  ),
  all(
    c("participant_id", "bmi", "diabetes_original", "ms_gr_longest") %in%
      names(outdoor_dat)
  )
)

# Check participant IDs
stopifnot(!anyNA(indoor_dat$participant_id))
stopifnot(!anyNA(outdoor_dat$participant_id))
stopifnot(!anyDuplicated(indoor_dat$participant_id))
stopifnot(!anyDuplicated(outdoor_dat$participant_id))

# The all-region indoor dataset must represent the eligible baseline.
if (!setequal(eligible$id_study, indoor_dat$participant_id)) {
  stop(
    "The eligible baseline IDs and all-region indoor dataset IDs differ. ",
    "Resolve this difference before using a single shared exclusion node."
  )
}

stopifnot(
  all(outdoor_dat$participant_id %in% indoor_dat$participant_id)
)

# ------------------------------------------------------------
# Shared BMI/diabetes exclusion before the population split
# ------------------------------------------------------------

bmi_missing <- is.na(indoor_dat$bmi)
diabetes_missing <- is.na(indoor_dat$diabetes_original)

n_bmi_removed <- sum(bmi_missing)
n_diabetes_removed <- sum(diabetes_missing)

# Participants missing both variables are counted only once.
n_bmi_diabetes_removed <- sum(bmi_missing | diabetes_missing)

complete_outcome_ids <- indoor_dat$participant_id[
  !bmi_missing & !diabetes_missing
]

n_after_bmi_diabetes <- length(complete_outcome_ids)

# Restrict both flowchart populations to the same retained IDs.
# These changes do not overwrite the saved analysis datasets.
indoor_dat <- indoor_dat %>%
  filter(participant_id %in% complete_outcome_ids)

outdoor_dat <- outdoor_dat %>%
  filter(participant_id %in% complete_outcome_ids)

# Check outcome availability is consistent across the two datasets.
stopifnot(
  !anyNA(outdoor_dat$bmi),
  !anyNA(outdoor_dat$diabetes_original)
)

n_indoor <- n_distinct(indoor_dat$participant_id)
n_outdoor <- n_distinct(outdoor_dat$participant_id)

# ------------------------------------------------------------
# ALAN exclusions after the BMI/diabetes exclusion
# ------------------------------------------------------------

indoor_alan_missing <- sum(is.na(indoor_dat$indoor_alan))
indoor_alan_available <- n_indoor - indoor_alan_missing

outdoor_alan_missing <- sum(is.na(outdoor_dat$ms_gr_longest))
outdoor_alan_available <- n_outdoor - outdoor_alan_missing

# ------------------------------------------------------------
# Build flowchart
# ------------------------------------------------------------

flow <- DiagrammeR::grViz(sprintf("
digraph participant_flow {
  graph [
    layout = dot,
    rankdir = TB,
    nodesep = 0.35,
    ranksep = 0.45
  ]

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
    label = 'Base cohort\\nn = %s'
  ]

  cancer_free [
    label = 'Cancer cases (%s) excluded\\nn = %s'
  ]

  eligible [
    label = 'Current/past night workers (%s) excluded\\nn = %s'
  ]

  bmi_diabetes [
    label = 'BMI or type 2 diabetes missing (%s) excluded\\n(BMI: %s; type 2 diabetes: %s)\\nn = %s'
  ]

  indoor [
    label = 'Indoor ALAN analysis population\\nAll regions\\nn = %s'
  ]

  outdoor [
    label = 'Outdoor ALAN analysis population\\nBarcelona and Madrid\\nn = %s'
  ]

  indoor_alan_available [
    label = 'Indoor ALAN missing (%s) excluded\\nn = %s'
  ]

  outdoor_alan_available [
    label = 'Outdoor ALAN missing (%s) excluded\\nn = %s'
  ]

  source -> cancer_free -> eligible -> bmi_diabetes

  bmi_diabetes -> indoor
  bmi_diabetes -> outdoor

  indoor -> indoor_alan_available
  outdoor -> outdoor_alan_available
}
",
n_source,
n_cancer_cases_removed,
n_cancer_free,
n_night_workers_removed,
n_eligible,
n_bmi_diabetes_removed,
n_bmi_removed,
n_diabetes_removed,
n_after_bmi_diabetes,
n_indoor,
n_outdoor,
indoor_alan_missing,
indoor_alan_available,
outdoor_alan_missing,
outdoor_alan_available
))

flow

# ------------------------------------------------------------
# Save interactive flowchart as HTML
# ------------------------------------------------------------

dir.create("output", showWarnings = FALSE, recursive = TRUE)

htmlwidgets::saveWidget(
  flow,
  "output/participant_flow.html",
  selfcontained = TRUE
)