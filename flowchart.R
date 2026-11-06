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

# ------------------------------------------------------------
# Count available participants after removing missing values
# ------------------------------------------------------------

# Count participants excluded due to BMI or diabetes missingness,
# separately and combined
make_bmi_diabetes_labels <- function(dat) {
  bmi_missing <- is.na(dat$bmi)
  diabetes_missing <- is.na(dat$diabetes_original)
  
  n_bmi_removed <- sum(bmi_missing)
  n_diabetes_removed <- sum(diabetes_missing)
  n_either_removed <- sum(bmi_missing | diabetes_missing)
  n_available <- nrow(dat) - n_either_removed
  
  list(
    available = n_available,
    removed_total = n_either_removed,
    removed_bmi = n_bmi_removed,
    removed_diabetes = n_diabetes_removed
  )
}

indoor_bmi_diabetes <- make_bmi_diabetes_labels(indoor_dat)
outdoor_bmi_diabetes <- make_bmi_diabetes_labels(outdoor_dat)

indoor_alan_missing <- sum(is.na(indoor_dat$indoor_alan))
indoor_alan_available <- nrow(indoor_dat) - indoor_alan_missing

outdoor_alan_missing <- sum(is.na(outdoor_dat$ms_gr_longest))
outdoor_alan_available <- nrow(outdoor_dat) - outdoor_alan_missing

n_indoor <- n_distinct(indoor_dat$participant_id)
n_outdoor <- n_distinct(outdoor_dat$participant_id)

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
label = 'Base cohort\\nn = %s'
]

cancer_free [
label = 'Cancer cases (%s) excluded\\nn = %s'
]

eligible [
label = 'Current/past night workers (%s) excluded\\nn = %s'
]

indoor [
label = 'Indoor ALAN analysis population\\nAll regions\\nn = %s'
]

outdoor [
label = 'Outdoor ALAN analysis population\\nBarcelona and Madrid\\nn = %s'
]

source -> cancer_free -> eligible
eligible -> indoor
eligible -> outdoor

indoor_bmi_diabetes [
label = 'BMI or type 2 diabetes missing (%s) excluded\\n(BMI: %s; type 2 diabetes: %s)\\nn = %s'
]

indoor_alan_available [
label = 'Indoor ALAN missing (%s) excluded\\nn = %s'
]

outdoor_bmi_diabetes [
label = 'BMI or type 2 diabetes missing (%s) excluded\\n(BMI: %s; type 2 diabetes: %s)\\nn = %s'
]

outdoor_alan_available [
label = 'Outdoor ALAN missing (%s) excluded\\nn = %s'
]

indoor -> indoor_bmi_diabetes -> indoor_alan_available
outdoor -> outdoor_bmi_diabetes -> outdoor_alan_available
}
",
n_source,
n_cancer_cases_removed,
n_cancer_free,
n_night_workers_removed,
n_eligible,
n_indoor,
n_outdoor,
indoor_bmi_diabetes$removed_total,
indoor_bmi_diabetes$removed_bmi,
indoor_bmi_diabetes$removed_diabetes,
indoor_bmi_diabetes$available,
indoor_alan_missing,
indoor_alan_available,
outdoor_bmi_diabetes$removed_total,
outdoor_bmi_diabetes$removed_bmi,
outdoor_bmi_diabetes$removed_diabetes,
outdoor_bmi_diabetes$available,
outdoor_alan_missing,
outdoor_alan_available
))

flow

# Optional: save the interactive flowchart as HTML
dir.create("output", showWarnings = FALSE, recursive = TRUE)

htmlwidgets::saveWidget(
  flow,
  "output/participant_flow.html",
  selfcontained = TRUE
)