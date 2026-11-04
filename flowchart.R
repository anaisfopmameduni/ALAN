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

# Returns a label of the form:
# n after removing missing values (number removed)
make_available_label <- function(dat, variable) {
  n_missing <- sum(is.na(dat[[variable]]))
  n_available <- nrow(dat) - n_missing
  
  sprintf("%d (%d removed)", n_available, n_missing)
}

indoor_bmi_label <- make_available_label(indoor_dat, "bmi")
indoor_diabetes_label <- make_available_label(indoor_dat, "diabetes_original")
indoor_shss_label <- make_available_label(
  indoor_dat,
  "simplified_healthy_sleep_score"
)
indoor_alan_label <- make_available_label(indoor_dat, "indoor_alan")

outdoor_bmi_label <- make_available_label(outdoor_dat, "bmi")
outdoor_diabetes_label <- make_available_label(outdoor_dat, "diabetes_original")
outdoor_alan_label <- make_available_label(outdoor_dat, "ms_gr_longest")

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
label = 'Base cohort\\nN = %s'
]

cancer_free [
label = 'Cancer cases excluded\\nRemoved: %s\\nCancer-free: n = %s'
]

eligible [
label = 'Current/past night workers excluded\\nRemoved: %s\\nEligible baseline population: n = %s'
]

indoor [
label = 'Indoor ALAN analysis dataset\\nAll regions\\nn = %s'
]

outdoor [
label = 'Outdoor ALAN analysis dataset\\nBarcelona and Madrid\\nn = %s'
]

source -> cancer_free -> eligible
eligible -> indoor
eligible -> outdoor

indoor_analysis [
label = 'Indoor analysis variables\\n
BMI: %s\\n
Type 2 diabetes: %s\\n
SHSS: %s\\n
Indoor ALAN: %s'
]

outdoor_analysis [
label = 'Outdoor analysis variables\\n
BMI: %s\\n
Type 2 diabetes: %s\\n
Outdoor ALAN: %s'
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
indoor_bmi_label,
indoor_diabetes_label,
indoor_shss_label,
indoor_alan_label,
outdoor_bmi_label,
outdoor_diabetes_label,
outdoor_alan_label
))

flow

# Optional: save the interactive flowchart as HTML
dir.create("output", showWarnings = FALSE, recursive = TRUE)

htmlwidgets::saveWidget(
  flow,
  "output/participant_flow.html",
  selfcontained = TRUE
)
