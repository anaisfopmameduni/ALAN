# ============================================================
# Descriptive table by indoor and outdoor ALAN exposure
# ============================================================

library(dplyr)
library(gtsummary)
library(gt)

# ------------------------------------------------------------
# Load prepared analysis datasets
# ------------------------------------------------------------

indoor_dat <- readRDS("data/cleaned_analysis_data_indoor.rds")
outdoor_dat <- readRDS("data/cleaned_analysis_data_outdoor.rds")

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

to_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

make_factor <- function(x, levels, labels) {
  factor(
    to_numeric(x),
    levels = levels,
    labels = labels
  )
}

# Make Missing an explicit category, always displayed last.
# This includes missing observations in categorical denominators.
include_missing <- function(x) {
  stopifnot(is.factor(x))
  
  existing_levels <- setdiff(levels(x), "Missing")
  values <- as.character(x)
  values[is.na(values)] <- "Missing"
  
  factor(
    values,
    levels = c(existing_levels, "Missing")
  )
}

categorical_vars <- c(
  "sex",
  "education",
  "socioeconomic_score",
  "bmi_who",
  "smoking_status",
  "physical_activity",
  "family_history_diabetes",
  "chronotype_3"
)

# ------------------------------------------------------------
# Prepare indoor-ALAN table data
# ------------------------------------------------------------

indoor_table_dat <- indoor_dat %>%
  mutate(
    indoor_alan_group = make_factor(
      indoor_alan,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Totally dark",
        "Almost dark",
        "Tenuous",
        "Bright"
      )
    ),
    
    sex = make_factor(
      sex,
      levels = c(1, 2),
      labels = c("Male", "Female")
    ),
    
    education = make_factor(
      education,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Less than primary school",
        "Primary school",
        "Secondary school",
        "University"
      )
    ),
    
    socioeconomic_score = make_factor(
      socioeconomic_score,
      levels = c(2, 3, 1),
      labels = c("Low", "Medium", "High")
    ),
    
    bmi_who = make_factor(
      bmi_who,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Underweight",
        "Normal weight",
        "Overweight",
        "Obese"
      )
    ),
    
    smoking_status = make_factor(
      smoking_status,
      levels = c(2, 0, 1),
      labels = c("Current", "Former", "Never")
    ),
    
    physical_activity = make_factor(
      physical_activity,
      levels = c(0, 1, 2, 3),
      labels = c(
        "0 min/week",
        "<500 min/week",
        "500–1,000 min/week",
        ">1,000 min/week"
      )
    ),
    
    family_history_diabetes = make_factor(
      family_history_diabetes,
      levels = c(2, 1),
      labels = c(
        "At least 1 relative",
        "No family history"
      )
    ),
    
    chronotype_3 = make_factor(
      chronotype_3,
      levels = c(1, 2, 3),
      labels = c("Morning", "Intermediate", "Evening")
    )
  ) %>%
  filter(!is.na(indoor_alan_group))

# ------------------------------------------------------------
# Prepare outdoor-ALAN table data
# ------------------------------------------------------------

outdoor_table_dat <- outdoor_dat %>%
  mutate(
    outdoor_alan_group = make_factor(
      ms_gr_longest_group,
      levels = c(1, 2, 3, 4, 5),
      labels = c(
        "Very low",
        "Low",
        "Intermediate",
        "High",
        "Very high"
      )
    ),
    
    sex = make_factor(
      sex,
      levels = c(1, 2),
      labels = c("Male", "Female")
    ),
    
    education = make_factor(
      education,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Less than primary school",
        "Primary school",
        "Secondary school",
        "University"
      )
    ),
    
    socioeconomic_score = make_factor(
      socioeconomic_score,
      levels = c(2, 3, 1),
      labels = c("Low", "Medium", "High")
    ),
    
    bmi_who = make_factor(
      bmi_who,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Underweight",
        "Normal weight",
        "Overweight",
        "Obese"
      )
    ),
    
    smoking_status = make_factor(
      smoking_status,
      levels = c(2, 0, 1),
      labels = c("Current", "Former", "Never")
    ),
    
    physical_activity = make_factor(
      physical_activity,
      levels = c(0, 1, 2, 3),
      labels = c(
        "0 min/week",
        "<500 min/week",
        "500–1,000 min/week",
        ">1,000 min/week"
      )
    ),
    
    family_history_diabetes = make_factor(
      family_history_diabetes,
      levels = c(2, 1),
      labels = c(
        "At least 1 relative",
        "No family history"
      )
    ),
    
    chronotype_3 = make_factor(
      chronotype_3,
      levels = c(1, 2, 3),
      labels = c("Morning", "Intermediate", "Evening")
    )
  ) %>%
  filter(!is.na(outdoor_alan_group))

# Include Missing in the denominator for all categorical variables.
# Do not apply this to age, UVI or the exposure-group variables.

indoor_table_dat <- indoor_table_dat %>%
  mutate(
    across(all_of(categorical_vars), include_missing)
  )

outdoor_table_dat <- outdoor_table_dat %>%
  mutate(
    across(all_of(categorical_vars), include_missing)
  )

# Total participants represented in each full exposure table.
# These exclude missing ALAN exposure, but include missing
# values in the characteristics being summarised.
n_indoor_alan <- nrow(indoor_table_dat)
n_outdoor_alan <- nrow(outdoor_table_dat)

# ------------------------------------------------------------
# Table labels
# ------------------------------------------------------------

table_labels <- list(
  age_years ~ "Age (years), mean (SD)",
  sex ~ "Sex",
  education ~ "Highest education",
  socioeconomic_score ~ "Socioeconomic score",
  uvi ~ "UVI, mean (SD)",
  bmi_who ~ "BMI (kg/m²)",
  smoking_status ~ "Smoking",
  physical_activity ~ "Physical activity",
  family_history_diabetes ~ "Family history of diabetes",
  chronotype_3 ~ "Chronotype"
)

# ------------------------------------------------------------
# Indoor ALAN descriptive table
# ------------------------------------------------------------

table1_indoor <- indoor_table_dat %>%
  select(
    age_years,
    sex,
    education,
    socioeconomic_score,
    uvi,
    bmi_who,
    smoking_status,
    physical_activity,
    family_history_diabetes,
    chronotype_3,
    indoor_alan_group
  ) %>%
  tbl_summary(
    by = indoor_alan_group,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ c(0, 1)
    ),
    type = list(
      all_of(categorical_vars) ~ "categorical",
      c(age_years, uvi) ~ "continuous"
    ),
    percent = "column",
    missing = "always",
    missing_text = "Missing",
    missing_stat = "{N_miss} ({p_miss}%)",
    label = table_labels
  ) %>%
  modify_table_body(
    ~ .x %>%
      filter(
        !(variable %in% categorical_vars & row_type == "missing")
      )
  ) %>%
  modify_header(label ~ "**Characteristics, n (%)**") %>%
  modify_header(all_stat_cols() ~ "**{level}**\nN = {n}") %>%
  bold_labels()

# ------------------------------------------------------------
# Outdoor ALAN descriptive table
# ------------------------------------------------------------

table1_outdoor <- outdoor_table_dat %>%
  select(
    age_years,
    sex,
    education,
    socioeconomic_score,
    uvi,
    bmi_who,
    smoking_status,
    physical_activity,
    family_history_diabetes,
    chronotype_3,
    outdoor_alan_group
  ) %>%
  tbl_summary(
    by = outdoor_alan_group,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ c(0, 1)
    ),
    type = list(
      all_of(categorical_vars) ~ "categorical",
      c(age_years, uvi) ~ "continuous"
    ),
    percent = "column",
    missing = "always",
    missing_text = "Missing",
    missing_stat = "{N_miss} ({p_miss}%)",
    label = table_labels
  ) %>%
  modify_table_body(
    ~ .x %>%
      filter(
        !(variable %in% categorical_vars & row_type == "missing")
      )
  ) %>%
  modify_header(label ~ "**Characteristics, n (%)**") %>%
  modify_header(all_stat_cols() ~ "**{level}**\nN = {n}") %>%
  bold_labels()


# ------------------------------------------------------------
# Display tables
# ------------------------------------------------------------

table1_indoor
table1_outdoor

# ------------------------------------------------------------
# Full table for supplements: all five outdoor categories
# ------------------------------------------------------------

exposure_spanners <- c(
  sprintf(
    "**Indoor ALAN levels (N = %s)**",
    format(n_indoor_alan, big.mark = ",", trim = TRUE)
  ),
  sprintf(
    "**Outdoor ALAN levels (N = %s)**",
    format(n_outdoor_alan, big.mark = ",", trim = TRUE)
  )
)

table1_combined_full <- tbl_merge(
  tbls = list(
    table1_indoor,
    table1_outdoor
  ),
  tab_spanner = exposure_spanners
)

# ------------------------------------------------------------
# Main table: hide outdoor Low and High columns
#
# Outdoor columns correspond to:
# stat_1 = Very low
# stat_2 = Low
# stat_3 = Intermediate
# stat_4 = High
# stat_5 = Very high
# ------------------------------------------------------------

# Verify column names and labels if needed.
show_header_names(table1_outdoor)

stopifnot(
  all(
    paste0("stat_", 1:5) %in%
      names(table1_outdoor$table_body)
  )
)

table1_outdoor_main <- table1_outdoor %>%
  modify_column_hide(
    columns = c(stat_2, stat_4)
  )

table1_combined_main <- tbl_merge(
  tbls = list(
    table1_indoor,
    table1_outdoor_main
  ),
  tab_spanner = exposure_spanners
)

table1_combined_main
table1_combined_full

table1_combined_main <- table1_combined_main %>%
  modify_source_note(
    source_note = paste(
      "Outdoor Low and High exposure categories are omitted",
      "from this display, not combined with Intermediate.",
      "The overall outdoor N includes all five exposure categories.",
      "The complete table is provided in the supplement."
    )
  )

# ------------------------------------------------------------
# Export main and supplementary tables to Word
# ------------------------------------------------------------

library(flextable)
library(officer)

dir.create("output", showWarnings = FALSE, recursive = TRUE)

export_table1 <- function(table, path) {
  table %>%
    as_flex_table() %>%
    flextable::fontsize(size = 8, part = "all") %>%
    flextable::font(fontname = "Arial", part = "all") %>%
    flextable::autofit() %>%
    flextable::set_table_properties(
      layout = "autofit",
      width = 1
    ) %>%
    flextable::save_as_docx(
      path = path,
      pr_section = officer::prop_section(
        page_size = officer::page_size(
          orient = "landscape",
          width = 8.3,
          height = 11.7
        ),
        page_margins = officer::page_mar(
          top = 0.5,
          bottom = 0.5,
          left = 0.5,
          right = 0.5
        )
      )
    )
}

export_table1(
  table1_combined_main,
  "output/Table1_indoor_outdoor_ALAN_main.docx"
)

export_table1(
  table1_combined_full,
  "output/Table1_indoor_outdoor_ALAN_supplement.docx"
)
