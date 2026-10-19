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
      levels = c(1, 2, 3),
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
    outdoor_alan_group = case_when(
      to_numeric(ms_gr_longest_group) == 1 ~ "Very low",
      to_numeric(ms_gr_longest_group) %in% c(2, 3, 4) ~ "Intermediate",
      to_numeric(ms_gr_longest_group) == 5 ~ "Very high",
      TRUE ~ NA_character_
    ),
    
    outdoor_alan_group = factor(
      outdoor_alan_group,
      levels = c("Very low", "Intermediate", "Very high")
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
      levels = c(1, 2, 3),
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
    missing = "always",
    missing_text = "Missing",
    label = table_labels
  ) %>%
  add_n(
    statistic = "{N_nonmiss}",
    col_label = "**N**"
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
    missing = "always",
    missing_text = "Missing",
    label = table_labels
  ) %>%
  add_n(
    statistic = "{N_nonmiss}",
    col_label = "**N**"
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
# Merge indoor and outdoor descriptive tables
# ------------------------------------------------------------

table1_combined <- tbl_merge(
  tbls = list(table1_indoor, table1_outdoor),
  tab_spanner = c(
    "**Indoor ALAN levels**",
    "**Outdoor ALAN levels**"
  )
)

table1_combined

# ------------------------------------------------------------
# Export combined table as an editable landscape Word document
# ------------------------------------------------------------

library(flextable)
library(officer)

dir.create("output", showWarnings = FALSE, recursive = TRUE)

table1_combined %>%
  as_flex_table() %>%
  flextable::fontsize(size = 8, part = "all") %>%
  flextable::font(fontname = "Arial", part = "all") %>%
  flextable::autofit() %>%
  flextable::set_table_properties(
    layout = "autofit",
    width = 1
  ) %>%
  flextable::save_as_docx(
    path = "output/Table1_indoor_outdoor_ALAN.docx",
    pr_section = officer::prop_section(
      page_size = officer::page_size(
        orient = "landscape",
        width = 11.7,
        height = 8.3
      ),
      page_margins = officer::page_mar(
        top = 0.5,
        bottom = 0.5,
        left = 0.5,
        right = 0.5
      )
    )
  )
