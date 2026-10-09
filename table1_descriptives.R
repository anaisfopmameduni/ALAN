#flowchart with exclusion and missing



# ============================================================
# Descriptive table by indoor and outdoor ALAN exposure
# ============================================================

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(gt)
library(gtsummary)

# ------------------------------------------------------------
# Load prepared analysis datasets
# ------------------------------------------------------------

indoor_dat <- readRDS("data/cleaned_analysis_data_indoor.rds")
outdoor_dat <- readRDS("data/cleaned_analysis_data_outdoor.rds")


# Three outdoor ALAN groups:
# Very low = category 1
# Intermediate = categories 2, 3, and 4
# Very high = category 5

dat <- outdoor_dat %>%
  mutate(
    outdoor_alan_3 = case_when(
      as.character(ms_gr_longest_group) == "1" ~ "Very low",
      as.character(ms_gr_longest_group) %in% c("2", "3", "4") ~ "Intermediate",
      as.character(ms_gr_longest_group) == "5" ~ "Very high",
      TRUE ~ NA_character_
    ),
    outdoor_alan_3 = factor(
      outdoor_alan_3,
      levels = c("Very low", "Intermediate", "Very high")
    ),
    smoking_status = factor(
      smoking_status,
      levels = c("Current", "Former", "Never")
    ),
    chronotype_3 = factor(
      chronotype_3,
      levels = c(1, 2, 3),
      labels = c("Morning", "Intermediate", "Evening")
    )
  )

table1_indoor <- indoor_dat %>%
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
    indoor_alan
  ) %>%
  tbl_summary(
    by = indoor_alan,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ c(0, 1)
    ),
    missing = "ifany",
    missing_text = "Missing",
    label = list(
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
  ) %>%
  modify_header(label ~ "**Characteristics, n (%)**") %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Indoor ALAN levels**")

table1_outdoor <- dat %>%
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
    outdoor_alan_3
  ) %>%
  tbl_summary(
    by = outdoor_alan_3,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ c(0, 1)
    ),
    missing = "ifany",
    missing_text = "Missing",
    label = list(
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
  ) %>%
  modify_header(label ~ "**Characteristics, n (%)**") %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Outdoor ALAN levels**")


