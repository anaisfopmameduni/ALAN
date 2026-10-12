# Requirements
source(file.path("functions.R"))

required_packages <- c(
  "dplyr", "haven", "readr")
library(haven)
library(dplyr)
library(tidyr)
library(nnet)
library(splines)
library(broom)
library(labelled)


# Read in data
data_dir <- "data"
original_dir <- file.path(data_dir, "original_database")

read_dta_checked <- function(path, encoding = "windows-1252") {
  if (!file.exists(path)) {
    stop("File not found: ", normalizePath(path, mustWork = FALSE))
  }
  
  message("Reading: ", path)
  
  haven::read_dta(
    file = path,
    encoding = encoding
  )
}

main <- read_dta_checked(
  file.path(data_dir, "MCC ALAN BCNMDR Work.dta")
)

all_work <- read_dta_checked(
  file.path(data_dir, "MCC ALAN all WORK.dta")
)

requested_variables <- read_dta_checked(
  file.path(
    original_dir,
    "MCC_ALAN_obesity_analysis_requested_variables_20240918.dta"
  )
)

# ============================================================
# Build separate analysis datasets
# ============================================================

# Indoor: all provinces
indoor_dat <- all_work %>%
  filter(nightever != 1 | is.na(nightever))

# Outdoor: Barcelona and Madrid only
outdoor_dat <- main %>%
  filter(nightever != 1 | is.na(nightever))

# Check target sample sizes
stopifnot(nrow(indoor_dat) == 3420)
stopifnot(nrow(outdoor_dat) == 1475)

# Apply preprocessing separately
indoor_dat <- prepare_analysis_data(indoor_dat)
outdoor_dat <- prepare_analysis_data(outdoor_dat)

# ============================================================
# Final analysis datasets
# ============================================================

required_vars <- c(
  "id_study", "nightever", "area",
  "a3_sexe", "a4_edat", "education_basic_final",
  "score_SE", "bmi_actual", "diabetes", "waist_hip",
  "g13_tiempo_dormir", "g14_problemas_sue_o",
  "g15_list_probl_medic", "g20_siesta", "g21_tiempo_siesta",
  
  "a2a_hora_apagar_luces", "a2b_min_apagar_luces",
  "a3_min_conciliar", "a4a_hora_despertar",
  "a4b_min_despertar", "a8a_hora_apagar_luces",
  "a8b_min_apagar_luces", "a9_min_conciliar",
  "a10a_hora_despertar", "a10b_min_despertar",
  
  "fumador_presente",
  "mets_act_10_2_total_rec",
  "gra_fruits", "gra_vegetables", "t_energy",
  
  "uvi_longest", "ndvi_500_longest",
  "msf", "msf_c3", "msf_c5",
  "indoorlight_lab",
  "msibg_current", "msibg_longest",
  "msigr_longest", "impmsigr_longest",
  "melgr_longest", "impmelgr_longest",
  
  "msw", "sdw", "sjlrel",
  "sow", "sof", "sl_group",
  "fam_history_diabetes"
)

derived_vars <- c(
  "centre_num", "diabetes_new", "WHR_cat",
  "bmi_WHO", "bmi_2", "bmi_3", "bmi_cat", "obesity",
  "sleep_cat", "sleep_cat_3", "bedtime_group",
  "siesta_general", "siesta_60", "siesta_frecat",
  "siesta_ultimate",
  "MSIbg_group_c", "MSIbg_group_l",
  "MSIgr_group_l", "ImpMSIgr_group_l",
  "Melgr_group_l", "ImpMelgr_group_l",
  "ALAN_combined_norm", "ALAN_combined_cat",
  "SHSS", "SHSS_group",
  "sl_mean", "sleep_latency_w", "sleep_latency_f",
  "bed_time_new", "sjl_abs",
  "gra_fruits_group", "gra_vegetables_group",
  "t_energy_group", "uvi_longest_group"
)

final_vars <- unique(c(required_vars, derived_vars))


indoor_final <- build_final_dataset(indoor_dat)
outdoor_final <- build_final_dataset(outdoor_dat)

# ============================================================
# Complete-case modelling datasets
# ============================================================

indoor_model <- build_model_dataset(indoor_final)
outdoor_model <- build_model_dataset(outdoor_final)

# ============================================================
# Save datasets
# ============================================================

saveRDS(
  indoor_final,
  file = "data/cleaned_analysis_data_indoor.rds"
)

saveRDS(
  outdoor_final,
  file = "data/cleaned_analysis_data_outdoor.rds"
)

saveRDS(
  indoor_model,
  file = "data/model_data_indoor.rds"
)

saveRDS(
  outdoor_model,
  file = "data/model_data_outdoor.rds"
)