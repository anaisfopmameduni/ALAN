# read in previously saved data
dat <- readRDS("data/cleaned_analysis_data.rds")

model_dat <- readRDS("data/model_data.rds")



library(tableone)

table1_vars <- c(
  "a4_edat", "a3_sexe", "area", "education_basic_final",
  "score_SE", "bmi_cat", "waist_hip", "WHR_cat",
  "gra_fruits", "gra_vegetables", "t_energy",
  "fumador_ever", "mets_act_10_2_total_rec",
  "sleep_cat", "bedtime_group", "siesta_general",
  "siesta_frecat", "g14_problemas_sue_o",
  "g21_tiempo_siesta", "msf_c5", "uvi_longest"
)

table1_vars <- intersect(table1_vars, names(model_dat))

table_bmi <- CreateTableOne(
  vars = table1_vars,
  strata = "bmi_3",
  data = model_dat,
  factorVars = intersect(
    c(
      "a3_sexe", "area", "education_basic_final", "score_SE",
      "bmi_cat", "WHR_cat", "fumador_ever",
      "mets_act_10_2_total_rec", "sleep_cat", "bedtime_group",
      "siesta_general", "siesta_frecat",
      "g14_problemas_sue_o", "msf_c5"
    ),
    names(model_dat)
  ),
  includeNA = TRUE
)

print(table_bmi, showAllLevels = TRUE)