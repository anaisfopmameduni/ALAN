# ============================================================
# Translation of Do-File-Lucas.do to R
# ============================================================

# Packages
required_packages <- c(
  "dplyr", "haven", "readr")
library(haven)
library(dplyr)
library(tidyr)
library(nnet)
library(splines)
library(broom)


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

all_work_ss <- read_dta_checked(
  file.path(data_dir, "MCC ALAN all WORK SS.dta")
)

additional_alan <- read_dta_checked(
  file.path(original_dir, "MCC_additional ALAN variables.dta")
)

requested_variables <- read_dta_checked(
  file.path(
    original_dir,
    "MCC_ALAN_obesity_analysis_requested_variables_20240918.dta"
  )
)

# sample restriction
dat <- main %>%
  filter(nightever != 1)

# ----------------------------
# Missing-value regrouping
# ----------------------------

dat <- dat %>%
  mutate(
    score_SE = replace(score_SE, is.na(score_SE), 9),
    fumador_ever = replace(fumador_ever, is.na(fumador_ever), 9),
    mets_act_10_2_total_rec =
      replace(mets_act_10_2_total_rec, is.na(mets_act_10_2_total_rec), 9),
    uvi_longest_group =
      replace(uvi_longest_group, is.na(uvi_longest_group), 9)
  )

# ----------------------------
# BMI variables
# ----------------------------

dat <- dat %>%
  mutate(
    bmi_WHO = case_when(
      bmi_actual >= 12 & bmi_actual < 18.5 ~ 1,
      bmi_actual >= 18.5 & bmi_actual < 25 ~ 2,
      bmi_actual >= 25 & bmi_actual < 30 ~ 3,
      bmi_actual >= 30 ~ 4,
      TRUE ~ NA_real_
    ),
    bmi_2 = case_when(
      bmi_WHO == 2 ~ 0,
      bmi_WHO %in% c(3, 4) ~ 1,
      bmi_WHO == 1 ~ NA_real_,
      TRUE ~ NA_real_
    ),
    bmi_3 = case_when(
      bmi_WHO == 2 ~ 0,
      bmi_WHO == 3 ~ 1,
      bmi_WHO == 4 ~ 2,
      TRUE ~ NA_real_
    ),
    bmi_cat = cut(
      bmi_actual,
      breaks = c(-Inf, 25, 30, Inf),
      labels = c("normal weight", "overweight", "obese"),
      right = FALSE
    ),
    obesity = case_when(
      bmi_actual < 30 ~ 0,
      bmi_actual >= 30 ~ 1,
      TRUE ~ NA_real_
    )
  )

# ----------------------------
# Sleep variables
# ----------------------------

dat <- dat %>%
  mutate(
    sleep_cat = cut(
      g13_tiempo_dormir,
      breaks = c(-Inf, 6, 7, 8, 9, Inf),
      labels = c("<=6h", "7h", "8h", ">=9h", NA),
      right = TRUE
    ),
    sleep_cat = as.character(sleep_cat),
    sleep_cat = if_else(
      g13_tiempo_dormir >= 7 & g13_tiempo_dormir < 8,
      "7h",
      sleep_cat
    ),
    sleep_cat = factor(
      sleep_cat,
      levels = c("<=6h", "7h", "8h", ">=9h")
    ),
    sleep_cat_3 = case_when(
      g13_tiempo_dormir <= 6 ~ 2,
      g13_tiempo_dormir %in% c(7, 8) ~ 1,
      g13_tiempo_dormir >= 9 ~ 3,
      TRUE ~ NA_real_
    ),
    siesta_general = case_when(
      g20_siesta == 0 ~ 0,
      g20_siesta %in% 1:7 ~ 1,
      TRUE ~ NA_real_
    ),
    siesta_60 = case_when(
      g21_tiempo_siesta < 60 ~ 0,
      g21_tiempo_siesta >= 60 ~ 1,
      TRUE ~ NA_real_
    ),
    siesta_frecat = case_when(
      g20_siesta == 0 ~ 0,
      g20_siesta %in% 1:2 ~ 1,
      g20_siesta %in% 3:6 ~ 2,
      g20_siesta == 7 ~ 3,
      TRUE ~ NA_real_
    ),
    siesta_ultimate = case_when(
      siesta_general == 0 ~ 0,
      siesta_60 == 0 & siesta_general != 0 ~ 1,
      siesta_60 == 1 & siesta_general != 0 ~ 2,
      TRUE ~ NA_real_
    )
  )

# ----------------------------
# Bedtime
# ----------------------------

# Stata's %tcHH:MM conversion depends on the underlying numeric
# datetime origin. Verify that g12_hora_dormir is stored as minutes
# or milliseconds before using this conversion.

dat <- dat %>%
  mutate(
    str_g12_hora_dormir = format(
      as.POSIXct(g12_hora_dormir, origin = "1960-01-01", tz = "UTC"),
      "%H:%M"
    ),
    h_dormir = as.numeric(substr(str_g12_hora_dormir, 1, 2)),
    min_dormir = as.numeric(substr(str_g12_hora_dormir, 4, 5)),
    h_dormir = na_if(h_dormir, 99),
    min_dormir = na_if(min_dormir, 99),
    time_sl_min_dec = min_dormir / 60,
    bed_time_new = h_dormir + time_sl_min_dec,
    bedtime_group = case_when(
      bed_time_new < 23 ~ 1,
      bed_time_new >= 23 & bed_time_new < 24 ~ 2,
      bed_time_new >= 0 & bed_time_new < 1 ~ 3,
      bed_time_new >= 1 & bed_time_new < 10 ~ 4,
      TRUE ~ NA_real_
    )
  )

# ----------------------------
# Centre and diabetes
# ----------------------------

dat <- dat %>%
  mutate(
    centre_num = as.integer(factor(area)),
    diabetes_new = case_when(
      diabetes == 1 ~ 0,
      diabetes %in% 2:5 ~ 1,
      TRUE ~ NA_real_
    ),
    WHR_cat = case_when(
      a3_sex == 1 & waist_hip > 0.90 ~ 1,
      a3_sex == 1 & waist_hip <= 0.90 ~ 0,
      a3_sex == 2 & waist_hip > 0.85 ~ 1,
      a3_sex == 2 & waist_hip <= 0.85 ~ 0,
      TRUE ~ NA_real_
    )
  )

# ----------------------------
# Tertiles and quintiles
# ----------------------------

ntile_na <- function(x, n) {
  out <- rep(NA_integer_, length(x))
  ok <- !is.na(x)
  out[ok] <- dplyr::ntile(x[ok], n)
  out
}

dat <- dat %>%
  mutate(
    gra_fruits_group = ntile_na(gra_fruits, 3),
    gra_vegetables_group = ntile_na(gra_vegetables, 3),
    t_energy_group = ntile_na(t_energy, 3),
    uvi_longest_group = ntile_na(uvi_longest, 3),
    ndvi_500_longest_group = ntile_na(ndvi_500_longest, 3),
    MSIbg_group_c = ntile_na(msibg_current, 3),
    MSIbg_group_l = ntile_na(msibg_longest, 3),
    VSL_group_c = ntile_na(vlggr_current, 3),
    VSL_group_l = ntile_na(vlggr_longest, 3),
    MSIgr_group_l = ntile_na(msigr_longest, 5),
    ImpMSIgr_group_l = ntile_na(impmsigr_longest, 5),
    Melgr_group_l = ntile_na(melgr_longest, 5),
    ImpMelgr_group_l = ntile_na(impmelgr_longest, 5)
  )

# ----------------------------
# Combined ALAN
# ----------------------------

dat <- dat %>%
  mutate(
    ALAN_combined_norm = (MSIgr_group_l + indoorlight_lab) / 9,
    ALAN_combined_cat = case_when(
      ALAN_combined_norm >= 0 & ALAN_combined_norm <= 0.40 ~ 1,
      ALAN_combined_norm > 0.40 & ALAN_combined_norm <= 0.60 ~ 2,
      ALAN_combined_norm > 0.60 & ALAN_combined_norm <= 0.80 ~ 3,
      ALAN_combined_norm > 0.80 & ALAN_combined_norm <= 1 ~ 4,
      TRUE ~ NA_real_
    )
  )

# ----------------------------
# Centre-specific quintiles
# ----------------------------

dat <- dat %>%
  group_by(area) %>%
  mutate(MSIgr_centre_group = ntile_na(msigr_longest, 5)) %>%
  ungroup() %>%
  mutate(
    MSIgr_BCN = if_else(area == "BCN", MSIgr_centre_group, NA_integer_),
    MSIgr_MAD = if_else(area == "Madrid", MSIgr_centre_group, NA_integer_)
  ) %>%
  select(-MSIgr_centre_group)

# ----------------------------
# Healthy sleep scores
# ----------------------------

dat <- dat %>%
  mutate(
    SHSS = if_else(
      rowSums(
        cbind(
          sleep_cat %in% c("<=6h", ">=9h"),
          g14_problemas_sue_o == 1,
          g15_list_probl_medic == 1,
          siesta_60 == 1
        ),
        na.rm = FALSE
      ) == rowSums(
        !is.na(cbind(
          sleep_cat,
          g14_problemas_sue_o,
          g15_list_probl_medic,
          siesta_60
        ))
      ),
      rowSums(
        cbind(
          sleep_cat %in% c("<=6h", ">=9h"),
          g14_problemas_sue_o == 1,
          g15_list_probl_medic == 1,
          siesta_60 == 1
        ),
        na.rm = FALSE
      ),
      NA_real_
    ),
    SHSS_group = case_when(
      SHSS == 0 ~ 0,
      SHSS == 1 ~ 1,
      SHSS %in% 2:4 ~ 2,
      TRUE ~ NA_real_
    ),
    SHSS3 = case_when(
      !is.na(sleep_cat) &
        !is.na(g14_problemas_sue_o) &
        !is.na(g15_list_probl_medic) &
        !is.na(siesta_ultimate) ~
        (sleep_cat %in% c("<=6h", ">=9h")) +
        (g14_problemas_sue_o == 1 | g15_list_probl_medic == 1) +
        (siesta_ultimate == 2),
      TRUE ~ NA_real_
    ),
    SHSS3_group = case_when(
      SHSS3 == 0 ~ 0,
      SHSS3 == 1 ~ 1,
      SHSS3%in% 2:3 ~ 2,
      TRUE ~ NA_real_
    ),
    EHSS = case_when(
      complete.cases(
        sleep_cat,
        g14_problemas_sue_o,
        siesta_ultimate,
        sl_group,
        msf_c5,
        sjl_c
      ) ~
        (sleep_cat %in% c("<=6h", ">=9h")) +
        (g14_problemas_sue_o == 1 | g15_list_probl_medic == 1) +
        (siesta_ultimate == 2) +
        (sl_group == 1) +
        (msf_c5 == 5) +
        (sjl_c == 3),
      TRUE ~ NA_real_
    ),
    EHSS_group = case_when(
      EHSS == 0 ~ 0,
      EHSS == 1 ~ 1,
      EHSS %in% 2:3 ~ 2,
      EHSS %in% 4:6 ~ 3,
      TRUE ~ NA_real_
    ),
    EHSS3_group = case_when(
      EHSS == 0 ~ 0,
      EHSS %in% 1:2 ~ 1,
      EHSS %in% 3:6 ~ 2,
      TRUE ~ NA_real_
    )
  )

# ----------------------------
# Sleep latency and timing
# ----------------------------

dat <- dat %>%
  mutate(
    min_lab_dec = a2b_min_apagar_luces / 60,
    sprepw = a2a_hora_apagar_luces + min_lab_dec,
    min_lab_conciliar = a3_min_conciliar / 60,
    sleep_latency_w = sprepw + min_lab_conciliar,
    sleep_latency_w = if_else(
      sleep_latency_w >= 24,
      sleep_latency_w - 24,
      sleep_latency_w
    ),
    min_lib_dec = a8b_min_apagar_luces / 60,
    sprepf = a8a_hora_apagar_luces + min_lib_dec,
    min_lib_conciliar = a9_min_conciliar / 60,
    sleep_latency_f = sprepf + min_lib_conciliar,
    sleep_latency_f = if_else(
      sleep_latency_f >= 24,
      sleep_latency_f - 24,
      sleep_latency_f
    ),
    sl_mean = (5 * sleep_latency_w + 2 * sleep_latency_f) / 7,
    sl_group = case_when(
      sl_mean <= 20 ~ 0,
      sl_mean > 20 ~ 1,
      TRUE ~ NA_real_
    ),
    sow = sleep_latency_w,
    sof = sleep_latency_f,
    min_lab_desp = a4b_min_despertar / 60,
    sew = a4a_hora_despertar + min_lab_desp,
    sdw = sew - sow,
    sdw = if_else(sdw < 0, sdw + 24, sdw),
    min_lib_desp = a10b_min_despertar / 60,
    sef = a10a_hora_despertar + min_lib_desp,
    msw = sow + sdw / 2,
    msw = if_else(msw > 24, msw - 24, msw),
    sjlrel = msf - msw
  )

# ----------------------------
# Modelling dataset
# ----------------------------

model_dat <- dat %>%
  filter(
    !is.na(obesity),
    !is.na(diabetes_new),
    !is.na(a4_edat),
    !is.na(a3_sexe),
    !is.na(centre_num),
    !is.na(education_basic_final),
    !is.na(score_SE),
    !is.na(mets_act_10_2_total_rec),
    !is.na(fumador_ever),
    !is.na(gra_fruits_group),
    !is.na(gra_vegetables_group),
    !is.na(t_energy_group)
  ) %>%
  mutate(
    across(
      c(
        SHSS, SHSS3_group, EHSS3_group, sleep_cat_3,
        a3_sexe, centre_num, education_basic_final, score_SE,
        mets_act_10_2_total_rec, fumador_ever,
        gra_fruits_group, gra_vegetables_group, t_energy_group,
        uvi_longest_group, indoorlight_lab, MSIbg_group_l,
        MSIgr_group_l, ImpMSIgr_group_l, Melgr_group_l,
        ImpMelgr_group_l, ALAN_combined_cat, siesta_ultimate,
        bedtime_group, g14_problemas_sue_o, g15_list_probl_medic
      ),
      as.factor
    )
  )

# ----------------------------
# Logistic regression helper
# ----------------------------

fit_logistic <- function(data, outcome, exposure, covariates = NULL,
                         subset = NULL) {
  vars <- c(outcome, exposure, covariates)
  d <- data

  if (!is.null(subset)) {
    d <- d %>% filter(!!rlang::parse_expr(subset))
  }

  d <- d %>% tidyr::drop_na(all_of(vars))

  rhs <- paste(c(exposure, covariates), collapse = " + ")
  f <- as.formula(paste(outcome, "~", rhs))

  glm(f, data = d, family = binomial())
}

fit_multinomial <- function(data, outcome, exposure, covariates = NULL,
                            subset = NULL) {
  vars <- c(outcome, exposure, covariates)
  d <- data

  if (!is.null(subset)) {
    d <- d %>% filter(!!rlang::parse_expr(subset))
  }

  d <- d %>% tidyr::drop_na(all_of(vars))

  rhs <- paste(c(exposure, covariates), collapse = " + ")
  f <- as.formula(paste(outcome, "~", rhs))

  d[[outcome]] <- relevel(as.factor(d[[outcome]]), ref = "0")

  nnet::multinom(f, data = d, trace = FALSE)
}

base_covars <- c(
  "a4_edat",
  "a3_sexe",
  "centre_num",
  "education_basic_final",
  "score_SE"
)

full_covars <- c(
  base_covars,
  "mets_act_10_2_total_rec",
  "fumador_ever",
  "gra_fruits_group",
  "gra_vegetables_group",
  "t_energy_group"
)

full_covars_uvi <- c(full_covars, "uvi_longest_group")

# ----------------------------
# Examples corresponding to Stata models
# ----------------------------

# SHSS and obesity
m_obesity_shss_1 <- fit_logistic(model_dat, "obesity", "SHSS")
m_obesity_shss_2 <- fit_logistic(
  model_dat, "obesity", "SHSS", base_covars
)
m_obesity_shss_3 <- fit_logistic(
  model_dat, "obesity", "SHSS", full_covars
)
m_obesity_shss_4 <- fit_logistic(
  model_dat, "obesity", "SHSS", full_covars_uvi
)

# SHSS and diabetes
m_diabetes_shss_1 <- fit_logistic(model_dat, "diabetes_new", "SHSS")
m_diabetes_shss_2 <- fit_logistic(
  model_dat, "diabetes_new", "SHSS", base_covars
)
m_diabetes_shss_3 <- fit_logistic(
  model_dat, "diabetes_new", "SHSS", full_covars
)

# SHSS3 and obesity
m_obesity_shss3_1 <- fit_multinomial(
  model_dat, "bmi_3", "SHSS3_group"
)
m_obesity_shss3_2 <- fit_multinomial(
  model_dat, "bmi_3", "SHSS3_group", base_covars
)
m_obesity_shss3_3 <- fit_multinomial(
  model_dat, "bmi_3", "SHSS3_group", full_covars
)

# EHSS3 and obesity
m_obesity_ehss3_1 <- fit_multinomial(
  model_dat, "bmi_3", "EHSS3_group"
)
m_obesity_ehss3_2 <- fit_multinomial(
  model_dat, "bmi_3", "EHSS3_group", base_covars
)
m_obesity_ehss3_3 <- fit_multinomial(
  model_dat, "bmi_3", "EHSS3_group", full_covars
)

# Sleep duration and obesity
m_obesity_sleep_1 <- fit_logistic(
  model_dat, "obesity", "sleep_cat_3"
)
m_obesity_sleep_2 <- fit_logistic(
  model_dat, "obesity", "sleep_cat_3", base_covars
)
m_obesity_sleep_3 <- fit_logistic(
  model_dat, "obesity", "sleep_cat_3", full_covars
)

# Sleep duration and diabetes
m_diabetes_sleep_1 <- fit_logistic(
  model_dat, "diabetes_new", "sleep_cat_3"
)
m_diabetes_sleep_2 <- fit_logistic(
  model_dat, "diabetes_new", "sleep_cat_3", base_covars
)
m_diabetes_sleep_3 <- fit_logistic(
  model_dat, "diabetes_new", "sleep_cat_3", full_covars
)

# Sleep problems and obesity
m_obesity_problems_1 <- fit_logistic(
  model_dat, "obesity", "g14_problemas_sue_o"
)
m_obesity_problems_2 <- fit_logistic(
  model_dat, "obesity", "g14_problemas_sue_o", base_covars
)
m_obesity_problems_3 <- fit_logistic(
  model_dat, "obesity", "g14_problemas_sue_o", full_covars
)

# Sleep medication and obesity
m_obesity_medication_1 <- fit_logistic(
  model_dat, "obesity", "g15_list_probl_medic"
)
m_obesity_medication_2 <- fit_logistic(
  model_dat, "obesity", "g15_list_probl_medic", base_covars
)
m_obesity_medication_3 <- fit_logistic(
  model_dat, "obesity", "g15_list_probl_medic", full_covars
)

# Napping and obesity
m_obesity_nap_1 <- fit_logistic(
  model_dat, "obesity", "siesta_ultimate"
)
m_obesity_nap_2 <- fit_logistic(
  model_dat, "obesity", "siesta_ultimate", base_covars
)
m_obesity_nap_3 <- fit_logistic(
  model_dat, "obesity", "siesta_ultimate", full_covars
)

# Indoor ALAN and BMI category
m_bmi_indoor_1 <- fit_multinomial(
  model_dat, "bmi_3", "indoorlight_lab"
)
m_bmi_indoor_2 <- fit_multinomial(
  model_dat, "bmi_3", "indoorlight_lab", base_covars
)
m_bmi_indoor_3 <- fit_multinomial(
  model_dat, "bmi_3", "indoorlight_lab", full_covars
)
m_bmi_indoor_4 <- fit_multinomial(
  model_dat, "bmi_3", "indoorlight_lab", full_covars_uvi
)

# Indoor ALAN and diabetes
m_diabetes_indoor_1 <- fit_logistic(
  model_dat, "diabetes_new", "indoorlight_lab"
)
m_diabetes_indoor_2 <- fit_logistic(
  model_dat, "diabetes_new", "indoorlight_lab", base_covars
)
m_diabetes_indoor_3 <- fit_logistic(
  model_dat, "diabetes_new", "indoorlight_lab", full_covars
)
m_diabetes_indoor_4 <- fit_logistic(
  model_dat, "diabetes_new", "indoorlight_lab", full_covars_uvi
)

# MSIgr and BMI category
m_bmi_msigr_1 <- fit_multinomial(
  model_dat, "bmi_3", "MSIgr_group_l"
)
m_bmi_msigr_2 <- fit_multinomial(
  model_dat, "bmi_3", "MSIgr_group_l",
  c(base_covars, "uvi_longest_group")
)

# MSIgr and diabetes
m_diabetes_msigr_1 <- fit_logistic(
  model_dat, "diabetes_new", "MSIgr_group_l"
)
m_diabetes_msigr_2 <- fit_logistic(
  model_dat, "diabetes_new", "MSIgr_group_l",
  c(base_covars, "uvi_longest_group")
)

# Combined ALAN
m_bmi_alan <- fit_multinomial(
  model_dat, "bmi_3", "ALAN_combined_cat",
  c(base_covars, "uvi_longest_group")
)

m_diabetes_alan <- fit_logistic(
  model_dat, "diabetes_new", "ALAN_combined_cat",
  c(base_covars, "uvi_longest_group")
)

# ----------------------------
# Stratified examples
# ----------------------------

m_bmi_msibg_bcn <- fit_logistic(
  model_dat, "bmi_2", "MSIbg_group_l",
  subset = "centre_num == 2"
)

m_bmi_msibg_madrid <- fit_logistic(
  model_dat, "bmi_2", "MSIbg_group_l",
  subset = "centre_num == 9"
)

m_bmi_indoor_male <- fit_multinomial(
  model_dat, "bmi_3", "indoorlight_lab",
  subset = "a3_sexe == 1"
)

m_bmi_indoor_female <- fit_multinomial(
  model_dat, "bmi_3", "indoorlight_lab",
  subset = "a3_sexe == 2"
)

m_diabetes_indoor_male <- fit_logistic(
  model_dat, "diabetes_new", "indoorlight_lab",
  subset = "a3_sexe == 1"
)

m_diabetes_indoor_female <- fit_logistic(
  model_dat, "diabetes_new", "indoorlight_lab",
  subset = "a3_sexe == 2"
)

# ----------------------------
# Wilcoxon tests and correlations
# ----------------------------

alan_vars <- c(
  "msibg_longest",
  "msigr_longest",
  "impmsibg_longest",
  "impmsigr_longest",
  "melbg_longest",
  "melgr_longest",
  "impmelbg_longest",
  "impmelgr_longest"
)

wilcox_results <- lapply(alan_vars, function(v) {
  rstatix::wilcox_test(
    dat,
    formula = as.formula(paste(v, "~ centre_num"))
  ) %>%
    mutate(variable = v)
}) %>%
  bind_rows()

cor_vars <- c(
  "msibg_longest",
  "msigr_longest",
  "impmsibg_longest",
  "impmsigr_longest",
  "melbg_longest",
  "melgr_longest",
  "impmelbg_longest",
  "impmelgr_longest"
)

pearson_correlations <- cor(
  dat[, cor_vars],
  use = "pairwise.complete.obs",
  method = "pearson"
)

spearman_correlations <- cor(
  dat[, cor_vars],
  use = "pairwise.complete.obs",
  method = "spearman"
)

# ----------------------------
# Restricted cubic spline models
# ----------------------------

# Stata mkspline cubic knots(...) is not identical to ns().
# This uses natural cubic splines and must be checked against Stata.
m_bmi_msigr_spline <- multinom(
  bmi_3 ~ ns(
    msigr_longest,
    knots = c(0.1236267, 0.1564941, 0.1881104),
    Boundary.knots = c(0, max(msigr_longest, na.rm = TRUE))
  ) +
    a4_edat + a3_sexe + centre_num + education_basic_final +
    score_SE + uvi_longest_group,
  data = model_dat,
  trace = FALSE
)

m_diabetes_msigr_spline <- glm(
  diabetes_new ~ ns(
    msigr_longest,
    knots = c(0.1236267, 0.1564941, 0.1881104),
    Boundary.knots = c(0, max(msigr_longest, na.rm = TRUE))
  ) +
    a4_edat + a3_sexe + centre_num + education_basic_final +
    score_SE + uvi_longest_group,
  data = model_dat,
  family = binomial()
)

# ----------------------------
# Odds ratios / relative-risk ratios
# ----------------------------

tidy_or <- function(model) {
  broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
}

tidy_or(m_obesity_shss_1)
tidy_or(m_diabetes_shss_1)

# For multinomial models, exponentiated coefficients are relative-risk ratios.
tidy_rrr <- function(model) {
  broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
}

tidy_rrr(m_bmi_msigr_1)
