# read in previously saved data
dat <- readRDS("data/cleaned_analysis_data.rds")

model_dat <- readRDS("data/model_data.rds")


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
