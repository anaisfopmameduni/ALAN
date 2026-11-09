# ============================================================
# Table 3B: Outdoor ALAN stratified analyses
#
# Outcomes: obesity and diabetes, analysed separately
# Stratification: sex, chronotype, and Barcelona vs Madrid
#
# Model 3:
# age, sex, centre, education, SES, smoking,
# physical activity and continuous UVI.
#
# Sex is omitted within sex strata.
# Centre is omitted within city strata.
# Chronotype is not added as an adjustment variable.
#
# No global exclusion for missing BMI or diabetes.
# No CSV or RDS exports.
# ============================================================

library(dplyr)
library(tidyr)
library(haven)
library(flextable)
library(officer)

# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------

outdoor_dat <- readRDS(
  "data/cleaned_analysis_data_outdoor.rds"
)

dir.create(
  "output",
  recursive = TRUE,
  showWarnings = FALSE
)

exposure_labels <- c(
  "Very low level",
  "Low level",
  "Intermediate level",
  "High level",
  "Very high level"
)

model3_covariates <- c(
  "age_years",
  "sex",
  "centre",
  "education",
  "socioeconomic_score",
  "smoking_status",
  "physical_activity",
  "uvi"
)

outcomes <- c(
  obesity_status = "Obesity",
  diabetes_binary = "Diabetes"
)

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

numeric_codes <- function(x, variable) {
  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(x)
  }
  
  if (is.factor(x)) {
    x <- as.character(x)
  }
  
  original <- x
  result <- suppressWarnings(as.numeric(x))
  
  if (any(!is.na(original) & is.na(result))) {
    stop(variable, " contains non-numeric labels.")
  }
  
  if (any(!is.na(result) & !is.finite(result))) {
    stop(variable, " contains non-finite values.")
  }
  
  result
}

check_codes <- function(x, allowed, variable) {
  unexpected <- setdiff(
    unique(x[!is.na(x)]),
    allowed
  )
  
  if (length(unexpected) > 0) {
    stop(
      "Unexpected codes in ", variable, ": ",
      paste(unexpected, collapse = ", ")
    )
  }
}

missing_category <- function(x) {
  x <- as.character(x)
  x[is.na(x) | x == "9"] <- "Missing"
  
  factor(
    x,
    levels = c(
      sort(setdiff(unique(x), "Missing")),
      "Missing"
    )
  )
}

identify_city <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- haven::as_factor(x, levels = "default")
  }
  
  x <- trimws(tolower(as.character(x)))
  
  city <- case_when(
    x %in% c("bcn", "barcelona") ~ "Barcelona",
    x %in% c("mad", "madrid") ~ "Madrid",
    TRUE ~ NA_character_
  )
  
  if (!all(c("Barcelona", "Madrid") %in% city)) {
    stop(
      "Could not identify both Barcelona and Madrid from area. ",
      "Observed area values: ",
      paste(sort(unique(x)), collapse = ", "),
      ". Update identify_city() using your actual area labels."
    )
  }
  
  if (anyNA(city)) {
    stop(
      "Some outdoor participants have missing or unrecognised city labels. ",
      "Check area before proceeding."
    )
  }
  
  factor(
    city,
    levels = c("Barcelona", "Madrid")
  )
}

# ------------------------------------------------------------
# Validate and prepare variables
# ------------------------------------------------------------

required <- unique(c(
  "participant_id",
  names(outcomes),
  model3_covariates,
  "chronotype_3",
  "area",
  "ms_gr_longest_group"
))

missing_vars <- setdiff(required, names(outdoor_dat))

if (length(missing_vars) > 0) {
  stop(
    "Required variables not found: ",
    paste(missing_vars, collapse = ", ")
  )
}

if (
  anyNA(outdoor_dat$participant_id) ||
  anyDuplicated(outdoor_dat$participant_id) > 0
) {
  stop("participant_id must be non-missing and unique.")
}

outdoor_dat$city <- identify_city(outdoor_dat$area)

numeric_vars <- setdiff(
  required,
  c("participant_id", "area")
)

for (variable in numeric_vars) {
  outdoor_dat[[variable]] <- numeric_codes(
    outdoor_dat[[variable]],
    variable
  )
}

check_codes(
  outdoor_dat$obesity_status,
  c(0, 1),
  "obesity_status"
)

check_codes(
  outdoor_dat$diabetes_binary,
  c(0, 1),
  "diabetes_binary"
)

check_codes(outdoor_dat$sex, c(1, 2), "sex")
check_codes(outdoor_dat$education, 1:4, "education")
check_codes(outdoor_dat$chronotype_3, 1:3, "chronotype_3")

check_codes(
  outdoor_dat$ms_gr_longest_group,
  1:5,
  "ms_gr_longest_group"
)

outdoor_model_dat <- outdoor_dat %>%
  mutate(
    exposure = factor(
      ms_gr_longest_group,
      levels = 1:5,
      labels = exposure_labels
    ),
    
    sex = factor(
      sex,
      levels = c(1, 2),
      labels = c("Male", "Female")
    ),
    
    centre = factor(centre),
    
    education = factor(
      education,
      levels = 1:4,
      labels = c(
        "Less than primary school",
        "Primary school",
        "Secondary school",
        "University"
      )
    ),
    
    socioeconomic_score = missing_category(
      socioeconomic_score
    ),
    
    smoking_status = missing_category(smoking_status),
    
    physical_activity = missing_category(
      physical_activity
    ),
    
    chronotype = factor(
      chronotype_3,
      levels = 1:3,
      labels = c(
        "Morning",
        "Intermediate",
        "Evening"
      )
    )
  )

# ------------------------------------------------------------
# Separate stratification specifications
# ------------------------------------------------------------

stratifications <- list(
  Sex = list(
    variable = "sex",
    levels = c("Male", "Female"),
    omit = "sex"
  ),
  
  Chronotype = list(
    variable = "chronotype",
    levels = c(
      "Morning",
      "Intermediate",
      "Evening"
    ),
    omit = character(0)
  ),
  
  City = list(
    variable = "city",
    levels = c("Barcelona", "Madrid"),
    omit = "centre"
  )
)

# ------------------------------------------------------------
# Fit one subgroup model
# ------------------------------------------------------------

fit_subgroup <- function(
    dat,
    outcome,
    covariates,
    context
) {
  if (nrow(dat) == 0) {
    stop("Empty analysis sample: ", context)
  }
  
  if (!setequal(unique(dat[[outcome]]), c(0, 1))) {
    stop(
      "Both cases and non-cases are required: ",
      context
    )
  }
  
  if (!(exposure_labels[1] %in%
        as.character(dat$exposure))) {
    stop(
      "Reference exposure category is absent: ",
      context
    )
  }
  
  if (nlevels(dat$exposure) < 2) {
    stop(
      "Fewer than two exposure categories: ",
      context
    )
  }
  
  varying_covariates <- covariates[
    vapply(
      dat[covariates],
      function(x) length(unique(x)) > 1,
      logical(1)
    )
  ]
  
  omitted <- setdiff(covariates, varying_covariates)
  
  if (length(omitted) > 0) {
    warning(
      context,
      ": constant covariates omitted: ",
      paste(omitted, collapse = ", ")
    )
  }
  
  model_formula <- reformulate(
    c("exposure", varying_covariates),
    response = outcome
  )
  
  factor_variables <- c(
    "exposure",
    varying_covariates
  )
  
  factor_variables <- factor_variables[
    vapply(
      dat[factor_variables],
      is.factor,
      logical(1)
    )
  ]
  
  treatment_contrasts <- setNames(
    rep(
      list("contr.treatment"),
      length(factor_variables)
    ),
    factor_variables
  )
  
  model <- glm(
    model_formula,
    data = dat,
    family = binomial(link = "logit"),
    contrasts = treatment_contrasts,
    na.action = na.fail
  )
  
  if (!model$converged) {
    stop("Model did not converge: ", context)
  }
  
  if (anyNA(coef(model))) {
    stop(
      "Aliased coefficients in ", context,
      ". Inspect this subgroup before reporting."
    )
  }
  
  if (
    any(
      fitted(model) < 1e-8 |
      fitted(model) > 1 - 1e-8
    )
  ) {
    warning(
      context,
      ": extreme fitted probabilities. ",
      "Inspect for separation or sparse data before reporting."
    )
  }
  
  model
}

# ------------------------------------------------------------
# Extract ORs and Wald 95% confidence intervals
# ------------------------------------------------------------

extract_results <- function(model, dat, outcome) {
  exposure_levels <- model$xlevels$exposure
  
  coefficient_names <- paste0(
    "exposure",
    exposure_levels[-1]
  )
  
  beta <- coef(model)
  se <- sqrt(diag(vcov(model)))
  
  positions <- match(
    coefficient_names,
    names(beta)
  )
  
  if (anyNA(positions)) {
    stop("Could not match exposure coefficients.")
  }
  
  z <- qnorm(0.975)
  b <- beta[positions]
  s <- se[positions]
  
  estimates <- data.frame(
    category = exposure_levels,
    OR = c(1, exp(b)),
    LCI = c(NA_real_, exp(b - z * s)),
    UCI = c(NA_real_, exp(b + z * s)),
    p_value = c(
      NA_real_,
      2 * pnorm(-abs(b / s))
    )
  )
  
  counts <- dat %>%
    group_by(exposure) %>%
    summarise(
      category_N = n(),
      cases = sum(.data[[outcome]] == 1),
      noncases = sum(.data[[outcome]] == 0),
      .groups = "drop"
    ) %>%
    transmute(
      category = as.character(exposure),
      category_N,
      cases,
      noncases
    )
  
  data.frame(category = exposure_labels) %>%
    left_join(estimates, by = "category") %>%
    left_join(counts, by = "category") %>%
    mutate(
      across(
        c(category_N, cases, noncases),
        ~ replace_na(.x, 0L)
      )
    )
}

# ------------------------------------------------------------
# Run outdoor subgroup models
# ------------------------------------------------------------

outdoor_stratified_analyses <- list()

for (outcome in names(outcomes)) {
  # Same outcome-specific parent sample as Table 2 Model 3.
  required_model <- c(
    outcome,
    "exposure",
    model3_covariates
  )
  
  parent_sample <- outdoor_model_dat[
    complete.cases(
      outdoor_model_dat[, required_model, drop = FALSE]
    ),
    ,
    drop = FALSE
  ]
  
  for (stratification in names(stratifications)) {
    specification <- stratifications[[stratification]]
    
    for (subgroup in specification$levels) {
      subgroup_values <- as.character(
        parent_sample[[specification$variable]]
      )
      
      subgroup_dat <- parent_sample[
        !is.na(subgroup_values) &
          subgroup_values == subgroup,
        ,
        drop = FALSE
      ] %>%
        droplevels()
      
      covariates <- setdiff(
        model3_covariates,
        specification$omit
      )
      
      context <- paste(
        "Outdoor ALAN",
        outcomes[[outcome]],
        stratification,
        subgroup,
        sep = " / "
      )
      
      model <- fit_subgroup(
        subgroup_dat,
        outcome,
        covariates,
        context
      )
      
      results <- extract_results(
        model,
        subgroup_dat,
        outcome
      ) %>%
        mutate(
          section = "Outdoor ALAN",
          outcome = .env$outcomes[[.env$outcome]],
          stratification = .env$stratification,
          subgroup = .env$subgroup,
          model_N = nobs(.env$model),
          .before = 1
        )
      
      key <- paste(
        outcome,
        stratification,
        subgroup,
        sep = "_"
      )
      
      outdoor_stratified_analyses[[key]] <- list(
        model = model,
        
        participant_ids = subgroup_dat$participant_id,
        
        results = results,
        
        sample_sizes = data.frame(
          section = "Outdoor ALAN",
          outcome = outcomes[[outcome]],
          stratification = stratification,
          subgroup = subgroup,
          N = nobs(model),
          cases = sum(subgroup_dat[[outcome]] == 1),
          noncases = sum(subgroup_dat[[outcome]] == 0)
        )
      )
    }
  }
}

outdoor_stratified_results <- bind_rows(
  lapply(
    outdoor_stratified_analyses,
    function(x) x$results
  )
)

outdoor_stratified_sample_sizes <- bind_rows(
  lapply(
    outdoor_stratified_analyses,
    function(x) x$sample_sizes
  )
)

print(
  outdoor_stratified_sample_sizes,
  row.names = FALSE
)

# ------------------------------------------------------------
# Format display data
# ------------------------------------------------------------

format_estimate <- function(or, lci, uci, reference) {
  result <- rep("Not estimable", length(or))
  
  valid <- is.finite(or) &
    is.finite(lci) &
    is.finite(uci)
  
  result[valid] <- sprintf(
    "%.2f (%.2f, %.2f)",
    or[valid],
    lci[valid],
    uci[valid]
  )
  
  result[reference] <- "1.00 (ref)"
  result
}

display_results <- outdoor_stratified_results %>%
  mutate(
    estimate = format_estimate(
      OR,
      LCI,
      UCI,
      category == exposure_labels[1]
    )
  ) %>%
  select(
    stratification,
    subgroup,
    category,
    outcome,
    model_N,
    estimate
  ) %>%
  pivot_wider(
    names_from = outcome,
    values_from = c(model_N, estimate),
    names_glue = "{outcome}_{.value}"
  )

display_order <- bind_rows(
  lapply(
    names(stratifications),
    function(stratification_name) {
      specification <- stratifications[[stratification_name]]
      
      tidyr::expand_grid(
        stratification = stratification_name,
        subgroup = specification$levels,
        category = exposure_labels
      )
    }
  )
)

outdoor_stratified_display <- display_order %>%
  left_join(
    display_results,
    by = c(
      "stratification",
      "subgroup",
      "category"
    )
  ) %>%
  select(
    stratification,
    subgroup,
    category,
    Obesity_model_N,
    Obesity_estimate,
    Diabetes_model_N,
    Diabetes_estimate
  )

# ------------------------------------------------------------
# Format Word table
# ------------------------------------------------------------

table3_outdoor <- flextable(
  outdoor_stratified_display
) %>%
  set_header_labels(
    stratification = "Stratified by",
    subgroup = "Subgroup",
    category = "Outdoor ALAN",
    Obesity_model_N = "N",
    Obesity_estimate = "OR (95% CI)",
    Diabetes_model_N = "N",
    Diabetes_estimate = "OR (95% CI)"
  ) %>%
  add_header_row(
    values = c("", "Obesity", "Diabetes"),
    colwidths = c(3, 2, 2)
  ) %>%
  merge_v(
    j = c("stratification", "subgroup")
  ) %>%
  theme_booktabs() %>%
  font(
    fontname = "Arial",
    part = "all"
  ) %>%
  fontsize(
    size = 9,
    part = "all"
  ) %>%
  bold(part = "header") %>%
  align(
    j = 1:3,
    align = "left",
    part = "all"
  ) %>%
  align(
    j = 4:7,
    align = "center",
    part = "all"
  ) %>%
  valign(
    valign = "top",
    part = "body"
  ) %>%
  padding(
    padding = 4,
    part = "all"
  ) %>%
  add_footer_lines(
    values = c(
      paste(
        "Model 3: adjusted for age, sex, centre, education,",
        "SES, smoking, physical activity and continuous UVI."
      ),
      paste(
        "Sex is omitted in sex-stratified models;",
        "centre is omitted in city-stratified models.",
        "Other constant covariates, if any, are omitted",
        "with a warning."
      ),
      paste(
        "Sex, chronotype and city are separate",
        "stratifications, not a joint cross-classification."
      ),
      paste(
        "All analyses use the outdoor dataset",
        "(Barcelona and Madrid)."
      ),
      paste(
        "Samples are outcome-specific.",
        "Missing SES, smoking and physical activity",
        "are retained as separate categories."
      ),
      paste(
        "Participants missing chronotype are excluded",
        "only from chronotype-stratified analyses."
      ),
      paste(
        "N = total participants in each subgroup model,",
        "repeated across ALAN categories;",
        "OR = odds ratio; CI = Wald 95% confidence interval;",
        "ref = reference."
      ),
      "No interaction tests are included."
    )
  ) %>%
  fontsize(
    size = 8,
    part = "footer"
  ) %>%
  align(
    align = "left",
    part = "footer"
  ) %>%
  autofit() %>%
  set_table_properties(
    layout = "autofit",
    width = 1
  )

# ------------------------------------------------------------
# Export editable Word document
# ------------------------------------------------------------

landscape_section <- officer::prop_section(
  page_size = officer::page_size(
    orient = "landscape",
    width = 8.27,
    height = 11.69
  ),
  page_margins = officer::page_mar(
    top = 0.5,
    bottom = 0.5,
    left = 0.5,
    right = 0.5
  )
)

word_doc <- officer::read_docx()

word_doc <- officer::body_set_default_section(
  word_doc,
  landscape_section
)

word_doc <- officer::body_add_par(
  word_doc,
  paste(
    "Table 4. Associations of outdoor ALAN with obesity",
    "and diabetes, stratified by sex, chronotype and city",
    "(Model 3)."
  ),
  style = "Normal"
)

word_doc <- flextable::body_add_flextable(
  word_doc,
  value = table3_outdoor
)

print(
  word_doc,
  target = "output/Table4_outdoor_ALAN_stratified.docx"
)

# Display table in R
table3_outdoor