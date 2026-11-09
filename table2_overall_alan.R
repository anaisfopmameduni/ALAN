# ============================================================
# Table 2: Indoor and outdoor ALAN associations
#
# TWO WORD TABLES:
# 1. Obesity
# 2. Diabetes
#
# Each contains indoor and outdoor ALAN sections and:
# - Main-sample cases and non-cases, n (%)
# - Model 1: demographic adjustment
# - Model 2: + lifestyle adjustment
# - Model 3: + environmental adjustment
#
# Models:
# Model 1: ALAN + age, sex, centre, education, SES
# Model 2: Model 1 + smoking, physical activity
# Model 3: Model 2 + continuous UVI
#
# Models 1 through 3 use a common sample within each
# exposure/outcome analysis.
#
# Missing SES, smoking and physical activity are retained
# as separate categories, including existing code 9.
#
# Nutrition, additional pollutants and combined ALAN
# are NOT included.
# ============================================================

# Install once if needed:
# install.packages(c("dplyr", "tidyr", "haven", "flextable", "officer"))

library(dplyr)
library(tidyr)
library(haven)
library(flextable)
library(officer)

# ------------------------------------------------------------
# Load cleaned datasets
# ------------------------------------------------------------

indoor_dat <- readRDS("data/cleaned_analysis_data_indoor.rds")
outdoor_dat <- readRDS("data/cleaned_analysis_data_outdoor.rds")

dir.create("output", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Exposure labels and covariate sets
# ------------------------------------------------------------

indoor_labels <- c(
  "Total darkness",
  "Almost dark",
  "Tenuous",
  "Bright"
)

outdoor_labels <- c(
  "Very low level",
  "Low level",
  "Intermediate level",
  "High level",
  "Very high level"
)

demographic_covariates <- c(
  "age_years",
  "sex",
  "centre",
  "education",
  "socioeconomic_score"
)

lifestyle_covariates <- c(
  demographic_covariates,
  "smoking_status",
  "physical_activity"
)

environmental_covariates <- c(
  lifestyle_covariates,
  "uvi"
)

main_model_sets <- list(
  Model1 = demographic_covariates,
  Model2 = lifestyle_covariates,
  Model3 = environmental_covariates
)

# ------------------------------------------------------------
# Helpers: numeric codes, validation and missing categories
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
    stop(
      variable,
      " contains non-numeric labels. ",
      "Use the cleaned dataset with original numeric codes."
    )
  }
  
  if (any(!is.na(result) & !is.finite(result))) {
    stop(variable, " contains non-finite values.")
  }
  
  result
}

check_codes <- function(x, allowed, variable) {
  unexpected <- setdiff(unique(x[!is.na(x)]), allowed)
  
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
  
  observed <- sort(setdiff(unique(x), "Missing"))
  
  factor(
    x,
    levels = c(observed, "Missing")
  )
}

# ------------------------------------------------------------
# Prepare variables without globally dropping participants
# ------------------------------------------------------------

prepare_table2_data <- function(
    dat,
    exposure_variable,
    exposure_labels) {
  
  required <- unique(c(
    "participant_id",
    "obesity_status",
    "diabetes_binary",
    environmental_covariates,
    exposure_variable
  ))
  
  missing_vars <- setdiff(required, names(dat))
  
  if (length(missing_vars) > 0) {
    stop(
      "Required variables not found: ",
      paste(missing_vars, collapse = ", ")
    )
  }
  
  if (
    anyNA(dat$participant_id) ||
    anyDuplicated(dat$participant_id) > 0
  ) {
    stop("participant_id must be non-missing and unique.")
  }
  
  for (variable in setdiff(required, "participant_id")) {
    dat[[variable]] <- numeric_codes(dat[[variable]], variable)
  }
  
  check_codes(dat$obesity_status, c(0, 1), "obesity_status")
  check_codes(dat$diabetes_binary, c(0, 1), "diabetes_binary")
  check_codes(dat$sex, c(1, 2), "sex")
  check_codes(dat$education, 1:4, "education")
  
  check_codes(
    dat[[exposure_variable]],
    seq_along(exposure_labels),
    exposure_variable
  )
  
  dat %>%
    mutate(
      exposure = factor(
        .data[[exposure_variable]],
        levels = seq_along(exposure_labels),
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
      
      smoking_status = missing_category(
        smoking_status
      ),
      
      physical_activity = missing_category(
        physical_activity
      )
    )
}

indoor_model_dat <- prepare_table2_data(
  indoor_dat,
  exposure_variable = "indoor_alan",
  exposure_labels = indoor_labels
)

# Keep the FIVE original outdoor categories.
outdoor_model_dat <- prepare_table2_data(
  outdoor_dat,
  exposure_variable = "ms_gr_longest_group",
  exposure_labels = outdoor_labels
)

# ------------------------------------------------------------
# Select analysis samples
# ------------------------------------------------------------

make_analysis_sample <- function(dat, outcome, covariates) {
  required <- unique(c(
    outcome,
    "exposure",
    covariates
  ))
  
  dat[
    complete.cases(dat[, required, drop = FALSE]),
    ,
    drop = FALSE
  ] %>%
    droplevels()
}

# ------------------------------------------------------------
# Fit logistic regression
# ------------------------------------------------------------

fit_model <- function(dat, outcome, covariates, reference) {
  if (nrow(dat) == 0) {
    stop("The analysis sample is empty.")
  }
  
  if (!setequal(unique(dat[[outcome]]), c(0, 1))) {
    stop(
      outcome,
      " must contain both cases (1) and non-cases (0)."
    )
  }
  
  if (
    nlevels(dat$exposure) < 2 ||
    levels(dat$exposure)[1] != reference
  ) {
    stop(
      "Exposure reference category is absent, ",
      "or fewer than two exposure categories remain."
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
      "Constant covariates omitted: ",
      paste(omitted, collapse = ", ")
    )
  }
  
  model_formula <- reformulate(
    c("exposure", varying_covariates),
    response = outcome
  )
  
  factor_variables <- c("exposure", varying_covariates)
  factor_variables <- factor_variables[
    vapply(dat[factor_variables], is.factor, logical(1))
  ]
  
  treatment_contrasts <- setNames(
    rep(list("contr.treatment"), length(factor_variables)),
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
    stop(
      "Model did not converge: ",
      paste(deparse(model_formula), collapse = " ")
    )
  }
  
  if (anyNA(coef(model))) {
    warning(
      "Aliased coefficients detected. Inspect the saved model: ",
      paste(deparse(model_formula), collapse = " ")
    )
  }
  
  model
}

# ------------------------------------------------------------
# Exposure estimates: OR and Wald 95% CI
# ------------------------------------------------------------

extract_results <- function(model, exposure_labels) {
  beta <- coef(model)
  standard_errors <- sqrt(diag(vcov(model)))
  critical <- qnorm(0.975)
  
  exposure_levels <- model$xlevels$exposure
  
  result <- data.frame(
    category = exposure_levels,
    OR = NA_real_,
    LCI = NA_real_,
    UCI = NA_real_,
    p_value = NA_real_
  )
  
  result$OR[1] <- 1
  
  coefficient_names <- paste0(
    "exposure",
    exposure_levels[-1]
  )
  
  positions <- match(
    coefficient_names,
    names(beta)
  )
  
  if (anyNA(positions)) {
    stop("Could not match exposure coefficients to category labels.")
  }
  
  b <- beta[positions]
  se <- standard_errors[positions]
  
  result$OR[-1] <- exp(b)
  result$LCI[-1] <- exp(b - critical * se)
  result$UCI[-1] <- exp(b + critical * se)
  result$p_value[-1] <- 2 * pnorm(-abs(b / se))
  
  data.frame(category = exposure_labels) %>%
    left_join(result, by = "category")
}

format_or_ci <- function(or, lci, uci, is_reference) {
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
  
  result[is_reference] <- "1.00 (ref)"
  
  result
}

# ------------------------------------------------------------
# Counts and percentages within each ALAN category
# ------------------------------------------------------------

count_outcomes <- function(dat, outcome, exposure_labels) {
  counts <- dat %>%
    group_by(exposure, .drop = FALSE) %>%
    summarise(
      N = n(),
      cases = sum(.data[[outcome]] == 1),
      noncases = sum(.data[[outcome]] == 0),
      .groups = "drop"
    ) %>%
    mutate(category = as.character(exposure)) %>%
    select(-exposure)
  
  data.frame(category = exposure_labels) %>%
    left_join(counts, by = "category") %>%
    mutate(
      across(
        c(N, cases, noncases),
        ~ replace_na(.x, 0L)
      ),
      
      cases_percent = if_else(
        N > 0,
        100 * cases / N,
        NA_real_
      ),
      
      noncases_percent = if_else(
        N > 0,
        100 * noncases / N,
        NA_real_
      ),
      
      cases_display = if_else(
        N > 0,
        sprintf("%d (%.1f%%)", cases, cases_percent),
        "0 (NA)"
      ),
      
      noncases_display = if_else(
        N > 0,
        sprintf("%d (%.1f%%)", noncases, noncases_percent),
        "0 (NA)"
      )
    )
}

# ------------------------------------------------------------
# Run one exposure/outcome analysis
# ------------------------------------------------------------

run_analysis <- function(
    dat,
    outcome,
    outcome_label,
    exposure_labels,
    section) {
  
  # Shared sample for Models 1 through 3.
  main_sample <- make_analysis_sample(
    dat,
    outcome = outcome,
    covariates = environmental_covariates
  )
  
  models <- lapply(
    main_model_sets,
    function(covariates) {
      fit_model(
        main_sample,
        outcome = outcome,
        covariates = covariates,
        reference = exposure_labels[1]
      )
    }
  )
  
  main_counts <- count_outcomes(
    main_sample,
    outcome,
    exposure_labels
  )
  
  results <- bind_rows(lapply(names(models), function(model_name) {
    extract_results(
      models[[model_name]],
      exposure_labels
    ) %>%
      mutate(
        section = section,
        outcome = outcome_label,
        model = model_name,
        model_N = nobs(models[[model_name]]),
        .before = 1
      )
  }))
  
  estimates_wide <- results %>%
    mutate(
      estimate = format_or_ci(
        OR,
        LCI,
        UCI,
        category == exposure_labels[1]
      )
    ) %>%
    select(category, model, estimate) %>%
    pivot_wider(
      names_from = model,
      values_from = estimate
    )
  
  display <- main_counts %>%
    select(
      category,
      main_cases = cases_display,
      main_noncases = noncases_display
    ) %>%
    left_join(
      estimates_wide,
      by = "category"
    ) %>%
    mutate(section = section, .before = 1) %>%
    select(
      section,
      category,
      main_cases,
      main_noncases,
      Model1,
      Model2,
      Model3
    )
  
  model_counts <- bind_rows(lapply(names(models), function(model_name) {
    response <- model.response(
      model.frame(models[[model_name]])
    )
    
    data.frame(
      section = section,
      outcome = outcome_label,
      model = model_name,
      N = length(response),
      cases = sum(response == 1),
      noncases = sum(response == 0)
    )
  }))
  
  all_counts <- main_counts %>%
    mutate(
      section = section,
      outcome = outcome_label,
      .before = 1
    )
  
  list(
    outcome = outcome_label,
    section = section,
    main_sample_ids = main_sample$participant_id,
    models = models,
    results = results,
    counts = all_counts,
    model_counts = model_counts,
    display = display
  )
}

# ------------------------------------------------------------
# Fit indoor/outdoor models for each outcome separately
# ------------------------------------------------------------

data_sets <- list(
  indoor = list(
    dat = indoor_model_dat,
    labels = indoor_labels,
    section = "Indoor ALAN"
  ),
  outdoor = list(
    dat = outdoor_model_dat,
    labels = outdoor_labels,
    section = "Outdoor ALAN"
  )
)

outcomes <- c(
  obesity_status = "Obesity",
  diabetes_binary = "Diabetes"
)

analyses <- list()

for (dataset_name in names(data_sets)) {
  specification <- data_sets[[dataset_name]]
  
  for (outcome in names(outcomes)) {
    analysis_name <- paste(
      dataset_name,
      outcome,
      sep = "_"
    )
    
    analyses[[analysis_name]] <- run_analysis(
      dat = specification$dat,
      outcome = outcome,
      outcome_label = unname(outcomes[outcome]),
      exposure_labels = specification$labels,
      section = specification$section
    )
  }
}

# ------------------------------------------------------------
# Save numeric results, counts and model objects
# ------------------------------------------------------------

numeric_results <- bind_rows(lapply(
  analyses,
  function(x) x$results
))

outcome_counts <- bind_rows(lapply(
  analyses,
  function(x) x$counts
))

model_sample_sizes <- bind_rows(lapply(
  analyses,
  function(x) x$model_counts
))

print(model_sample_sizes)

# ------------------------------------------------------------
# Build the TWO display tables
# ------------------------------------------------------------

make_display_data <- function(analyses, outcome_label) {
  selected <- analyses[
    vapply(
      analyses,
      function(x) identical(x$outcome, outcome_label),
      logical(1)
    )
  ]
  
  bind_rows(lapply(
    selected,
    function(x) x$display
  ))
}

obesity_display <- make_display_data(
  analyses,
  "Obesity"
)

diabetes_display <- make_display_data(
  analyses,
  "Diabetes"
)


# ------------------------------------------------------------
# Format editable Word tables
# ------------------------------------------------------------

make_word_table <- function(dat, outcome_label) {
  sections <- unique(dat$section)
  
  body <- bind_rows(lapply(sections, function(section_name) {
    section_dat <- dat %>%
      filter(section == section_name) %>%
      select(-section)
    
    heading <- section_dat[1, , drop = FALSE]
    heading[] <- ""
    heading$category <- section_name
    
    bind_rows(heading, section_dat)
  }))
  
  section_rows <- which(
    body$category %in% sections
  )
  
  notes <- c(
    "Model 1: ALAN + age, sex, centre, education and SES.",
    "Model 2: Model 1 + smoking and physical activity.",
    "Model 3: Model 2 + continuous UVI.",
    paste(
      "Counts describe Models 1 through 3, which use identical",
      "participants within each ALAN section."
    ),
    paste(
      "Percentages are within each ALAN category and analysis sample.",
      "Cases and non-cases sum to 100% apart from rounding."
    ),
    paste(
      "Missing SES, smoking and physical activity are retained as",
      "separate categories. Other required missing values are excluded."
    ),
    "OR = odds ratio; CI = Wald 95% confidence interval; ref = reference.",
    "Nutrition, additional pollutants and combined ALAN are not included."
  )
  
  ft <- flextable(body)
  
  ft <- set_header_labels(
    ft,
    category = "ALAN",
    main_cases = "Cases, n (%)",
    main_noncases = "Non-cases, n (%)",
    Model1 = "Model 1",
    Model2 = "Model 2",
    Model3 = "Model 3"
  )
  
  ft <- add_header_row(
    ft,
    values = c(
      "",
      "Main sample",
      "OR (95% CI)"
    ),
    colwidths = c(1, 2, 3)
  )
  
  ft <- ft %>%
    theme_booktabs() %>%
    font(fontname = "Arial", part = "all") %>%
    fontsize(size = 7.5, part = "all") %>%
    bold(part = "header") %>%
    bold(i = section_rows, part = "body") %>%
    bg(
      i = section_rows,
      bg = "#EEEEEE",
      part = "body"
    ) %>%
    align(
      j = 2:ncol(body),
      align = "center",
      part = "all"
    ) %>%
    align(
      j = "category",
      align = "left",
      part = "all"
    ) %>%
    valign(valign = "center", part = "all") %>%
    padding(padding = 3, part = "all") %>%
    add_footer_lines(values = notes) %>%
    fontsize(size = 7, part = "footer") %>%
    align(align = "left", part = "footer") %>%
    autofit() %>%
    set_table_properties(
      layout = "autofit",
      width = 1
    )
  
  ft
}

table2_obesity <- make_word_table(
  obesity_display,
  outcome_label = "Obesity"
)

table2_diabetes <- make_word_table(
  diabetes_display,
  outcome_label = "Diabetes"
)

# ------------------------------------------------------------
# Export both tables in ONE editable landscape Word document
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
    left = 0.4,
    right = 0.4
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
    "Table 2A. Associations of indoor and outdoor ALAN",
    "with obesity"
  ),
  style = "Normal"
)

word_doc <- flextable::body_add_flextable(
  word_doc,
  value = table2_obesity
)

word_doc <- officer::body_add_break(word_doc)

word_doc <- officer::body_add_par(
  word_doc,
  paste(
    "Table 2B. Associations of indoor and outdoor ALAN",
    "with diabetes"
  ),
  style = "Normal"
)

word_doc <- flextable::body_add_flextable(
  word_doc,
  value = table2_diabetes
)

print(
  word_doc,
  target = "output/Table2_ALAN_obesity_and_diabetes.docx"
)

# ------------------------------------------------------------
# Display the TWO tables in R
# ------------------------------------------------------------

table2_obesity
table2_diabetes