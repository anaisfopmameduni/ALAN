library(dplyr)
library(tidyr)

check_missing_overlap <- function(dat, dataset_name) {
  
  miss_vars <- c(
    bmi = "bmi",
    diabetes = "diabetes_binary",
    shss = "simplified_healthy_sleep_score",
    indoor_alan = "indoor_alan"
  )
  
  miss <- dat %>%
    transmute(
      across(
        all_of(unname(miss_vars)),
        ~ is.na(.x),
        .names = "{.col}"
      )
    )
  
  cat("\n==============================\n")
  cat(dataset_name, "\n")
  cat("==============================\n\n")
  
  # Number missing for each variable
  cat("Missing counts:\n")
  
  n_total <- nrow(miss)
  
  print(
    miss %>%
      summarise(
        across(everything(), ~ sum(.x))
      ) %>%
      pivot_longer(
        everything(),
        names_to = "variable",
        values_to = "n_missing"
      ) %>%
      mutate(
        n_total = n_total,
        pct_missing = round(100 * n_missing / n_total, 1)
      )
  )
  
  # Pairwise overlap
  cat("\nPairwise missingness overlap:\n")
  
  pairs <- combn(names(miss), 2, simplify = FALSE)
  
  pairwise_results <- map_dfr(pairs, function(p) {
    a <- miss[[p[1]]]
    b <- miss[[p[2]]]
    
    tibble(
      variable_1 = p[1],
      variable_2 = p[2],
      both_missing = sum(a & b),
      only_1_missing = sum(a & !b),
      only_2_missing = sum(!a & b),
      neither_missing = sum(!a & !b),
      jaccard = if_else(
        sum(a | b) == 0,
        NA_real_,
        sum(a & b) / sum(a | b)
      )
    )
  })
  
  print(pairwise_results)
  
  # Four-way overlap
  cat("\nFour-way missingness pattern:\n")
  
  pattern_counts <- miss %>%
    count(
      bmi_missing = bmi,
      diabetes_missing = diabetes_binary,
      shss_missing = simplified_healthy_sleep_score,
      alan_missing = indoor_alan,
      name = "n"
    ) %>%
    arrange(desc(n))
  
  print(pattern_counts)
  
  # Summary of four-way overlap
  all_four <- miss %>%
    filter(
      bmi &
        diabetes_binary &
        simplified_healthy_sleep_score &
        indoor_alan
    ) %>%
    nrow()
  
  any_missing <- miss %>%
    filter(
      bmi |
        diabetes_binary |
        simplified_healthy_sleep_score |
        indoor_alan
    ) %>%
    nrow()
  
  none_missing <- miss %>%
    filter(
      !bmi &
        !diabetes_binary &
        !simplified_healthy_sleep_score &
        !indoor_alan
    ) %>%
    nrow()
  
  cat("\nSummary:\n")
  cat("All four missing:", all_four, "\n")
  cat("At least one missing:", any_missing, "\n")
  cat("None missing:", none_missing, "\n")
  cat("Total participants:", nrow(miss), "\n")
  
  invisible(
    list(
      missing_indicators = miss,
      pairwise = pairwise_results,
      patterns = pattern_counts
    )
  )
}

indoor_missing_checks <- check_missing_overlap(
  indoor_final,
  "Indoor analysis dataset"
)

outdoor_missing_checks <- check_missing_overlap(
  outdoor_final,
  "Outdoor analysis dataset"
)