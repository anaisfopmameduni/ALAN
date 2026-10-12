# ------------------------------------------------------------
# Helper: tertiles/quintiles allowing missing values
# ------------------------------------------------------------

ntile_na <- function(x, n) {
  out <- rep(NA_integer_, length(x))
  ok <- !is.na(x)
  out[ok] <- dplyr::ntile(x[ok], n)
  out
}

# ------------------------------------------------------------
# Preprocessing function
# ------------------------------------------------------------

prepare_analysis_data <- function(dat) {
  
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
      # Stata: egen sleep_cat = cut(g13_tiempo_dormir),
      #                  at(2,6,7,8,9,12)
      # Then: recode sleep_cat (2=.)
      sleep_cat_code = case_when(
        g13_tiempo_dormir >= 2  & g13_tiempo_dormir < 6  ~ 2,
        g13_tiempo_dormir >= 6  & g13_tiempo_dormir < 7  ~ 6,
        g13_tiempo_dormir >= 7  & g13_tiempo_dormir < 8  ~ 7,
        g13_tiempo_dormir >= 8  & g13_tiempo_dormir < 9  ~ 8,
        g13_tiempo_dormir >= 9  & g13_tiempo_dormir < 12 ~ 9,
        TRUE ~ NA_real_
      ),
      sleep_cat_code = na_if(sleep_cat_code, 2),
      
      # Text labels used later by the SHSS code
      sleep_cat = case_when(
        sleep_cat_code == 6 ~ "<=6h",
        sleep_cat_code == 7 ~ "7h",
        sleep_cat_code == 8 ~ "8h",
        sleep_cat_code == 9 ~ ">=9h",
        TRUE ~ NA_character_
      ),
      sleep_cat = factor(
        sleep_cat,
        levels = c("<=6h", "7h", "8h", ">=9h")
      ),
      
      # Stata recodes sleep_cat codes 6, 7, 8, 9 to 2, 1, 1, 3
      sleep_cat_3 = case_when(
        sleep_cat_code == 6 ~ 2,
        sleep_cat_code %in% c(7, 8) ~ 1,
        sleep_cat_code == 9 ~ 3,
        TRUE ~ NA_real_
      ),
      
      siesta_general = case_when(
        g20_siesta == 0 ~ 0,
        g20_siesta %in% 1:7 ~ 1,
        TRUE ~ NA_real_
      ),
      
      # Stata: egen siesta_60 = cut(g21_tiempo_siesta),
      #                         at(5,60,1000)
      # Then: recode siesta_60 (5=0) (60=1)
      siesta_60 = case_when(
        g20_siesta == 0 ~ 0,  # Does not nap: not a long nap
        g20_siesta > 0 &
          g21_tiempo_siesta >= 60 &
          g21_tiempo_siesta < 1000 ~ 1,
        g20_siesta > 0 &
          g21_tiempo_siesta >= 5 &
          g21_tiempo_siesta < 60 ~ 0,
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
    ) %>%
    select(-sleep_cat_code)
  
  shss_vars <- c(
    "sleep_cat",
    "g14_problemas_sue_o",
    "g15_list_probl_medic",
    "siesta_60"
  )
  
  print(data.frame(
    variable = shss_vars,
    missing_n = sapply(dat[shss_vars], \(x) sum(is.na(x))),
    missing_pct = sapply(dat[shss_vars], \(x) mean(is.na(x)) * 100)
  ))
  
  cat(
    "Complete on Do-file SHSS fields:",
    sum(complete.cases(dat[c("sleep_cat", "g14_problemas_sue_o", "siesta_60")])),
    "\n"
  )
  
  print(table(
    cut(
      dat$g21_tiempo_siesta,
      breaks = c(-Inf, 5, 60, 1000, Inf),
      right = FALSE
    ),
    useNA = "ifany"
  ))
  
  print(dat %>%
          filter(is.na(siesta_60)) %>%
          count(g21_tiempo_siesta, sort = TRUE))
  
  # ----------------------------
  # Bedtime
  # ----------------------------
  
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
        a3_sexe == 1 & waist_hip > 0.90 ~ 1,
        a3_sexe == 1 & waist_hip <= 0.90 ~ 0,
        a3_sexe == 2 & waist_hip > 0.85 ~ 1,
        a3_sexe == 2 & waist_hip <= 0.85 ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  # ----------------------------
  # Tertiles and quintiles
  # ----------------------------
  
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
  # Simplified Healthy Sleep Score
  # ----------------------------
  
  dat <- dat %>%
    mutate(
      SHSS = 0,
      SHSS = SHSS + if_else(
        is.na(sleep_cat) | sleep_cat %in% c(">=9h"),
        1,
        0
      ),
      SHSS = SHSS + if_else(
        !is.na(g14_problemas_sue_o) & g14_problemas_sue_o == 1,
        1,
        0
      ),
      SHSS = SHSS + if_else(
        !is.na(g15_list_probl_medic) & g15_list_probl_medic == 1,
        1,
        0
      ),
      SHSS = SHSS + if_else(
        !is.na(siesta_60) & siesta_60 == 1,
        1,
        0
      ),
      
      # Match: replace SHSS = . if missing(sleep_cat, sleep_problems, siesta_60)
      SHSS = if_else(
        is.na(sleep_cat) |
          is.na(g14_problemas_sue_o) |
          is.na(siesta_60),
        NA_real_,
        as.numeric(SHSS)
      ),
      
      SHSS_group = case_when(
        SHSS == 0 ~ 0,
        SHSS == 1 ~ 1,
        SHSS %in% 2:4 ~ 2,
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
  
  dat
}


build_final_dataset <- function(dat) {
  
  dat %>%
    select(any_of(final_vars)) %>%
    rename(
      participant_id = id_study,
      night_shift_ever = nightever,
      sex = a3_sexe,
      age_years = a4_edat,
      education = education_basic_final,
      socioeconomic_score = score_SE,
      bmi = bmi_actual,
      waist_hip_ratio = waist_hip,
      sleep_duration = g13_tiempo_dormir,
      sleep_problems = g14_problemas_sue_o,
      sleep_medication = g15_list_probl_medic,
      nap_frequency = g20_siesta,
      nap_duration = g21_tiempo_siesta,
      
      smoking_status = fumador_presente,
      
      physical_activity = mets_act_10_2_total_rec,
      fruits = gra_fruits,
      vegetables = gra_vegetables,
      energy_intake = t_energy,
      uvi = uvi_longest,
      ndvi = ndvi_500_longest,
      
      chronotype_3 = msf_c3,
      chronotype_5 = msf_c5,
      
      indoor_alan = indoorlight_lab,
      diabetes_original = diabetes,
      diabetes_binary = diabetes_new,
      family_history_diabetes = fam_history_diabetes,
      
      centre = centre_num,
      waist_hip_risk = WHR_cat,
      bmi_who = bmi_WHO,
      bmi_binary = bmi_2,
      bmi_three_category = bmi_3,
      bmi_category = bmi_cat,
      obesity_status = obesity,
      
      sleep_category = sleep_cat,
      sleep_duration_category = sleep_cat_3,
      
      ms_bg_current = msibg_current,
      ms_bg_longest = msibg_longest,
      ms_gr_longest = msigr_longest,
      impaired_ms_gr_longest = impmsigr_longest,
      melanopic_pigment_gr_longest = melgr_longest,
      impaired_melanopic_pigment_gr_longest = impmelgr_longest,
      
      ms_bg_current_group = MSIbg_group_c,
      ms_bg_longest_group = MSIbg_group_l,
      ms_gr_longest_group = MSIgr_group_l,
      impaired_ms_gr_group = ImpMSIgr_group_l,
      melanopic_pigment_gr_group = Melgr_group_l,
      impaired_melanopic_pigment_gr_group = ImpMelgr_group_l,
      
      alan_combined_normalized = ALAN_combined_norm,
      alan_combined_category = ALAN_combined_cat,
      
      simplified_healthy_sleep_score = SHSS,
      simplified_healthy_sleep_group = SHSS_group,
      
      weighted_sleep_latency = sl_mean,
      sleep_latency_category = sl_group,
      sleep_latency_workdays = sleep_latency_w,
      sleep_latency_free_days = sleep_latency_f,
      sleep_onset_workdays = sow,
      sleep_onset_free_days = sof,
      midsleep_workdays = msw,
      sleep_duration_workdays = sdw,
      relative_social_jetlag = sjlrel,
      bedtime = bed_time_new,
      
      fruits_tertile = gra_fruits_group,
      vegetables_tertile = gra_vegetables_group,
      energy_intake_tertile = t_energy_group,
      uvi_tertile = uvi_longest_group
    )
}



build_model_dataset <- function(dat_final) {
  
  dat_final %>%
    filter(
      !is.na(obesity_status),
      !is.na(diabetes_binary),
      !is.na(age_years),
      !is.na(sex),
      !is.na(centre),
      !is.na(education),
      !is.na(socioeconomic_score),
      !is.na(physical_activity),
      !is.na(smoking_status),
      !is.na(fruits_tertile),
      !is.na(vegetables_tertile),
      !is.na(energy_intake_tertile)
    ) %>%
    mutate(
      across(
        c(
          simplified_healthy_sleep_score,
          simplified_healthy_sleep_group,
          sleep_duration_category,
          sex,
          centre,
          education,
          socioeconomic_score,
          physical_activity,
          smoking_status,
          fruits_tertile,
          vegetables_tertile,
          energy_intake_tertile,
          uvi_tertile,
          indoor_alan,
          ms_bg_longest_group,
          ms_gr_longest_group,
          impaired_ms_gr_group,
          melanopic_pigment_gr_group,
          impaired_melanopic_pigment_gr_group,
          alan_combined_category,
          siesta_ultimate,
          bedtime_group,
          sleep_problems,
          sleep_medication,
          family_history_diabetes,
          chronotype_3
        ),
        as.factor
      )
    )
}
