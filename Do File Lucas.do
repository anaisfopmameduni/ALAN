*Do File Lucas Melmer-Wolf MA Sleep, ALAN and Obesity/diabetes

*Changes to the sample size
drop if nightever == 1

*Only BCN/MDR sample
keep in 1/1475

*delete mi information
gen _mi_miss=0
gen _mi_id=0
mi extract 0, clear

*Regroup confounders to keep n
replace score_SE = 9 if missing(score_SE)
label define score_SE_lbl 9 "Missing", add
label values score_SE score_SE_lbl

replace fumador_ever = 9 if missing(fumador_ever)
label define fumador_ever_lbl 9 "Missing", add
label values fumador_ever fumador_ever_lbl

replace mets_act_10_2_total_rec = 9 if missing(mets_act_10_2_total_rec)
label define mets_act_lbl 9 "Missing", add
label values mets_act_10_2_total_rec mets_act_lbl

replace uvi_longest_group = 9 if missing(uvi_longest_group)
label define uvi_longest_lbl 9 "Missing", add
label values uvi_longest_group uvi_longest_lbl

*Creation of new variables
gen bmi_WHO = .
replace bmi_WHO = 1 if bmi_actual >= 12 & bmi_actual < 18.5     // Underweight
replace bmi_WHO = 2 if bmi_actual >= 18.5 & bmi_actual < 25      // Normal weight
replace bmi_WHO = 3 if bmi_actual >= 25 & bmi_actual < 30        // Overweight
replace bmi_WHO = 4 if bmi_actual >= 30                         // Obesity
replace bmi_WHO = . if bmi_actual < 12 | bmi_actual == .
label define bmi_labels 1 "Underweight" 2 "Normal weight" 3 "Overweight" 4 "Obesity"
label values bmi_WHO bmi_labels

gen bmi_new = .
replace bmi_2 = 0 if bmi_WHO == 2   // Normal weight (control group)
replace bmi_2 = 1 if bmi_WHO == 3 | bmi_WHO == 4   // Overweight and obese
replace bmi_2 = . if bmi_WHO == 1
label define bmi_2_labels 1 "Control" 2 "Overweight and obese"
label values bmi_2 bmi_new_labels

gen bmi_3 = .
replace bmi_3 = 0 if bmi_WHO == 2
replace bmi_3 = 1 if bmi_WHO == 3
replace bmi_3 = 2 if bmi_WHO == 4
label define bmi_3_labels 1 "normalweight" 2 "overweight" 3 "obese"
label values bmi_3 bmi_3_labels

egen sleep_cat = cut(g13_tiempo_dormir), at(2,6,7,8,9,12)
recode sleep_cat (2=.)
label define sleep_cat 6 "≤6h" 7 "7h" 8 "8h" 9 "≥9h"
label values sleep_cat sleep_cat

egen bmi_cat= cut(bmi_actual), at(0,25,30,100)
label define bmi_cat 0 "normal weight" 25 "overweight" 30 "obese"
label values bmi_cat bmi_cat
gen obesity = .
replace obesity = 0 if bmi_cat == 0
replace obesity = 0 if bmi_cat == 25
replace obesity = 1 if bmi_cat == 30
label define obesity 0 "non-obese" 1 "obese"
label values obesity obesity

encode area, gen(centre_num)

gen WHR_cat = .
replace WHR_cat = 1 if a3_sex == 1 & waist_hip > 0.90
replace WHR_cat = 0 if a3_sex == 1 & waist_hip <= 0.90
replace WHR_cat = 1 if a3_sex == 2 & waist_hip > 0.85
replace WHR_cat = 0 if a3_sex == 2 & waist_hip <= 0.85
label define WHR_cat 0 "non-obese" 1 "obese"
label values WHR_cat WHR_cat

recode diabetes (1=1) (2=2) (3=2) (4=2) (5=2) (missing=9), gen(diabetes_new)
recode diabetes_new (1=0) (2=1)
label define diabetes_new 0 "non-diabetic" 1 "diabetic"
label values diabetes_new diabetes_new

xtile gra_fruits_group = gra_fruits, nquantiles(3)
xtile gra_vegetables_group = gra_vegetables, nquantiles(3)
xtile t_energy_group = t_energy, nquantiles(3)
xtile uvi_longest_group = uvi_longest, nquantiles(3)
xtile ndvi_500_longest_group = ndvi_500_longest, nquantiles(3)

gen siesta_general = g20_siesta
recode siesta_general (0=0) (1=1) (2=1) (3=1) (4=1) (5=1) (6=1) (7=1)
*0 = no siesta, 1 = siesta
label define siesta_general 0 "no" 1 "yes"
label values siesta_general siesta_general

egen siesta_60 = cut(g21_tiempo_siesta), at(5,60,1000)
recode siesta_60 (5=0) (60=1)
*0 = less then 60min, 1 = more then 60min
label define siesta_60 0 "<60min" 1 "≥60min"
label values siesta_60 siesta_60

gen siesta_frecat = g20_siesta
recode siesta_general (0=0) (1=1) (2=1) (3=2) (4=2) (5=2) (6=2) (7=3)
*0 = no siesta, 1 = 1-2, 2 = 3-6, 3 = 7
label define siesta_frecat 0 "never" 1 "1-2/week" 2 "3-6/week" 3 "everyday"
label values siesta_frecat siesta_frecat

gen siesta_ultimate = .
replace siesta_ultimate = 0 if siesta_gen == 0
replace siesta_ultimate = 1 if siesta_60 == 0 & siesta_gen != 0
replace siesta_ultimate = 2 if siesta_60 == 1 & siesta_gen != 0
label define siesta_ultimate 0 "never" 1 "<60" 2 "≥60"
label values siesta_ultimate siesta_ultimate

*Bed Timing
gen str_g12_hora_dormir = string(g12_hora_dormir, "%tcHH:MM")
gen h_dormir = real(substr(str_g12_hora_dormir, 1, 2))
gen min_dormir = real(substr(str_g12_hora_dormir, 4, 2))
list g12_hora_dormir str_g12_hora_dormir h_dormir min_dormir in 1/10
recode h_dormir (99=.)
recode min_dormir (99=.)
gen time_sl_min_dec = min_dormir / 60
gen bed_time_new = h_dormir + time_sl_min_dec
list bed_time_new in 1/100

gen bedtime_group = .
replace bedtime_group = 1 if bed_time_new < 23
replace bedtime_group = 2 if bed_time_new >= 23 & bed_time_new < 24
replace bedtime_group = 3 if bed_time_new >= 0 & bed_time_new < 1
replace bedtime_group = 4 if (bed_time_new >= 1 & bed_time_new < 10)
replace bedtime_group = . if bed_time_new >= 10 & bed_time_new < 17
label define bedtime_group 1 "before 23:00" 2 "23:01-00:00" 3 "00:01-01:00" 4 "after 01:00"
label values bedtime_group bedtime_group
tabulate bedtime_group, missing

*Circadian variables
gen sleep_cat_3 = sleep_cat
recode sleep_cat_3 (6=2) (7=1) (8=1) (9=3)
label define sleep_cat_3 1 "optimal" 2 "short" 3 "long"
label values sleep_cat_3 sleep_cat_3
tabulate sleep_cat_3, missing

*Grouping outdoor ALAN
xtile MSIbg_group_c = msibg_current, nquantiles(3)
xtile MSIbg_group_l = msibg_longest, nquantiles(3)
xtile VSL_group_c = vlggr_current, nquantiles(3)
xtile VSL_group_l = vlggr_longest, nquantiles(3)

xtile MSIgr_group_l = msigr_longest, nquantiles(5)
label define MSIgr_group_l 1 "very low" 2 "low" 3 "intermediate" 4 "high" 5 "very high"
label values MSIgr_group_l MSIgr_group_l
tabulate MSIgr_group_l

xtile ImpMSIgr_group_l = impmsigr_longest, nquantiles(5)
label define ImpMSIgr_group_l 1 "very low" 2 "low" 3 "intermediate" 4 "high" 5 "very high"
label values ImpMSIgr_group_l ImpMSIgr_group_l
tabulate ImpMSIgr_group_l

xtile Melgr_group_l = melgr_longest, nquantiles(5)
label define Melgr_group_l 1 "very low" 2 "low" 3 "intermediate" 4 "high" 5 "very high"
label values Melgr_group_l Melgr_group_l
tabulate Melgr_group_l

xtile ImpMelgr_group_l = impmelgr_longest, nquantiles(5)
label define ImpMelgr_group_l 1 "very low" 2 "low" 3 "intermediate" 4 "high" 5 "very high"
label values ImpMelgr_group_l ImpMelgr_group_l
tabulate ImpMelgr_group_l

*Grouping MSIbg by centre specific cutoffs
gen MSIgr_BCN = .
gen MSIgr_MAD = .

quietly _pctile msigr_longest if area=="BCN", p(20 40 60 80)
replace MSIgr_BCN = 1 if msigr_longest <= r(c_1)
replace MSIgr_BCN = 2 if msigr_longest > r(c_1) & msigr_longest <= r(c_2)
replace MSIgr_BCN = 3 if msigr_longest > r(c_2) & msigr_longest <= r(c_3)
replace MSIgr_BCN = 4 if msigr_longest > r(c_3) & msigr_longest <= r(c_4)
replace MSIgr_BCN = 5 if msigr_longest > r(c_4)

quietly _pctile msigr_longest if area=="Madrid", p(20 40 60 80)
replace MSIgr_MAD = 1 if msigr_longest <= r(c_1)
replace MSIgr_MAD = 2 if msigr_longest > r(c_1) & msigr_longest <= r(c_2)
replace MSIgr_MAD = 3 if msigr_longest > r(c_2) & msigr_longest <= r(c_3)
replace MSIgr_MAD = 4 if msigr_longest > r(c_3) & msigr_longest <= r(c_4)
replace MSIgr_MAD = 5 if msigr_longest > r(c_4)

label define MSIgr_lab 1 "very low" 2 "low" 3 "intermediate" 4 "high" 5 "very high"
label values MSIgr_BCN MSIgr_lab
label values MSIgr_MAD MSIgr_lab

tabulate MSIgr_BCN area
tabulate MSIgr_MAD area

*Combining indoor and outdoor ALAN
gen ALAN_combined_norm = (MSIgr_group_l + indoorlight_lab) / 9
gen ALAN_combined_cat = .
replace ALAN_combined_cat = 1 if ALAN_combined_norm >= 0 & ALAN_combined_norm <= 0.40
replace ALAN_combined_cat = 2 if ALAN_combined_norm > 0.40 & ALAN_combined_norm <= 0.60
replace ALAN_combined_cat = 3 if ALAN_combined_norm > 0.60 & ALAN_combined_norm <= 0.80
replace ALAN_combined_cat = 4 if ALAN_combined_norm > 0.80 & ALAN_combined_norm <= 1
label define ALAN_comb_lab 1 "very low" 2 "low" 3 "high" 4 "very high"
label values ALAN_combined_cat ALAN_comb_lab
tabulate ALAN_combined_cat, missing

*Merging with family history
*Drop everything but id_study and j6_diabetes
*Table (j6_diabetes) showed 0 and 9 as missing values, 1 (4 917), 2 (68 626)
replace j6_diabetes = 9 if j6_diabetes == 0 
collapse (min) j6_diabetes, by (id_study)
table (j6_diabetes)
merge 1:1 id_study using "Family History Merge"
*_merge (nur master = 1, nur history = 2, beide = 3)
*1=no family history of diabetes
*2=at least 1 relative
label define fam_history_diabetes 1 "no fam history" 2 "at least 1 relative"
label values fam_history_diabetes fam_history_diabetes
rename _merge _merge_familyhistory

*Merging with additional ALAN variables
*Drop variables that already are in the master file
merge 1:1 id_study using "Additional ALAN Merge prepared"
*merge (nur master=1, nur using=2, beides=3)
drop if _merge ==2

*Sleeping Scores
*Simplified Healthy Sleep Score (SHSS)
gen SHSS = 0
replace SHSS = SHSS + 1 if sleep_cat <= 6 | sleep_cat >= 9
replace SHSS = SHSS + 1 if g14_problemas_sue_o == 1
replace SHSS = SHSS + 1 if g15_list_probl_medic == 1
replace SHSS = SHSS + 1 if siesta_60 == 1
replace SHSS = . if missing(sleep_cat, g14_problemas_sue_o, siesta_60)

*Grouped SHSS
gen SHSS_group = SHSS
recode SHSS_group (0=0) (1=1) (2=2) (3=2) (4=2) (.=.)
label define SHSS_group 0 "optimal sleep" 1 "slightly unhealthy sleep" 2 "unhealthy sleep"
label values SHSS_group SHSS_group
tabulate SHSS_group, missing

*Revised SHSS3
gen SHSS3 = 0
replace SHSS3 = SHSS3 + 1 if sleep_cat <= 6 | sleep_cat >= 9
replace SHSS3 = SHSS3 + 1 if g14_problemas_sue_o == 1 | g15_list_probl_medic == 1
replace SHSS3 = SHSS3 + 1 if siesta_ultimate == 2
replace SHSS3 = . if missing(sleep_cat, g14_problemas_sue_o, siesta_ultimate)
*Grouped SHSS3
gen SHSS3_group = SHSS3
recode SHSS3_group (0=0) (1=1) (2=2) (3=2) (.=.)
label define SHSS3_group 0 "optimal sleep" 1 "slightly unhealthy sleep" 2 "unhealthy sleep"
label values SHSS3_group SHSS3_group
tabulate SHSS3_group, missing

*Extended Healthy Sleep Score (EHSS)
gen EHSS = 0
replace EHSS = EHSS + 1 if sleep_cat <= 6 | sleep_cat >= 9
replace EHSS = EHSS + 1 if g14_problemas_sue_o == 1 | g15_list_probl_medic == 1
replace EHSS = EHSS + 1 if siesta_ultimate == 2
replace EHSS = EHSS + 1 if sl_group == 1
replace EHSS = EHSS + 1 if msf_c5 == 5
replace EHSS = EHSS + 1 if sjl_c == 3
replace EHSS = . if missing(sleep_cat, g14_problemas_sue_o, siesta_ultimate, sl_group, msf_c5, sjl_c)
tabulate EHSS, missing

gen EHSS_group = EHSS
recode EHSS_group (0=0) (1=1) (2=2) (3=2) (4=3) (5=3) (6=3) (.=.)
label define EHSS_group 0 "optimal sleep" 1 "adequate sleep" 2 "slightly unhealthy sleep" 3 "unhealthy sleep"
label values EHSS_group EHSS_group
tabulate EHSS_group, missing

gen EHSS3_group = EHSS
recode EHSS3_group (0=0) (1=1) (2=1) (3=2) (4=2) (5=2) (6=2) (.=.)
label define EHSS3_group 0 "optimal sleep" 1 "slighlty unhealthy sleep" 2 "unhealthy sleep"
label values EHSS3_group EHSS3_group
tabulate EHSS3_group, missing

*Sleep preparation workdays and Sleep Latency Workdays
gen min_lab_dec=a2b_min_apagar_luces/60
gen sprepw=a2a_hora_apagar_luces+min_lab_dec
sum sprepw
label var sprepw "Sleep preparation workdays"
gen min_lab_conciliar=a3_min_conciliar/60
gen sleep_latency_w=sprepw+min_lab_conciliar
replace sleep_latency=sleep_latency_w-24 if sleep_latency_w>=24
tab sleep_latency_w
label var sleep_latency_w "Sleep latency workdays"

*Sleep preparation weekends and Sleep Latency Weekends
gen min_lib_dec=a8b_min_apagar_luces/60
gen min_lib_conciliar=a9_min_conciliar/60
gen sprepf=a8a_hora_apagar_luces+min_lib_dec
label var sprepf "Sleep preparation free days"
gen sleep_latency_f=sprepf+min_lib_conciliar
replace sleep_latency_f=sleep_latency_f-24 if sleep_latency_f>=24
tab sleep_latency_f
label var sleep_latency_f "Sleep latency free days"

*Weighted mean
gen sl_mean=5*sleep_latency_w+2*sleep_latency_f/7
label var sl_mean "Sleep latency weighted"

*Sleep latency groups
gen sl_group = .
replace sl_group = 0 if sl_mean <= 20 & sl_mean != .
replace sl_group = 1 if sl_mean > 20 & sl_mean != .
label var sl_group "Sleep latency groups"
label define sl_group 0 "≤20min" 1 ">20min"
label values sl_group sl_group

*Sleep Onset from Kyriaki
gen min_lab_conciliar=a3_min_conciliar/60
gen sow=sprepw+min_lab_conciliar
replace sow=sow-24 if sow>=24
tab sow
label var sow "Sleep onset workdays"
gen sof=sprepf+min_lib_conciliar
replace sof=sof-24 if sof>=24
tab sof
label var sof "Sleep onset free days"

gen msw=sow+sdw/2
replace msw=msw-24 if msw>24
format msw %-9.2fc
tab msw
label var msw "Mid-sleep time workdays"
browse msf msw 

gen sjlrel=msf-msw
tab sjlrel
codebook sjlrel
format sjlrel  %-9.2fc
label var sjlrel "Relative social Jet lag"
histogram sjlrel if sjlrel>0, percent 

gen min_lab_desp=a4b_min_despertar/60
gen sew=a4a_hora_despertar+min_lab_desp
tab sew
label var sew "Sleep end workdays"

gen sdw=sew-sow
replace sdw=sdw+24 if sdw<0
label var sdw "Sleep duration workdays"
tab sdw
histogram sdw, percent normal

gen sof=sprepf+min_lib_conciliar
replace sof=sof-24 if sof>=24
tab sof
label var sof "Sleep onset free days"

gen min_lib_desp=a10b_min_despertar/60
gen sef=a10a_hora_despertar+min_lib_desp
tab sef
label var sef "Sleep end free days"

gen sjl_group = .
replace sjl_group = 0 if sjlrel 


*Descriptive Tables:
*Installieren table1_mc Funktion:
ssc install table1_mc
*Table of descriptives divided by outcomes (obesity and diabetes)
table1_mc, by (bmi_3) vars(a4_edat contn %4.1f\a3_sexe cat %4.1f\area cat %4.1f\education_basic_final cat %4.1f\score_SE cat %4.1f\bmi_cat cat %4.1f\waist_hip contn %4.1f\WHR_cat cat %4.1f\fam_history_diabetes cat %4.1f\gra_fruits contn %4.1f\gra_vegetables contn %4.1f\t_energy contn %4.1f\fumador_presente cat %4.1f\mets_act_10_2_total_rec cat %4.1f\sleep_cat cat %4.1f\bedtime_group cat %4.1f\siesta_general cat %4.1f\siesta_20 cat %4.1f\siesta_frecat cat %4.1f\g14_problemas_sue_o cat %4.1f\g21_tiempo_siesta contn %4.1f\msf_c3 cat %4.1f\uvi_longest contn %4.1f\) nospace missing onecol total(before)
table1_mc, by (diabetes_new) vars(a4_edat contn %4.1f\a3_sexe cat %4.1f\area cat %4.1f\education_basic_final cat %4.1f\score_SE cat %4.1f\bmi_actual contn %4.1f\bmi_cat cat %4.1f\waist_hip contn %4.1f\WHR_cat cat %4.1f\diabetes_new cat %4.1f\fam_history_diabetes cat %4.1f\gra_fruits contn %4.1f\gra_vegetables contn %4.1f\t_energy contn %4.1f\fumador_presente cat %4.1f\mets_act_10_2_total_rec cat %4.1f\sleep_cat cat %4.1f\bedtime_group cat %4.1f\siesta_general cat %4.1f\siesta_20 cat %4.1f\siesta_frecat cat %4.1f\g14_problemas_sue_o cat %4.1f\g21_tiempo_siesta contn %4.1f\msf_c3 cat %4.1f\uvi_longest contn %4.1f\) nospace missing onecol total(before)
*Table of descriptives divided by centre (BCN vs. Madrid)
table1_mc, by (area) vars(a4_edat contn %4.1f\a3_sexe cat %4.1f\area cat %4.1f\education_basic_final cat %4.1f\score_SE cat %4.1f\bmi_actual contn %4.1f\bmi_cat cat %4.1f\waist_hip contn %4.1f\WHR_cat cat %4.1f\diabetes_new cat %4.1f\fam_history_diabetes cat %4.1f\gra_fruits contn %4.1f\gra_vegetables contn %4.1f\t_energy contn %4.1f\fumador_presente cat %4.1f\mets_act_10_2_total_rec cat %4.1f\sleep_cat cat %4.1f\bedtime_group cat %4.1f\siesta_general cat %4.1f\siesta_20 cat %4.1f\siesta_frecat cat %4.1f\g14_problemas_sue_o cat %4.1f\g21_tiempo_siesta contn %4.1f\msf_c3 cat %4.1f\uvi_longest contn %4.1f\) nospace missing onecol total(before)
*Table of descriptives divided by indoor ALAN exposure
table1_mc, by (indoorlight_lab) vars(a4_edat contn %4.1f\a3_sexe cat %4.1f\area cat %4.1f\education_basic_final cat %4.1f\score_SE cat %4.1f\bmi_cat cat %4.1f\waist_hip contn %4.1f\WHR_cat cat %4.1f\fam_history_diabetes cat %4.1f\gra_fruits contn %4.1f\gra_vegetables contn %4.1f\t_energy contn %4.1f\fumador_presente cat %4.1f\mets_act_10_2_total_rec cat %4.1f\sleep_cat cat %4.1f\bedtime_group cat %4.1f\siesta_general cat %4.1f\siesta_20 cat %4.1f\siesta_frecat cat %4.1f\g14_problemas_sue_o cat %4.1f\g21_tiempo_siesta contn %4.1f\msf_c3 cat %4.1f\uvi_longest contn %4.1f\) nospace missing onecol total(before)
*Table of descriptives divided by outdoor ALAN exposure
table1_mc, by (MSIgr_group_l) vars(a4_edat contn %4.1f\a3_sexe cat %4.1f\area cat %4.1f\education_basic_final cat %4.1f\score_SE cat %4.1f\bmi_cat cat %4.1f\waist_hip contn %4.1f\WHR_cat cat %4.1f\fam_history_diabetes cat %4.1f\gra_fruits contn %4.1f\gra_vegetables contn %4.1f\t_energy contn %4.1f\fumador_presente cat %4.1f\mets_act_10_2_total_rec cat %4.1f\sleep_cat cat %4.1f\bedtime_group cat %4.1f\siesta_general cat %4.1f\siesta_20 cat %4.1f\siesta_frecat cat %4.1f\g14_problemas_sue_o cat %4.1f\g21_tiempo_siesta contn %4.1f\msf_c3 cat %4.1f\uvi_longest contn %4.1f\) nospace missing onecol total(before)
* Table of sleep descriptives divided by outcomes (obesity and diabetes)
table1_mc, by (bmi_3) vars(g13_tiempo_dormir contn %4.1f\sleep_cat cat %4.1f\g14_problemas_sue_o cat %4.1f\g15_list_probl_medic cat %4.1f\bedtime_group cat %4.1f\siesta_general cat %4.1f\siesta_frecat cat %4.1f\g21_tiempo_siesta contn %4.1f\siesta_ultimate cat %4.1f\sl_mean contn %4.1f\sl_group cat %4.1f\sjl_c cat %4.1f\) nospace missing onecol total(before)
table1_mc, by (diabetes_new) vars(g13_tiempo_dormir contn %4.1f\sleep_cat cat %4.1f\g14_problemas_sue_o cat %4.1f\g15_list_probl_medic cat %4.1f\bedtime_group cat %4.1f\siesta_general cat %4.1f\siesta_frecat cat %4.1f\g21_tiempo_siesta contn %4.1f\siesta_ultimate cat %4.1f\sl_mean contn %4.1f\sl_group cat %4.1f\sjl_c cat %4.1f\) nospace missing onecol total(before)
* Table of outdoor ALAN variables by outcomes
table1_mc, by (bmi_3) vars(MSIgr_group_l cat %4.1f\ImpMSIgr_group_l cat %4.1f\Melgr_group_l cat %4.1f\ImpMelgr_group_l cat %4.1f\) nospace missing onecol total(before)
table1_mc, by (diabetes_new) vars(MSIgr_group_l cat %4.1f\ImpMSIgr_group_l cat %4.1f\Melgr_group_l cat %4.1f\ImpMelgr_group_l cat %4.1f\) nospace missing onecol total(before)

*Logistic models

drop if missing(obesity, diabetes_new, a4_edat, a3_sexe, centre_num, education_basic_final, score_SE, mets_act_10_2_total_rec, fumador_ever, gra_fruits_group, gra_vegetables_group, t_energy_group)
 
*Association SHSS and obesity
logistic obesity i.SHSS
logistic obesity i.SHSS a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE
logistic obesity i.SHSS a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group
logistic obesity i.SHSS a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group i.uvi_longest_group
*Association SHSS and diabetes
logistic diabetes_new i.SHSS
logistic diabetes_new i.SHSS a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE
logistic diabetes_new i.SHSS a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group
*Association SHSS3 and obesity
mlogit bmi_3 i.SHSS3_group, rrr baseoutcome(0)
mlogit bmi_3 i.SHSS3_group a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE, rrr baseoutcome(0)
mlogit bmi_3 i.SHSS3_group a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group, rrr baseoutcome(0)
*Association SHSS3 and diabetes
logistic diabetes_new i.SHSS3_group
logistic diabetes_new i.SHSS3_group a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE
logistic diabetes_new i.SHSS3_group a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group
*Association SHSS and WHR_cat
logistic WHR_cat i.SHSS
logistic WHR_cat i.SHSS a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE
logistic WHR_cat i.SHSS a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group
*Association EHSS3 and obesity
mlogit bmi_3 i.EHSS3_group, rrr
mlogit bmi_3 i.EHSS3_group a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE, rrr
mlogit bmi_3 i.EHSS3_group a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group, rrr
*Association EHSS3 and diabetes
logistic diabetes_new i.EHSS3_group
logistic diabetes_new i.EHSS3_group a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE
logistic diabetes_new i.EHSS3_group a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group

*Association sleep duration and obesity
logistic obesity i.sleep_cat_3
logistic obesity i.sleep_cat_3 a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE
logistic obesity i.sleep_cat_3 a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group
*Association sleep duration and diabetes
logistic diabetes_new i.sleep_cat_3
logistic diabetes_new i.sleep_cat_3 a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE
logistic diabetes_new i.sleep_cat_3 a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group
*Association sleep problems and obesity
logistic obesity i.g14_problemas_sue_o
logistic obesity i.g14_problemas_sue_o a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE
logistic obesity i.g14_problemas_sue_o a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group
*Association sleep problems and diabetes
logistic diabetes_new i.g14_problemas_sue_o
logistic diabetes_new i.g14_problemas_sue_o a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE
logistic diabetes_new i.g14_problemas_sue_o a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group
*Association sleep medication and obesity
logistic obesity i.g15_list_probl_medic
logistic obesity i.g15_list_probl_medic a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE
logistic obesity i.g15_list_probl_medic a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group
*Association sleep medication and diabetes
logistic diabetes_new i.g15_list_probl_medic
logistic diabetes_new i.g15_list_probl_medic a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE
logistic diabetes_new i.g15_list_probl_medic a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group
*Association napping and obesity
logistic obesity i.siesta_ultimate
logistic obesity i.siesta_ultimate a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE
logistic obesity i.siesta_ultimate a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group
*Association napping and diabetes
logistic diabetes_new i.siesta_ultimate
logistic diabetes_new i.siesta_ultimate a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE
logistic diabetes_new i.siesta_ultimate a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group
*Association obesity and indoor ALAN
mlogit bmi_3 i.indoorlight_lab, rrr baseoutcome(0)
mlogit bmi_3 i.indoorlight_lab a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE, rrr baseoutcome(0)
mlogit bmi_3 i.indoorlight_lab a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group, rrr baseoutcome(0)
mlogit bmi_3 i.indoorlight_lab a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group i.uvi_longest_group, rrr baseoutcome(0)
mlogit bmi_3 i.indoorlight_lab a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group i.uvi_longest_group i.SHSS3_group, rrr baseoutcome(0)
*Association diabetes and indoor ALAN
logistic diabetes_new i.indoorlight_lab
logistic diabetes_new i.indoorlight_lab a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE
logistic diabetes_new i.indoorlight_lab a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group
logistic diabetes_new i.indoorlight_lab a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group i.uvi_longest_group
logistic diabetes_new i.indoorlight_lab a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group i.uvi_longest_group i.sleep_cat i.bedtime_group i.g14_problemas_sue_o i.siesta_frecat i.siesta_60
*Association obesity and MSIbg_group_l
logistic bmi_2 i. MSIbg_group_l
logistic bmi_2 i. MSIbg_group_l a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE
logistic bmi_2 i. MSIbg_group_l a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group 
logistic bmi_2 i. MSIbg_group_l a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group i.uvi_longest_group i.ndvi_500_longest_group
logistic bmi_2 i. MSIbg_group_l a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group i.uvi_longest_group i.ndvi_500_longest_group i.sleep_cat i.bedtime_group i.g14_problemas_sue_o i.siesta_frecat i.siesta_60
*Association diabetes and MSIbg_group_l
logistic diabetes_new i. MSIbg_group_l
logistic diabetes_new i. MSIbg_group_l a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE
logistic diabetes_new i. MSIbg_group_l a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group
logistic diabetes_new i. MSIbg_group_l a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group i.uvi_longest_group i.ndvi_500_longest_group
logistic diabetes_new i. MSIbg_group_l a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group i.uvi_longest_group i.ndvi_500_longest_group i.sleep_cat i.bedtime_group i.g14_problemas_sue_o i.siesta_frecat i.siesta_60

*Association obesity and MSIgr_group_l
mlogit bmi_3 i.MSIgr_group_l, rrr baseoutcome(0)
mlogit bmi_3 i.MSIgr_group_l a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.uvi_longest_group, rrr baseoutcome(0)

*Association diabetes and MSIgr_group_l
logistic diabetes_new i.MSIgr_group_l
logistic diabetes_new i.MSIgr_group_l a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.uvi_longest_group

*Association obesity and ImpMSIgr_group_l
mlogit bmi_3 i.ImpMSIgr_group_l, rrr baseoutcome(0)
mlogit bmi_3 i.ImpMSIgr_group_l a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.uvi_longest_group, rrr baseoutcome(0)

*Association diabetes and ImpMSIgr_group_l
logistic diabetes_new i.ImpMSIgr_group_l
logistic diabetes_new i.ImpMSIgr_group_l a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.uvi_longest_group

*Association obesity and Melgr_group_l
mlogit bmi_3 i.Melgr_group_l, rrr baseoutcome(0)
mlogit bmi_3 i.Melgr_group_l a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.uvi_longest_group, rrr baseoutcome(0)

*Association diabetes and Melgr_group_l
logistic diabetes_new i.Melgr_group_l
logistic diabetes_new i.Melgr_group_l a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.uvi_longest_group

*Association obesity and ImpMelgr_group_l
mlogit bmi_3 i.ImpMelgr_group_l, rrr baseoutcome(0)
mlogit bmi_3 i.ImpMelgr_group_l a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.uvi_longest_group, rrr baseoutcome(0)

*Association diabetes and ImpMelgr_group_l
logistic diabetes_new i.ImpMelgr_group_l
logistic diabetes_new i.ImpMelgr_group_l a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.uvi_longest_group

*Association obesity and combined ALAN
mlogit bmi_3 i.ALAN_combined_cat, rrr baseoutcome(0)
mlogit bmi_3 i.ALAN_combined_cat a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.uvi_longest_group, rrr baseoutcome(0)

*Association diabetes and combined ALAN
logistic diabetes_new i.ALAN_combined_cat
logistic diabetes_new i.ALAN_combined_cat a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.uvi_longest_group

*Association indoor ALAN and SHSS3_group
logistic SHSS3_group i.indoorlight_lab
logistic SHSS3_group i.indoorlight_lab a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE
logistic SHSS3_group i.indoorlight_lab a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever
logistic SHSS3_group i.indoorlight_lab a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.uvi_longest_group

*Association MSIgr_group_l and SHSS3_group
logistic SHSS3_group i.MSIgr_group_l
logistic SHSS3_group i.MSIgr_group_l a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.uvi_longest_group

*Association ImpMSIgr_group_l and SHSS3_group
logistic SHSS3_group i.ImpMSIgr_group_l
logistic SHSS3_group i.ImpMSIgr_group_l a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.uvi_longest_group

*Association Melgr_group_l and SHSS3_group
logistic SHSS3_group i.Melgr_group_l
logistic SHSS3_group i.Melgr_group_l a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.uvi_longest_group

*Association ImpMelgr_group_l and SHSS3_group
logistic SHSS3_group i.ImpMelgr_group_l
logistic SHSS3_group i.ImpMelgr_group_l a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.uvi_longest_group

*Stratified analysis
*BMI and MSIbg by centre
logistic bmi_2 i.MSIbg_group_l if centre_num == 2 //BCN
logistic bmi_2 i.MSIbg_group_l if centre_num == 9 //Madrid
logistic bmi_2 i.MSIbg_group_l a4_edat i.a3_sexe i.education_basic_final i.score_SE if centre_num == 2 //BCN
logistic bmi_2 i.MSIbg_group_l a4_edat i.a3_sexe i.education_basic_final i.score_SE if centre_num == 9 //Madrid
*BMI and MSIgr by centre
logistic bmi_2 i.MSIgr_group_l if centre_num == 2 //BCN
logistic bmi_2 i.MSIgr_group_l if centre_num == 9 //Madrid
logistic bmi_2 i.MSIgr_group_l a4_edat i.a3_sexe i.education_basic_final i.score_SE if centre_num == 2 //BCN
logistic bmi_2 i.MSIgr_group_l a4_edat i.a3_sexe i.education_basic_final i.score_SE if centre_num == 9 //Madrid
*BMI and MSIgr_group_l by centre
mlogit bmi_3 i.MSIgr_group_l if centre_num == 2, rrr baseoutcome(0) 
mlogit bmi_3 i.MSIgr_group_l if centre_num == 9, rrr baseoutcome(0)
mlogit bmi_3 i.MSIgr_group_l a4_edat i.a3_sexe i.education_basic_final i.score_SE i.uvi_longest_group if centre_num == 2, rrr baseoutcome(0)
mlogit bmi_3 i.MSIgr_group_l a4_edat i.a3_sexe i.education_basic_final i.score_SE i.uvi_longest_group if centre_num == 9, rrr baseoutcome(0)
*BMI and ImpMSIgr_group_l by centre
mlogit bmi_3 i.ImpMSIgr_group_l if centre_num == 2, rrr baseoutcome(0) 
mlogit bmi_3 i.ImpMSIgr_group_l if centre_num == 9, rrr baseoutcome(0)
mlogit bmi_3 i.ImpMSIgr_group_l a4_edat i.a3_sexe i.education_basic_final i.score_SE if centre_num == 2, rrr baseoutcome(0)
mlogit bmi_3 i.ImpMSIgr_group_l a4_edat i.a3_sexe i.education_basic_final i.score_SE if centre_num == 9, rrr baseoutcome(0)
*Diabetes and MSIgr_group_l by centre
logistic diabetes_new i.MSIgr_group_l if centre_num == 2
logistic diabetes_new i.MSIgr_group_l if centre_num == 9
logistic diabetes_new i.MSIgr_group_l a4_edat i.a3_sexe i.education_basic_final i.score_SE i.uvi_longest_group if centre_num == 2
logistic diabetes_new i.MSIgr_group_l a4_edat i.a3_sexe i.education_basic_final i.score_SE i.uvi_longest_group if centre_num == 9
*Diabetes and ImpMSIgr_group_l by centre
logistic diabetes_new i.ImpMSIgr_group_l if centre_num == 2
logistic diabetes_new i.ImpMSIgr_group_l if centre_num == 9
logistic diabetes_new i.ImpMSIgr_group_l a4_edat i.a3_sexe i.education_basic_final i.score_SE if centre_num == 2
logistic diabetes_new i.ImpMSIgr_group_l a4_edat i.a3_sexe i.education_basic_final i.score_SE if centre_num == 9

*BMI and indoor ALAN by sex
mlogit bmi_3 i.indoorlight_lab, rrr baseoutcome(0)
mlogit bmi_3 i.indoorlight_lab if a3_sexe == 1, rrr baseoutcome(0) //male
mlogit bmi_3 i.indoorlight_lab if a3_sexe == 2, rrr baseoutcome(0) //female
mlogit bmi_3 i.indoorlight_lab a4_edat i.education_basic_final i.score_SE, rrr baseoutcome(0)
mlogit bmi_3 i.indoorlight_lab a4_edat i.education_basic_final i.score_SE if a3_sexe == 1, rrr baseoutcome(0) //male
mlogit bmi_3 i.indoorlight_lab a4_edat i.education_basic_final i.score_SE if a3_sexe == 2, rrr baseoutcome(0) //female
*Diabetes and indoor ALAN by sex
logistic diabetes_new i.indoorlight_lab
logistic diabetes_new i.indoorlight_lab if a3_sexe == 1 //male
logistic diabetes_new i.indoorlight_lab if a3_sexe == 1 //female
logistic diabetes_new i.indoorlight_lab a4_edat i.education_basic_final i.score_SE i.uvi_longest_group
logistic diabetes_new i.indoorlight_lab a4_edat i.education_basic_final i.score_SE i.uvi_longest_group if a3_sexe == 1 //male
logistic diabetes_new i.indoorlight_lab a4_edat i.education_basic_final i.score_SE i.uvi_longest_group if a3_sexe == 2 //female
*BMI and MSIGR by sex
mlogit bmi_3 i.MSIgr_group_l, rrr baseoutcome(0)
mlogit bmi_3 i.MSIgr_group_l if a3_sexe == 1, rrr baseoutcome(0) //male
mlogit bmi_3 i.MSIgr_group_l if a3_sexe == 2, rrr baseoutcome(0) //female
mlogit bmi_3 i.MSIgr_group_l a4_edat i.education_basic_final i.score_SE, rrr baseoutcome(0)
mlogit bmi_3 i.MSIgr_group_l a4_edat i.education_basic_final i.score_SE if a3_sexe == 1, rrr baseoutcome(0) //male
mlogit bmi_3 i.MSIgr_group_l a4_edat i.education_basic_final i.score_SE if a3_sexe == 2, rrr baseoutcome(0) //female
*Diabetes and MSIGR by sex
logistic diabetes_new i.MSIgr_group_l
logistic diabetes_new i.MSIgr_group_l if a3_sexe == 1 //male
logistic diabetes_new i.MSIgr_group_l if a3_sexe == 1 //female
logistic diabetes_new i.MSIgr_group_l a4_edat i.education_basic_final i.score_SE i.uvi_longest_group
logistic diabetes_new i.MSIgr_group_l a4_edat i.education_basic_final i.score_SE i.uvi_longest_group if a3_sexe == 1 //male
logistic diabetes_new i.MSIgr_group_l a4_edat i.education_basic_final i.score_SE i.uvi_longest_group if a3_sexe == 2 //female

*BMI and indoor ALAN by chronotype
mlogit bmi_3 i.indoorlight_lab, rrr baseoutcome(0)
mlogit bmi_3 i.indoorlight_lab if msf_c3 == 1, rrr baseoutcome(0) //morning
mlogit bmi_3 i.indoorlight_lab if msf_c3 == 2, rrr baseoutcome(0) //intermediate
mlogit bmi_3 i.indoorlight_lab if msf_c3 == 3, rrr baseoutcome(0) //evening
mlogit bmi_3 i.indoorlight_lab a4_edat i.a3_sexe i.education_basic_final i.score_SE, rrr baseoutcome(0)
mlogit bmi_3 i.indoorlight_lab a4_edat i.a3_sexe i.education_basic_final i.score_SE if msf_c3 == 1, rrr baseoutcome(0) //morning
mlogit bmi_3 i.indoorlight_lab a4_edat i.a3_sexe i.education_basic_final i.score_SE if msf_c3 == 2, rrr baseoutcome(0) //intermediate
mlogit bmi_3 i.indoorlight_lab a4_edat i.a3_sexe i.education_basic_final i.score_SE if msf_c3 == 3, rrr baseoutcome(0) //evening
*Diabetes and indoor ALAN by chronotype
logistic diabetes_new i.indoorlight_lab
logistic diabetes_new i.indoorlight_lab if msf_c3 == 1 //morning
logistic diabetes_new i.indoorlight_lab if msf_c3 == 2 //intermediate
logistic diabetes_new i.indoorlight_lab if msf_c3 == 3 //evening
logistic diabetes_new i.indoorlight_lab a4_edat i.a3_sexe i.education_basic_final i.score_SE i.uvi_longest_group
logistic diabetes_new i.indoorlight_lab a4_edat i.a3_sexe i.education_basic_final i.score_SE i.uvi_longest_group if msf_c3 == 1 //morning
logistic diabetes_new i.indoorlight_lab a4_edat i.a3_sexe i.education_basic_final i.score_SE i.uvi_longest_group if msf_c3 == 2 //intermediate
logistic diabetes_new i.indoorlight_lab a4_edat i.a3_sexe i.education_basic_final i.score_SE i.uvi_longest_group if msf_c3 == 3 //evening
*BMI and MSIGR by chronotype
mlogit bmi_3 i.MSIgr_group_l, rrr baseoutcome(0)
mlogit bmi_3 i.MSIgr_group_l if msf_c3 == 1, rrr baseoutcome(0) //morning
mlogit bmi_3 i.MSIgr_group_l if msf_c3 == 2, rrr baseoutcome(0) //intermediate
mlogit bmi_3 i.MSIgr_group_l if msf_c3 == 3, rrr baseoutcome(0) //evening
mlogit bmi_3 i.MSIgr_group_l a4_edat i.a3_sexe i.education_basic_final i.score_SE i.uvi_longest_group, rrr baseoutcome(0)
mlogit bmi_3 i.MSIgr_group_l a4_edat i.a3_sexe i.education_basic_final i.score_SE i.uvi_longest_group if msf_c3 == 1, rrr baseoutcome(0) //morning
mlogit bmi_3 i.MSIgr_group_l a4_edat i.a3_sexe i.education_basic_final i.score_SE i.uvi_longest_group if msf_c3 == 2, rrr baseoutcome(0) //intermediate
mlogit bmi_3 i.MSIgr_group_l a4_edat i.a3_sexe i.education_basic_final i.score_SE i.uvi_longest_group if msf_c3 == 2, rrr baseoutcome(0) //evening
*Diabetes and MSIGR by chronotype
logistic diabetes_new i.MSIgr_group_l
logistic diabetes_new i.MSIgr_group_l if msf_c3 == 1 //morning
logistic diabetes_new i.MSIgr_group_l if msf_c3 == 2 //intermediate
logistic diabetes_new i.MSIgr_group_l if msf_c3 == 3 //evening
logistic diabetes_new i.MSIgr_group_l a4_edat i.a3_sexe i.education_basic_final i.score_SE i.uvi_longest_group
logistic diabetes_new i.MSIgr_group_l a4_edat i.a3_sexe i.education_basic_final i.score_SE i.uvi_longest_group if msf_c3 == 1 //morning
logistic diabetes_new i.MSIgr_group_l a4_edat i.a3_sexe i.education_basic_final i.score_SE i.uvi_longest_group if msf_c3 == 2 //intermediate
logistic diabetes_new i.MSIgr_group_l a4_edat i.a3_sexe i.education_basic_final i.score_SE i.uvi_longest_group if msf_c3 == 3 //evening

*Other commands:
*Wilcoxon rank sum test
local vars msibg_longest msigr_longest impmsibg_longest impmsigr_longest melbg_longest melgr_longest impmelbg_longest impmelgr_longest
foreach v of local vars {
    ranksum `v', by(centre_num)
    di "Results for: `v'"
}

*correlation
correlate X Y
spearman X Y
tabulate X Y, chi2 cell/row/column

pwcorr X Y Z, sig //correlation matrix Pearson
spearman X Y Z, stats(rho p) // correlation matrix Spearman
spearman msibg_longest msigr_longest impmsibg_longest impmsigr_longest melbg_longest melgr_longest impmelbg_longest impmelgr_longest, stats(rho p)

*Normality testing
histogram X
twoway (histogram X, color(blue)) (histogram Y, color(red)), legend (label(1 "XXX") label(2 "YYY"))
swilk X

*Associations BMI variables with indoor ALAN Vergleiche
logistic obesity i.indoorlight_lab
logistic obesity i.indoorlight_lab a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE
logistic obesity i.indoorlight_lab a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group
logistic bmi_2 i.indoorlight_lab
logistic bmi_2 i.indoorlight_lab a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE
logistic bmi_2 i.indoorlight_lab a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group
logistic bmi_NO i.indoorlight_lab
logistic bmi_NO i.indoorlight_lab a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE
logistic bmi_NO i.indoorlight_lab a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.mets_act_10_2_total_rec i.fumador_ever i.gra_fruits_group i.gra_vegetables_group i.t_energy_group

*Splined Associations
mkspline msigr = msigr_longest, cubic knots(0 0.1236267 0.1564941 0.1881104)
mkspline impmsigr = impmsigr_longest, cubic knots(0 0.000639 0.0008786 0.001205)
mkspline melgr = melgr_longest, cubic knots(0 0.3284912 0.3925781 0.4467773)
mkspline impmelgr = impmelgr_longest, cubic knots(0 0.0016308 0.0022078 0.002985)

*Splined association obesity and MSIgr_group_l
mlogit bmi_3 msigr1 msigr2 msigr3, rrr baseoutcome(0)
mlogit bmi_3 msigr1 msigr2 msigr3 a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.uvi_longest_group, rrr baseoutcome(0)

*Splined association diabetes and MSIgr_group_l
logistic diabetes_new i.MSIgr_group_l
logistic diabetes_new i.MSIgr_group_l a4_edat i.a3_sexe i.centre_num i.education_basic_final i.score_SE i.uvi_longest_group
