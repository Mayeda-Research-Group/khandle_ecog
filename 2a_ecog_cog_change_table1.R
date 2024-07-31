# Author: Natalie Gradwohl
# Creation Date: 6/2/2023
# Purpose: To create Table 1 with imputed dataset and supplemental 'Table 1' with
# complete case data
#
# UPDATED TO WAVE 4
#
# ----loading packages----
if (!require("pacman"))
  install.packages("pacman", repos = 'http://cran.us.r-project.org')

p_load("tidyverse",
       # here,
       "magrittr",
       "labelled",
       "gtsummary",
       "writexl")

# ----loading imputed dataset----
# load(here("output", "datasets", "khandle_long_imputed_final.RData"))

# ----table 1: categorical variables----
catvars <-
  c("w1_d_female",
    "w1_d_race_summary",
    "dementia_yn",
    "censor_death",
    "censor_refuse")

# making gtsummary table of categorical variables from long imputed dataset
table_cat <-
  khandle_long_imputed_final %>% filter(wave == "w1", .imp != 0) %>%
  dplyr::select(dplyr::all_of(catvars)) %>%
  mutate_all(as.character) %>%
  gtsummary::tbl_summary(statistic = list(everything() ~ c("{n} {p}")),
                         digits = everything() ~ 1)


# converting table to tibble for exporting to excel, reformatting the output
table1_cat <- table_cat %>%
  as_tibble() %>%
  set_colnames(c("Characteristic", "stats")) %>%
  separate(stats, c("n", "prop"), " ") %>%
  mutate(n = round(as.numeric(gsub(",", "", n)) / 40),
         prop = round(as.numeric(prop), 1)) %>%
  mutate(`n (%)` = paste0(n, " (", prop, ")")) %>%
  dplyr::select(Characteristic, `n (%)`) %>%
  mutate(`n (%)` = if_else(grepl("(NA)", `n (%)`), " ", `n (%)`)) %>%
  print(n = Inf)

# exporting categorical table
# write_xlsx(table1_cat,
#            here("output", "tables", "table1_cat.xlsx"))

write_xlsx(table1_cat,
           paste0(path_to_output, "table1_cat.xlsx"))

# ----table 1: continuous variables----
khandle_long_imputed_final %<>% # calculating number of assessments for t1
  group_by(studyid, .imp) %>%
  mutate(num_assess_exec = length(na.omit(d_senas_exec_z)),
         num_assess_vrmem = length(na.omit(d_senas_vrmem_z))) %>%
  ungroup()

temp_cont_tib <- data.frame()
for (i in 1:40) {
  temp_cont_tib <- temp_cont_tib %>%
    rbind(
      khandle_long_imputed_final %>% filter(.imp == i, wave == "w1") %>%
        summarise(
          mean_baseline_age_imp = mean(baseline_age),
          mean_edu_yrs_cert_imp = mean(edu_yrs_cert),
          mean_depressive_sx_imp = mean(w1_nihtlbx_depr_theta),
          mean_ecog_score_imp = mean(ecog.score, na.rm = T),
          mean_max_studytime_imp = mean(max.studytime, na.rm = T),
          mean_num_assess_exec_imp = mean(num_assess_exec, na.rm = T),
          mean_num_assess_vrmem_imp = mean(num_assess_vrmem, na.rm = T),
          
          var_baseline_age_imp =   var(baseline_age),
          var_edu_yrs_cert_imp =   var(edu_yrs_cert),
          var_depressive_sx_imp =  var(w1_nihtlbx_depr_theta),
          var_ecog_score_imp = var(ecog.score, na.rm = T),
          var_max_studytime_imp = var(max.studytime, na.rm = T),
          var_num_assess_exec_imp = var(num_assess_exec, na.rm = T),
          var_num_assess_vrmem_imp = var(num_assess_vrmem, na.rm = T)
        ) %>%
        mutate(imp = i)
    )
}

# averaging across imputations to produce one summary estimate for each continuous variable
table1_cont <- temp_cont_tib %>%
  summarise(
    mean_baseline_age = mean(mean_baseline_age_imp),
    mean_edu_yrs_cert = mean(mean_edu_yrs_cert_imp),
    mean_depressive_sx = mean(mean_depressive_sx_imp),
    mean_ecog_score = mean(mean_ecog_score_imp),
    mean_max_studytime = mean(mean_max_studytime_imp),
    mean_num_assess_exec = mean(mean_num_assess_exec_imp),
    mean_num_assess_vrmem = mean(mean_num_assess_vrmem_imp),
    
    sd_baseline_age =   sqrt(mean(var_baseline_age_imp)),
    sd_edu_yrs_cert =   sqrt(mean(var_edu_yrs_cert_imp)),
    sd_var_depressive_sx =  sqrt(mean(var_depressive_sx_imp)),
    sd_ecog_score = sqrt(mean(var_ecog_score_imp)),
    sd_max_studytime = sqrt(mean(var_max_studytime_imp)),
    sd_num_assess_exec = sqrt(mean(var_num_assess_exec_imp)),
    sd_num_assess_vrmem = sqrt(mean(var_num_assess_vrmem_imp))
  ) %>%
  pivot_longer(cols = everything(),
               names_to = c("var"),
               values_to = "value")




# exporting continuous table
# write_xlsx(
#   table1_cont,
#   here("output", "tables", "table1_cont.xlsx")
# )

write_xlsx(table1_cont,
           paste0(path_to_output, "table1_cont.xlsx"))

# ----table 1 (complete case): categorical----
cc_data <-
  khandle_long_imputed_final %>% filter(.imp == 0, wave == 'w1') %>%
  mutate(ecog_score_cc = ifelse(ecog.items >= 6, ecog.score, NA)) %>%
  select(-ecog.score)

# this table ends up being exactly the same because there is no missingness among
# the categorical variables in this analysis

# making gtsummary table of categorical variables from long imputed dataset
table_cat_cc <- cc_data %>%
  dplyr::select(dplyr::all_of(catvars)) %>%
  mutate_all(as.character) %>%
  gtsummary::tbl_summary(statistic = list(everything() ~ c("{n} {p}")),
                         digits = everything() ~ 2)

# converting table to tibble for exporting to excel, reformatting the output
table1_cat_cc <- table_cat_cc %>%
  as_tibble() %>%
  set_colnames(c("Characteristic", "stats")) %>%
  separate(stats, c("n", "prop"), " ") %>%
  mutate(n = round(as.numeric(gsub(",", "", n))),
         prop = round(as.numeric(prop), 1)) %>%
  mutate(`n (%)` = paste0(n, " (", prop, ")")) %>%
  dplyr::select(Characteristic, `n (%)`) %>%
  mutate(`n (%)` = if_else(grepl("(NA)", `n (%)`), " ", `n (%)`)) %>%
  print(n = Inf) %>% as_tibble()

# exporting table to excel
# write_xlsx(table1_cat_cc, here("output", "tables", "table1_cat_cc.xlsx"))

write_xlsx(table1_cat_cc, 
           paste0(path_to_output, "table1_cat_cc.xlsx"))

# ----table 1 (complete case): continuous----
table1_cont_cc <- cc_data %>%
  dplyr::select(baseline_age,
                edu_yrs_cert,
                w1_nihtlbx_depr_theta,
                ecog_score_cc,
                max.studytime) %>%
  gtsummary::tbl_summary(
    type = all_continuous() ~ "continuous2",
    missing = "no",
    statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                          "{N_miss} ({p_miss}%)")),
    digits = everything() ~ 2
  ) %>%
  modify_table_body(dplyr::mutate,
                    label = ifelse(label == "N missing (% missing)",
                                   "Unknown",
                                   label)) %>% as_tibble()

# exporting table to excel
# write_xlsx(table1_cont_cc, here("output", "tables", "table1_cont_cc.xlsx"))

write_xlsx(table1_cont_cc,
           paste0(path_to_output, "table1_cont_cc.xlsx"))
