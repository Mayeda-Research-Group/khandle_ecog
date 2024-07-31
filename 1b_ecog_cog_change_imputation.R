# Author: Natalie Gradwohl
# Creation Date: 6/2/2023
# Purpose: To create an imputed dataset and derive variables needed for analysis
#
# UPDATED TO WAVE 4
#
# ----loading packages----
if (!require("pacman"))
  install.packages("pacman", repos = 'http://cran.us.r-project.org')

p_load('tidyverse',
       # 'here',
       'mice',
       'mitools',
       'stats')

# ----loading cleaned dataset----
# load(here("datasets", "khandle2.RData"))

# ----imputation prep----
khandle_impute <-  khandle2 %>% dplyr::select(
  -c(
    censor_death,
    censor_refuse,
    w1_d_senas_exec_z,
    w1_d_senas_vrmem_z,
    w1_d_race_summary,
    w1_d_senas_avg_cognition_z,
    w1_d_senas_sem_z,
    w1_senas_telephone,
    w2_interview_age_final,
    w2_d_senas_exec_z,
    w2_d_senas_vrmem_z,
    w2_d_senas_avg_cognition_z,
    w2_d_senas_sem_z,
    w2_senas_telephone,
    w3_interview_age_final,
    w3_d_senas_exec_z,
    w3_d_senas_vrmem_z,
    w3_d_senas_avg_cognition_z,
    w3_d_senas_sem_z,
    w3_senas_telephone,
    w4_interview_age_final,
    w4_d_senas_exec_z,
    w4_d_senas_vrmem_z,
    w4_d_senas_avg_cognition_z,
    w4_d_senas_sem_z,
    w4_senas_telephone
  )
)

# summarizing missingness in cleaned data set
khandle_missing <- md.pattern(khandle_impute, plot = F)

# creating a data frame for percent missingness
khandle_miss_summary <- data.frame(
  varname = colnames(khandle_impute),
  pctmiss = sapply(khandle_impute, function(x)
    100 * sum(is.na(x)) / nrow(khandle_impute))
)

# ordering by lowest to highest percent missingness
data_col_name <-
  rownames(khandle_miss_summary[order(khandle_miss_summary$pctmiss), ])

# creating a dataframe with selected columns
khandle_impute <- khandle_impute[, data_col_name]

# converting variables to factors for imputation
cols_to_convert <- c(
  "w1_d_race_dummy_asian_n",
  "w1_d_race_dummy_black_n",
  "w1_d_race_dummy_latinx_n",
  "w1_d_female",
  "dementia_yn"
)

ecog_items <-
  khandle_impute %>% select(contains("ecog")) %>% colnames()

khandle_impute <- khandle_impute %>%
  mutate_at(cols_to_convert, as.factor) %>%
  mutate_at(ecog_items, as.ordered)

# initializing multiple imputation using mice package
init = mice(khandle_impute, maxit = 0) # 1 logged event (studyid) - ignore!
meth = init$method
predictM = init$predictorMatrix
init$loggedEvents
table(init$nmis)

# replacing study id with zeros
predictM[, c('studyid')] = 0

# ---- actual imputation- takes about 15 mins to run----
set.seed(1233534)
# generating 40 imputed datasets with a maximum of 10 iterations
imputed = mice(
  khandle_impute,
  method = meth,
  predictorMatrix = predictM,
  m = 40,
  maxit = 10,
  print = T
)

# saving imputed datasets object
save(imputed,
     file = here("datasets", "khandle_imputed_raw.RData"))

# displaying logged events
imputed$loggedEvents # NULL

# visualizing imputations
plot(imputed, layout = c(6, 5))

# creating dataset from imputation mids object
khandle_wide_imputed <- complete(imputed, action = 'long',
                                 include = TRUE) # 69249 obs

# checking number of observations for each imputed dataset
table(khandle_wide_imputed$.imp, exclude = NULL)

# ----deriving baseline variables----
ecog_items <-
  c(
    "w1_ecog_mem1_c",
    "w1_ecog_mem2_c",
    "w1_ecog_lang1_c",
    "w1_ecog_lang2_c",
    "w1_ecog_visual_spatial2_c",
    "w1_ecog_visual_spatial5_c",
    "w1_ecog_planning1_c",
    "w1_ecog_planning3_c",
    "w1_ecog_organization1_c",
    "w1_ecog_organization2_c",
    "w1_ecog_divided_attention1_c",
    "w1_ecog_divided_attention2_c"
  )

# right joining imputed datasets with cleaned data
khandle_wide_imputed_join <- khandle2 %>%
  dplyr::select(
    -c(
      "w1_d_female",
      "w1_d_race_dummy_asian_n",
      "w1_d_race_dummy_black_n",
      "w1_d_race_dummy_latinx_n",
      "dementia_yn",
      "w1_interview_age_final",
      "edu_yrs_cert",
      "w1_ecog_lang1_c",
      "w1_ecog_visual_spatial5_c",
      "w1_ecog_mem1_c",
      "w1_ecog_organization1_c",
      "w1_ecog_mem2_c",
      "w1_nihtlbx_depr_theta",
      "w1_ecog_lang2_c",
      "w1_ecog_planning3_c",
      "w1_ecog_planning1_c",
      "w1_ecog_divided_attention1_c",
      "w1_ecog_divided_attention2_c",
      "w1_ecog_visual_spatial2_c",
      "w1_ecog_organization2_c"
    )
  ) %>%
  right_join(khandle_wide_imputed, by = 'studyid') %>%
  mutate_at(ecog_items, as.numeric) %>%  # need to convert back to numeric after imputation
  mutate(
    # summing ecog items for each person
    "ecog.sum" = rowSums(across(contains("w1_ecog")), na.rm = T),
    "ecog.items" = rowSums(!is.na(across(
      contains("w1_ecog_")
    )), na.rm = T),
    "ecog.score" = case_when(ecog.sum > 0 ~ (ecog.sum / ecog.items),
                             TRUE ~ NA_real_),
    "ecog.cent" = ecog.score - 1
  ) %>%
  group_by(.imp, studyid) %>%
  mutate(
    # baseline age based on first visit
    "baseline_age" = min(
      c(
        w1_interview_age_final,
        w2_interview_age_final,
        w3_interview_age_final,
        w4_interview_age_final
      ),
      na.rm = T
    ),
    # centering, decading age
    'baseline_age_c75_decade' = ((baseline_age - 75) / 10)
  ) %>%
  ungroup()  

# ----making long dataset----
# creating list of longitudinal variables
vars <- c(
  "interview_age_final",
  "d_senas_exec_z",
  "d_senas_vrmem_z",
  "d_senas_avg_cognition_z",
  "d_senas_sem_z",
  "senas_telephone"
)

# adding wave numbers to above list of variables
cols <-
  paste0("w", apply(expand_grid(seq(1, 4), vars) %>% arrange(vars), 1, 
                    paste, collapse = "_"))

# creating long data set with imputed variables
khandle_long_imputed <-
  khandle_wide_imputed_join %>% dplyr::select(studyid, .imp, all_of(cols)) %>%
  pivot_longer(
    cols = !c(studyid, .imp),
    names_to = c("wave", ".value"),
    names_pattern = "(...)(.*)"
  ) %>%
  # creating wave indicator variable
  mutate("wave" = str_replace(wave, "_", "")) %>% 
  filter(!is.na(d_senas_exec_z) | !is.na(d_senas_vrmem_z)) # 201146 obs

# pulling covariates from imputed data set
khandle_imputed_covs <- khandle_wide_imputed_join %>%
  dplyr::select(
    studyid,
    w1_d_female,
    w1_d_race_summary,
    baseline_age,
    baseline_age_c75_decade,
    edu_yrs_cert,
    dementia_yn,
    w1_nihtlbx_depr_theta,
    ecog.sum,
    ecog.items,
    ecog.score,
    ecog.cent,
    censor_death,
    censor_refuse,
    .imp
  )

# ----left joining long data set with imputed values with non-imputed values----
khandle_long_imputed_final <-
  left_join(khandle_long_imputed,
            khandle_imputed_covs,
            by = c("studyid", ".imp")) %>%
  group_by(.imp, studyid) %>%
  mutate(
    # creating variable for time on study
    'studytime' = interview_age_final - baseline_age,
    # recoding mode of interview variable (phone vs. in person)
    'phone' = as.factor(
      case_when(
        senas_telephone == 'Y' ~ 1,
        senas_telephone == 'N' ~ 0,
        TRUE ~ NA_real_
      )
    ),
    # creating max studytime variable to determine average follow-up time
    'max.studytime' = max(studytime, na.rm = T),
    # creating first assessment indicator
    'first_assess' = (case_when(wave == min(wave) ~ 1,
                                TRUE ~ 0)),
    # creating offset variable for executive function analysis
    'off' = (case_when(first_assess == 1 ~ -0.06,
                       TRUE ~ 0)),
    # creating offset variable for verbal memory analysis
    'off2' = (case_when(first_assess == 1 ~ -0.09,
                        TRUE ~ 0)),
    # generating new id numbers for each person
    'new_id' = paste0(studyid, wave, .imp),
    # convert variables to factors
    'w1_d_race_summary' = as.factor(w1_d_race_summary),
    'censor_death' = as.factor(censor_death),
    'censor_refuse' = as.factor(censor_refuse),
    'w1_d_female' = as.factor(w1_d_female)
  ) %>%
  ungroup() %>%
  arrange(.imp, new_id) # 201146 obs

# ----exporting datasets----
# exporting long analytic dataset
# save(
#   khandle_long_imputed_final,
#   file = here("output", "datasets", "khandle_long_imputed_final.RData")
# )

# keeping a version without .imp 0
khandle_long_pooled <-
  khandle_long_imputed_final %>% filter(.imp != 0)

# save(khandle_long_pooled,
#      file = here("output", "datasets", "khandle_long_pooled.RData"))

# converting final imputed dataset to mids object for analysis
khandle_imputed_analysis <-
  as.mids(khandle_long_imputed_final,
          .imp = ".imp",
          .id = "new_id")

# exporting mids object to be used in lmer models
# save(
#   khandle_imputed_analysis,
#   file = here("output", "datasets", "khandle_imputed_analysis.RData")
# )
