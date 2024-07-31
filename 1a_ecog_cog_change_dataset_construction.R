# Author: Natalie Gradwohl
# Creation Date: 6/2/2023
# Purpose: To import raw KHANDLE data, recode missing data, create new variables
# for analysis, and export a dataset to be imputed
#
# UPDATED TO WAVE 4
#
# ----loading packages----
if (!require("pacman"))
  install.packages("pacman", repos = 'http://cran.us.r-project.org')

p_load(
  "tidyverse",
  "here",
  "haven",
  "janitor",
  "knitr",
  "tableone",
  "openxlsx",
  "gtsummary",
  "writexl"
)

# ----reading in raw dataset (only want to look at cohort 1)----
source(here("scripts", "0_ecog_cog_change_filepaths.R"))

khandle_raw <- read_sas(path_to_raw_data) %>% 
  clean_names() %>% 
  filter(cohort == 1)

# ----FOR UCLA USE ONLY: KHANDLE 90+ age hot-swaps----
# for participants who age into the 90+ age group, we are going to sort of hot
# swap their 90+ age with their most recent value + median F/U time
khandle_wide_median_fu <- khandle_raw %>%
  select(studyid,
         w1_interview_age,
         w2_interview_age,
         w3_interview_age,
         w4_interview_age) %>%

  mutate(
    #Recoding interview age to have missing 89.9999 y/o's
    w1_interview_age_na90 = case_when(
      w1_interview_age < 89.9999 ~ w1_interview_age,
      TRUE ~ NA_integer_),
    w2_interview_age_na90 = case_when(
      w2_interview_age < 89.9999 ~ w2_interview_age,
      TRUE ~ NA_integer_),
    w3_interview_age_na90 = case_when(
      w3_interview_age < 89.9999 ~ w3_interview_age,
      TRUE ~ NA_integer_),
    w4_interview_age_na90 = case_when(
      w4_interview_age < 89.9999 ~ w4_interview_age,
      TRUE ~ NA_integer_),

    #calculting a new variable for f/u time!
    w1_w2_fu = w2_interview_age_na90 - w1_interview_age_na90,
    w2_w3_fu = w3_interview_age_na90 - w2_interview_age_na90,
    w3_w4_fu = w4_interview_age_na90 - w3_interview_age_na90,
    w1_w3_fu = w3_interview_age_na90 - w1_interview_age_na90,
    w1_w4_fu = w4_interview_age_na90 - w1_interview_age_na90,
    w2_w4_fu = w4_interview_age_na90 - w2_interview_age_na90) %>%
  summarise(
    w1_w2_fu_median = median(w1_w2_fu, na.rm = T),
    w2_w3_fu_median = median(w2_w3_fu, na.rm = T),
    w3_w4_fu_median = median(w3_w4_fu, na.rm = T),
    w1_w3_fu_median = median(w1_w3_fu, na.rm = T),
    w1_w4_fu_median = median(w1_w4_fu, na.rm = T),
    w2_w4_fu_median = median(w2_w4_fu, na.rm = T))

khandle_wide_age <- khandle_wide_median_fu %>%
  slice(rep(1:n(), each = 1712))

khandle_wide <- cbind(khandle_wide_age, khandle_raw) %>%
  mutate(
    w1_interview_age_final = case_when(
      w1_interview_age < 89.9999 ~
        w1_interview_age,
      w1_interview_age >= 89.9999 ~ 90),

    w2_interview_age_final = case_when(
      w2_interview_age >= 89.9999 &
        !is.na(w1_interview_age) ~
        w1_interview_age_final + w1_w2_fu_median,
      TRUE ~ w2_interview_age),

    w3_interview_age_final = case_when(
      w3_interview_age >= 89.9999 &
        !is.na(w2_interview_age_final) ~
        w2_interview_age_final + w2_w3_fu_median,
      w3_interview_age >= 89.9999 &
        is.na(w2_interview_age_final) ~
        w1_interview_age_final + w1_w3_fu_median,
      TRUE ~ w3_interview_age),

    w4_interview_age_final = case_when(
      w4_interview_age >= 89.9999 &
        !is.na(w3_interview_age_final) ~
        w3_interview_age_final + w3_w4_fu_median,
      w4_interview_age >= 89.9999 &
        is.na(w2_interview_age_final) &
        is.na(w3_interview_age_final) ~
        w1_interview_age_final + w1_w4_fu_median,
      w4_interview_age >= 89.9999 &
        !is.na(w2_interview_age_final) &
        is.na(w3_interview_age_final) ~
        w2_interview_age_final + w2_w4_fu_median,
      TRUE ~ w4_interview_age))

# ----FOR KAISER USE ONLY: NON-TOPCODED AGE----
# khandle_raw <- khandle_raw %>%
#   select(-c(w1_interview_age,
#             w2_interview_age,
#             w3_interview_age,
#             w4_interview_age)) %>%
#   rename(w1_interview_age_final = w1_interview_age_phi,
#          w2_interview_age_final = w2_interview_age_phi,
#          w3_interview_age_final = w3_interview_age_phi,
#          w4_interview_age_final = w4_interview_age_phi)

# ----recoding variables----

# recoding gender
khandle_wide$w1_d_female <-
  as.factor(ifelse(khandle_wide$w1_d_gender == 1, 0, 1))

# recoding each relative with dementia variable to be numeric (0/1)
for (n in 1:14) {
  v <- paste0("w1_rel_dementia_", n)
  khandle_wide[, v] <- as.numeric(unlist(khandle_wide[, v]))
  khandle_wide[, v] <- ifelse(khandle_wide[, v] == 1, 1,
                             ifelse(khandle_wide[, v] == 2, 0, NA))
}

# summing number of relatives with dementia across rows
khandle_wide$dementia_sum <-
  rowSums(khandle_wide[, c(paste0("w1_rel_dementia_", 1:14))], na.rm = T)

# creating indicator variable for any relative with dementia vs. no relatives with dementia
khandle_wide$dementia_yn <- as.factor(ifelse(
  khandle_wide$dementia_sum >= 1,
  1,
  ifelse(khandle_wide$dementia_sum == 0, 0, NA)
))

# deriving education variable
khandle_wide$edu_yrs <-
  ifelse(
    !is.na(khandle_wide$w1_edu_education_text),
    khandle_wide$w1_edu_education_text,
    # years 0-12 carry through
    ifelse(
      khandle_wide$w1_edu_education == 1,
      13,
      # some college
      ifelse(
        khandle_wide$w1_edu_education == 2,
        14,
        # associates
        ifelse(
          khandle_wide$w1_edu_education == 3,
          16,
          # bachelors
          ifelse(
            khandle_wide$w1_edu_education == 4,
            18,
            # masters
            ifelse(khandle_wide$w1_edu_education ==
                     5, 20,
                   NA)
          )
        )
      )
    )
  ) # doctoral degree

# adding years based on certification
khandle_wide$cert.flag <-
  ifelse(khandle_wide$w1_edu_trncert == 2 &
           khandle_wide$w1_edu_longcert == 4,
         1,
         0)

# summing years of education and years of education from certification
khandle_wide$edu_yrs_cert <-
  ifelse(
    khandle_wide$edu_yrs <= 12 & !is.na(khandle_wide$cert.flag),
    khandle_wide$edu_yrs + khandle_wide$cert.flag,
    khandle_wide$edu_yrs
  )

# making a vector of longitudinal variables
vars <- c(
  "d_senas_exec_z",
  "d_senas_vrmem_z",
  "d_senas_avg_cognition_z",
  "d_senas_sem_z",
  "senas_telephone"
)

# adding wave numbers to list of variables above
cols <-
  paste0("w", apply(expand_grid(seq(1, 4), vars) %>% arrange(vars), 1,
                    paste, collapse = "_"))

# making a list of ecog items on the ecog-12 (2011 version)
ecog_items <- c(
  "w1_ecog_mem1",
  "w1_ecog_mem2",
  "w1_ecog_lang1",
  "w1_ecog_lang2",
  "w1_ecog_visual_spatial2",
  "w1_ecog_visual_spatial5",
  "w1_ecog_planning1",
  "w1_ecog_planning3",
  "w1_ecog_organization1",
  "w1_ecog_organization2",
  "w1_ecog_divided_attention1",
  "w1_ecog_divided_attention2"
)

# replacing ecog item values 88, 99 with na for missing
for (variable in ecog_items) {
  khandle_wide[[paste(variable, "_c", sep = "")]] <-
    ifelse(khandle_wide[[variable]] %in% c(77, 88, 99), NA, khandle_wide[[variable]])
}

# creating race dummy variables for imputation
khandle_wide$w1_d_race_dummy_asian_n <-
  ifelse(khandle_wide$w1_d_race_summary == "Asian", 1, 0)

khandle_wide$w1_d_race_dummy_black_n <-
  ifelse(khandle_wide$w1_d_race_summary == "Black", 1, 0)

khandle_wide$w1_d_race_dummy_latinx_n <-
  ifelse(khandle_wide$w1_d_race_summary == "LatinX", 1, 0)

# creating a dataset to be exported for imputation
khandle2 <- khandle_wide %>% dplyr::select(
  studyid,
  w1_ecog_mem1_c,
  w1_ecog_mem2_c,
  w1_ecog_lang1_c,
  w1_ecog_lang2_c,
  w1_ecog_visual_spatial2_c,
  w1_ecog_visual_spatial5_c,
  w1_ecog_planning1_c,
  w1_ecog_planning3_c,
  w1_ecog_organization1_c,
  w1_ecog_organization2_c,
  w1_ecog_divided_attention1_c,
  w1_ecog_divided_attention2_c,
  w1_d_female,
  w1_d_race_summary,
  w1_d_race_dummy_asian_n,
  w1_d_race_dummy_black_n,
  w1_d_race_dummy_latinx_n,
  #w1_d_race_dummy_native_n,
  dementia_yn,
  w1_nihtlbx_depr_theta,
  edu_yrs_cert,
  censor_death,
  censor_refuse,
  all_of(cols),
  #w1_interview_age,
  w1_interview_age_final,
  w2_interview_age_final,
  w3_interview_age_final,
  w4_interview_age_final
) %>%
  filter(
    !is.na(w1_d_senas_exec_z) &
      !is.na(w1_d_senas_vrmem_z) &
      !is.na(w1_interview_age_final) &
    w1_d_race_summary != "Native American" # dropping this individual for analysis
  )

# 1689 observations

# exporting dataset
# save(khandle2, file = paste0("datasets", "khandle2.RData"))
