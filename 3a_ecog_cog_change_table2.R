# Author: Natalie Gradwohl
# Creation Date: 3/29/2023
# Purpose: To perform main analysis on imputed dataset, using linear mixed effects
# models to assess the association between ECog and change in executive function/
# verbal memory scores
#
# UPDATED TO WAVE 4
#
# ----loading packages----
if (!require("pacman"))
  install.packages("pacman", repos = 'http://cran.us.r-project.org')

p_load(
  "tidyverse",
  "here",
  "stats",
  "lme4",
  "sjPlot",
  "broom.mixed",
  "lmtest",
  "mice",
  "parameters",
  "mitools",
  "gt"
)

# ----loading mids object for analysis----
# load(
#   here("output", "datasets", "khandle_imputed_analysis.RData")
# )

# loading long pooled dataset for fake dataset values
# load(
#   here("output", "datasets", "khandle_long_pooled.RData")
# )

# ----model elements to be pulled into loop----
# model names vector to feed into loop
model_names <-
  rep(c(
    "Model 1",
    "Model 2a",
    "Model 2b",
    "Model 2c",
    "Model 2d",
    "Model 2e",
    "Model 2f"
  ),
  2)

# outcomes vector to feed into loop
outcome <- rep(c("d_senas_exec_z", "d_senas_vrmem_z"), each = 7)

# modifiers + interactions vector to feed into loop
modifier <- rep(
  c(
    NA,
    # For empty model
    "baseline_age_c75_decade:ecog.cent:studytime",
    "w1_d_race_summary + w1_d_race_summary:studytime + w1_d_race_summary:ecog.cent:studytime",
    "w1_d_female + w1_d_female:studytime + w1_d_female:ecog.cent:studytime",
    "w1_nihtlbx_depr_theta + w1_nihtlbx_depr_theta:studytime + w1_nihtlbx_depr_theta:ecog.cent:studytime",
    "edu_yrs_cert + edu_yrs_cert:studytime + edu_yrs_cert:ecog.cent:studytime",
    "dementia_yn + dementia_yn:studytime + dementia_yn:ecog.cent:studytime"
  ),
  2
)

offset <- rep(c("off", "off2"), each = 7)

# ----creating a fake dataset with all possible combinations of covariates----
fake_dat_combined <- expand.grid(
  studytime = 0:3,
  ecog.cent = c(0, 1),
  baseline_age_c75_decade = c(
    quantile(khandle_long_pooled$baseline_age_c75_decade, probs = 0.25),
    quantile(khandle_long_pooled$baseline_age_c75_decade, probs = 0.75)
  ),
  phone = 0,
  w1_d_race_summary = c("White", "Black", "Asian", "LatinX"),
  w1_d_female = c(0, 1),
  w1_nihtlbx_depr_theta = c(
    quantile(khandle_long_pooled$w1_nihtlbx_depr_theta, probs = 0.25),
    quantile(khandle_long_pooled$w1_nihtlbx_depr_theta, probs = 0.75)
  ),
  edu_yrs_cert = c(
    quantile(khandle_long_pooled$edu_yrs_cert, probs = 0.25),
    quantile(khandle_long_pooled$edu_yrs_cert, probs = 0.75)
  ),
  dementia_yn = c(0, 1)
) %>%
  mutate(
    first_assess = ifelse(studytime == 0, 1, 0),
    off = ifelse(first_assess == 1, -0.06, 0),
    off2 = ifelse(first_assess == 1, -0.09, 0),
    phone = as.factor(phone),
    w1_d_race_summary = as.factor(w1_d_race_summary),
    w1_d_female = as.factor(w1_d_female),
    dementia_yn = as.factor(dementia_yn)
  )

# manually adding studyid and making sure it's a factor
fake_dat_combined$studyid <- as.factor(seq(1, nrow(fake_dat_combined)))

# ----function: predicting outcome on fake data----
khandle_predict <- function(formula, offset_var, new_data) {
  point_est <- NULL
  
  for (i in 1:40) {
    curr_data <- khandle_long_pooled %>% filter(.imp == i)
    lmer_mod <- lmer(as.formula(formula), data = curr_data, offset = curr_data[[offset_var]], REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
    
    # predicting new outcome values for fake data based on model
    estimate <- predict(lmer_mod, newdata = new_data, allow.new.levels = TRUE)
    
    # initializing or updating point estimate
    if (is.null(point_est)) {
      point_est <- estimate
    } else {
      point_est <- rbind(point_est, estimate)
    }
  }
  
  # calculating mean point estimate across imputations
  est <- colMeans(point_est)
  
  # creating a result dataframe
  result_df <- data.frame(est = est)
  
  # binding dataframe with fake dataset in order to plot
  result_df <- cbind(new_data, result_df)
  
  # returning complete dataframe
  return(result_df)
}

# ----looping through models and applying function----
# creating an empty list to store data frames for each model
mod_list <- list()

fake_data_output <- list()

# initializing a vector to store all unique terms across all models
all_terms <- character()

# setting seed for reproducibility - predicting on fake dataset
set.seed(123456)

for (i in 1:14) {
  # for base model
  if (length(na.omit(modifier[i])) > 0) {
    formula <- paste(
      outcome[i],
      "~ ecog.cent * studytime + phone + baseline_age_c75_decade * studytime + (1 | studyid) +",
      modifier[i]
    )
  } else {
    # adding in modifiers for other 6 models
    formula <- paste(
      outcome[i],
      "~ ecog.cent * studytime + phone + baseline_age_c75_decade * studytime + (1 | studyid)"
    )
  }
  
  # specifying offset based on outcome variable
  if (outcome[i] == "d_senas_exec_z") {
    model <-
      with(khandle_imputed_analysis,
           lmer(formula, offset = off, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))
  } else {
    model <-
      with(khandle_imputed_analysis,
           lmer(formula, offset = off2, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))
  }
  
  # storing the results in a data frame
  model_results <-
    as.data.frame(summary(pool(model), conf.int = TRUE)) %>%
    # pulling relevant statistics
    select(term, estimate, `2.5 %`, `97.5 %`, p.value) %>%
    mutate(
      model_id = paste(model_names[i], "_model"),
      # creating a formatted confidence interval variable that includes upper/lower bounds
      ci = paste("(", round(`2.5 %`, 3), ",", round(`97.5 %`, 3), ")", sep = ""),
      # creating a formatted variable containing the estimate and ci to go into table 2
      est_conf = paste(round(estimate, 3), ci, sep = " ")
    ) %>%
    select(term, est_conf, p.value)
  
  
  # calling in the model name for each model
  model_name <- model_names[i]
  
  # adding model name to column to show which estimates correspond to which models
  colnames(model_results)[-1] <-
    paste(colnames(model_results)[-1], model_names[i], sep = "_")
  
  # updating list of terms
  all_terms <- union(all_terms, model_results$term)
  
  # adding data frame to a list
  mod_list[[i]] <- model_results
  
  formula <- formula
  offset_var <- offset[i]
  new_data <- fake_dat_combined

  result <- khandle_predict(formula, offset_var, new_data)
  fake_data_output[[i]] <- result

}

# displaying statsitics from all 14 models
#print(mod_list)

# saving model output
# save(mod_list, file = here("output", "datasets", "models.RData"))

# saving fake dataset with predictions to be used in figure (script 3b)
# save(fake_data_output, file = here("output", "datasets", "fake_data_with_preds.RData"))

# ----table 2: executive function----
# create separate data frame with all unique terms for each outcome
tab2_df_exec <- data.frame(term = all_terms)

# left joining each model's data to the list of all unique terms
for (i in 1:7) {
  tab2_df_exec <-
    left_join(tab2_df_exec,
              mod_list[[i]],
              by = "term") %>%
    select(-contains("p.value"))
}

# converting data frame to tibble to be fed into gt()
tab2_df_exec <- as_tibble(tab2_df_exec) %>%
  # replacing all na's with blanks
  mutate_all( ~ ifelse(is.na(.), "", .))

# creating gt object containing all the information from the data frame, preserving structure
table_exec <- gt(tab2_df_exec) %>% 
  # renaming columns to specify model name
  cols_label(.list = setNames(as.list(c(
    "Term", model_names[1:7]
  )), colnames(tab2_df_exec)))

# displaying table 2: executive function
# print(table_exec)

# saving table 2: executive function as an html file
# gt::gtsave(table_exec, file = here("output", "tables", "exec_table2.html"))
gt::gtsave(table_exec, file = paste0(path_to_output, "exec_table2.html"))

# ----table 2: verbal memory----
# create separate data frame with all unique terms for each outcome
tab2_df_vrmem <- data.frame(term = all_terms)

# left joining each model's data to the list of all unique terms
for (i in 8:14) {
  tab2_df_vrmem <-
    left_join(tab2_df_vrmem,
              mod_list[[i]],
              by = "term") %>%
    select(-contains("p.value"))
}

# converting data frame to tibble to be fed into gt()
tab2_df_vrmem <- as_tibble(tab2_df_vrmem) %>%
  # replacing all na's with blanks
  mutate_all( ~ ifelse(is.na(.), "", .))

# creating gt object containing all the information from the data frame, preserving structure
table_vrmem <- gt(tab2_df_vrmem) %>% 
  # renaming columns to specify model name
  cols_label(.list = setNames(as.list(c(
    "Term", model_names[1:7]
  )), colnames(tab2_df_vrmem)))

# displaying table 2: verbal memory
print(table_vrmem)

# saving table 2: verbal memory as an html file
# gt::gtsave(table_vrmem, file = here("output", "tables", "vrmem_table2.html"))
gt::gtsave(table_vrmem, file = paste0(path_to_output, "vrmem_table2.html"))
