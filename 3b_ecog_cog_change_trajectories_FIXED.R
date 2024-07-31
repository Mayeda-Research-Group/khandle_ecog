# Author: Natalie Gradwohl
# Creation Date: 7/17/2023
# Purpose: To create trajectory plots based on predicted values for executive function
# and verbal memory at each time point
#
# UPDATED TO WAVE 4
#
# ----loading packages----
if (!require("pacman"))
  install.packages("pacman", repos = 'http://cran.us.r-project.org')

p_load("tidyverse",
       "gridExtra")

# ----loading data----
# loading mids object
# load(
#   here("output", "datasets", "fake_data_with_preds.RData")
# )

# loading long pooled dataset
# load(
#   here("output", "datasets",  "khandle_long_pooled.RData")
# )

options(scipen = 999)

# ----setting up plot details----
color_vars <- rep(
  c(
    "",
    "baseline_age_c75_decade",
    "w1_d_race_summary",
    "w1_d_female",
    "w1_nihtlbx_depr_theta",
    "edu_yrs_cert",
    "dementia_yn"
  ),
  2
)

legend_values <- list(
  c(""),
  c("1st quartile", "3rd quartile"),
  c("White", "Black", "Asian", "Latinx"),
  c("Male", "Female"),
  c("1st quartile", "3rd quartile"),
  c("13 years", "16 years"),
  c("Yes", "No"),
  c(""),
  c("1st quartile", "3rd quartile"),
  c("White", "Black", "Asian", "Latinx"),
  c("Male", "Female"),
  c("1st quartile", "3rd quartile"),
  c("13 years", "16 years"),
  c("Yes", "No")
)

# list to store filtered data frames
filtered_dfs <- list()

# ----function: filtering datasets for plot----
# looping through each model and filter the data
for (i in 1:14) {
  df <- as.data.frame(fake_data_output[[i]])
  
  # applying filters based on the model
  if (i %in% c(1, 8)) {  # First plot for both outcomes
    df_filtered <- df %>%
      filter(
        baseline_age_c75_decade == quantile(khandle_long_pooled$baseline_age_c75_decade, probs = 0.25),
        w1_d_female == 0,
        w1_d_race_summary == "White",
        w1_nihtlbx_depr_theta == quantile(khandle_long_pooled$w1_nihtlbx_depr_theta, probs = 0.25),
        edu_yrs_cert == 13,
        dementia_yn == 0
      )
  } else if (i %in% c(2, 9)) { 
    df_filtered <- df %>%
      filter(
        w1_d_female == 0,
        w1_d_race_summary == "White",
        w1_nihtlbx_depr_theta == quantile(khandle_long_pooled$w1_nihtlbx_depr_theta, probs = 0.25),
        edu_yrs_cert == quantile(khandle_long_pooled$edu_yrs_cert, probs = 0.25),
        dementia_yn == 0)
  } else if (i %in% c(3, 10)) { 
    df_filtered <- df %>%
      filter(
        baseline_age_c75_decade == quantile(khandle_long_pooled$baseline_age_c75_decade, probs = 0.25),
        w1_d_female == 0,
        w1_nihtlbx_depr_theta == quantile(khandle_long_pooled$w1_nihtlbx_depr_theta, probs = 0.25),
        edu_yrs_cert == quantile(khandle_long_pooled$edu_yrs_cert, probs = 0.25),
        dementia_yn == 0)
  } else if (i %in% c(4, 11)) { 
    df_filtered <- df %>%
      filter(baseline_age_c75_decade == quantile(khandle_long_pooled$baseline_age_c75_decade, probs = 0.25), 
             w1_d_race_summary == "White",
             w1_nihtlbx_depr_theta == quantile(khandle_long_pooled$w1_nihtlbx_depr_theta, probs = 0.25),
             edu_yrs_cert == 13,
             dementia_yn == 0)
  }   else if (i %in% c(5, 12)) { 
    df_filtered <- df %>%
      filter(baseline_age_c75_decade == quantile(khandle_long_pooled$baseline_age_c75_decade, probs = 0.25), 
             w1_d_female == 0,
             w1_d_race_summary == "White",
             edu_yrs_cert == 13,
             dementia_yn == 0)
  }   else if (i %in% c(6, 13)) { 
    df_filtered <- df %>%
      filter(baseline_age_c75_decade == quantile(khandle_long_pooled$baseline_age_c75_decade, probs = 0.25), 
             w1_d_female == 0,
             w1_d_race_summary == "White",
             w1_nihtlbx_depr_theta == quantile(khandle_long_pooled$w1_nihtlbx_depr_theta, probs = 0.25),
             dementia_yn == 0)
  }  else if (i %in% c(7, 14)) { 
    df_filtered <- df %>%
      filter(baseline_age_c75_decade == quantile(khandle_long_pooled$baseline_age_c75_decade, probs = 0.25), 
             w1_d_female == 0,
             w1_d_race_summary == "White",
             w1_nihtlbx_depr_theta == quantile(khandle_long_pooled$w1_nihtlbx_depr_theta, probs = 0.25),
             edu_yrs_cert == 13)
  } 
  
  
  # storing the filtered data frame in the list
  filtered_dfs[[i]] <- df_filtered
}

# defining a function to generate plots
generate_plot <- function(df, color, ylab, values, title, position) {
  ggplot(data = df,
         aes(
           x = studytime,
           y = est,
           linetype = as.factor(ecog.cent),
           color = if (color != "")
             as.factor(df[[color]])
           else
             NULL
         )) +
    geom_line(linewidth = 1.5, alpha = 0.8) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      legend.position = position,
      axis.title = element_text(vjust = 0.5, hjust = 0.5),
      axis.line = element_line(linewidth = 0.5),
      plot.title = element_text(face = "bold")
    ) +
    xlab('Follow up time (years)') + ylab(ylab) +
    scale_color_discrete(name = NULL, labels = values) +
    scale_linetype_manual(name = NULL, values = c('solid', 'dotted'), labels = c("Low ECog", "High ECog")) +
    #guides(linetype = "none") +
    ggtitle(title) +
    scale_y_continuous(breaks = seq(-0.8, 1.2, by = 0.2), limits = c(-0.8, 1.2), labels = scales::number_format(scale = 1, accuracy = 0.1))
}

exec_lab <- "Executive Function Z-score"
vrmem_lab <- "Verbal Memory Z-score"

exec_titles <- c("", "A: Age", "B: Race", "C: Sex/gender",
                 "D: Depression", "E: Education", "F: Family history of dementia")
vrmem_title <- ""

exec_position <- "none"
vrmem_position <- c(.8, .85)

plots <- list()

# looping through each filtered data frame and generate the plot
for (i in 1:14) {
  color <- color_vars[i]
  
  if(i %in% c(1:7)) {
  plot <- generate_plot(filtered_dfs[[i]], color, exec_lab, legend_values[[i]], exec_titles[i], exec_position) }
  else if (i %in% (8:14)) {
  plot <- generate_plot(filtered_dfs[[i]], color, vrmem_lab, legend_values[[i]], vrmem_title, vrmem_position) 
  }
  
  plots[[i]] <- plot
  
  print(plot)
  
}

# ----arranging and saving plots----

base <- do.call("grid.arrange", c(plots[c(1,8)], ncol = 2)) 
# ggsave(here("output", "figures", "base_trajectories.png"), base, width = 10, height = 6)
ggsave(paste0(path_to_output, "base_trajectories.png"), base, width = 10, height = 6)

model_2a <- do.call("grid.arrange", c(plots[c(2,9)], ncol = 2))
# ggsave(here("output", "figures", "model_2a_trajectories.png"), model_2a, width = 10, height = 6)
ggsave(paste0(path_to_output, "model_2a_trajectories.png"), model_2a, width = 10, height = 6)

model_2b <- do.call("grid.arrange", c(plots[c(3,10)], ncol = 2))
# ggsave(here("output", "figures", "model_2b_trajectories.png"), model_2b, width = 10, height = 6)
ggsave(paste0(path_to_output, "model_2b_trajectories.png"), model_2b, width = 10, height = 6)

model_2c <- do.call("grid.arrange", c(plots[c(4,11)], ncol = 2))
# ggsave(here("output", "figures", "model_2c_trajectories.png"), model_2c, width = 10, height = 6)
ggsave(paste0(path_to_output, "model_2c_trajectories.png"), model_2c, width = 10, height = 6)

model_2d <- do.call("grid.arrange", c(plots[c(5,12)], ncol = 2))
# ggsave(here("output", "figures", "model_2d_trajectories.png"), model_2d, width = 10, height = 6)
ggsave(paste0(path_to_output, "model_2d_trajectories.png"), model_2d, width = 10, height = 6)

model_2e <- do.call("grid.arrange", c(plots[c(6,13)], ncol = 2))
# ggsave(here("output", "figures","model_2e_trajectories.png"), model_2e, width = 10, height = 6)
ggsave(paste0(path_to_output, "model_2e_trajectories.png"), model_2e, width = 10, height = 6)

model_2f <- do.call("grid.arrange", c(plots[c(7,14)], ncol = 2))
# ggsave(here("output", "figures", "model_2f_trajectories.png"), model_2f, width = 10, height = 6)
ggsave(paste0(path_to_output, "model_2f_trajectories.png"), model_2f, width = 10, height = 6)

