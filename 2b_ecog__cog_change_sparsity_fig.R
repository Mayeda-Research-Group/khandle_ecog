# Author: Natalie Gradwohl
# Creation Date: 8/3/2023
# Purpose: To visualize sparsity among higher ECog scores
#
# BASELINE DATA ONLY
#
# ----loading packages----
if (!require("pacman"))
  install.packages("pacman", repos = 'http://cran.us.r-project.org')

p_load("tidyverse",
       # here,
       "lme4",
       "mice",
       "ggplot2",
       "patchwork")

# ----loading mids object----
# load(
#   here("output", "datasets", "khandle_imputed_analysis.RData")
# )

# creating long dataset of first imputation
imp_1 <- khandle_imputed_analysis %>% complete(action = 1)

# ----executive function estimated slopes----
# running executive function model 1 on first imputation dataset
exec_mod <- lmer(
  d_senas_exec_z ~ ecog.cent * studytime +
    baseline_age_c75_decade * studytime +
    phone + (1 |
               studyid),
  data = imp_1,
  offset = off,
  REML = T
)

# summarizing model
sum_exec_mod <- summary(exec_mod)
# extracting coefficients from model
slope_terms_exec <- sum_exec_mod$coefficients %>%
  as_tibble(rownames = "Term") %>%
  filter(str_detect(Term, "studytime"))

# creating matrix of slope terms
betas <- slope_terms_exec$Estimate %>% as.matrix()

# creating dataset of unique values of variables for each individual studyid
exec_slopes <- imp_1 %>%
  distinct(studyid,
           ecog.cent,
           baseline_age_c75_decade) %>%
  mutate(# setting base_slope to 1 to get studytime term
    base_slope = 1) %>%
  select(studyid,
         base_slope,
         ecog.cent,
         baseline_age_c75_decade)

# adding estimated slope column by multiplying all of the other columns by their beta coefficients
exec_slopes[, "est_slope"] <-
  # -1 to remove studyid column from operation
  as.numeric(as.matrix(exec_slopes[, c(-1)]) %*% betas)

# creating binned ecog variable for ggplot
exec_slopes$ecog.bin <-
  as.factor(ifelse(
    # separate category for ecog = 1 because of the skewness
    exec_slopes$ecog.cent == 0,
    1,
    ifelse(
      exec_slopes$ecog.cent > 0 &
        exec_slopes$ecog.cent < 0.5,
      2,
      ifelse(
        exec_slopes$ecog.cent >= 0.5 &
          exec_slopes$ecog.cent < 1,
        3,
        ifelse(
          exec_slopes$ecog.cent >= 1 &
            exec_slopes$ecog.cent < 1.5 ,
          4,
          ifelse(
            exec_slopes$ecog.cent >= 1.5 &
              exec_slopes$ecog.cent < 2 ,
            5,
            ifelse(exec_slopes$ecog.cent >= 2, 6, NA)
          )
        )
      )
    )
  ))

# setting variable labels for the figure
exec_slopes$ecog.bin <- factor(
  exec_slopes$ecog.bin,
  levels = c(1, 2, 3, 4, 5, 6),
  labels = c("1", "(1, 1.5)", "[1.5, 2)", "[2, 2.5)", "[2.5, 3)", "≥3")
)

# ----verbal memory estimated slopes----
# running verbal memory model 1 on first imputation dataset
vrmem_mod <- lmer(
  d_senas_exec_z ~ ecog.cent * studytime +
    baseline_age_c75_decade * studytime + (1 |
                                             studyid),
  data = imp_1,
  offset = off2,
  REML = T
)

# summarizing model
sum_vrmem_mod <- summary(vrmem_mod)
# extracting coefficients from model
slope_terms_vrmem <- sum_vrmem_mod$coefficients %>%
  as_tibble(rownames = "Term") %>%
  filter(str_detect(Term, "studytime"))

# creating matrix of slope terms
betas <- slope_terms_vrmem$Estimate %>% as.matrix()

# creating dataset of unique values of variables for each individual studyid
vrmem_slopes <- imp_1 %>%
  distinct(studyid,
           ecog.cent,
           baseline_age_c75_decade) %>%
  mutate(# setting base_slope to 1 to get studytime term
    base_slope = 1) %>%
  select(studyid,
         base_slope,
         ecog.cent,
         baseline_age_c75_decade)

# adding estimated slope column by multiplying all of the other columns by their beta coefficients
vrmem_slopes[, "est_slope"] <-
  # -1 to remove studyid column from operation
  as.numeric(as.matrix(vrmem_slopes[, c(-1)]) %*% betas)

# creating binned ecog variable for ggplot
vrmem_slopes$ecog.bin <-
  as.factor(ifelse(
    # separate category for ecog = 1 because of the skewness
    vrmem_slopes$ecog.cent == 0,
    1,
    ifelse(
      vrmem_slopes$ecog.cent > 0 &
        vrmem_slopes$ecog.cent < 0.5,
      2,
      ifelse(
        vrmem_slopes$ecog.cent >= 0.5 &
          vrmem_slopes$ecog.cent < 1,
        3,
        ifelse(
          vrmem_slopes$ecog.cent >= 1 &
            vrmem_slopes$ecog.cent < 1.5 ,
          4,
          ifelse(
            vrmem_slopes$ecog.cent >= 1.5 &
              vrmem_slopes$ecog.cent < 2 ,
            5,
            ifelse(vrmem_slopes$ecog.cent >= 2, 6, NA)
          )
        )
      )
    )
  ))

# setting variable labels for the figure
vrmem_slopes$ecog.bin <- factor(
  vrmem_slopes$ecog.bin,
  levels = c(1, 2, 3, 4, 5, 6),
  labels = c("1", "(1, 1.5)", "[1.5, 2)", "[2, 2.5)", "[2.5, 3)", "≥3")
)

# ----creating plot----
# combining datasets for facet wrapped figure
combined_data <- bind_rows(
  vrmem_slopes %>% mutate(dataset = "Executive Function"),
  exec_slopes %>% mutate(dataset = "Verbal Memory")
)

# using combined dataset to display estimated slope across ecog bins, by cognitive domain
sparsity <- ggplot(combined_data, aes(x = est_slope)) +
  geom_histogram(
    binwidth = 0.01,
    color = "black",
    fill = "blueviolet"
  ) +
  # making a row for each cognitive domain
  facet_grid(dataset ~ ecog.bin, scales = "free_y", space = "free") +
  xlab("Estimated Slope") +
  ylab("Frequency") +
  ggtitle(
    "Histograms of Estimated Slopes of Verbal Memory trajectories by ECog score and Depression Quartiles"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

# exporting plot as image
# ggsave(
#   here("output", "figures", "ecog_sparsity_plot.png"),
#   plot = sparsity,
#   width = 12,
#   height = 9
# )

ggsave(
  paste0(path_to_output, "ecog_sparsity_plot.png" ),
  plot = sparsity,
  width = 12,
  height = 9
)
