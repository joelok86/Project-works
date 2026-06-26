############################################################
# Project: Predicting PM2.5 Trends Using Machine Learning
# Author: Jyothi Sri Lokanadham
#
# Objective:
# Compare Random Forest, Support Vector Machine, and LASSO
# regression models to predict tract-level PM2.5 trends using
# demographic and socioeconomic predictors from ACS data.
#
# Skills demonstrated:
# - Data cleaning
# - Feature engineering
# - ACS data integration
# - Machine learning with tidymodels
# - Hyperparameter tuning
# - Model comparison
# - Variable importance
############################################################

# -------------------------------
# 1. Loading Packages
# -------------------------------

library(tidyverse)
library(tidymodels)
library(tidycensus)
library(vip)
library(glmnet)
library(kernlab)
library(ranger)
library(broom)
library(readr)

set.seed(20211130)

# -------------------------------
# 2. Loading PM2.5 Data
# -------------------------------

# Load preprocessed environmental dataset
# Expected object in .RData: DF
load("data/pm25_environmental_data.RData")

pm25_data <- DF

# -------------------------------
# 3. Creating Tract-Level PM2.5 Trend Outcome
# -------------------------------

# Keeping records with tract identifiers
pm25_tract_data <- pm25_data %>%
  filter(!is.na(Tract_code))

# Functioning to calculate slope of PM2.5 prediction over time
calculate_slope <- function(date, concentration) {
  time_days <- as.numeric(difftime(date, min(date, na.rm = TRUE), units = "days"))
  model <- lm(concentration ~ time_days)
  coef(model)[["time_days"]]
}

# Calculating PM2.5 trend slope for each tract
slopes_df <- pm25_tract_data %>%
  group_by(Tract_code) %>%
  summarise(
    slope = calculate_slope(Date, Ens_pred),
    .groups = "drop"
  )

# -------------------------------
# 4. Downloading ACS Predictor Variables
# -------------------------------

# Add Census API key here or use census_api_key()
key <- "YOUR_CENSUS_API_KEY"

acs_variables <- load_variables(2016, "acs5", cache = TRUE)

selected_variables <- acs_variables %>%
  filter(
    str_detect(concept, "RACE|HOUSING|INCOME"),
    !str_detect(concept, "BY|\\(|RESPONSE|SAMPLE|QUINTILE|SELECTED")
  )

ineq_vars <- selected_variables$name %>% sort()

tract_2016_arizona <- get_acs(
  geography = "tract",
  variables = ineq_vars,
  geometry = TRUE,
  key = key,
  state = "Arizona",
  year = 2016
)

# -------------------------------
# 5. Preparing ACS Data
# -------------------------------

tract_2016_arizona_wide <- tract_2016_arizona %>%
  mutate(variable_group = str_split_fixed(variable, "_", n = 2)[, 1]) %>%
  group_by(GEOID, variable_group) %>%
  mutate(estimate = estimate / max(c(estimate, 1), na.rm = TRUE)) %>%
  ungroup() %>%
  select(-moe, -variable_group) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  select(-ends_with("_001"))

# Creating tract code for merging
tract_2016_arizona_wide <- tract_2016_arizona_wide %>%
  mutate(
    tract_num = gsub("[^[:digit:].]+", "", NAME),
    Tract_code = as.numeric(gsub("\\.", "", tract_num))
  )

slopes_df <- slopes_df %>%
  mutate(Tract_code = as.numeric(Tract_code))

# -------------------------------
# 6. Merging PM2.5 Trends with ACS Predictors
# -------------------------------

model_data <- left_join(
  slopes_df,
  tract_2016_arizona_wide,
  by = "Tract_code"
) %>%
  st_drop_geometry() %>%
  select(-NAME, -GEOID, -tract_num, -Tract_code) %>%
  drop_na()

# -------------------------------
# 7. Train/Test Split
# -------------------------------

train_test_split <- initial_split(model_data, prop = 0.70)

train_data <- training(train_test_split)
test_data  <- testing(train_test_split)

validation_split_data <- validation_split(train_data, prop = 0.50)

# -------------------------------
# 8. Recipe
# -------------------------------

pm25_recipe <- recipe(slope ~ ., data = train_data) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.98)

# -------------------------------
# 9. Random Forest Model
# -------------------------------

rf_model <- rand_forest(
  trees = tune(),
  mtry = tune()
) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")

rf_workflow <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(pm25_recipe)

rf_grid <- grid_regular(
  trees(range = c(100, 2000)),
  mtry(range = c(3, 20)),
  levels = c(10, 10)
)

rf_tuned <- rf_workflow %>%
  tune_grid(
    resamples = validation_split_data,
    grid = rf_grid,
    metrics = metric_set(rmse, rsq, mae)
  )

rf_results <- collect_metrics(rf_tuned)

write_csv(rf_results, "results/random_forest_results.csv")

# -------------------------------
# 10. Support Vector Machine Model
# -------------------------------

svm_model <- svm_rbf(
  cost = tune(),
  rbf_sigma = tune()
) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

svm_workflow <- workflow() %>%
  add_model(svm_model) %>%
  add_recipe(pm25_recipe)

svm_grid <- grid_regular(
  cost(range = c(-2, 6)),
  rbf_sigma(range = c(-5, 1)),
  levels = c(5, 5)
)

svm_tuned <- svm_workflow %>%
  tune_grid(
    resamples = validation_split_data,
    grid = svm_grid,
    metrics = metric_set(rmse, rsq, mae)
  )

svm_results <- collect_metrics(svm_tuned)

write_csv(svm_results, "results/svm_results.csv")

# -------------------------------
# 11. LASSO Regression Model
# -------------------------------

lasso_model <- linear_reg(
  penalty = tune(),
  mixture = 1
) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

lasso_workflow <- workflow() %>%
  add_model(lasso_model) %>%
  add_recipe(pm25_recipe)

lasso_grid <- grid_regular(
  penalty(range = c(-3, 1), trans = log10_trans()),
  levels = 20
)

lasso_tuned <- lasso_workflow %>%
  tune_grid(
    resamples = validation_split_data,
    grid = lasso_grid,
    metrics = metric_set(rmse, rsq, mae)
  )

lasso_results <- collect_metrics(lasso_tuned)

write_csv(lasso_results, "results/lasso_results.csv")

# -------------------------------
# 12. Comparing Model Performance
# -------------------------------

rf_best <- rf_tuned %>%
  show_best(metric = "rmse", n = 1) %>%
  mutate(model = "Random Forest")

svm_best <- svm_tuned %>%
  show_best(metric = "rmse", n = 1) %>%
  mutate(model = "Support Vector Machine")

lasso_best <- lasso_tuned %>%
  show_best(metric = "rmse", n = 1) %>%
  mutate(model = "LASSO Regression")

model_comparison <- bind_rows(rf_best, svm_best, lasso_best)

write_csv(model_comparison, "results/model_comparison.csv")

print(model_comparison)

# -------------------------------
# 13. Final Random Forest Model
# -------------------------------

# Random Forest selected for final evaluation because it supports
# nonlinear relationships and variable importance estimation.

rf_best_params <- select_best(rf_tuned, metric = "rmse")

rf_final_workflow <- rf_workflow %>%
  finalize_workflow(rf_best_params)

rf_final_fit <- rf_final_workflow %>%
  last_fit(train_test_split, metrics = metric_set(rmse, rsq, mae))

rf_final_metrics <- collect_metrics(rf_final_fit)

write_csv(rf_final_metrics, "results/final_random_forest_metrics.csv")

print(rf_final_metrics)

# -------------------------------
# 14. Variable Importance
# -------------------------------

# Fit final RF model to training data for variable importance
rf_fit_for_importance <- rf_final_workflow %>%
  fit(data = train_data)

rf_varimp <- rf_fit_for_importance %>%
  extract_fit_parsnip() %>%
  vip(num_features = 20)

# Save variable importance plot
ggsave(
  filename = "figures/random_forest_variable_importance.png",
  plot = rf_varimp,
  width = 8,
  height = 6,
  dpi = 300
)

print(rf_varimp)

# -------------------------------
# 15. Saving Best Model Object
# -------------------------------

saveRDS(
  rf_final_workflow,
  file = "results/final_random_forest_workflow.rds"
)

############################################################
