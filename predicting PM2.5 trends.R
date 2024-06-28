install.packages("tidymodels")
install.packages("vip")
install.packages("rlang")
library("rlang")
library("GSODR")
library(foreach)
library(tidyverse)
library("stars")
library(tmaptools)
library(tmap)
library(tidycensus)
library("tidymodels")
library("readxl") 
library("vip")

load("G:\\CSPH\\R Geocomputation\\LabExercise8\\Ensemble_preds_no_CMAQ_arizona (1).RData")
 lab8 <- DF
 View(lab8)
 
 #filtering out NAs from the tract concentrations##
 
 lab8_tractconc <- lab8[complete.cases(lab8$Tract_code), ]
 
 ##In each tract, calculate the slope of the concentration over time.## 
 ##Use the ensemble PM2.5 prediction (the Ens_pred column) as the concentration##
 ##variable. The slope values will be the outcome variable of your predictive models.##
 
# Create an empty list to store the slope values for each tract
    slope_list <- list()
  
 # Loop over each unique tract code
 for (Tract_code in unique(lab8_tractconc$Tract_code)) {
          
# Subset the data for the current tract
tract_data <- lab8_tractconc[lab8_tractconc$Tract_code == Tract_code, ]
            
 # Calculate the slope of Ens_pred over time (in days) for the current tract
slope <- calc_slope(x = as.numeric(difftime(tract_data$Date, min(tract_data$Date), units = "days")),
                                   +                         y = tract_data$Ens_pred)
              
 # Store the slope value for the current tract in the list
slope_list[[as.character(Tract_code)]] <- slope
                
}
  
 # Convert the list of slopes to a data frame
slopes_df <- data.frame(Tract_code = names(slope_list), slope = unlist(slope_list), stringsAsFactors = FALSE)
 
View(slopes_df)


##Download race, income, and housing variables from the 2016 5-year ACS for all tracts in the##
##state of Arizona. Use the same set of variables that I used in this week’s recorded demonstration.##
 

key <- "cae655db94de4eb8814a2f5267333dc600a98dc8"


library(tidycensus)
 
 vv <- load_variables(2016, "acs5", cache = TRUE) 
 
ineq <- vv %>% filter(str_detect(concept,"RACE|HOUSING|INCOME") & !str_detect(concept,"BY|\\(|RESPONSE|SAMPLE|QUINTILE|SELECTED")) 
 
 ineq_vars <- ineq %>% .$name %>% sort()
 
 unique(ineq$concept)
 
 
  tract_2016_arizona <- get_acs(geography = "tract",
                        variables = ineq_vars, 
                         geometry = TRUE, 
                          key = key, 
                           state = "Arizona",
                           year = 2016)
  
  
  
tract_2016_arizona_wide <- tract_2016_arizona %>% 
mutate(variable_group = str_split_fixed(.$variable,"_",n=2)[,1]) %>%
 group_by(GEOID,variable_group) %>%
 mutate(estimate = estimate/max(c(estimate,1),na.rm=TRUE)) %>%
ungroup() %>%
select(-moe,-variable_group) %>% 
spread(key="variable",value="estimate") %>%
select(-ends_with("_001")) 

ineq_vars <- ineq_vars[ineq_vars %in% colnames(tract_2016_arizona_wide)]  




##merging with the slope values ##

 tract_2016_arizona_wide$tract_num <- gsub("[^[:digit:].]+", "", tract_2016_arizona_wide$NAME)

tract_2016_arizona_wide$Tract_code <- as.numeric(gsub("\\.", "", tract_2016_arizona_wide$tract_num))

tract_2016_arizona_wide <- tract_2016_arizona_wide %>% 
mutate(Tract_code = as.double(Tract_code))

slopes_df <- slopes_df %>% 
  mutate(Tract_code = as.double(Tract_code))



tract_demographics <- left_join(slopes_df, tract_2016_arizona_wide, by = c("Tract_code" = "Tract_code"))

tract_demographicsnew <- na.omit(tract_demographics)




tract_demographicsnew <- tract_demographicsnew %>% 
  tidyr::unnest(geometry) %>% # expand list into separate rows
  mutate(x = unlist(geometry)[seq(1, nrow(.)*2, by = 2)], # extract x-coordinates from list
         y = unlist(geometry)[seq(2, nrow(.)*2, by = 2)]) %>% # extract y-coordinates from list
  select(-geometry , -NAME, -GEOID , -tract_num , -Tract_code , -tract_code , -x , -y) # convert x and y coordinates to numeric



##Use initial_split() to set aside 30% of your##
##data for the test set.  The remainder will be your training+validation set.##

library(rsample)

set.seed(20211130)  # for reproducibility

trainval_test_split <- initial_split(tract_demographicsnew,
                                     prop = 0.70)

trainval <- training(trainval_test_split)
test <- testing(trainval_test_split)



##Define a 10-fold bootstrap object.##


train_val_split <- validation_split(trainval,
                                    prop = 0.50) #note that this is NOT the same as the split in the previous step!

#(1x-repeated) 10-fold cross-validation - normally you'd want these numbers to be larger
x_validation <- vfold_cv(trainval,
                         v=10,
                         repeats = 1)

#bootstrap resampling - normally you'd want more bootstrap samples
boot_resamps <- bootstraps(trainval,
                           times=10)

 

##Create a recipe containing the model formula and the processing steps for your data.##


tract_recipe <- recipe(slope~., 
                             data = trainval) %>%
  step_normalize(all_numeric()) %>% 
  step_zv(all_predictors()) %>%
  step_corr(all_predictors(),threshold=0.98)

##Create a random forest model object with tunable parameters and use it to build a random
##forest workflow. Don’t forget to set the mode to “regression”. Create a tuning grid with at 
##least 10 points. Use tune_grid() to find the best tuning parameter value.

randfor_model <- rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>%
  set_mode("regression")

####################################################################
### Step 8b: Define a workflow object that links to the model    ###
###  object and the data set recipe.                             ###  
####################################################################

randfor_workflow <- workflow() %>%
  add_model(randfor_model) %>%
  add_recipe(tract_recipe)

randfor_fit <- randfor_workflow %>%
  fit(data = trainval)

##Asking it to select a good number for tuning ##

randfor_model <- rand_forest(trees = tune()) %>%
  set_engine("ranger") %>%
  set_mode("regression")



randfor_workflow <- workflow() %>%
  add_model(randfor_model) %>%
  add_recipe(tract_recipe)

randfor_grid <- grid_regular(trees(range=c(100,2000)),
                             levels=10) 
randfor_grid <- grid_regular(
  trees(range = c(100, 2000), levels = 20),
  mtry(range = c(3, 20), levels = 10)
)


library("ranger")

randfor_fit_ss <- randfor_workflow %>% 
  tune_grid(resamples = train_val_split,
            grid = randfor_grid)

rf_results <- randfor_fit_ss %>% collect_metrics()

randfor_fit_ss %>% autoplot()

rfresults <- as.data.frame(rf_results)

# Export data frame to CSV using write_excel_csv() function
write_excel_csv(rfresults, "rf_results.csv")


##svm model##

svm_model <- svm_rbf(cost = tune(), 
                     rbf_sigma = tune()) %>%#tuning parameters (there are others as well)
  set_engine("kernlab") %>%
  set_mode("regression")

svm_workflow <- workflow() %>%
  add_model(svm_model) %>%
  add_recipe(tract_recipe)

#tuning parameter grid
svm_grid <- grid_regular(cost(range=c(-4,14)),#cost() has default arguments range=c(-10,5) and trans=log2_trans()
                         rbf_sigma(range=c(-8,-2)),#rbf_sigma() has default arguments range=c(0,0.2) and log10_trans()
                         levels=c(14,10))

svm_grid <- grid_regular(
  cost(range = c(-2, 6)),
  rbf_sigma(range = c(-5, 1)),
  levels = c(5, 5)
)



svm_fit_ss <- svm_workflow %>% 
  tune_grid(resamples = train_val_split,
            grid = svm_grid) #tune_grid() replaces fit(), train_val_split replaces trainval, and svm_grid replaces fixed values of tuning parameters

svm_results <- svm_fit_ss %>% collect_metrics()

svm_fit_ss %>% autoplot()


svmresults <- as.data.frame(svm_results)

# Export data frame to CSV using write_excel_csv() function
write_excel_csv(svmresults, "svm_results.csv")


#lasso##


# Set up Lasso model
lasso_model <- linear_reg(penalty = tune(), mixture = tune()) %>%
set_engine("glmnet") %>%
set_mode("regression")

# Set up recipe
tract_recipe <- recipe(slope ~ ., data = trainval) %>%
 step_normalize(all_numeric()) %>%
step_zv(all_predictors()) %>%
step_corr(all_predictors(), threshold = 0.98)

 # Set up workflow
lasso_workflow <- workflow() %>%
add_model(lasso_model) %>%
add_recipe(tract_recipe)


 # Define the grid
lasso_grid <- grid_regular(
 penalty(range = c(-3, 1), trans = log10_trans()),
mixture(range = c(0, 1))
)

install.packages("glmnet")
library("glmnet")

lasso_tune <- lasso_workflow %>%
tune_grid(
 resamples = train_val_split,
grid = lasso_grid
)


lasso_results <- lasso_tune %>% collect_metrics()


lasso_tune %>% autoplot()



lassoresults <- as.data.frame(lasso_results)

# Export data frame to CSV using write_excel_csv() function
write_excel_csv(lassoresults, "lasso_results.csv")



randfor_fit_ss %>% show_best("rmse",n=1)
lasso_tune %>% show_best("rmse",n=1)
svm_fit_ss %>% show_best("rmse",n=1)


rf_best_ss <- randfor_fit_ss %>% 
  select_best("rmse")

rf_workflow_final_ss <- randfor_workflow %>%
  finalize_workflow(rf_best_ss)

rf_finalfit_ss <- rf_workflow_final_ss %>%
  last_fit(trainval_test_split) #final evaluation results in the test set

rf_finalfit_ss %>% collect_metrics()

library("dplyr")
library("parsnip")

install.packages("kernlab")
library("glmnet")
library("kernlab")
library("workflows")

rf_varimp_bt <- rf_finalfit_ss %>% 
  extract_fit_parsnip() %>%
  vip(num_features = 20, 
      method = "permute", 
      target = "slope", #outcome variable
      metric = "rmse",
      pred_wrapper = kernlab::predict,
      train = trainval,
      nsim = 100,
      parallel = TRUE) 

rf_varimp_bt

rf_mostimp_bt <- rf_varimp_bt$data$Variable[rf_varimp_bt$data$Importance > 0.001] 

ineq %>% filter(name %in% rf_mostimp_bt) %>% dplyr::select(label,concept)




rf_varimp_bt <- rf_finalfit_ss %>% 
extract_fit_parsnip() %>%
vip(num_features = 20, 
method = "permute", 
target = "slope", #outcome variable
metric = "rmse",
pred_wrapper = function(model, newdata) predict(model, data = newdata)$predictions,
 train = trainval,
 nsim = 100,
  parallel = TRUE) 