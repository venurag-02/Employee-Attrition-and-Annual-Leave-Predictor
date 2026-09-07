#===============================================================================================
#         SCRIPT FOR THE ATTRITION AND ANNUAL LEAVE PREDICTOR 2026
#===============================================================================================

library(tidyverse)
library(janitor)    #Useful for converting the variable name to suitable formats.
library(readxl)     #Reading xlsx files.
library(poissonreg) #Engine for poisson regression.
library(tidymodels) #Contains important tools for modelling e.g. recipes, parsnip, etc.
library(broom)      #Contains tools that provide polished odds ratios and IRRs (results).
library(themis)     #Contains SMOTE, used for removing imbalances in data.
library(furrr)      #Tool for maximizing the vectorization of Monte Carlo simulations.
library(xgboost)    #Required for XGBoost.
library(glmnet)     #Required for the log regression.


set.seed(2026) #Seed is set to make sure that whenever the model is run it has the same distribution. As a result the model would produce predictions as accurate as possible. Numbers being generated would be the same each time the script is run.

if (!dir.exists("outputs")) dir.create("outputs") #Creates new folders for outputs/results and models. 
if (!dir.exists("models")) dir.create("models")

plan(multisession, workers = availableCores() - 1)   #Optimizes the cores available.

#===============================================================================================
#         CLEANING DATA AND SETTING THE PITCH
#===============================================================================================

raw_ibm <- read_excel ("WA_Fn-UseC_-HR-Employee-Attrition.xlsx") |> #Creating a copy of the original data set. It is easier for RStudio to read is xlsx form and make sure it's available in the working directory.
  
  clean_names() #Automatically changes the variable names to suitable formats.

cleaned_hr_dataset <- raw_ibm |>
  
  #Zero variance variables are being dropped to make sure the ML models are not confused. 
  #Variables such as 'employee_count', 'over18' and 'standard_hours' are always '1'.
  select(-employee_count, -over18, -standard_hours) |>
  
  #Next is to select the target variable along with other desired highly predictive variables which includes 'Psychological' and 'Environmental' variables.
  
  select(
    #Target variable.
    attrition, 
    
    #Psychological variables.
    environment_satisfaction,
    job_satisfaction,
    marital_status,
    over_time,
    relationship_satisfaction,
    work_life_balance,
    
    #Career/Enviornmental variables.
    age,
    business_travel, #Too much of business travel affects work_life_balance.
    daily_rate,
    department,
    distance_from_home, #Longer distance of travel affects work_life_balance.
    education,
    education_field, 
    gender,
    hourly_rate,
    job_involvement,
    job_level, #Might make the employee feel less important or the need to move affecting the job_satisfaction.
    job_role,
    monthly_income,
    monthly_rate,
    num_companies_worked, 
    percent_salary_hike,
    performance_rating,
    stock_option_level, #If the stocks are valuable they might stay to keep them.
    total_working_years,
    years_at_company,
    years_in_current_role, #Many years of working could lead to lower job_satisfaction.
    years_since_last_promotion,
    years_with_curr_manager
  ) |>
  
  #Converting raw data to factors, a crucial step when using tidymodels.
  mutate(
    
    #Conversions for Binary and Categorical variables
    attrition = factor(attrition, levels = c("Yes", "No")),
    
    over_time = factor(over_time, levels = c("No", "Yes")), #The levels in over_time is switched to make 'not' working over_time the baseline and compare against how it affects attrition.
    
    business_travel = factor(business_travel, levels = c("Travel_Rarely", "Travel_Frequently", "Non-Travel")),
    
    marital_status = factor(marital_status, levels = c("Married", "Single", "Divorced")),
    department = factor(department, levels = c("Research & Development", "Human Resources", "Sales")),
    
    education_field = factor(education_field, levels = c("Human Resources", "Technical Degree", "Life Sciences", "Marketing", "Medical", "Other")),
    
    gender = factor(gender, levels = c("Male", "Female")),
    
    job_role = factor(job_role, levels = c("Healthcare Representative", "Human Resources", "Laboratory Technician", "Manager", "Manufacturing Director", "Research Director", "Research Scientist", "Sales Executive", "Sales Representative")),
    
    performance_rating = factor(performance_rating, levels = c("3", "4")),
    
    #Conversions for numeric ratings so the models treat them as scales (Ordinal).
    environment_satisfaction = factor(environment_satisfaction, ordered = TRUE),
    
    job_satisfaction = factor(job_satisfaction, ordered = TRUE),
    
    relationship_satisfaction = factor(relationship_satisfaction, ordered = TRUE),
    
    work_life_balance = factor(work_life_balance, ordered = TRUE),
    
    education = factor(education, ordered = TRUE),
    
    job_involvement = factor(job_involvement, ordered = TRUE),
    
    job_level = factor(job_level, ordered = TRUE)
  )

#-----------------------------------------------------------------------------------------------
# **IMPORTANT**

#Multicollinearity for variables accounting to salary were checked and the results were good enough to proceed. cor(raw_ibm[, c("hourly_rate", "monthly_rate", "monthly_income", "daily_rate")]).

#-----------------------------------------------------------------------------------------------

#===============================================================================================
#         ANNUAL LEAVE AND LEAVE QUARTER PREDICTION VARIABLES 
#===============================================================================================

#Next a new variable will be generated for annual leave since the original data set does not have such a variable. 

final_analytics_dataset <- cleaned_hr_dataset |>
  mutate(
    
    #The upper bound (5.2) and lower bound (1.3) of lambda value were taken from reports by Australian companies and are not hard limits.
    predicted_lambda = case_when(  #Acts as an 'if' condition.
      
      over_time == "Yes" & as.numeric(job_satisfaction) <= 2 ~ 5.2, #Working over time -> higher frequency of leave.
      
      over_time == "No" & as.numeric(job_satisfaction) >= 3 ~ 1.3, #Not working over time -> lower frequency of leave.
      
      TRUE ~ 2.6  #Acts as the third condition where employee who don't fit in with previous options e.g. working overtime yet satisfied or not working overtime yet unsatisfied.
    ),
    total_annual_leaves = rpois(n(), lambda = predicted_lambda), #Poisson count is used to determine how many annual leaves an employee would take according to the predicted_lambda. 
    
    peak_quarter_leaves = factor(
      sample(c("Q1", "Q2", "Q3", "Q4"), size = n(), replace = TRUE, prob = c(0.15, 0.25, 0.35, 0.25)), #Multinomial class is used to flip a four sided probability coin where Q1 = 0.15, Q2 = 0.25, Q3 = 0.35 and Q4 = 0.25. The probabilities where determined depending on the seasons and holidays that fall before, during or after the quarter. The probability then decides the which quarter the employee is most likely to take the leave.
      
      levels = c("Q1", "Q2", "Q3", "Q4") #Assigns a chronological order.
    )
  ) |>
  select(-predicted_lambda) #Removes the predicted_lambda variable since it's not needed.

write_rds(final_analytics_dataset, "outputs/final_analytics_dataset.rds") #Export cleaned data set for later use.

#===============================================================================================
#        DATA SPLITING AND CROSS-VALIDATION SET-UP
#=============================================================================================== 

#80/20 Train-Test Split (80% used to train the model and 20% left untouched to test the model).
data_split <- initial_split(final_analytics_dataset,prop = 0.80, strata = attrition)
train_data <- training(data_split)
test_data <- testing(data_split)

#10-fold cross-validation set on the training data, where the data is further split into 10 parts and each part is tested against the model before the actual test.
cv_folds <- vfold_cv(train_data, v = 10, strata = attrition)


#===============================================================================================
#        ATTRITION ENGINE (LOGISTIC REGRESSION - ODDS RATIOS)
#===============================================================================================

attrition_rec <- recipe(attrition ~ ., data = train_data) |> #Target variable is attrition.
  
  step_rm(total_annual_leaves, peak_quarter_leaves) |> #Synthetic (made-up) variables removed to stop the model from getting confused.
  
  step_rm(hourly_rate, daily_rate, monthly_rate) |> #Removes multicollinearity to prevent confusion.
  
  step_novel(all_nominal_predictors()) |> #Handles factor levels in categorical variables.
  
  step_dummy(all_nominal_predictors()) |> #Converts the categorical variables into binary 0/1.
  
  step_zv(all_predictors()) |> #Zero-variance variables removed since they don't help with prediction (they interfere with the math).
  
  step_normalize(all_numeric_predictors()) |> #Center and scale numeric variables so they are comparable.
  
  step_smote(attrition) #SMOTE applying to balance out minorities.

log_specs <- logistic_reg(penalty = 0.01, mixture = 0.5) |> #Standard binary classification logistic regression declared.
  set_engine("glmnet") |>
  set_mode("classification")

log_wf <- workflow() |> #Combines existing recipe (attrition_rec) with the regression (log_specs) into one.
  add_recipe(attrition_rec) |>
  add_model(log_specs)

log_fit <- fit(log_wf, data = train_data) #log_wf is used on the training data.

write_rds(log_fit, "models/log_fit.rds") #Save the log regression model.

attrition_odds_ratios <- broom::tidy(   #Obtain the Odds Ratios. "broom" cleans and structure the raw coefficients.
  extract_fit_parsnip(log_fit),  #Fitted model is pulled out of the workflow (log_wf).
  exponentiate = TRUE,           #Converts the raw log-odds into readable odds ratios.
  conf.int = TRUE                #Adds 95% confidence intervals.
)

attrition_preds <- predict(log_fit, test_data, type = "prob") |>  #Obtains the "Yes" or "No".
  bind_cols(predict(log_fit, test_data)) |>   #Combines the predicted probabilities, classes and actual actual ground truth labels.  
  bind_cols(test_data |> select(attrition))

auc_metric <- yardstick::roc_auc(attrition_preds, truth = attrition, .pred_Yes)  #Compares the "Yes" or "No" metric against the attrition variable to evaluate the performances.

#===============================================================================================
#        LEAVE FREQUENCY ENGINE (POISSON REGRESSION - IRR)
#===============================================================================================

#If comments needed refer to the above section.

poisson_rec <- recipe(total_annual_leaves ~ ., data = train_data) |>
  step_rm(attrition, peak_quarter_leaves) |>
  step_rm(hourly_rate, daily_rate, monthly_rate) |>
  step_novel(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors())

poisson_spec <- poisson_reg() |>
  set_engine("glm") |>
  set_mode("regression") 

poisson_wf <- workflow() |>
  add_recipe(poisson_rec)|>
  add_model(poisson_spec)

poisson_fit <- fit(poisson_wf, data = train_data)

write_rds(poisson_fit, "models/poisson_fit.rds")

leave_irr <- broom::tidy(            #Obtain the IRR values.
  extract_fit_parsnip(poisson_fit),
  exponentiate = TRUE,
  conf.int = TRUE
)

poisson_preds <- predict(poisson_fit, test_data) |> #Compares the predicted results with the actual values in the test data set.
  bind_cols(test_data |>
              select(total_annual_leaves))

#Provides a performance metric on how well the model works.
poisson_metrics <- metrics(poisson_preds, truth = total_annual_leaves, estimate = .pred) 


#-----------------------------------------------------------------------------------------------
# **IMPORTANT**

#This section saves the outputs of the models in the directory. Use if needed.

attrition_odds_ratios |>      #Log regression
  write_csv("outputs/attrition_odds_ratios.csv")

leave_irr |>                  #Poisson regression
  write_csv("outputs/leave_irr.csv")

tibble(   #Poisson regression performance metrics (Root Mean Squared Error and Mean Absolute Error).
  model = "Poisson Regression",
  metric = poisson_metrics$.metric,
  value = poisson_metrics$.estimate
) |>
  write_csv("outputs/poisson_metrics.csv")


bind_rows(                    #ROC-AUC metrics
  tibble(model = "Logistic Regression", metric = "ROC-AUC", value = auc_metric$.estimate)
) |>
  write_csv("outputs/model_metrics.csv")

attrition_roc_curve <- yardstick::roc_curve(attrition_preds, truth = attrition, .pred_Yes)
write_csv(attrition_roc_curve, "outputs/attrition_roc_curve.csv")

#Obtains the variable importance, top 10 factors that greatly affect the attrition. 
log_fit |>
  extract_fit_parsnip() |>
  broom::tidy() |>
  filter(term != "(Intercept)") |>
  mutate(importance = abs(estimate)) |>
  slice_max(importance, n =10) |>
  write_csv("outputs/attrition_vip.csv")

#To see the output of the results see "report.pdf". To get the R-script for the output see "report.qmd".

#-----------------------------------------------------------------------------------------------



#===============================================================================================
#        ADVANCED ML AND XGBOOST TUNING
#===============================================================================================


xgb_spec <- boost_tree(       #Declaring the model used (XGBoost).
  trees = tune(),            
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune()
) |>
  set_engine("xgboost") |>
  set_mode("classification")

xgb_wf <- workflow() |>            #The same recipe "attrition_rec" will be used to run.
  add_recipe(attrition_rec) |>
  add_model(xgb_spec)

xgb_grid <- grid_latin_hypercube(   #Latin Hyper-cube Sampling used to ensure the model explores evenly without going too deep.
  trees(range = c(100, 1000)),      #No. of decision trees made.
  tree_depth(range = c(3, 10)),     #No. of levels or depth the trees are allowed to form.
  learn_rate(range = c(-3, -1), trans = log10_trans()),  #How quick the algorithm learns from errors on a log scale from 0.001 to 0.1 .
  loss_reduction(),    #The minimum gain required for the leaf node to keep growing. In tidymodels the "()" means a range of "c(0, 10)", or (10^-10 to 10^1.5) on a log scale.
  size = 15                         #No. of models/parameter sets.
)

xgb_results <- tune_grid(        #15 parameter sets are run across 10 cross-validation folds (cv_folds), this trains and test all 15 models parallel to determine one with highest yield.
  xgb_wf,
  resamples = cv_folds,
  grid = xgb_grid,
  metrics = metric_set(roc_auc, accuracy),
  control = control_grid(save_pred = TRUE)
)

best_xgb_para <- select_best(xgb_results, metric = "roc_auc")  #Selects the best model.
final_xgb_wf <- finalize_workflow(xgb_wf, best_xgb_para)

xgb_fit <- fit(final_xgb_wf, data = train_data)   #Runs the best model across the entire training data set. 

write_rds(xgb_fit, "models/xgb_fit.rds")

xgb_preds <- predict(xgb_fit, test_data, type = "prob") |>  #Compares the predicted probabilities with the test data set.
  bind_cols(predict(xgb_fit, test_data)) |>
  bind_cols(test_data |> 
              select(attrition))

xgb_auc <- yardstick::roc_auc(xgb_preds, truth = attrition, .pred_Yes) #Gets the performance metrics of the model.


#-----------------------------------------------------------------------------------------------
# **IMPORTANT**

#This section is for saving and visualizing the outputs of XGBoost tuning.

model_metrics <- read_csv("outputs/model_metrics.csv", show_col_types = FALSE) |>
  bind_rows(
    tibble(model = "XGBoost", metric = "ROC-AUC", value = xgb_auc$.estimate)
  )

write_csv(model_metrics, "outputs/model_metrics.csv")

xgb_roc_curve <- yardstick::roc_curve(xgb_preds, truth = attrition, .pred_Yes) |>
  mutate(model = "XGBoost")

read_csv("outputs/attrition_roc_curve.csv", show_col_types = FALSE) |>
  select(-any_of("model")) |>     #Removes existing models to avoid confusion. 
  mutate(model = "Logistic Regression") |>
  bind_rows(xgb_roc_curve) |>
  write_csv("outputs/combined_roc_curves.csv")


#Combine all the results for a direct comparison of performance.

bind_rows(
  tibble(
    model = "Logistic Regression",
    target = "attrition",
    metric = "ROC-AUC",
    value = auc_metric$.estimate
  ),
  tibble(
    model = "XGBoost",
    target = "attrition",
    metric = "ROC-AUC",
    value = xgb_auc$.estimate
  ),
  poisson_metrics |>
    transmute(
      model = "Poisson Regression",
      target = "total_annual_leaves",
      metric = toupper(.metric),
      value = .estimate 
    )
) |>
  write_csv("outputs/all_models_metrics.csv")


#To see the output of the results see "report.pdf". To get the R-script for the output see "report.qmd".

#-----------------------------------------------------------------------------------------------



#===============================================================================================
#        MONTE CARLO SIMULATIONS
#===============================================================================================

#New copy of the data set is generated where the two models run for each employee to predict their attrition or leave days.
mc_base_data <- final_analytics_dataset |>      
  mutate(
    p_attrition = predict(log_fit, final_analytics_dataset, type = "prob")$.pred_Yes,
    lambda_leaves = predict(poisson_fit, final_analytics_dataset)$.pred
  )

n_sims <- 10000     #No. of simulations run.

#Workload is distributed across the cores. "rbinom" (binomial) flips a weighted coin according to personal probability to determine their attrition. "rpois" (poisson) draws random no. of leaves according to an employee's predicted count.
simulation_results <- future_map_dfr(1:n_sims, function(sim_id) {  
  
  sim_attrition <- rbinom(nrow(mc_base_data), size = 1, prob = mc_base_data$p_attrition)
  sim_leaves <- rpois(nrow(mc_base_data), lambda = mc_base_data$lambda_leaves)
  
  tibble(                #Calculates the total of each variable.
    sim_id = sim_id,
    total_attrition = sum(sim_attrition),
    total_leaves = sum(sim_leaves)
  )
}, .options = furrr_options(seed = 2026)) #Ensures the cores run statistically valid and reproducible numbers. It also ensures that the cores don't overlap with the generated simulations.


plan(sequential) #End core optimization.


#Calculates the average across all 10,000 simulations and produces a 95% CI.
mc_summary <- simulation_results |>   
  summarise(
    mean_attrition = mean(total_attrition),
    attrition_p2_5 = quantile(total_attrition, 0.025),
    attrition_p97_5 = quantile(total_attrition, 0.975),
    
    mean_leaves = mean(total_leaves),
    leaves_p2_5 = quantile(total_leaves, 0.025),
    leaves_p97_5 = quantile(total_leaves, 0.975)
  )

#Produces the results of the simulations. See report.pdf for the output and report.qmd for the R script.
write_csv(simulation_results, "outputs/monte_carlo_results.csv")
write_csv(mc_summary, "outputs/monte_carlo_summary.csv")