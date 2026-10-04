library(butcher)
library(dplyr)
library(parsnip)

# 1. Load models
log_fit <- readRDS("models/log_fit.rds")
poisson_fit <- readRDS("models/poisson_fit.rds")

# 2. Trim memory
log_fit_trimmed <- butcher(log_fit)
poisson_fit_trimmed <- butcher(poisson_fit)

# 3. Test predictions using parsnip-compatible prediction types
sample_data <- readRDS("outputs/final_analytics_dataset.rds") %>% head(1)

log_pred <- predict(log_fit_trimmed, new_data = sample_data, type = "prob")
poisson_pred <- predict(poisson_fit_trimmed, new_data = sample_data, type = "numeric")

print("Logistic Model Probabilities:")
print(log_pred)

print("Poisson Model Predicted Count:")
print(poisson_pred)

# 4. Overwrite RDS files once verified
saveRDS(log_fit_trimmed, "models/log_fit.rds")
saveRDS(poisson_fit_trimmed, "models/poisson_fit.rds")