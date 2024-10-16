library(MASS)
library(car)
library(caret)

# Read the data
data <- read.csv("BodyFat.csv")

# Using IQR to find outliers
find_iqr_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  x < lower_bound | x > upper_bound
}

data_no_id <- data[ , !(names(data) %in% c("IDNO"))]
iqr_outliers_flags <- as.data.frame(lapply(data_no_id, find_iqr_outliers))
outliers <- data[apply(iqr_outliers_flags, 1, any), ]
outliers_columns <- apply(iqr_outliers_flags[apply(iqr_outliers_flags, 1, any), ], 1, function(x) {
  colnames(data_no_id)[x]
})
outliers$Outlier_Variables <- sapply(outliers_columns, paste, collapse = ", ")
print(outliers)


# Remove outliers 39, 41, and 216
data <- data[!(data$IDNO %in% c(39, 41, 216)), ]

# Using adiposity to adjust height for 42
data$HEIGHT <- round(data$HEIGHT * 2.54 , 2)
weight_kg <- data$WEIGHT[42] * 0.453592  
height_m <- sqrt(weight_kg / data$ADIPOSITY[42])  
height_cm <- round(height_m * 100, 2)  
data$HEIGHT[data$IDNO == 42] <- height_cm

# Preprocessed data
df <- data[, !names(data) %in% c("IDNO", "DENSITY")]
df_scaled <- as.data.frame(scale(df))
write.csv(df_scaled, "data.csv", row.names = FALSE)

# Stepwise Regression
d <- read.csv("data.csv")
dim(d)
full_model <- lm(BODYFAT ~ ., data = d)
model1 <- stepAIC(full_model, direction = "both", trace = FALSE)
summary(model1)
adjusted_r_squared1 <- round(summary(model1)$adj.r.squared, 3)

# Simple Linear Regression
variables <- colnames(d)
variables <- variables[variables != "BODYFAT"]

max_adjusted_r_squared <- 0 
best_variable <- ""
for (var in variables) {
  formula <- as.formula(paste("BODYFAT ~", var))
  model <- lm(formula, data = d)
  adjusted_r_squared <- summary(model)$adj.r.squared
  if (adjusted_r_squared > max_adjusted_r_squared) {
    max_adjusted_r_squared <- adjusted_r_squared
    best_variable <- var
  }
}

best_formula <- as.formula(paste("BODYFAT ~", best_variable))
model2 <- lm(best_formula, data = d)
adjusted_r_squared2 <- round(max_adjusted_r_squared, 3)

# Two variable linear regression
max_adjusted_r_squared <- 0
best_variable_pair <- c("","")

for (i in 1:(length(variables) - 1)) {
  for (j in (i + 1):length(variables)) {
    var1 <- variables[i]
    var2 <- variables[j]
    formula <- as.formula(paste("BODYFAT ~", var1, "+", var2))
    model <- lm(formula, data = d)
    
    adjusted_r_squared <- summary(model)$adj.r.squared
    
    if (adjusted_r_squared > max_adjusted_r_squared) {
      max_adjusted_r_squared <- adjusted_r_squared
      best_variable_pair <- c(var1, var2)
    }
  }
}

best_formula <- as.formula(paste("BODYFAT ~", best_variable_pair[1], "+", best_variable_pair[2]))
model3 <- lm(best_formula, data = d)
adjusted_r_squared3 <- round(max_adjusted_r_squared, 3)

# Bivariate product term
max_adjusted_r_squared <- 0
best_variable_pair <- c("","")

for (i in 1:(length(variables) - 1)) {
  for (j in (i + 1):length(variables)) {
    var1 <- variables[i]
    var2 <- variables[j]
    d$interaction_term <- d[[var1]] * d[[var2]]
    formula <- as.formula("BODYFAT ~ interaction_term")
    model <- lm(formula, data = d)
    adjusted_r_squared <- summary(model)$adj.r.squared
    if (adjusted_r_squared > max_adjusted_r_squared) {
      max_adjusted_r_squared <- adjusted_r_squared
      best_variable_pair <- c(var1, var2)
    }
  }
}

d$best_interaction_term <- d[[best_variable_pair[1]]] * d[[best_variable_pair[2]]]
best_formula <- as.formula("BODYFAT ~ best_interaction_term")
model4 <- lm(best_formula, data = d)
adjusted_r_squared4 <- round(max_adjusted_r_squared, 3)

# ABDOMEN^2
d$ABDOMEN_squared <- d$ABDOMEN^2
model5 <- lm(BODYFAT ~ ABDOMEN_squared, data = d)
adjusted_r_squared5 <- round(summary(model5)$adj.r.squared, 3)

# ABDOMEN^2 + other 1-order
variables <- variables[!variables %in% c("BODYFAT", "ABDOMEN", "ABDOMEN_squared")]

max_adjusted_r_squared <- 0
best_variable <- ""

for (var in variables) {
  formula <- as.formula(paste("BODYFAT ~ ABDOMEN_squared +", var))
  model <- lm(formula, data = d)
  adjusted_r_squared <- summary(model)$adj.r.squared
  if (adjusted_r_squared > max_adjusted_r_squared) {
    max_adjusted_r_squared <- adjusted_r_squared
    best_variable <- var
  }
}

best_formula <- as.formula(paste("BODYFAT ~ ABDOMEN_squared +", best_variable))
model6 <- lm(best_formula, data = d)
adjusted_r_squared6 <- round(max_adjusted_r_squared, 3)

# ABDOMEN^2 + ABDOMEN + other 1-order
max_adjusted_r_squared <- 0
best_variable <- ""
for (var in variables) {
  
  formula <- as.formula(paste("BODYFAT ~ ABDOMEN + ABDOMEN_squared +", var))
  model <- lm(formula, data = d)
  adjusted_r_squared <- summary(model)$adj.r.squared
  if (adjusted_r_squared > max_adjusted_r_squared) {
    max_adjusted_r_squared <- adjusted_r_squared
    best_variable <- var
  }
}

best_formula <- as.formula(paste("BODYFAT ~ ABDOMEN + ABDOMEN_squared +", best_variable))
model7 <- lm(best_formula, data = d)
adjusted_r_squared7 <- round(max_adjusted_r_squared, 3)

# 10-fold CV
train_control <- trainControl(method = "cv", number = 10)
mse_results <- c()

for (i in 1:7) {
  model <- get(paste0("model", i))
  cv_model <- train(
    formula(model),
    data = d,  
    method = "lm",  
    trControl = train_control,  
    metric = "RMSE"  
  )
  mse_results <- c(mse_results, mean((cv_model$results$RMSE)^2))
}

# Print results
model_names <- c(
  "BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + THIGH + FOREARM + WRIST",  
  "BODYFAT ~ ABDOMEN ",  
  "BODYFAT ~ WEIGHT + ABDOMEN",  
  "BODYFAT ~ ABDOMEN * WEIGHT",  
  "BODYFAT ~ ABDOMEN_squared", 
  "BODYFAT ~ ABDOMEN_squared + WEIGHT",  
  "BODYFAT ~ ABDOMEN + ABDOMEN_squared + WEIGHT"  
)
mse_results <- round(mse_results, 3)

results_df <- data.frame(
  model = model_names,
  adjusted_R_squared = c(adjusted_r_squared1, adjusted_r_squared2, adjusted_r_squared3, adjusted_r_squared4, adjusted_r_squared5, adjusted_r_squared6, adjusted_r_squared7),
  MSE = mse_results
)

print(results_df)

write.csv(d, "data_new.csv", row.names = FALSE)
write.csv(results_df, "results.csv", row.names = FALSE)

# Trade off
dn <- read.csv("data_new.csv")
model_names <- c(
  "BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + THIGH + FOREARM + WRIST",  
  "BODYFAT ~ ABDOMEN ",  
  "BODYFAT ~ WEIGHT + ABDOMEN",  
  "BODYFAT ~ ABDOMEN * WEIGHT",  
  "BODYFAT ~ ABDOMEN_squared", 
  "BODYFAT ~ ABDOMEN_squared + WEIGHT",  
  "BODYFAT ~ ABDOMEN + ABDOMEN_squared + WEIGHT"  
)

#add noise N(0, sd(y)*0.1) for each column y
add_noise <- function(y, noise_level = 0.1) {
    noise_sd = sd(y)
    return(y + rnorm(length(y), mean = 0, sd = (noise_level* noise_sd)  ))
}
#metric = mse
metric <- function(predictions, actual) {
  return(mean((predictions - actual)^2))
}
#Robustness test
folds <- createFolds(dn$BODYFAT, k = 10, list = TRUE)
mse_list <- matrix(0, nrow = 10, ncol = 7)
mse_list_noisy <- matrix(0, nrow = 10, ncol = 7)
for(i in 1:10) {
  test_index <- folds[[i]]
  trainset <- dn[-test_index,]
  testset <- dn[test_index,]
  trainset_noisy <- as.data.frame(lapply(trainset,add_noise))
  for (j in 1:7) {
      model_formula <- model_names[j]
      model <- lm(as.formula(model_formula), data = trainset)
      predictions <- predict(model, newdata = testset)
      mse <- metric(predictions, testset$BODYFAT)
      mse_list[i,j]<-mse

      model_noisy <- lm(as.formula(model_formula), data = trainset_noisy)
      predictions_noisy <- predict(model_noisy, newdata = testset)
      mse_noisy <- metric(predictions_noisy, testset$BODYFAT)
      mse_list_noisy[i,j]<-mse_noisy
  }
}
average_mse <- colMeans(mse_list)
average_mse_noisy <- colMeans(mse_list_noisy)
retention_rate <- 1 - abs(average_mse_noisy - average_mse) / average_mse

results_df = read.csv("results.csv")
results_df$retention_MSE_rate = retention_rate
results_df$num_predictors = c(7, 1, 2, 1, 1, 2, 3)

# Model Diagnostics
