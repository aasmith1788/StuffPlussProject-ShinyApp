# Install and load required packages
packages <- c("xgboost", "ggplot2", "gridGraphics", "vip", "kableExtra", "dplyr", "knitr", "caret")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

lapply(packages, library, character.only = TRUE)

# Set seed for reproducibility
set.seed(1234)

# Load the datasets

StuffPlusTrain <- read.csv("C:/Users/aasmi/OneDrive/StuffPlusModel/StuffPlusTrain.csv")
StuffPlusTest <- read.csv("C:/Users/aasmi/OneDrive/StuffPlusModel/StuffPlusTest.csv")
LinearWeights <- read.csv("C:/Users/aasmi/OneDrive/StuffPlusModel/Linear Weights.csv")

LinearWeights <- LinearWeights[c(1, 2)]
LinearWeights <- LinearWeights[-c(25, 26, 27),]

combined_data <- rbind(StuffPlusTest)

# Clean combined_data
combined_data$events[combined_data$events == "" | combined_data$events == " "] <- NA
combined_data_new <- combined_data
combined_data_new$events[is.na(combined_data_new$events)] <- combined_data_new$description[is.na(combined_data_new$events)]
combined_data_new <- combined_data_new %>%
  mutate(events = if_else(events == "strikeout", description, events))
combined_data_new <- left_join(LinearWeights, combined_data_new, by = c("Event" = "events"))

combined_data_new <- combined_data_new %>%
  filter(description %in% c("swinging_strike", "swinging_strike_blocked", "foul", "hit_into_play")) %>%
  mutate(
    launch_speed = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked"), 0, launch_speed),
    launch_angle = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked"), 0, launch_angle)
  )

combined_data_new <- combined_data_new %>%
  mutate(release_magnitude = sqrt(release_pos_x^2 + release_extension^2 + release_pos_z^2))

combined_data_new <- combined_data_new %>%
  mutate(cos_arm_angle = release_pos_z / release_magnitude)

combined_data_new <- combined_data_new %>%
  mutate(arm_angle_rad = acos(cos_arm_angle),
         arm_slot = arm_angle_rad * (180 / pi))

combined_data_new <- combined_data_new %>%
  mutate(
    EEV = launch_speed - ifelse(
      launch_angle < 28,
      (28 - launch_angle),
      3 * (launch_angle - 28)
    )
  )

combined_data_new <- combined_data_new[!is.na(combined_data_new$EEV), ]

# Clean 2023 Data
StuffPlusTest$events[StuffPlusTest$events == "" | StuffPlusTest$events == " "] <- NA
StuffPlusTest_new <- StuffPlusTest
StuffPlusTest_new$events[is.na(StuffPlusTest_new$events)] <- StuffPlusTest_new$description[is.na(StuffPlusTest_new$events)]
StuffPlusTest_new <- StuffPlusTest_new %>%
  mutate(events = if_else(events == "strikeout", description, events))
StuffPlusTest_new <- left_join(LinearWeights, StuffPlusTest_new, by = c("Event" = "events"))

StuffPlusTest_new <- StuffPlusTest_new %>%
  filter(description %in% c("swinging_strike", "swinging_strike_blocked", "foul", "hit_into_play")) %>%
  mutate(
    launch_speed = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked"), 0, launch_speed),
    launch_angle = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked"), 0, launch_angle)
  )

StuffPlusTest_new <- StuffPlusTest_new %>%
  mutate(release_magnitude = sqrt(release_pos_x^2 + release_extension^2 + release_pos_z^2))

StuffPlusTest_new <- StuffPlusTest_new %>%
  mutate(cos_arm_angle = release_pos_z / release_magnitude)

StuffPlusTest_new <- StuffPlusTest_new %>%
  mutate(arm_angle_rad = acos(cos_arm_angle),
         arm_slot = arm_angle_rad * (180 / pi))

StuffPlusTest_new <- StuffPlusTest_new %>%
  mutate(
    EEV = launch_speed - ifelse(
      launch_angle < 28,
      (28 - launch_angle),
      3 * (launch_angle - 28)
    )
  )

StuffPlusTest_new <- StuffPlusTest_new[!is.na(StuffPlusTest_new$EEV), ]

# Prepare the training data
training <- combined_data_new 
training <- subset(training, select = c(Linear_weight, release_speed, pfx_x, pfx_z, release_spin_rate,
                                        release_extension, release_pos_z, release_pos_x, player_name,
                                        pitcher, pitch_type, arm_slot, spin_axis, EEV))
training <- na.omit(training)

x <- training[, c("release_speed", "release_extension", "release_pos_x", "release_pos_z",
                  "pfx_x", "pfx_z", "arm_slot")]
y <- training$EEV

# Prepare the test data
testing <- StuffPlusTest_new 
testing <- subset(testing, select = c(Linear_weight, release_speed, pfx_x, pfx_z, release_spin_rate,
                                      release_extension, release_pos_z, release_pos_x, player_name,
                                      pitcher, pitch_type, arm_slot, spin_axis, EEV))
testing <- na.omit(testing)

x_test <- as.matrix(testing[, c("release_speed", "release_extension", "release_pos_x", "release_pos_z",
                                "pfx_x", "pfx_z", "arm_slot")])
y_test <- testing$EEV

# Define the parameter space for random search
param_space <- list(
  ntree = 100:500,
  depth = 3:10,
  alpha = c(0, 0.01, 0.1, 0.5, 1),
  eta = c(0.01, 0.05, 0.1, 0.3),
  subsample = seq(0.5, 1, by = 0.1),
  colsample_bytree = seq(0.5, 1, by = 0.1)
)

# Number of random combinations to try
n_combinations <- 100

# Function to sample random parameters
sample_params <- function() {
  list(
    ntree = sample(param_space$ntree, 1),
    depth = sample(param_space$depth, 1),
    alpha = sample(param_space$alpha, 1),
    eta = sample(param_space$eta, 1),
    subsample = sample(param_space$subsample, 1),
    colsample_bytree = sample(param_space$colsample_bytree, 1)
  )
}

# Variables to store the best parameters and correlation
best_params <- NULL
best_correlation <- -Inf

# Random search
for (i in 1:n_combinations) {
  # Sample random parameters
  current_params <- sample_params()
  
  # Train XGBoost model with current parameter combination
  model_xgb <- xgboost(data = as.matrix(x), label = y,
                       nrounds = current_params$ntree,
                       max_depth = current_params$depth,
                       eta = current_params$eta,
                       alpha = current_params$alpha,
                       subsample = current_params$subsample,
                       colsample_bytree = current_params$colsample_bytree,
                       objective = "reg:squarederror",
                       verbose = 0)
  
  # Predict on test data
  newdata <- xgb.DMatrix(data = x_test)
  predictions <- predict(model_xgb, newdata)
  
  # Combine predictions with test data
  combined <- cbind(as.data.frame(predictions), testing)
  
  # Summarise the predictions by pitcher and pitch type
  PredictionFinal <- combined %>%
    group_by(pitcher, player_name, pitch_type) %>%
    summarise(
      xEEV = mean(predictions, na.rm = TRUE),  # Changed from sum to mean
      EEV = mean(EEV, na.rm = TRUE),           # Changed from sum to mean
      n = n(),
      .groups = "drop"
    )
  
  # The correlation calculation remains the same, but now uses mean values
  correlation <- cor(PredictionFinal$xEEV, PredictionFinal$EEV)
  
  # Update best parameters if current combination gives better correlation
  if (correlation > best_correlation) {
    best_correlation <- correlation
    best_params <- current_params
    
    cat("New best correlation:", best_correlation, "at iteration", i, "\n")
    print(best_params)
  }
}

# Print final best parameters & correlation
cat("\nFinal Best Parameters:\n")
print(best_params)
cat("Best correlation:", best_correlation, "\n")

# Train the final model with best parameters
final_model <- xgboost(data = as.matrix(x), label = y,
                       nrounds = best_params$ntree,
                       max_depth = best_params$depth,
                       eta = best_params$eta,
                       alpha = best_params$alpha,
                       subsample = best_params$subsample,
                       colsample_bytree = best_params$colsample_bytree,
                       objective = "reg:squarederror",
                       verbose = 0)
final_predictions <- predict(final_model, newdata)
# Final aggregation and correlation calculation
final_combined <- cbind(as.data.frame(final_predictions), testing)
FinalPrediction <- final_combined %>%
  group_by(pitcher, player_name, pitch_type) %>%
  summarise(
    xEEV = mean(final_predictions, na.rm = TRUE),
    EEV = mean(EEV, na.rm = TRUE),
    n = n(),
    'xEEV/100' = xEEV,  # This is now directly the mean xEEV
    Velocity = mean(release_speed, na.rm = TRUE),
    Horizontal_Break = mean(pfx_x, na.rm = TRUE),
    Vertical_Break = mean(pfx_z, na.rm = TRUE),
    Spin_Rate = mean(release_spin_rate, na.rm = TRUE),
    Extension = mean(release_extension, na.rm = TRUE),
    Vertical_Release = mean(release_pos_z, na.rm = TRUE),
    Horizontal_Release = mean(release_pos_x, na.rm = TRUE),
    Pitches = n(),
    RV = mean(Linear_weight, na.rm = TRUE),
    .groups = "drop"
  )

final_correlation <- cor(FinalPrediction$xEEV, FinalPrediction$EEV)

cat("Final correlation after aggregation:", final_correlation, "\n")

# Save the final model
saveRDS(final_model, "best_xgboost_model.rds")

# Overfitting Checks

# 1. Train-Test Split Performance Comparison
train_predictions <- predict(final_model, as.matrix(x))
test_predictions <- predict(final_model, newdata)

train_mse <- mean((y - train_predictions)^2)
test_mse <- mean((y_test - test_predictions)^2)

cat("Train MSE:", train_mse, "\n")
cat("Test MSE:", test_mse, "\n")

# 2. Cross-Validation
cv_results <- xgb.cv(
  data = as.matrix(x),
  label = y,
  nrounds = best_params$ntree,
  max_depth = best_params$depth,
  eta = best_params$eta,
  alpha = best_params$alpha,
  subsample = best_params$subsample,
  colsample_bytree = best_params$colsample_bytree,
  objective = "reg:squarederror",
  nfold = 5,
  metrics = "rmse",
  early_stopping_rounds = 10
)

print(cv_results$evaluation_log)

# 3. Learning Curves
train_sizes <- seq(0.1, 1, by = 0.1)
train_scores <- numeric(length(train_sizes))
test_scores <- numeric(length(train_sizes))

for (i in seq_along(train_sizes)) {
  sample_size <- floor(train_sizes[i] * nrow(x))
  indices <- sample(1:nrow(x), sample_size)
  
  x_sample <- x[indices, ]
  y_sample <- y[indices]
  
  model <- xgboost(data = as.matrix(x_sample), label = y_sample,
                   nrounds = best_params$ntree,
                   max_depth = best_params$depth,
                   eta = best_params$eta,
                   alpha = best_params$alpha,
                   subsample = best_params$subsample,
                   colsample_bytree = best_params$colsample_bytree,
                   objective = "reg:squarederror",
                   verbose = 0)
  
  train_pred <- predict(model, as.matrix(x_sample))
  test_pred <- predict(model, newdata)
  
  train_scores[i] <- sqrt(mean((y_sample - train_pred)^2))
  test_scores[i] <- sqrt(mean((y_test - test_pred)^2))
}

png("learning_curves.png", width = 800, height = 600)
plot(train_sizes, train_scores, type = "l", col = "blue", ylim = range(c(train_scores, test_scores)),
     xlab = "Training Set Size", ylab = "RMSE", main = "Learning Curves")
lines(train_sizes, test_scores, col = "red")
legend("topright", legend = c("Train", "Test"), col = c("blue", "red"), lty = 1)
dev.off()

# 4. Feature Importance
importance_matrix <- xgb.importance(model = final_model)
print(importance_matrix)

png("feature_importance.png", width = 800, height = 600)
xgb.plot.importance(importance_matrix)
dev.off()

# Continuation of the Additional check: Regularization
high_reg_model <- xgboost(data = as.matrix(x), label = y,
                          nrounds = best_params$ntree,
                          max_depth = best_params$depth,
                          eta = best_params$eta,
                          alpha = best_params$alpha * 2,  # Double the regularization
                          subsample = best_params$subsample,
                          colsample_bytree = best_params$colsample_bytree,
                          objective = "reg:squarederror",
                          verbose = 0)

high_reg_predictions <- predict(high_reg_model, newdata)
high_reg_mse <- mean((y_test - high_reg_predictions)^2)

cat("Original Test MSE:", test_mse, "\n")
cat("High Regularization Test MSE:", high_reg_mse, "\n")

# Scale xEEV to Stuff+ Scale
FinalPrediction$xEEVScaledNegative <- FinalPrediction$xEEV - max(FinalPrediction$xEEV)
FinalPrediction$ABSxEEVScaledNeg <- abs(FinalPrediction$xEEVScaledNegative)
FinalPrediction$`Stuff+` <- (FinalPrediction$ABSxEEVScaledNeg / mean(FinalPrediction$ABSxEEVScaledNeg)) * 100

# Evaluate model performance at different pitch count thresholds

# Define the pitch count thresholds to evaluate
pitch_count_thresholds <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)

# Define the pitch count ranges to evaluate
pitch_count_ranges <- list(
  "0-15" = c(0, 15),
  "15-30" = c(15, 30),
  "30-45" = c(30, 45),
  "45-60" = c(45, 60)
)

# Create lists to store results
threshold_results <- list()
range_results <- list()

# Initialize variables to store the best correlation and the corresponding threshold
best_correlation <- -Inf
best_threshold <- NA

# Loop through each threshold
for (threshold in pitch_count_thresholds) {
  
  # Filter the data for the current pitch count threshold
  FinalPrediction_filtered <- FinalPrediction %>%
    filter(Pitches >= threshold)
  
  # Calculate correlation between xEEV and EEV for the filtered data
  correlation <- cor(FinalPrediction_filtered$xEEV, FinalPrediction_filtered$EEV)
  
  # Store the results
  threshold_results[[as.character(threshold)]] <- list(
    "Pitch Count Threshold" = threshold,
    "Correlation" = correlation,
    "Number of Pitcher-Pitch Type Combinations" = nrow(FinalPrediction_filtered)
  )
  
  # Update the best correlation and threshold if the current one is better
  if (correlation > best_correlation) {
    best_correlation <- correlation
    best_threshold <- threshold
  }
  
  # Print results for each threshold
  cat("Pitch Count Threshold:", threshold, 
      " | Correlation:", correlation, 
      " | Number of Pitcher-Pitch Type Combinations:", nrow(FinalPrediction_filtered), "\n")
}

# Loop through each pitch count range
for (range_name in names(pitch_count_ranges)) {
  range <- pitch_count_ranges[[range_name]]
  
  # Filter the data for the current pitch count range
  FinalPrediction_filtered <- FinalPrediction %>%
    filter(Pitches >= range[1] & Pitches < range[2])
  
  # Calculate correlation between xEEV and EEV for the filtered data
  correlation <- cor(FinalPrediction_filtered$xEEV, FinalPrediction_filtered$EEV)
  
  # Store the results
  range_results[[range_name]] <- list(
    "Pitch Count Range" = range_name,
    "Correlation" = correlation,
    "Number of Pitcher-Pitch Type Combinations" = nrow(FinalPrediction_filtered)
  )
  
  # Print results for each range
  cat("Pitch Count Range:", range_name, 
      " | Correlation:", correlation, 
      " | Number of Pitcher-Pitch Type Combinations:", nrow(FinalPrediction_filtered), "\n")
}

# Convert the results lists to data frames for easy viewing
threshold_results_df <- do.call(rbind, lapply(threshold_results, as.data.frame))
range_results_df <- do.call(rbind, lapply(range_results, as.data.frame))

# Print the summary of the threshold and range results
cat("\n--- Threshold-Based Results ---\n")
print(threshold_results_df)

cat("\n--- Range-Based Results ---\n")
print(range_results_df)

# Print the best threshold and its correlation
cat("Best Threshold:", best_threshold, 
    " | Best Correlation:", best_correlation, "\n")



yearly_stats <- StuffPlusTest_new %>%
  group_by(player_name, pitch_type) %>%
  summarize(
    Pitch_Count = n(),
    Velocity = mean(release_speed, na.rm = TRUE),
    Horizantle_Movement = mean(pfx_x, na.rm = TRUE),
    Vertical_Movement = mean(pfx_z, na.rm = TRUE),
    Horizantle_Release = mean(release_pos_x, na.rm = TRUE),
    Verticle_Release = mean(release_pos_z, na.rm = TRUE),
    Extension = mean(release_extension, na.rm = TRUE),
    Spin_Rate = mean(release_spin_rate, na.rm = TRUE),
    Arm_Slot = mean(arm_slot, na.rm = TRUE),
    Exit_Velo = mean(launch_speed, na.rm = TRUE),
    Whiff_Rate = sum(description %in% c("swinging_strike", "swinging_strike_blocked"), na.rm = TRUE) / 
      sum(description %in% c("swinging_strike", "swinging_strike_blocked", "foul", "hit_into_play"), na.rm = TRUE),
    
    # Batting Average calculation
    Batting_Avg = sum(Event %in% c("single", "double", "triple", "home_run"), na.rm = TRUE) / n(),
    
    # Slugging Percentage calculation
    Slugging_Pct = sum(case_when(
      Event == "single" ~ 1,
      Event == "double" ~ 2,
      Event == "triple" ~ 3,
      Event == "home_run" ~ 4,
      TRUE ~ 0
    ), na.rm = TRUE) / n(),
    
    .groups = "drop"
  )

EnhancedPrediction <- FinalPrediction %>%
  select(player_name, pitch_type, `Stuff+`) %>%
  left_join(yearly_stats, by = c("player_name", "pitch_type"))

# Save EnhancedPrediction to CSV in the working directory
write.csv(EnhancedPrediction, file = "EnhancedPrediction.csv", row.names = FALSE)

# Print summary of overfitting checks
cat("\nOverfitting Check Summary:\n")
cat("1. Train-Test Split Comparison:\n")
cat("   Train MSE:", train_mse, "\n")
cat("   Test MSE:", test_mse, "\n")
cat("   Ratio (Test/Train):", test_mse/train_mse, "\n")
cat("2. Cross-Validation: Check the evaluation_log printed above.\n")
cat("3. Learning Curves: Check the 'learning_curves.png' file.\n")
cat("4. Feature Importance: Check the 'feature_importance.png' file and importance matrix printed above.\n")
cat("5. Regularization Check:\n")
cat("   Original Test MSE:", test_mse, "\n")
cat("   High Regularization Test MSE:", high_reg_mse, "\n")


# Install and load required packages
if (!require(gridExtra)) install.packages("gridExtra")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(scales)) install.packages("scales")
if (!require(ggthemes)) install.packages("ggthemes")

library(dplyr)
library(ggplot2)
library(gridExtra)
library(scales)
library(ggthemes)

# Filter the data for pitch counts of 50 or more
FilteredPrediction <- EnhancedPrediction %>%
  filter(Pitch_Count >= best_threshold)

# Improved scatter plot function with enhanced Tufte style
improved_plot_scatter <- function(df, x_var, y_var, title) {
  x_range <- range(df[[x_var]], na.rm = TRUE)
  y_range <- range(df[[y_var]], na.rm = TRUE)
  
  correlation <- round(cor(df[[x_var]], df[[y_var]], use = "complete.obs"), 3)
  r_squared <- round(correlation^2, 3)
  
  ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(shape = 16, size = 1.5, color = "black") +
    geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.5, linetype = "solid") +
    labs(title = title, 
         x = x_var, 
         y = y_var,
         caption = paste("Minimum 50 pitches | Correlation:", correlation, "| R-squared:", r_squared)) +
    theme_tufte() +
    geom_rangeframe() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      plot.caption = element_text(size = 12, hjust = 0.5, margin = margin(t = 15, b = 5))  # Increased size and centered
    ) +
    scale_x_continuous(limits = x_range, expand = c(0, 0), labels = comma) +
    scale_y_continuous(limits = y_range, expand = c(0, 0), labels = comma)
}

# Create improved plots
plot1 <- improved_plot_scatter(FilteredPrediction, "Stuff+", "Batting_Avg", "Stuff+ vs Batting Average")
plot2 <- improved_plot_scatter(FilteredPrediction, "Stuff+", "Slugging_Pct", "Stuff+ vs Slugging Percentage")
plot3 <- improved_plot_scatter(FilteredPrediction, "Stuff+", "Exit_Velo", "Stuff+ vs Exit Velocity")
plot4 <- improved_plot_scatter(FilteredPrediction, "Stuff+", "Whiff_Rate", "Stuff+ vs Whiff Rate")

# Display each plot individually
print(plot1)
print(plot2)
print(plot3)
print(plot4)

# Save each plot separately
ggsave("Stuff+_vs_Batting_Avg_50plus_EnhancedTufte.png", plot1, width = 8, height = 6, dpi = 300)
ggsave("Stuff+_vs_Slugging_Pct_50plus_EnhancedTufte.png", plot2, width = 8, height = 6, dpi = 300)
ggsave("Stuff+_vs_Exit_Velo_50plus_EnhancedTufte.png", plot3, width = 8, height = 6, dpi = 300)
ggsave("Stuff+_vs_Whiff_Rate_50plus_EnhancedTufte.png", plot4, width = 8, height = 6, dpi = 300)

cat("\nEnhanced Tufte-style correlation plots with Correlation and R-squared (for pitches with 50+ count) have been displayed and saved as separate PNG files in your working directory.")
