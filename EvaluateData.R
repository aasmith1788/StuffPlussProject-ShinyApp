# Load required libraries
library(xgboost)
library(dplyr)

# Load the saved model
model <- readRDS("best_xgboost_model.rds")

# Load the FinalPrediction data frame
FinalPrediction <- read.csv("FinalPrediction.csv")

# Calculate MLB stats from FinalPrediction
mlb_stats <- list(
  max_xEEV = max(FinalPrediction$xEEV),
  mean_abs_scaled_xEEV = mean(abs(FinalPrediction$xEEV - max(FinalPrediction$xEEV)))
)

# Function to preprocess input data
preprocess_data <- function(pitch_data) {
  required_cols <- c("release_speed", "release_extension", "release_pos_x", "release_pos_z",
                     "pfx_x", "pfx_z", "arm_slot", "pitch_type")
  
  if (!all(required_cols %in% colnames(pitch_data))) {
    stop("Input data is missing required columns")
  }
  
  pitch_data[, required_cols]
}

# Function to calculate Stuff+
calculate_stuff_plus <- function(xEEV, mlb_max_xEEV, mlb_mean_abs_scaled_xEEV) {
  xEEV_scaled_negative <- xEEV - mlb_max_xEEV
  abs_xEEV_scaled_neg <- abs(xEEV_scaled_negative)
  (abs_xEEV_scaled_neg / mlb_mean_abs_scaled_xEEV) * 100
}

# Function to evaluate a college pitcher
evaluate_college_pitcher <- function(model, pitch_data, mlb_stats) {
  # Preprocess the data
  X <- preprocess_data(pitch_data)
  
  # Separate features and pitch type
  features <- X[, c("release_speed", "release_extension", "release_pos_x", "release_pos_z",
                    "pfx_x", "pfx_z", "arm_slot")]
  
  # Make predictions
  xEEV_predictions <- predict(model, as.matrix(features))
  
  # Combine predictions with pitch type
  results <- data.frame(
    pitch_type = X$pitch_type,
    xEEV = xEEV_predictions
  )
  
  # Calculate Stuff+ for each pitch type
  results <- results %>%
    group_by(pitch_type) %>%
    summarise(
      xEEV = mean(xEEV),
      stuff_plus = calculate_stuff_plus(mean(xEEV), mlb_stats$max_xEEV, mlb_stats$mean_abs_scaled_xEEV),
      num_pitches = n(),
      .groups = 'drop'
    )
  
  results
}

# Example usage
main <- function() {
  # Example college pitcher data with multiple pitch types
  college_data <- data.frame(
    release_speed = rep(84.91, 40),
    release_extension = rep(6.23, 40),
    release_pos_x = rep(1.23, 40),
    release_pos_z = rep(6.25, 40),
    pfx_x = rep(1.32, 40),
    pfx_z = rep(1.04, 40),
    arm_slot = rep(45.51, 40),
    pitch_type = rep("Changeup", 40))
  
  
  # Evaluate the college pitcher
  result <- evaluate_college_pitcher(model, college_data, mlb_stats)
  
  # Print results
  cat("Stuff+ Results by Pitch Type:\n")
  print(result)
  
  # Add a note about sample size
  if (any(result$num_pitches < 50)) {
    cat("\nNote: Some pitch types have fewer than 50 pitches. Results based on small sample sizes may not be representative of the pitcher's true abilities.\n")
  }
}

# Run the main function
main()