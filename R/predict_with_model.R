#' Predict with a trained model
#'
#' This function takes a trained model and patient data as input and returns predictions.
#'
#' @param model A trained model object.
#' @param patient_data A data frame containing patient data.
#' @return A vector of predictions, chart (pr vs actual), RMSE, R-sq
#' @export


predict_with_model <- function(model, model_data) {
  # Check the format of patient data
  if (!is.data.frame(model_data)) {
    stop("Data must be a data frame.")
  }

  # Check if all required columns are present in patient data
  required_columns <- colnames(model$terms)[!grepl("(^Intercept|\\+)", colnames(model$terms))]
  missing_columns <- setdiff(required_columns, colnames(model_data))
  if (length(missing_columns) > 0) {
    stop(paste("Data is missing the following required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Make predictions using the model
  prediction <- predict(model, newdata = model_data)

  # Calculate model metrics
  residuals <- resid(model)
  rmse <- sqrt(mean(residuals^2))  # Root Mean Squared Error (RMSE)
  r_squared <- summary(model)$r.squared  # R-squared

  cat("Root Mean Squared Error (RMSE):", rmse, "\n")
  cat("R-squared:", r_squared, "\n")

  # Visualize the results
  plot(prediction, col = "blue", pch = 19, xlab = "Observation Number", ylab = "Predicted Value", main = "Predicted vs. Actual Values")
  points(model_data$actual, col = "red", pch = 19)
  legend("topleft", legend = c("Predicted Values", "Actual Values"), col = c("blue", "red"), pch = 19)

  # Return the predicted values
  return(prediction)
}

