#' @title Ensemble Model Fitting Using Standardized Prediction
#' @description This function fits multiple models to the same dataset and combines their predictions using a
#' standardized prediction method. Supports various regression model types.
#' @param data Data frame containing the training dataset including the response variable.
#' @param response_var Name of the response variable in the data frame.
#' @param model_list List of model function names as character strings. Each model is assumed to return an object
#' compatible with the predict_model function.
#' @return A list containing combined predictions and details for each fitted model.
#' @import stats
#' @import randomForest
#' @importFrom randomForest randomForest predict glmnet glmnet cv.glmnet
#' @examples
#' \dontrun{
#' data(mtcars)
#' model_functions <- c("fit_ridge_model", "fit_logistic_model", "fit_random_forest_model")
#' result <- ensemble_model_fitting(mtcars, "mpg", model_functions)
#' print(result$combined_predictions)
#' }
#' @export
ensemble_model_fitting <- function(data, response_var, model_list) {
  predictions_list <- list()
  model_details_list <- list()

  for (model_function in model_list) {
    # Extract the response and predictors
    y <- data[[response_var]]
    X <- data[, setdiff(names(data), response_var), drop = FALSE]

    # Dynamically call the model fitting function
    model_fit <- do.call(model_function, list(y = y, X = X))

    # Use the general prediction function to get predictions for the training data
    predictions <- predict_model(model_fit, newdata = X, type = "response")

    # Store predictions and model details
    predictions_list[[length(predictions_list) + 1]] <- predictions
    model_details_list[[length(model_details_list) + 1]] <- model_fit
  }

  # Combine predictions - average for regression
  combined_predictions <- Reduce("+", predictions_list) / length(predictions_list)

  return(list(
    combined_predictions = combined_predictions,
    model_details = model_details_list
  ))
}
