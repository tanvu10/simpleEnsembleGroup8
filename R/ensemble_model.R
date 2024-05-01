#' @title Ensemble Model Fitting Using Standardized Prediction
#' @description Fits multiple models specified by the model list to the same dataset and combines their predictions using a
#' standardized prediction method. Supports regression (gaussian) and classification (binomial) types.
#' @param X Data frame or matrix of predictors.
#' @param y Vector of response variables.
#' @param model_type Character specifying whether to fit models for 'gaussian' (regression) or 'binomial' (classification).
#' @param model_list List of model fitting functions expressed as character strings.
#' @param threshold Threshold for converting probabilities to class labels in case of 'binomial' model type, default is 0.5.
#' @return A list containing combined predictions and details for each fitted model including individual model predictions.
#' @export
#' @examples
#' \dontrun{
#'   # Assuming `fit_linear_model` and `fit_logistic_model` are defined elsewhere in your package.
#'   data(mtcars)
#'   X <- mtcars[, -which(names(mtcars) == "mpg")]
#'   y <- mtcars$mpg  # For gaussian; use a binary response for 'binomial'
#'   model_list <- c("fit_linear_model", "fit_lasso_model")
#'   results <- ensemble_model_fitting(X, y, model_type = 'gaussian', model_list = model_list)
#'   print(results$combined_predictions)
#'   # To view individual model details and predictions:
#'   print(results$model_details)
#'   print(results$individual_predictions)
#' }
ensemble_model_fitting <- function(X, y, model_type, model_list, threshold = 0.5) {
  predictions_list <- list()
  model_details_list <- list()

  if (!model_type %in% c('gaussian', 'binomial')) {
    stop("Unsupported model type provided: ", model_type)
  }

  for (model_name in model_list) {
    print(model_name)
    model_function <- match.fun(model_name)
    model_fit <- model_function(y = y, X = X, model_type = model_type)
    pred_type <- if (model_type == 'binomial') "probabilities" else "response"
    predictions <- predict_model(model_fit, newdata = X, type = pred_type)
    predictions_list[[length(predictions_list) + 1]] <- predictions
    model_details_list[[length(model_details_list) + 1]] <- model_fit

  }

  combined_predictions <- if (model_type == 'gaussian') {
    Reduce("+", predictions_list) / length(predictions_list)
  } else {
    average_probabilities <- Reduce("+", predictions_list) / length(predictions_list)
    final_classes <- ifelse(average_probabilities > threshold, 1, 0)
    list(probabilities = average_probabilities, final_classes = final_classes)
  }

  return(list(
    combined_predictions = combined_predictions,
    individual_predictions = predictions_list,
    model_details = model_details_list
  ))
}
