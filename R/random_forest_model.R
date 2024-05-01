#' @title Random Forest Model Fitting
#' @description Fits a random forest model to the data, useful for both regression and classification.
#' Random forest is an ensemble learning method that operates by constructing multiple decision trees
#' at training time and outputting the class that is the mode of the classes (classification) or
#' mean prediction (regression) of the individual trees.
#'
#' @param y Response vector with continuous or binary outcomes.
#' @param X Data frame of predictors, either numeric or factor types.
#' @param model_type A character string specifying the type of model: 'gaussian' for regression or 'binomial' for classification.
#' @return A list containing the random forest model object with model details and fitted values.
#' @importFrom randomForest randomForest
#' @export
#' @examples
#' data(mtcars)
#' result_rf <- fit_random_forest_model(mtcars$mpg, mtcars[, -1], model_type = 'gaussian')
fit_random_forest_model <- function(y, X, model_type = 'gaussian') {
  # Ensure the randomForest package is loaded
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Package 'randomForest' needs to be installed.")
  }

  # Check if model_type is correctly specified
  if (!model_type %in% c('gaussian', 'binomial')) {
    stop("model_type must be either 'gaussian' for regression or 'binomial' for classification")
  }

  # Convert y to factor if model_type is 'binomial' for classification
  if (model_type == 'binomial') {
    y <- factor(y)
  }

  # Add y as a temporary column in X for the formula interface
  X$y <- y
  formula <- as.formula("y ~ .")

  # Determine the appropriate type for the randomForest model
  type <- if (model_type == 'gaussian') "regression" else "classification"

  # Fit the model using the formula interface
  rf_model <- randomForest::randomForest(formula, data = X, type = type)

  # Remove the temporary column
  X$y <- NULL

  # Store predictions for the training set
  predictions <- predict(rf_model, X)

  # Compile model details
  model_details <- list(
    model_type = "random_forest",
    model = rf_model,
    predictions = predictions
  )

  return(model_details)
}
