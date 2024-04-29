#' @title Random Forest Model Fitting
#' @description Fits a random forest model to the data, useful for both regression and classification.
#' Random forest is an ensemble learning method that operates by constructing multiple decision trees
#' at training time and outputting the class that is the mode of the classes (classification) or
#' mean prediction (regression) of the individual trees.
#'
#' @param y Response vector with continuous or binary outcomes.
#' @param X Data frame of predictors, either numeric or factor types.
#' @param add_intercept Logical indicating whether to include an intercept in the model.
#'                      Random Forest does not use an intercept; this parameter is ignored.
#' @return A list containing the random forest model object with model details and fitted values.
#' @importFrom randomForest randomForest
#' @export
#' @examples
#' data(mtcars)
#' result_rf <- fit_random_forest_model(mtcars$mpg, mtcars[, -1], add_intercept = FALSE)
fit_random_forest_model <- function(y, X, add_intercept = FALSE) {
  # Ensure the randomForest package is loaded
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Package 'randomForest' needs to be installed.")
  }

  # Random forest does not use an intercept. The parameter is thus ignored.
  # Add y as a temporary column in X for the formula interface
  X$y <- y
  formula <- as.formula("y ~ .")

  # Fit the model using the formula interface
  rf_model <- randomForest::randomForest(formula, data = X)

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
