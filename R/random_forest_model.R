#' @title Random Forest Model Fitting
#' @description Fits a random forest model to the data, useful for both regression and classification.
#' Random forest is an ensemble learning method that operates by constructing multiple decision trees
#' at training time and outputting the class that is the mode of the classes (classification) or
#' mean prediction (regression) of the individual trees.
#'
#' @param y Response vector with continuous or binary outcomes.
#' @param X Data frame of predictors, either numeric or factor types.
#' @param model_type A character string specifying the type of model: 'gaussian' for regression or 'binomial' for classification.
#' @return A list containing the random forest model object with model details
#' and fitted values (continuous values for gaussian and probability values for binomial)
#' @importFrom randomForest randomForest
#' @export
#' @examples
#' # Example for regression
#' data(mtcars)
#' result_rf_gaussian <- fit_random_forest_model(mtcars$mpg, mtcars[, -1], model_type = 'gaussian')
#' print(result_rf_gaussian)
#'
#' # Example for classification
#' data(iris)
#' X <- iris[iris$Species != "setosa", c("Sepal.Length", "Sepal.Width")]
#' y <- as.numeric(iris$Species[iris$Species != "setosa"] == "versicolor")
#' result_rf_binomial <- fit_random_forest_model(y, X, model_type = 'binomial')
#' print(result_rf_binomial)
fit_random_forest_model <- function(y, X, model_type = 'gaussian') {

  validate_inputs(y, X)

  # Ensure the randomForest package is loaded
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Package 'randomForest' needs to be installed.")
  }

  # Validate model type
  if (!model_type %in% c('gaussian', 'binomial')) {
    stop("model_type must be either 'gaussian' for regression or 'binomial' for classification")
  }

  # Convert y to factor if model_type is 'binomial' for classification
  if (model_type == 'binomial') {
    y <- factor(y)
  }

  # Fit the random forest model
  if (model_type == 'gaussian') {
    rf_model <- randomForest(x = X, y = y, type = "regression")
  } else {
    rf_model <- randomForest(x = X, y = y, type = "classification")
  }

  # Extract predictions
  if (model_type == 'gaussian') {
    predictions <- predict(rf_model, newdata = X)  # Continuous values for regression
  } else {
    predictions <- predict(rf_model, newdata = X, type = "prob")[,2]
  }

  # Compile model details
  model_details <- list(
    model_type = "random_forest",
    model = rf_model,
    predictions = predictions,
    names = colnames(X)  # Ensure names are taken from X without temporary 'y'
  )

  return(model_details)
}




