#' @title Fit Ridge Regression Model with Optional Bagging and Intercept
#' @description Performs ridge regression using glmnet, optimizing the balance between model complexity
#' and prediction accuracy through L2 regularization. This method is ideal for handling multicollinearity,
#' reducing overfitting, and shrinking coefficient values, particularly in situations where the number of
#' predictors exceeds the number of observations. Optionally, bagging can be incorporated to further
#' enhance model stability and prediction accuracy by aggregating results from multiple bootstrap samples.
#'
#' @param y Response vector with continuous or binary outcomes.
#' @param X Matrix of predictors, either numeric or factor types.
#' @param lambda Regularization penalty parameter for ridge regression; if NULL, it is determined via
#' cross-validation.
#' @param add_intercept Logical indicating whether to include an intercept in the model.
#' @param model_type A character string specifying whether to use 'gaussian' for regression or 'binomial' for classification.
#' @param bagging Logical indicating whether to perform bagging.
#' @param R Integer specifying the number of bootstrap samples to use if bagging is enabled.
#' @return A list containing the ridge regression model details, or if bagging is enabled, an aggregated
#' result from multiple bootstrap samples including the model, lambda value, coefficients,
#' and fitted values (continuous values for gaussian and probability values for binomial)
#' @importFrom glmnet glmnet cv.glmnet
#' @export
#' @examples
#' # Example for Gaussian model
#' data(mtcars)
#' X <- mtcars[, c("hp", "wt")]
#' y <- mtcars$mpg
#' result_ridge_gaussian <- fit_ridge_model(y, X, model_type = "gaussian", add_intercept = TRUE, bagging = FALSE)
#' print(result_ridge_gaussian)
#'
#' # Example for Binomial model
#' data(mtcars)
#' X <- mtcars[, c("hp", "wt")]
#' y <- ifelse(mtcars$am == 1, 1, 0)  # Converting 'am' to a binary outcome
#' result_ridge_binomial <- fit_ridge_model(y, X, model_type = "binomial", add_intercept = TRUE, bagging = TRUE, R = 50)
#' print(result_ridge_binomial)
fit_ridge_model <- function(y, X, lambda = NULL, model_type = "gaussian", add_intercept = TRUE, bagging = FALSE, R = 100) {
  validate_inputs(y, X)

  # family <- if (all(y %in% c(0, 1))) "binomial" else "gaussian"

  if (!model_type %in% c("binomial", "gaussian")) {
    stop("'family' must be either 'binomial' or 'gaussian'")
  }

  if (is.null(lambda)) {
    cv_fit <- cv.glmnet(x = as.matrix(X), y = y, alpha = 0, family = model_type, intercept = add_intercept)
    lambda <- cv_fit$lambda.min
  }

  model_details <- if (bagging) {
    perform_bagging(y, X, function(y_sample, X_sample) {
      glmnet_model <- glmnet(x = as.matrix(X_sample), y = y_sample, alpha = 0, lambda = lambda, family = model_type, intercept = add_intercept)
      list(coefficients = coef(glmnet_model), fitted_values = predict(glmnet_model, newx = as.matrix(X_sample), type = "response"))
    }, R)
  } else {
    glmnet_model <- glmnet(x = as.matrix(X), y = y, alpha = 0, lambda = lambda, family = model_type, intercept = add_intercept)
    coefficients <- coef(glmnet_model, s = "lambda.min")  # Include intercept if add_intercept is TRUE
    fitted_values <- predict(glmnet_model, newx = as.matrix(X), type = "response", s = "lambda.min")
    list(model = glmnet_model, lambda = lambda, coefficients = coefficients, fitted_values = fitted_values)
    }

  model_details$model_type <- model_type  # Identifying the model type for prediction function


  # Assign coefficient names including intercept if applicable
  if (add_intercept) {
    model_details$names <- c("Intercept", colnames(X))
  } else {
    model_details$names <- colnames(X)
  }


  return(model_details)
}

