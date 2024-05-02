#' @title Lasso Regression Model Fitting with Optional Bagging
#' @description Performs lasso regression using glmnet, optimizing variable selection and shrinkage of coefficients
#' through L1 regularization. This approach is particularly useful in models with high dimensionality or
#' multicollinearity among predictors. The function allows specifying whether to include an intercept and
#' can automatically determine the optimal regularization penalty (lambda) via cross-validation. Optionally,
#' bagging can be applied to improve model accuracy and robustness against overfitting.
#'
#' @param y Response vector with continuous or binary outcomes.
#' @param X Matrix of predictors, either numeric or factor types.
#' @param lambda Regularization penalty parameter for lasso regression; if NULL, it is determined using cross-validation.
#' @param add_intercept Logical indicating whether to include an intercept in the model.
#' @param model_type Specifies the model type, 'gaussian' for regression or 'binomial' for classification.
#' @param bagging Logical indicating whether to perform bagging.
#' @param R The number of bootstrap samples to use if bagging is enabled.
#' @return A list containing the lasso regression model details, or if bagging is enabled, an aggregated
#' result from multiple bootstrap samples including the model, lambda value, coefficients,
#' and fitted values (continuous values for gaussian and probability values for binomial)
#' @importFrom glmnet glmnet cv.glmnet
#' @export
#' @examples
#' # Example for Gaussian model
#' data(mtcars)
#' X <- mtcars[, c("hp", "wt")]
#' y <- mtcars$mpg
#' result_lasso_gaussian <- fit_lasso_model(y, X, model_type = "gaussian", add_intercept = TRUE, bagging = FALSE)
#' print(result_lasso_gaussian)
#'
#' # Example for Binomial model
#' data(mtcars)
#' X <- mtcars[, c("hp", "wt")]
#' y <- ifelse(mtcars$am == 1, 1, 0)  # Converting 'am' to a binary outcome
#' result_lasso_binomial <- fit_lasso_model(y, X, model_type = "binomial", add_intercept = TRUE, bagging = TRUE, R = 50)
#' print(result_lasso_binomial)
fit_lasso_model <- function(y, X, lambda = NULL, model_type = "gaussian", add_intercept = TRUE, bagging = FALSE, R = 100) {
  validate_inputs(y, X)

  # family <- if (all(y %in% c(0, 1))) "binomial" else "gaussian"

  if (!model_type %in% c("binomial", "gaussian")) {
    stop("'family' must be either 'binomial' or 'gaussian'")
  }

  if (is.null(lambda)) {
    cv_fit <- cv.glmnet(x = as.matrix(X), y = y, alpha = 1, family = model_type, intercept = add_intercept)  # alpha = 1 for lasso
    lambda <- cv_fit$lambda.min
  }

  model_details <- if (bagging) {
    perform_bagging(y, X, function(y_sample, X_sample) {
      glmnet_model <- glmnet(x = as.matrix(X_sample), y = y_sample, alpha = 1, lambda = lambda, family = model_type, intercept = add_intercept)
      list(coefficients = coef(glmnet_model), fitted_values = predict(glmnet_model, newx = as.matrix(X_sample), type = "response"))
    }, R)
  } else {
    glmnet_model <- glmnet(x = as.matrix(X), y = y, alpha = 1, lambda = lambda, family = model_type, intercept = add_intercept)
    coefficients <- coef(glmnet_model, s = "lambda.min")  # Include intercept if add_intercept is TRUE
    fitted_values <- predict(glmnet_model, newx = as.matrix(X), type = "response", s = "lambda.min")
    list(model = glmnet_model, lambda = lambda, coefficients = coefficients, fitted_values = fitted_values)
  }

  # Assign coefficient names including intercept if applicable
  if (add_intercept) {
    model_details$names <- c("Intercept", colnames(X))
  } else {
    model_details$names <- colnames(X)
  }

  model_details$model_type <- model_type  # Identifying the model type for prediction function
  return(model_details)
}
