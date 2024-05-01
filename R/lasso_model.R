#' @title Lasso Regression Fitting
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
#' @param bagging Logical indicating whether to perform bagging.
#' @param R The number of bootstrap samples to use if bagging is enabled.
#' @return A glmnet model object fitted with lasso regression, or if bagging is enabled, an aggregated result from
#' multiple bootstrap samples.
#' @importFrom glmnet glmnet cv.glmnet
#' @export
#' @examples
#' data(mtcars)
#' result_lasso <- fit_lasso_model(mtcars$mpg, mtcars[, c("hp", "wt")], add_intercept = TRUE, bagging = FALSE)
fit_lasso_model <- function(y, X, lambda = NULL, family = "gaussian", add_intercept = TRUE, bagging = FALSE, R = 100) {
  validate_inputs(y, X)

  # family <- if (all(y %in% c(0, 1))) "binomial" else "gaussian"

  if (!family %in% c("binomial", "gaussian")) {
    stop("'family' must be either 'binomial' or 'gaussian'")
  }

  if (is.null(lambda)) {
    cv_fit <- cv.glmnet(x = as.matrix(X), y = y, alpha = 1, family = family, intercept = add_intercept)  # alpha = 1 for lasso
    lambda <- cv_fit$lambda.min
  }

  model_details <- if (bagging) {
    perform_bagging(y, X, function(y_sample, X_sample) {
      glmnet_model <- glmnet(x = as.matrix(X_sample), y = y_sample, alpha = 1, lambda = lambda, family = family, intercept = add_intercept)
      list(coefficients = coef(glmnet_model), fitted_values = predict(glmnet_model, newx = as.matrix(X_sample), type = "response"))
    }, R)
  } else {
    glmnet_model <- glmnet(x = as.matrix(X), y = y, alpha = 1, lambda = lambda, family = family, intercept = add_intercept)
    coefficients <- coef(glmnet_model, s = "lambda.min")  # Include intercept if add_intercept is TRUE
    fitted_values <- predict(glmnet_model, newx = as.matrix(X), type = "response", s = "lambda.min")
    list(model = glmnet_model, lambda = lambda, coefficients = coefficients, fitted_values = fitted_values)
  }

  model_details$model_type <- family  # Identifying the model type for prediction function
  return(model_details)
}
