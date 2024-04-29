#' @title Ridge Regression Fitting
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
#' @param bagging Logical indicating whether to perform bagging.
#' @param R Integer specifying the number of bootstrap samples to use if bagging is enabled.
#' @return A glmnet model object fitted using ridge regression, or if bagging is enabled, an aggregated
#' result from multiple bootstrap samples.
#' @importFrom glmnet glmnet cv.glmnet
#' @export
#' @examples
#' data(mtcars)
#' result_ridge <- fit_ridge_model(mtcars$mpg, mtcars[, c("hp", "wt")], add_intercept = TRUE, bagging = FALSE)
fit_ridge_model <- function(y, X, lambda = NULL, add_intercept = TRUE, bagging = FALSE, R = 100) {
  validate_inputs(y, X)

  family <- if (all(y %in% c(0, 1))) "binomial" else "gaussian"

  if (is.null(lambda)) {
    cv_fit <- cv.glmnet(x = as.matrix(X), y = y, alpha = 0, family = family, intercept = add_intercept)
    lambda <- cv_fit$lambda.min
  }

  model_details <- if (bagging) {
    perform_bagging(y, X, function(y_sample, X_sample) {
      glmnet_model <- glmnet(x = as.matrix(X_sample), y = y_sample, alpha = 0, lambda = lambda, family = family, intercept = add_intercept)
      list(coefficients = coef(glmnet_model), fitted_values = predict(glmnet_model, newx = as.matrix(X_sample), type = "response"))
    }, R)
  } else {
    glmnet_model <- glmnet(x = as.matrix(X), y = y, alpha = 0, lambda = lambda, family = family, intercept = add_intercept)
    list(model = glmnet_model, lambda = lambda)
  }

  model_details$model_type <- "ridge"  # Adding model type identifier
  return(model_details)
}

