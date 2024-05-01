#' @title Elastic Net Regression Fitting
#' @description This function fits an elastic net regression model, which utilizes both lasso and ridge regularization
#' techniques to improve model performance and interpretability. The function is effective for datasets with
#' multicollinearity or more predictors than observations, adjusting the model complexity by blending L1 and L2
#' penalties controlled by the alpha parameter. Optionally, bagging can be used to enhance stability and accuracy
#' in high-variance scenarios through bootstrap aggregating.
#'
#' @param y Response vector with continuous or binary outcomes.
#' @param X Predictor matrix, accepting numeric or factor types.
#' @param alpha The elastic net mixing parameter, ranging from 0 (ridge) to 1 (lasso).
#' @param lambda Regularization penalty parameter; auto-determined via cross-validation if NULL.
#' @param add_intercept Logical indicating whether to include an intercept in the model.
#' @param use_bagging Logical for enabling bagging, using multiple bootstrap samples.
#' @param R The number of bootstrap samples for bagging, applicable if bagging is TRUE.
#' @return A glmnet model object fitted using elastic net regularization, or an aggregated result from multiple
#' bootstrap samples if bagging is enabled.
#' @importFrom glmnet glmnet cv.glmnet
#' @export
#' @examples
#' data(mtcars)
#' result <- fit_elastic_net_model(mtcars$mpg, mtcars[, c("hp", "wt")], alpha = 0.5, add_intercept = TRUE, use_bagging = TRUE)
#' print(result)
#' @title Elastic Net Regression Fitting
#' @description This function fits an elastic net regression model, which utilizes both lasso and ridge regularization
#' techniques to improve model performance and interpretability. The function is effective for datasets with
#' multicollinearity or more predictors than observations, adjusting the model complexity by blending L1 and L2
#' penalties controlled by the alpha parameter. Optionally, bagging can be used to enhance stability and accuracy
#' in high-variance scenarios through bootstrap aggregating.
#'
#' @param y Response vector with continuous or binary outcomes.
#' @param X Predictor matrix, accepting numeric or factor types.
#' @param alpha The elastic net mixing parameter, ranging from 0 (ridge) to 1 (lasso).
#' @param lambda Regularization penalty parameter; auto-determined via cross-validation if NULL.
#' @param add_intercept Logical indicating whether to include an intercept in the model.
#' @param use_bagging Logical for enabling bagging, using multiple bootstrap samples.
#' @param R The number of bootstrap samples for bagging, applicable if bagging is TRUE.
#' @return A glmnet model object fitted using elastic net regularization, or an aggregated result from multiple
#' bootstrap samples if bagging is enabled. The result includes the model type for enhanced prediction functionality.
#' @importFrom glmnet glmnet cv.glmnet
#' @export
#' @examples
#' data(mtcars)
#' result <- fit_elastic_net_model(mtcars$mpg, mtcars[, c("hp", "wt")], alpha = 0.5, add_intercept = TRUE, use_bagging = TRUE)
#' print(result)
fit_elastic_net_model <- function(y, X, alpha = 0.5, lambda = NULL, add_intercept = TRUE, use_bagging = FALSE, R = 100) {
  validate_inputs(y, X)

  if (!is.numeric(alpha) || alpha < 0 || alpha > 1) {
    stop("'alpha' must be a number between 0 and 1")
  }

  family <- if (all(y %in% c(0, 1))) "binomial" else "gaussian"

  if (is.null(lambda)) {
    cv_fit <- cv.glmnet(x = as.matrix(X), y = y, alpha = alpha, family = family, intercept = add_intercept)
    lambda <- cv_fit$lambda.min
  }


  model_details <- if (bagging) {
    perform_bagging(y, X, function(y_sample, X_sample) {
      glmnet_model <- glmnet(x = as.matrix(X_sample), y = y_sample, alpha = alpha, lambda = lambda, family = family, intercept = add_intercept)
      list(coefficients = coef(glmnet_model), fitted_values = predict(glmnet_model, newx = as.matrix(X_sample), type = "response"))
    }, R)
  } else {
    glmnet_model <- glmnet(x = as.matrix(X), y = y, alpha = alpha, lambda = lambda, family = family, intercept = add_intercept)
    coefficients <- coef(glmnet_model, s = "lambda.min")  # Include intercept if add_intercept is TRUE
    fitted_values <- predict(glmnet_model, newx = as.matrix(X), type = "response", s = "lambda.min")
    list(model = glmnet_model, lambda = lambda, alpha= alpha, coefficients = coefficients, fitted_values = fitted_values)
  }


  model_details <- if (use_bagging) {
    # Perform bagging with the specified number of bootstrap samples
    perform_bagging(y, X, function(y_sample, X_sample) {
      glmnet_model <- glmnet(x = as.matrix(X_sample), y = y_sample, alpha = alpha, lambda = lambda, family = family, intercept = add_intercept)
      list(coefficients = coef(glmnet_model), fitted_values = predict(glmnet_model, newx = as.matrix(X_sample), type = "response"))
    }, R)
  } else {
    glmnet_model <- glmnet(x = as.matrix(X), y = y, alpha = alpha, lambda = lambda, family = family, intercept = add_intercept)
    list(model = glmnet_model, lambda = lambda, alpha = alpha)
  }

  model_details$model_type <- "elastic_net"  # Adding model type identifier
  return(model_details)
}

