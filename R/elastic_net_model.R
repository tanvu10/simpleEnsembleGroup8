#' @title Elastic Net Regression Model Fitting with Optional Bagging
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
#' @param model_type Specifies the model type, 'gaussian' for regression or 'binomial' for classification.
#' @param use_bagging Logical for enabling bagging, using multiple bootstrap samples.
#' @param R The number of bootstrap samples for bagging, applicable if bagging is TRUE.
#' @return A list containing the elastic net regression model details, or if bagging is enabled, an aggregated
#' result from multiple bootstrap samples including the model, lambda value, coefficients,
#' and fitted values (continuous values for gaussian and probability values for binomial)
#' @importFrom glmnet glmnet cv.glmnet
#' @export
#' @examples
#' data(mtcars)
#' X <- mtcars[, c("hp", "wt")]
#' y <- mtcars$mpg
#' result_elastic_net_gaussian <- fit_elastic_net_model(y, X, alpha = 0.5, model_type = "gaussian", add_intercept = TRUE, bagging = FALSE)
#' print(result_elastic_net_gaussian)
#'
#' # Example for Binomial model
#' data(mtcars)
#' X <- mtcars[, c("hp", "wt")]
#' y <- ifelse(mtcars$am == 1, 1, 0)  # Converting 'am' to a binary outcome
#' result_elastic_net_binomial <- fit_elastic_net_model(y, X, alpha = 0.5, model_type = "binomial", add_intercept = TRUE, bagging = TRUE, R = 50)
#' print(result_elastic_net_binomial)
fit_elastic_net_model <- function(y, X, alpha = 0.5, lambda = NULL, model_type = "gaussian", add_intercept = TRUE, bagging = FALSE, R = 100) {
  validate_inputs(y, X)

  if (!is.numeric(alpha) || alpha < 0 || alpha > 1) {
    stop("'alpha' must be a number between 0 and 1")
  }

  # family <- if (all(y %in% c(0, 1))) "binomial" else "gaussian"

  if (!model_type %in% c("binomial", "gaussian")) {
    stop("'family' must be either 'binomial' or 'gaussian'")
  }

  if (is.null(lambda)) {
    cv_fit <- cv.glmnet(x = as.matrix(X), y = y, alpha = alpha, family = model_type, intercept = add_intercept)
    lambda <- cv_fit$lambda.min
  }


  model_details <- if (bagging) {
    perform_bagging(y, X, function(y_sample, X_sample) {
      glmnet_model <- glmnet(x = as.matrix(X_sample), y = y_sample, alpha = alpha, lambda = lambda, family = model_type, intercept = add_intercept)
      list(coefficients = coef(glmnet_model), fitted_values = predict(glmnet_model, newx = as.matrix(X_sample), type = "response"))
    }, R)
  } else {
    glmnet_model <- glmnet(x = as.matrix(X), y = y, alpha = alpha, lambda = lambda, family = model_type, intercept = add_intercept)
    coefficients <- coef(glmnet_model, s = "lambda.min")  # Include intercept if add_intercept is TRUE
    fitted_values <- predict(glmnet_model, newx = as.matrix(X), type = "response", s = "lambda.min")
    list(model = glmnet_model, lambda = lambda, alpha= alpha, coefficients = coefficients, fitted_values = fitted_values)
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

