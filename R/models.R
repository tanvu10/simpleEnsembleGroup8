#' Linear Regression Model Fitting with Summary Output
#'
#' Fits a linear regression model using the least squares method. This function supports
#' both numerical and categorical variables by converting categorical variables
#' into dummy variables. It checks for matrix singularity and provides a detailed
#' summary of the regression model similar to the `lm` function in R.
#'
#' @param X Matrix or data frame of predictor variables, possibly including categorical variables.
#' @param y Response variable, either numeric or factor.
#' @return A list containing model details such as coefficients, standard errors, t-values, p-values,
#'         R-squared, adjusted R-squared, and F-statistic.
#' @importFrom MASS ginv
#' @export
#' @examples
#' data(iris)
#' model <- lin_reg(iris$Sepal.Length, iris[, -1])
#' print(model$summary)
fit_linear_model <- function(y, X) {
  validate_inputs(y, X)

  factor_cols <- sapply(X, is.factor)
  if (any(factor_cols)) {
    dummy_matrix <- create_dummy_matrix(X[factor_cols])
    X <- cbind(Intercept = 1, X[!factor_cols], dummy_matrix)
  } else {
    X <- cbind(Intercept = 1, X)
  }
  X <- as.matrix(X)

  # Using pseudoinverse in case of singular matrix
  XtX <- t(X) %*% X
  XtX_inv <- MASS::ginv(XtX)
  coefficients <- XtX_inv %*% t(X) %*% y

  # Compute diagnostics
  fitted_values <- X %*% coefficients
  residuals <- y - fitted_values
  rss <- sum(residuals^2)
  tss <- sum((y - mean(y))^2)
  r_squared <- 1 - rss/tss
  adj_r_squared <- 1 - (1 - r_squared) * (length(y) - 1) / (length(y) - ncol(X) - 1)
  se_coefficients <- sqrt(diag(XtX_inv) * rss / (length(y) - ncol(X)))
  t_values <- coefficients / se_coefficients
  p_values <- 2 * pt(-abs(t_values), df = length(y) - ncol(X))
  f_statistic <- (tss - rss) / ncol(X) / (rss / (length(y) - ncol(X) - 1))

  # Summary table construction
  summary_table <- data.frame(
    Estimate = coefficients,
    StdError = se_coefficients,
    tValue = t_values,
    Prt = p_values
  )

  # Return a detailed list
  return(list(
    coefficients = coefficients,
    residuals = residuals,
    fitted_values = fitted_values,
    r_squared = r_squared,
    adj_r_squared = adj_r_squared,
    se_coefficients = se_coefficients,
    t_values = t_values,
    p_values = p_values,
    f_statistic = f_statistic,
    df = length(y) - ncol(X),
    rss = rss,
    tss = tss,
    summary = summary_table
  ))
}

#' Logistic Regression Model Fitting with Summary Output
#'
#' Fits a logistic regression model using the Newton-Raphson method. This function
#' supports both numerical and categorical variables by converting categorical variables
#' into dummy variables. It provides a summary similar to glm output in R.
#'
#' @param X Matrix or data frame of predictor variables, possibly including categorical variables.
#' @param y Binary response variable.
#' @return A list containing model coefficients, standard errors, z-values, p-values,
#'         and model statistics such as null and residual deviance and AIC.
#' @export
#' @examples
#' data(mtcars)
#' X <- mtcars[, c("hp", "wt")]
#' y <- as.numeric(mtcars$am == 1)
#' result <- logistic_reg(X, y)
#' print(result$summary)
fit_logistic_model <- function(X, y) {
  validate_inputs(y, X)

  # Convert factors to dummy variables and bind with numeric predictors
  factor_cols <- sapply(X, is.factor)
  if (any(factor_cols)) {
    dummy_matrix <- create_dummy_matrix(X[factor_cols])
    X <- cbind(1, X[!factor_cols], dummy_matrix)
  } else {
    X <- cbind(1, X)
  }

  # Ensure the matrix is numerically stable
  X <- as.matrix(X)
  beta <- rep(0, ncol(X))

  # Newton-Raphson method for logistic regression
  for (i in 1:max_iter) {
    p <- 1 / (1 + exp(-X %*% beta))
    W <- diag(p * (1 - p))
    z <- X %*% beta + solve(W) %*% (y - p)
    beta_new <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% z

    if (sqrt(sum((beta - beta_new)^2)) < tol) {
      break
    }
    beta <- beta_new
  }

  # Standard errors calculation
  V <- solve(t(X) %*% W %*% X)
  se_beta <- sqrt(diag(V))

  # Z-values and P-values
  z_values <- beta / se_beta
  p_values <- 2 * pnorm(-abs(z_values))

  # Model deviances and AIC
  loglik <- sum(dbinom(y, size = 1, prob = p, log = TRUE))
  null_loglik <- sum(dbinom(y, size = 1, prob = mean(y), log = TRUE))
  residual_deviance <- -2 * loglik
  null_deviance <- -2 * null_loglik
  aic <- -2 * loglik + 2 * length(beta)

  # Summary table construction
  summary_table <- data.frame(
    Estimate = beta,
    StdError = se_beta,
    zValue = z_values,
    Prz = p_values
  )
  row.names(summary_table) <- c("(Intercept)", names(X)[-1])

  # Return a detailed list
  return(list(
    coefficients = beta,
    se = se_beta,
    z_values = z_values,
    p_values = p_values,
    null_deviance = null_deviance,
    residual_deviance = residual_deviance,
    aic = aic,
    summary = summary_table
  ))
}

#' @title Ridge Regression Fitting
#' @description Fits a ridge regression model using glmnet to a given set of predictors and response.
#' @param y A response vector with continuous or binary outcomes.
#' @param X A matrix of predictors.
#' @param lambda The regularization penalty parameter for ridge regression.
#' If NULL, cv.glmnet is used to find the optimal lambda.
#' @return A glmnet model object fitted with ridge regression.
#' @importFrom glmnet glmnet cv.glmnet
#' @export
#' @examples
#' data(mtcars)
#' result_ridge <- fit_ridge_model(mtcars$mpg, mtcars[, c("hp", "wt")])
fit_ridge_model <- function(y, X, lambda = NULL) {
  validate_inputs(y, X)

  family <- if (all(y %in% c(0, 1))) "binomial" else "gaussian"

  if (is.null(lambda)) {
    cv_fit <- cv.glmnet(x = as.matrix(X), y = y, alpha = 0, family = family)
    lambda <- cv_fit$lambda.min
  }

  glmnet_model <- glmnet(x = as.matrix(X), y = y, alpha = 0, lambda = lambda, family = family)
  return(list(model = glmnet_model, lambda = lambda))
}

#' @title Lasso Regression Fitting
#' @description Fits a lasso regression model using glmnet to a given set of predictors and response.
#' @param y A response vector with continuous or binary outcomes.
#' @param X A matrix of predictors.
#' @param lambda The regularization penalty parameter for lasso regression.
#' If NULL, cv.glmnet is used to find the optimal lambda.
#' @return A glmnet model object fitted with lasso regression.
#' @importFrom glmnet glmnet cv.glmnet
#' @export
#' @examples
#' data(mtcars)
#' result_lasso <- fit_lasso_model(mtcars$mpg, mtcars[, c("hp", "wt")])
fit_lasso_model <- function(y, X, lambda = NULL) {
  validate_inputs(y, X)

  family <- if (all(y %in% c(0, 1))) "binomial" else "gaussian"

  if (is.null(lambda)) {
    cv_fit <- cv.glmnet(x = as.matrix(X), y = y, alpha = 1, family = family)
    lambda <- cv_fit$lambda.min
  }

  glmnet_model <- glmnet(x = as.matrix(X), y = y, alpha = 1, lambda = lambda, family = family)
  return(list(model = glmnet_model, lambda = lambda))
}

#' @title Elastic Net Regression Fitting
#' @description Fits an elastic net regression model using glmnet to a given set of predictors and response.
#' @param y A response vector with continuous or binary outcomes.
#' @param X A matrix of predictors.
#' @param alpha The elastic net mixing parameter, with 0 <= alpha <= 1.
#' @param lambda The regularization penalty parameter for elastic net regression.
#' If NULL, cv.glmnet is used to find the optimal lambda.
#' @return A glmnet model object fitted with elastic net regression.
#' @importFrom glmnet glmnet cv.glmnet
#' @export
#' @examples
#' data(mtcars)
#' result_elastic_net <- fit_elastic_net_model(mtcars$mpg, mtcars[, c("hp", "wt")], alpha = 0.5)
fit_elastic_net_model <- function(y, X, alpha = 0.5, lambda = NULL) {
  validate_inputs(y, X)

  family <- if (all(y %in% c(0, 1))) "binomial" else "gaussian"

  if (!is.numeric(alpha) || alpha < 0 || alpha > 1) {
    stop("'alpha' must be a number between 0 and 1")
  }

  if (is.null(lambda)) {
    cv_fit <- cv.glmnet(x = as.matrix(X), y = y, alpha = alpha, family = family)
    lambda <- cv_fit$lambda.min
  }

  glmnet_model <- glmnet(x = as.matrix(X), y = y, alpha = alpha, lambda = lambda, family = family)
  return(list(model = glmnet_model, lambda = lambda, alpha = alpha))
}

