#' Fit Logistic Regression Model with Optional Bagging and Intercept
#'
#' Fits a logistic regression model with optional bagging and the ability to include or exclude an intercept.
#' The function uses the Newton-Raphson method for optimization and supports categorical variables through dummy encoding.
#'
#' @param X Predictor variables, matrix or data frame.
#' @param y Response variable, binary vector.
#' @param add_intercept Logical, if TRUE, an intercept term is added to the model.
#' @param bagging Logical, if TRUE, performs bagging.
#' @param R Integer, number of bootstrap samples for bagging (only used if bagging is TRUE).
#' @return A list containing the logistic regression model details, or if bagging is TRUE,
#'         returns the aggregated results from the bagging procedure.
#' @export
#' @examples
#' data(iris)
#' X <- iris[iris$Species != "setosa", c("Sepal.Length", "Sepal.Width")]
#' y <- as.numeric(iris$Species[iris$Species != "setosa"] == "versicolor")
#' model <- logistic_reg(X, y, add_intercept = TRUE, bagging = FALSE)
#' print(model$summary)
logistic_reg <- function(X, y, add_intercept = TRUE, bagging = FALSE, R = 100) {
  validate_inputs(y, X)

  if (add_intercept && !"Intercept" %in% colnames(X)) {
    X <- cbind(Intercept = 1, X)
  }

  # Handle categorical variables by creating dummy variables
  factor_cols <- sapply(X, is.factor)
  if (any(factor_cols)) {
    dummy_matrix <- create_dummy_matrix(X[factor_cols])
    X <- cbind(X[!factor_cols], dummy_matrix)
  }

  X <- as.matrix(X)

  # Perform bagging or regular fitting based on 'bagging' argument
  if (!bagging) {
    # Fit the model without bagging
    model_details <- fit_log_internal(X, y, add_intercept)
  } else {
    # Perform bagging
    model_details <- perform_bagging(y, X, function(y_sample, X_sample) {
      fit_log_internal(X_sample, y_sample, add_intercept)
    }, R)
  }

  model_details$model_type <- "logistic"  # Add model type to the output
  return(model_details)
}


#' Internal function to fit logistic regression model
#'
#' @param X Design matrix with predictors.
#' @param y Response variable vector.
#' @return A list containing model fitting details.
fit_log_internal <- function(X, y, add_intercept) {


  max_iter <- 25  # Set maximum number of iterations for convergence
  tol <- 1e-6     # Tolerance for convergence

  # Initial estimates
  beta <- rep(0, ncol(X))

  for (i in 1:max_iter) {
    p <- 1 / (1 + exp(-X %*% beta))  # Calculate probabilities
    W <- diag(as.vector(p * (1 - p)))  # Weight matrix for the Newton-Raphson update
    z <- X %*% beta + solve(W, y - p)  # Adjusted response
    beta_new <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% z

    # Check for convergence
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
  p_values <- 2 * pnorm(-abs(z_values), lower.tail = FALSE)

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

  # Make sure that row names are aligned with the beta coefficients
  if (length(beta) == length(colnames(X))) {
    row.names(summary_table) <- colnames(X)
  } else {
    stop("Mismatch in the number of coefficients and predictor names")
  }

  # Return a detailed list
  return(list(
    coefficients = beta,
    se = se_beta,
    z_values = z_values,
    p_values = p_values,
    null_deviance = null_deviance,
    residual_deviance = residual_deviance,
    aic = aic,
    summary = summary_table,
    model_type = "logistic"  # Identifying the model type for prediction function
  ))
}
