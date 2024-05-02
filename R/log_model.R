#' @title Fit Logistic Regression Model with Optional Bagging and Intercept
#'
#' @description Fits a logistic regression model with optional bagging and the ability to include or exclude an intercept.
#' The function uses the Newton-Raphson method for optimization and supports categorical variables through dummy encoding.
#'
#' @param X Predictor variables, matrix or data frame.
#' @param y Response variable, binary vector (must contain only two unique values).
#' @param add_intercept Logical, if TRUE, an intercept term is added to the model.
#' @param bagging Logical, if TRUE, performs bagging.
#' @param R Integer, number of bootstrap samples for bagging (only used if bagging is TRUE).
#' @return A list containing the logistic regression model details, or if bagging is TRUE,
#'         returns the aggregated results from the bagging procedure.
#' @export
#' @examples
#' data(iris)
#' # Prepare the data
#' X <- iris[iris$Species != "setosa", c("Sepal.Length", "Sepal.Width")]
#' y <- as.numeric(iris$Species[iris$Species != "setosa"] == "versicolor")
#'
#' # Fit the logistic regression model without bagging
#' model <- fit_logistic_model(y, X, add_intercept = TRUE, bagging = FALSE)
#'
#' # Print the model summary which includes coefficients and other statistics
#' print(model)
fit_logistic_model <- function(y, X, model_type = "binomial", add_intercept = TRUE, bagging = FALSE, R = 100) {

  # Ensure the model type is always 'binomial'
  if (model_type != "binomial") {
    stop("Invalid model type. Only 'binomial' is supported.")
  }

  validate_inputs(y, X)

  # Ensure that y contains only binary outcomes 0 or 1
  if (!all(y %in% c(0, 1))) {
    stop("Invalid values in y. Only binary values 0 or 1 are allowed for logistic regression.")
  }

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

  model_details$model_type <- model_type  # Add model type to the output
  model_details$names <- colnames(X)

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
  p_values <- 2 * (1 - pnorm(abs(z_values)))

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

  names(beta) <- colnames(X)

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
    fitted_values = p,
    model_type = "binomial"  # Identifying the model type for prediction function
  ))
}
