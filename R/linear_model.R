#' @title Linear Regression Model Fitting with Optional Bagging
#'
#' @description Fits a linear regression model using the least squares method. Supports both numerical
#' and categorical variables by converting categorical variables into dummy variables.
#' When bagging is TRUE, performs bagging to improve the robustness of the model fit.
#'
#' @param y Response variable, numeric vector.
#' @param X Predictor variables, matrix or data frame.
#' @param add_intercept Logical, if TRUE, an intercept term is added to the model.
#' @param bagging Logical, if TRUE, performs bagging.
#' @param R Integer, number of bootstrap samples for bagging.
#' @return A list containing model details such as coefficients, standard errors,
#'         t-values, p-values, and R-squared values. If bagging is TRUE, returns
#'         a list of aggregated results from the bagging procedure.
#' @importFrom MASS ginv
#' @export
#' @examples
#' data(mtcars)
#' model <- fit_linear_model(mtcars$mpg, mtcars[, -1], add_intercept = TRUE, bagging = FALSE)
#' print(model)
fit_linear_model <- function(y, X, model_type = "gaussian",add_intercept = TRUE, bagging = FALSE, R = 100) {

  # Ensure the model type is always 'gaussian'
  if (model_type != "gaussian") {
    stop("Invalid model type. Only 'gaussian' is supported.")
  }

  # Ensure this function is defined to validate y and X
  validate_inputs(y, X)

  if(add_intercept){
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
    model_details <- fit_linear_internal(y, X, add_intercept)
  } else {
    model_details <- perform_bagging(y, X, function(y_sample, X_sample) {
      fit_linear_internal(y_sample, X_sample, add_intercept)
    }, R)
  }

  model_details$model_type <- model_type  # Identify the model type for predict_model compatibility
  model_details$names <- colnames(X)
  return(model_details)
}



#' Internal function to fit linear model
#'
#' @param y Response variable vector.
#' @param X Design matrix with predictors.
#' @return A list containing model fitting details.
fit_linear_internal <- function(y, X, add_intercept) {
  if (det(X %*% t(X)) == 0) {
    message("Matrix X is singular. Using SVD to compute coefficients.")
    pseudo_inverse <- compute_pseudoinverse(t(X) %*% X)
    coefficients <- pseudo_inverse %*% t(X) %*% y
    XtX_inv <- pseudo_inverse

  } else {
    coefficients <- solve(t(X) %*% X) %*% t(X) %*% y
    XtX_inv <- solve(t(X) %*% X)
  }

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

  # Set the row names of the summary table to match the coefficient names
  colnames_for_beta <- colnames(X)
  if (length(coefficients) == length(colnames_for_beta)) {
    rownames(summary_table) <- colnames_for_beta
  } else {
    stop("Mismatch in the number of coefficients and predictor names")
  }


  names(coefficients) <- colnames(X)

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
    summary = summary_table,
    model_type = "gaussian"  # Identifying the model type for prediction function
  ))
}




