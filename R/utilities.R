#' Validate Inputs for Statistical Models
#'
#' This function checks the inputs to statistical model functions
#' to ensure they meet the package's standards.
#' @param y The response variable.
#' @param X The matrix or data frame of predictor variables.
#' @return Invisible TRUE if inputs are valid, otherwise throws an error.
#' @export
validate_inputs <- function(y, X) {
  # Check for NA values in X
  if (is.data.frame(X) && any(sapply(X, function(col) any(is.na(col))))) {
    stop("Data frame X should not contain NA values.")
  }

  # Ensure y is either numeric or a factor
  if (!is.numeric(y) && !is.factor(y)) {
    stop("Response variable y must be numeric or a factor.")
  }

  # Ensure X is a matrix or data frame
  if (!is.matrix(X) && !is.data.frame(X)) {
    stop("Predictor variables X must be a matrix or data frame.")
  }

  # Check for NA values in matrix X
  if (is.matrix(X) && any(is.na(X))) {
    stop("Matrix X should not contain NA values.")
  }

  # Check for character columns in X and recommend conversion to factor
  if (is.data.frame(X) && any(sapply(X, is.character))) {
    stop("Columns in X that are of type character should be converted to factors. Use the as.factor() function to convert these columns.")
  }

  invisible(TRUE)
}

#' Create Dummy Variables for Categorical Predictors
#'
#' Converts categorical variables within a matrix or data frame into dummy variables.
#' Each level of a factor results in a new binary column, except the first level to avoid multicollinearity.
#'
#' @param X Matrix or data frame containing categorical variables.
#' @return A matrix containing dummy variables.
#' @export
#' @examples
#' create_dummy_matrix(iris[, -1])
create_dummy_matrix <- function(X) {
  dummy_matrix <- NULL
  for (col in names(X)) {
    if (is.factor(X[[col]])) {
      levels_col <- levels(X[[col]])
      dummy_col <- sapply(levels_col[-1], function(level) as.numeric(X[[col]] == level))
      dummy_matrix <- cbind(dummy_matrix, dummy_col)
    }
  }
  return(dummy_matrix)
}


invert_nonzero <- function(x, eps = 1.0e-14){
  if (abs(x) <= eps) {
    return(0)
  }
  else{
    return(1/x)
  }
}


compute_pseudoinverse <- function(A){
  svd_result <- svd(A)
  u <- svd_result$u
  d <- svd_result$d
  v <- svd_result$v

  pinv <- v %*% diag(lapply(d, invert_nonzero), nrow = dim(A)[2], ncol = dim(A)[1]) %*% Conj(t(u))

  return(pinv)
}


