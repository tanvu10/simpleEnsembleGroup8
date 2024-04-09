#' Validate Inputs for Statistical Models
#'
#' This function checks the inputs to statistical model functions
#' to ensure they meet the package's standards.
#' @param y The response variable.
#' @param X The matrix or data frame of predictor variables.
#' @return Invisible TRUE if inputs are valid, otherwise throws an error.
#' @export
validate_inputs <- function(y, X) {
  if (!is.numeric(y) && !is.factor(y)) {
    stop("Response variable y must be numeric or a factor.")
  }
  if (!is.matrix(X) && !is.data.frame(X)) {
    stop("Predictor variables X must be a matrix or data frame.")
  }
  if (is.matrix(X) && any(is.na(X))) {
    stop("Matrix X should not contain NA values.")
  }
  invisible(TRUE)
}
