#' Select the Top K Informative Predictors Using SVD
#'
#' This function performs Singular Value Decomposition (SVD) on the predictor matrix \( X \)
#' and selects the top \( K \) most informative predictors based on the first right singular vector.
#' It checks that \( X \) is either a matrix or a data frame and ensures \( K \) is less than or equal to
#' the number of features in \( X \).
#'
#' @param X A matrix or data frame of predictors.
#' @param K The number of top informative predictors to return.
#' @return A list containing a data frame with the top \( K \) predictors, their names, and their scores.
#' @examples
#' \dontrun{
#' data(mtcars)
#' X <- mtcars[, -which(names(mtcars) == "mpg")]
#' result <- select_informative_predictors(X, 3)
#' print(result$top_predictors_data)
#' print(result$predictor_names)
#' print(result$scores)
#' }
#' @export
select_informative_predictors <- function(X, K) {
  if (!is.matrix(X) && !is.data.frame(X)) {
    stop("X must be a matrix or a data frame.")
  }

  if (K > ncol(X)) {
    stop("K must be less than or equal to the number of predictors in X.")
  }

  # Perform Singular Value Decomposition
  svd_result <- svd(X)

  # Extract the right singular vectors (V matrix)
  V <- svd_result$v

  # Identify the top K informative predictors based on the first right singular vector
  # Considering the absolute values to measure impact regardless of sign
  informative_scores <- abs(V[, 1])
  top_predictors_indices <- order(informative_scores, decreasing = TRUE)[1:K]

  # Retrieve the names of the top predictors
  top_predictor_names <- colnames(X)[top_predictors_indices]

  # Create a data frame of only the top K predictors
  top_predictors_data <- X[, top_predictor_names, drop = FALSE]


  return(list(
    top_predictors_data = top_predictors_data,
    predictor_names = top_predictor_names,
    scores = informative_scores[top_predictors_indices]
  ))
}
