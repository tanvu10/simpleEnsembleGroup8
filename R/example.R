#' Summarize Data
#'
#' This function calculates the mean and median of a numeric vector.
#' @param x A numeric vector.
#' @return A list containing the mean and median of the vector.
#' @export
#' @examples
#' summarize_data(c(1, 2, 3, 4, 5))
summarize_data <- function(x) {
  # if (!is.numeric(x)) {
  #   stop("Input x must be a numeric vector.")
  # }
  mean_value <- mean(x, na.rm = TRUE)
  median_value <- calculate_median(x)

  return(list(mean = mean_value, median = median_value))
}

#' Calculate Median
#'
#' This function calculates the median of a numeric vector.
#' @param x A numeric vector.
#' @return The median of the vector.
#' @export
#' @examples
#' calculate_median(c(1, 2, 3, 4, 5))
calculate_median <- function(x) {
  x <- x[!is.na(x)]  # Remove NAs before sorting
  sorted_x <- sort(x)
  n <- length(sorted_x)
  if (n %% 2 == 1) {
    return(sorted_x[(n + 1) / 2])
  } else {
    return(mean(sorted_x[(n / 2):((n / 2) + 1)]))
  }
}

linear_regression <- function(y, X) {
  validate_inputs(y, X)  # Validate inputs

  # Proceed with linear regression assuming inputs are valid
  # ...
}

# Similar validation call would be placed in other functions



