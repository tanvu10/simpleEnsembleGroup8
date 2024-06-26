% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/select_top_k.R
\name{select_informative_predictors}
\alias{select_informative_predictors}
\title{Select the Top K Informative Predictors Using SVD}
\usage{
select_informative_predictors(X, K)
}
\arguments{
\item{X}{A matrix or data frame of predictors.}

\item{K}{The number of top informative predictors to return.}
}
\value{
A list containing a data frame with the top \( K \) predictors, their names, and their scores.
}
\description{
This function performs Singular Value Decomposition (SVD) on the predictor matrix \( X \)
and selects the top \( K \) most informative predictors based on the first right singular vector.
It checks that \( X \) is either a matrix or a data frame and ensures \( K \) is less than or equal to
the number of features in \( X \).
}
\examples{
\dontrun{
data(mtcars)
X <- mtcars[, -which(names(mtcars) == "mpg")]
result <- select_informative_predictors(X, 3)
print(result$top_predictors_data)
print(result$predictor_names)
print(result$scores)
}
}
