% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{validate_inputs}
\alias{validate_inputs}
\title{Validate Inputs for Statistical Models}
\usage{
validate_inputs(y, X)
}
\arguments{
\item{y}{The response variable.}

\item{X}{The matrix or data frame of predictor variables.}
}
\value{
Invisible TRUE if inputs are valid, otherwise throws an error.
}
\description{
This function checks the inputs to statistical model functions
to ensure they meet the package's standards.
}
