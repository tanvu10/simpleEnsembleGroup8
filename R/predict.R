#' @title Predict Method for Regression Models
#' @description Generates predictions from regression models fitted using various techniques,
#' including standard and bagged approaches. Handles linear, logistic, and elastic net models.
#' @param model The model object returned by fitting functions.
#' @param newdata New data on which to predict responses.
#' @param type The type of prediction required: "response" for predicted values, "coefficients" for model coefficients,
#' or "probabilities" for logistic regression probability estimates.
#' @return Predicted values, coefficients, or probabilities based on the input model and specified type.
predict_model <- function(model, newdata, type = "response") {
  if (!"coefficients" %in% names(model)) {
    stop("The model object does not appear to have valid coefficients.")
  }

  # Ensure newdata is a dataframe or matrix
  if (!is.data.frame(newdata) && !is.matrix(newdata)) {
    stop("newdata must be a data frame or matrix.")
  }

  # Check for necessary variables in newdata based on the model's coefficients
  model_vars <- names(model$coefficients)
  if (any(!model_vars %in% colnames(newdata))) {
    stop("Some variables required by the model are missing in newdata.")
  }

  # Add an intercept if necessary
  if ("Intercept" %in% model_vars && !("Intercept" %in% colnames(newdata))) {
    newdata <- cbind(Intercept = 1, newdata)
  }

  # Ensure that newdata variables are in the correct order
  newdata <- newdata[, model_vars, drop = FALSE]

  # Handle predictions based on the specified type
  switch(as.character(type),
         "response" = {
           if (model$model_type == "logistic") {
             # Calculate logistic probabilities
             exp_eta <- exp(as.matrix(newdata) %*% model$coefficients)
             probabilities <- exp_eta / (1 + exp_eta)
             return(ifelse(probabilities > 0.5, 1, 0))
           } else {
             # Linear and elastic net models
             return(as.matrix(newdata) %*% model$coefficients)
           }
         },
         "coefficients" = {
           return(model$coefficients)
         },
         "probabilities" = {
           if (model$model_type != "logistic") {
             stop("Probabilities can only be predicted for logistic regression models.")
           }
           # Calculate logistic probabilities
           exp_eta <- exp(as.matrix(newdata) %*% model$coefficients)
           return(exp_eta / (1 + exp_eta))
         },
         stop("Invalid 'type' specified. Use 'response', 'coefficients', or 'probabilities'.")
  )
}

