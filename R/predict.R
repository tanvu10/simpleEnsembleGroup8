#' @title Predict Method for Regression Models
#' @description Generates predictions from regression models fitted using various techniques,
#' including Lasso, Ridge, Elastic Net models using glmnet, and Random Forest.
#' @param model The model object returned by fitting functions which should include coefficients and model type.
#' @param newdata New data on which to predict responses.
#' @param type The type of prediction required: "response" for predicted values or "probabilities" for logistic regression probability estimates.
#' @param threshold Optional threshold for converting probabilities to binary outcomes in binomial models (default is 0.5).
#' @return Predicted values or probabilities based on the input model and specified type.
predict_model <- function(model, newdata, type = "response", threshold = 0.5) {
  if (!"model_type" %in% names(model)) {
    stop("The model object does not have a 'model_type' attribute.")
  }

  # Ensure newdata is a data frame or matrix
  if (!is.data.frame(newdata) && !is.matrix(newdata)) {
    stop("newdata must be a data frame or matrix.")
  }

  # Handle prediction based on model type
  switch(model$model_type,
         binomial = {
           if (!"coefficients" %in% names(model)) {
             stop("The model object does not appear to have valid coefficients for binomial model.")
           }
           eta <- as.matrix(newdata) %*% model$coefficients
           probabilities <- 1 / (1 + exp(-eta))  # Logistic transformation
           if (type == "response") {
             return(ifelse(probabilities > threshold, 1, 0))  # Convert probabilities to binary outcome
           } else if (type == "probabilities") {
             return(probabilities)  # Return probabilities directly
           } else {
             stop("Invalid 'type' specified for binomial model. Use 'response' or 'probabilities'.")
           }
         },
         gaussian = {
           if (!"coefficients" %in% names(model)) {
             stop("The model object does not appear to have valid coefficients for gaussian model.")
           }
           if (type == "response") {
             return(as.matrix(newdata) %*% model$coefficients)  # Linear prediction
           } else {
             stop("Invalid 'type' specified for gaussian model. Only 'response' is valid.")
           }
         },
         random_forest = {
           if (type != "response") {
             stop("Random Forest models can only return 'response' type predictions.")
           }
           requireNamespace("randomForest", quietly = TRUE)
           return(predict(model$model, newdata))  # Use the predict method for randomForest objects
         },
         stop("Unsupported model type provided.")
  )
}
