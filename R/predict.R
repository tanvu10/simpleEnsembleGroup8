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

  if (!is.data.frame(newdata) && !is.matrix(newdata)) {
    stop("newdata must be a data frame or matrix.")
  }


  # Extract model variables and check if 'Intercept' is required
  if (is.null(model$names)) {
    stop("Model coefficients are not named or missing.")
  }
  model_vars <- model$names

  needs_intercept <- "Intercept" %in% model_vars

  # Add an intercept if it's part of the model but not in newdata
  if (needs_intercept && !"Intercept" %in% colnames(newdata)) {
    newdata <- cbind(Intercept = 1, newdata)
  }


  # Ensure newdata contains all the necessary predictors
  if (any(!model_vars %in% colnames(newdata))) {
    missing_vars <- model_vars[!model_vars %in% colnames(newdata)]
    stop("newdata is missing required columns: ", paste(missing_vars, collapse=", "))
  }


  # Ensure that newdata variables are in the correct order
  newdata_matrix <- as.matrix(newdata[, model_vars, drop = FALSE])

  # Handle prediction based on model type
  switch(model$model_type,
         binomial = {
           eta <- newdata_matrix %*% model$coefficients
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
           if (type == "response") {
             return(newdata_matrix %*% model$coefficients)  # Linear prediction
           } else {
             stop("Invalid 'type' specified for gaussian model. Only 'response' is valid.")
           }
         },
         random_forest = {
           if (type == "response") {
             return(predict(model$model, newdata)) # it will return a label or a continuous value depending on the model type
           }
           else if (type == "probabilities") {
             return(predict(model$model, newdata, type = "prob")[,2])
           } else {
             stop("Invalid 'type' specified for binomial model. Use 'response' or 'probabilities'.")
           }
           requireNamespace("randomForest", quietly = TRUE)
            # Use the predict method for randomForest objects
         },
         stop("Unsupported model type provided.")
  )
}


