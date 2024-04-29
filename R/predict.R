#' @title Predict Method for Regression Models
#' @description Generates predictions from regression models fitted using various techniques,
#' including standard, bagged approaches, and Random Forest. Handles linear, logistic, elastic net models,
#' and Random Forest.
#' @param model The model object returned by fitting functions.
#' @param newdata New data on which to predict responses.
#' @param type The type of prediction required: "response" for predicted values, "coefficients" for model coefficients
#' (not applicable for Random Forest), or "probabilities" for logistic regression probability estimates.
#' @return Predicted values, coefficients, or probabilities based on the input model and specified type.
#' @importFrom randomForest predict.randomForest
predict_model <- function(model, newdata, type = "response") {
  if (!"model_type" %in% names(model)) {
    stop("The model object does not have a 'model_type' attribute.")
  }

  # Ensure newdata is a dataframe or matrix
  if (!is.data.frame(newdata) && !is.matrix(newdata)) {
    stop("newdata must be a data frame or matrix.")
  }

  # Random Forest doesn't use coefficients in the same way, so skip the check for model coefficients
  if (model$model_type != "random_forest") {
    if (!"coefficients" %in% names(model)) {
      stop("The model object does not appear to have valid coefficients.")
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
  }

  # Predict based on the specified type
  switch(model$model_type,
         linear = {
           switch(as.character(type),
                  "response" = return(as.matrix(newdata) %*% model$coefficients),
                  "coefficients" = return(model$coefficients),
                  stop("Invalid 'type' specified for linear model. Use 'response' or 'coefficients'.")
           )
         },
         logistic = {
           switch(as.character(type),
                  "response" = {
                    exp_eta <- exp(as.matrix(newdata) %*% model$coefficients)
                    probabilities <- exp_eta / (1 + exp_eta)
                    return(ifelse(probabilities > 0.5, 1, 0))
                  },
                  "probabilities" = {
                    exp_eta <- exp(as.matrix(newdata) %*% model$coefficients)
                    return(exp_eta / (1 + exp_eta))
                  },
                  "coefficients" = return(model$coefficients),
                  stop("Invalid 'type' specified for logistic model. Use 'response', 'probabilities', or 'coefficients'.")
           )
         },
         elastic_net = {
           switch(as.character(type),
                  "response" = return(as.matrix(newdata) %*% model$coefficients),
                  "coefficients" = return(model$coefficients),
                  stop("Invalid 'type' specified for elastic net model. Use 'response' or 'coefficients'.")
           )
         },
         lasso = {
           switch(as.character(type),
                  "response" = return(as.matrix(newdata) %*% model$coefficients),
                  "coefficients" = return(model$coefficients),
                  stop("Invalid 'type' specified for elastic net model. Use 'response' or 'coefficients'.")
           )
         },
         ridge = {
           switch(as.character(type),
                  "response" = return(as.matrix(newdata) %*% model$coefficients),
                  "coefficients" = return(model$coefficients),
                  stop("Invalid 'type' specified for elastic net model. Use 'response' or 'coefficients'.")
           )
         },
         random_forest = {
           if (type != "response") {
             stop("Random Forest models can only return 'response' type predictions.")
           }
           # Use the predict method for randomForest objects
           return(predict(model$model, newdata))
         },
         stop("Unsupported model type provided.")
  )
}
