#' Train and evaluate a MARS model on a dataset
#'
#' This function takes a dataset, splits it into training and test sets,
#' performs grid search over a predefined set of hyperparameters, and trains
#' a MARS model on the training set using cross-validation. The best model is
#' then used to make predictions on the test set, and the root mean squared error
#' (RMSE) of the predictions is calculated and returned.
#'
#' @importFrom earth earth
#' @importFrom caret createDataPartition train trainControl
#' @param data The dataset to use for training and testing the model
#' @param p The proportion of the data to use for training (default: 0.8)
#' @param degree A vector of integers specifying the polynomial degree of the
#'   MARS model to fit (default: 1:3)
#' @param nprune A vector of integers specifying the number of basis functions
#'   to prune from the MARS model (default: 5, 10, 15)
#' @param method The method to use for model fitting (default: "earth")
#' @param search The method to use for searching the hyperparameter space
#'   (default: "grid")
#' @param number The number of folds to use for cross-validation (default: 5)
#' @return The RMSE of the predictions on the test set
#' @examples
#' # Load iris dataset
#' data(iris)
#' 
#' # Rename Sepal.Length to response
#' names(iris)[names(iris) == "Sepal.Length"] <- "response
#' 
#' # Test your function
#' result <- fs_mars(iris, p = 0.7, method = "lm")
#' 
#' # Print the full model results
#' print(result$model)
#' 
#' # Assuming you have obtained the model object from fs_mars
#' model <- result$model
#' 
#' # Get variable importance
#' var_importance <- varImp(model)
#' 
#' # Print variable importance
#' print(var_importance)
#' 
# Save function
fs_mars <- function(data, p = 0.8, degree = 1:3, nprune = c(5, 10, 15),
                    method = "earth", search = "grid", number = 5) {
  library(earth)
  library(caret)
  
  # Split data into training and test sets
  set.seed(123)
  trainIndex <- createDataPartition(data$response, p = p, list = FALSE)
  train <- data[trainIndex, ]
  test <- data[-trainIndex, ]
  
  # Define the grid of hyperparameters to search over
  hyper_params <- expand.grid(nprune = nprune, degree = degree)
  
  # Define the control parameters for the model training
  ctrl <- trainControl(method = "repeatedcv", number = number, search = search)
  
  # Train the model using grid search for hyperparameters
  set.seed(123)
  if(method == "earth") {
    mars_model <- train(response ~ ., data = train, method = method, trControl = ctrl, 
                        tuneGrid = hyper_params)
  } else {
    mars_model <- train(response ~ ., data = train, method = method, trControl = ctrl)
  }
  
  # Make predictions on test set
  pred <- predict(mars_model, newdata = test)
  
  # Calculate the RMSE if the response variable is numeric
  if(is.numeric(test$response)){
    rmse <- sqrt(mean((test$response - pred)^2))
    return(list(model = mars_model, rmse = rmse))
  }
  # Calculate accuracy if the response variable is categorical (factor)
  else if(is.factor(test$response)){
    accuracy <- sum(test$response == pred) / length(pred)
    return(list(model = mars_model, accuracy = accuracy))
  }
  else{
    stop("Unsupported response variable type.")
  }
}