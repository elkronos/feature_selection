library(randomForest)
library(doParallel)
library(foreach)
library(dplyr)
library(caret)

#' Random forest prediction function
#'
#' This function applies the random forest algorithm for a classification or regression task. 
#' It has options for specifying the number of trees, whether to compute importance,
#' feature selection, data preprocessing, and parallel computing.
#'
#' @param data A data frame with columns for features and a column for the target variable.
#' @param target A string indicating the name of the column in the \code{data} that represents the target variable.
#' @param type A string indicating the type of the prediction task. Should be "classification" or "regression".
#' @param ntree An integer indicating the number of trees to grow in the random forest.
#' @param importance A logical value indicating whether importance of features should be computed.
#' @param sample_size An integer indicating the number of observations to sample from \code{data}.
#' @param feature_select A function that takes a data frame as input and returns a data frame with selected features.
#' @param preprocess A function that takes a data frame as input and returns a processed data frame.
#' @param n_cores An integer indicating the number of cores to use for parallel computing.
#'
#' @return A list with elements 'model' (the trained random forest model), 
#' 'predictions' (a vector of predictions for the target variable in the test set), and
#' 'accuracy' (the model's classification accuracy for classification tasks, or RMSE for regression tasks).
#'
#' @examples
#' data(iris)
#' iris_result <- fs_randomforest(data = iris, target = "Species", type = "classification")
#' print(iris_result$accuracy)
#' print(head(iris_result$predictions))
#'
#' data(mtcars)
#' mtcars_result <- fs_randomforest(data = mtcars, target = "mpg", type = "regression")
#' print(mtcars_result$accuracy)
#' print(head(mtcars_result$predictions))
#'
#' @export
fs_randomforest <- function(data, target, type, ntree = 500, importance = TRUE, 
                            sample_size = NULL, feature_select = NULL, 
                            preprocess = NULL, n_cores = 1, split_ratio = 0.75) {
  
  # Check required packages and load them
  required_packages <- c("randomForest", "doParallel", "foreach", "dplyr", "caret")
  new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(required_packages, require, character.only = TRUE)
  
  # Check if target exists in the data
  if (!target %in% names(data)) {
    stop("The target variable does not exist in the dataset")
  }
  
  target_idx <- match(target, names(data))
  
  if(type == "classification") {
    data[[target]] <- as.factor(data[[target]])
  }
  
  # Apply preprocessing, if specified
  if(!is.null(preprocess)) {
    if (!is.function(preprocess)) stop("preprocess must be a function")
    data <- preprocess(data)
  }
  
  # Apply feature selection, if specified
  if(!is.null(feature_select)) {
    if (!is.function(feature_select)) stop("feature_select must be a function")
    data <- feature_select(data)
  }
  
  # Sample data, if specified
  if(!is.null(sample_size)) {
    data <- data %>% sample_n(sample_size)
  }
  
  # Split the data into training and testing
  inTrain <- createDataPartition(y = data[[target]], p = split_ratio, list = FALSE)
  training <- data[inTrain,]
  testing <- data[-inTrain,]
  
  # Use parallel computing, if specified
  if(n_cores > 1) {
    registerDoParallel(cores = n_cores)
    on.exit(stopImplicitCluster())
    rf <- foreach(ntree = rep(floor(ntree/n_cores), n_cores), .combine = randomForest::combine, 
                  .packages = c("randomForest")) %dopar% {
                    randomForest(x = training[, -target_idx],
                                 y = training[[target]],
                                 ntree = ntree, importance = importance)
                  }
  } else {
    rf <- randomForest(x = training[, -target_idx],
                       y = training[[target]],
                       ntree = ntree, importance = importance)
  }
  
  
  # Make predictions on the test data
  if(type == "classification") {
    predictions <- predict(rf, newdata = testing, type = "class")
    accuracy <- sum(predictions == testing[[target]]) / length(predictions)
  } else if(type == "regression") {
    predictions <- predict(rf, newdata = testing)
    accuracy <- sqrt(mean((predictions - testing[[target]])^2))
  } else {
    stop("Invalid type parameter. Choose 'classification' or 'regression'")
  }
  
  # Return the model, predictions and accuracy
  list(model = rf, predictions = predictions, accuracy = accuracy)
}