#' Train an SVM model using cross-validation and grid search
#'
#' This function trains an SVM model using cross-validation and grid search. It can be used for both classification and regression tasks. For classification, the function uses the svmLinear method and for regression, it uses the svmRadial method.
#'
#' @param data A data frame containing the data to be used for training and testing the model.
#' @param target The name of the target variable in the data.
#' @param task The type of task to be performed. "classification" for classification task and "regression" for regression task.
#' @param nfolds The number of folds to be used for cross-validation.
#' @param tune_grid A data frame containing the tuning grid for the SVM model. If NULL, the default tuning parameters will be used.
#'
#' @return A list containing the trained SVM model, the predictions, and the accuracy or R-squared score.
#'
#' @examples
#' \dontrun{
#' # Classification without tuning grid
#' data(iris)
#' output <- svm_model(iris, "Species", "classification")
#' print(output)
#'
#' # Regression without tuning grid
#' data(mtcars)
#' output <- svm_model(mtcars, "mpg", "regression")
#' print(output)
#'
#' # Classification with tuning grid
#' data(iris)
#' tune_grid <- expand.grid(C = seq(0.1, 1, by = 0.1))
#' output <- svm_model(iris, "Species", "classification", tune_grid = tune_grid)
#' print(output)
#'
#' # Regression with tuning grid
#' data(mtcars)
#' tune_grid <- expand.grid(sigma = seq(0.01, 1, by = 0.1), C = seq(1, 10, by = 1))
#' output <- svm_model(mtcars, "mpg", "regression", tune_grid = tune_grid)
#' print(output)
#' }
#'
#' @import e1071 caret
#' @export
# Load packages
library(e1071)
library(caret)
# Save function
fs_svm <- function(data, target, task, nfolds=5, tune_grid=NULL) {
  
  # Create formula for dependent and independent variables
  all_vars <- names(data)
  independent_vars <- all_vars[!all_vars %in% target]
  formula <- as.formula(paste(target, "~", paste(independent_vars, collapse = "+")))
  
  # Train SVM model using cross-validation and grid search
  if (task == "classification") {
    svm_fit <- train(formula, data = data, method = "svmLinear", trControl = trainControl(method = "cv", number = nfolds), tuneGrid = tune_grid)
  } else if (task == "regression") {
    if (!is.null(tune_grid)) {
      setNames(tune_grid, c("sigma", "C"))
    }
    svm_fit <- train(formula, data = data, method = "svmRadial", trControl = trainControl(method = "cv", number = nfolds), preProc = c("center", "scale"), tuneGrid = tune_grid)
  } else {
    stop("Invalid task. Please choose 'classification' or 'regression'.")
  }
  
  # Make predictions on test set
  predictions <- predict(svm_fit, newdata = data)
  
  # Calculate accuracy or R-squared score
  if (task == "classification") {
    accuracy <- sum(predictions == data[, target]) / nrow(data)
    output <- list(model = svm_fit, predictions = predictions, accuracy = accuracy)
  } else {
    rsq <- 1 - sum((predictions - data[, target])^2) / sum((data[, target] - mean(data[, target]))^2)
    output <- list(model = svm_fit, predictions = predictions, r_squared = rsq)
  }
  
  return(output)
}