#' Train and test a Lasso regression model on a dataset
#'
#' This function fits a Lasso regression model to the training data using cross-validation, and extracts variable importance scores.
#'
#' @param x The predictor variables in the training dataset.
#' @param y The response variable in the training dataset.
#' @param alpha The Lasso penalty parameter.
#' @param nfolds The number of folds to use for cross-validation.
#'
#' @importFrom glmnet cv.glmnet coef
#'
#' @return a data.frame containing the variable importance scores sorted by importance.
#'
#' @examples
#' data(mtcars)
#' trainData <- mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")]
#' lassoImp <- train_lasso_model(trainData[, -1], trainData[, 1])
#' 
#' @export
# Load packages
library(glmnet)
# Save function
fs_lasso <- function(x, y, alpha = 1, nfolds = 5) {
  
  # Fit a Lasso regression model
  lassoModel <- cv.glmnet(as.matrix(x), y, alpha = alpha, nfolds = nfolds)
  
  # Extract variable importance scores
  lassoImp <- coef(lassoModel, s = "lambda.min")
  lassoImp <- abs(lassoImp)[-1]
  
  # Sort by importance and add original variable names
  varNames <- colnames(x)
  lassoImp <- data.frame(Variable = varNames, Importance = sort(lassoImp, decreasing = TRUE))
  
  # Return the Lasso importance scores
  return(lassoImp)
}