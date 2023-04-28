#' Load and preprocess data for Recursive Feature Elimination
#' 
#' This function loads the specified dataset, splits it into training and testing sets, and performs Recursive Feature Elimination (RFE) on the training set using the Random Forest algorithm. The function returns the variable importance scores computed from the RFE analysis.
#' 
#' @importFrom caret createDataPartition rfeControl rfe rfFuncs
#' 
#' @param data_name A character string specifying the name of the dataset to be used. The dataset must be available in R.
#'
#' @return A data frame containing the variable importance scores computed from the RFE analysis.
#' 
#' @examples
#' # Load iris dataset and perform RFE
#' iris_imp <- rfe_var_imp("iris")
#'
#' # Load mtcars dataset and perform RFE
#' mtcars_imp <- rfe_var_imp("mtcars")
#'
#' @export
# Load package
library(caret)
# Save function
fs_recrusivefeature <- function(data_name) {
  
  # Load a dataset
  data <- get(data_name)
  
  # Split the data into training and testing sets
  set.seed(123)
  trainIndex <- createDataPartition(data$Species, p = .8, list = FALSE)
  trainData <- data[trainIndex, ]
  testData <- data[-trainIndex, ]
  
  # Recursive Feature Elimination (RFE)
  ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 5)
  rfeProfile <- rfe(x = trainData[, -ncol(trainData)], y = trainData[, ncol(trainData)], sizes = c(1:(ncol(trainData)-1)),
                    rfeControl = ctrl)
  
  # Get the variable importance scores
  rfeImp <- rfeProfile$variables
  
  # Return the variable importance scores
  return(rfeImp)
}
