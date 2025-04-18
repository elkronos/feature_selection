# Load required packages
library(caret)
library(doParallel)

##############################################
# Validation and Preprocessing Helper Functions
##############################################

#' Validate Control Parameters
#'
#' Validates the structure and contents of the control parameters list.
#'
#' @param control_params A list containing control parameters. Must include `method` and `number` elements.
#'
#' @return Invisibly returns TRUE if validation is successful. Throws an error otherwise.
#'
#' @examples
#' validate_control_params(list(method = "cv", number = 5))
validate_control_params <- function(control_params) {
  if (!is.list(control_params)) {
    stop("control_params should be a list.")
  }
  required_elements <- c("method", "number")
  if (!all(required_elements %in% names(control_params))) {
    stop("control_params must contain 'method' and 'number' elements.")
  }
  valid_methods <- c("cv", "LOOCV", "repeatedcv", "boot", "none")
  if (!(control_params$method %in% valid_methods)) {
    stop(paste0("Invalid method in control_params. Must be one of: ", paste(valid_methods, collapse = ", "), "."))
  }
  if (!is.numeric(control_params$number) || control_params$number <= 0) {
    stop("control_params$number must be a positive integer.")
  }
  invisible(TRUE)
}

#' Handle Categorical Variables via One-Hot Encoding
#'
#' Converts categorical variables in a data frame to numeric variables using one-hot encoding.
#'
#' @param data A data frame.
#'
#' @return A data frame with one-hot encoded categorical variables.
#'
#' @examples
#' data <- data.frame(cat1 = factor(c("A", "B", "A")), num = 1:3)
#' handle_categorical_variables(data)
handle_categorical_variables <- function(data) {
  dummies <- dummyVars(" ~ .", data = data)
  data_transformed <- data.frame(predict(dummies, newdata = data))
  return(data_transformed)
}

#' Split Data into Training and Testing Sets
#'
#' Splits a dataset into training (80%) and testing (20%) sets.
#'
#' @param data A data frame.
#' @param response_var_index An integer index of the response variable.
#' @param seed An integer seed for reproducibility.
#'
#' @return A list with elements `trainData` and `testData`.
#'
#' @examples
#' data <- data.frame(response = rnorm(100), predictor = rnorm(100))
#' split_data(data, response_var_index = 1, seed = 123)
split_data <- function(data, response_var_index, seed) {
  set.seed(seed)
  
  response <- data[[response_var_index]]
  # For classification tasks, ensure response is a factor
  if (is.factor(response) || is.character(response)) {
    response <- as.factor(response)
  } else {
    response <- as.numeric(response)
  }
  
  trainIndex <- createDataPartition(response, p = 0.8, list = FALSE)
  list(trainData = data[trainIndex, , drop = FALSE],
       testData = data[-trainIndex, , drop = FALSE])
}

##############################################
# Modeling and Feature Selection Functions
##############################################

#' Perform Recursive Feature Elimination (RFE)
#'
#' Performs recursive feature elimination on the training data.
#'
#' @param trainData A data frame for training.
#' @param response_var_index An integer index for the response variable.
#' @param sizes A numeric vector of feature subset sizes to evaluate.
#' @param control_params A list with control parameters (must contain `method` and `number`).
#' @param feature_funcs A list of functions to be used for RFE (e.g., `rfFuncs`).
#' @param parallel Logical. Should parallel processing be used?
#' @param early_stop Logical. Should early stopping based on resampling results be enabled?
#'
#' @return An object of class `rfe` containing the results.
#'
#' @examples
#' # Example for regression with random forest functions:
#' data <- data.frame(response = rnorm(100), predictor1 = rnorm(100), predictor2 = rnorm(100))
#' control_params <- list(method = "cv", number = 5)
#' trainData <- split_data(data, 1, seed = 123)$trainData
#' perform_rfe(trainData, 1, sizes = c(1,2), control_params, feature_funcs = rfFuncs, parallel = FALSE, early_stop = FALSE)
perform_rfe <- function(trainData, response_var_index, sizes, control_params, feature_funcs, parallel, early_stop) {
  ctrl <- rfeControl(functions = feature_funcs,
                     method = control_params$method,
                     number = control_params$number,
                     verbose = TRUE,
                     allowParallel = parallel)
  if (early_stop) {
    ctrl$returnResamp <- "final"
    ctrl$saveDetails <- TRUE
  }
  
  # Remove response variable column for predictors
  predictors <- trainData[, -response_var_index, drop = FALSE]
  response <- trainData[[response_var_index]]
  
  rfeProfile <- tryCatch({
    rfe(x = predictors, y = response, sizes = sizes, rfeControl = ctrl)
  }, error = function(e) {
    stop("Error during RFE: ", e$message)
  })
  
  return(rfeProfile)
}

#' Train the Final Model with Selected Features
#'
#' Trains a model using the selected optimal features.
#'
#' @param data A data frame containing the dataset.
#' @param optimal_vars A character vector of variable names selected by RFE.
#' @param response_var_index An integer index of the response variable.
#' @param control_params A list with control parameters for training.
#' @param model_method A character string specifying the modeling method (default "rf").
#' @param cross_validate Logical. Should cross-validation be performed?
#'
#' @return A trained model object of class `train`.
#'
#' @examples
#' data <- data.frame(response = rnorm(100), predictor1 = rnorm(100), predictor2 = rnorm(100))
#' train_final_model(data, optimal_vars = c("predictor1", "predictor2"), response_var_index = 1,
#'                   control_params = list(method = "cv", number = 5), model_method = "rf")
train_final_model <- function(data, optimal_vars, response_var_index, control_params, model_method = "rf", cross_validate = TRUE) {
  if (cross_validate) {
    tr_control <- trainControl(method = control_params$method, number = control_params$number)
  } else {
    tr_control <- trainControl(method = "none")
  }
  
  predictors <- data[, optimal_vars, drop = FALSE]
  response <- data[[response_var_index]]
  
  final_model <- train(x = predictors, y = response, method = model_method, trControl = tr_control)
  return(final_model)
}

##############################################
# Main Recursive Feature Selection Wrapper
##############################################

#' Recursive Feature Selection and Model Training Wrapper
#'
#' Orchestrates the recursive feature elimination process and trains the final model (if requested).
#'
#' @param data A data frame containing the dataset.
#' @param response_var_index An integer index for the response variable.
#' @param seed Integer seed for reproducibility (default 123).
#' @param control_params A list with control parameters for RFE and model training (default: list(method = "cv", number = 5)).
#' @param sizes Numeric vector of feature subset sizes to evaluate (default: all possible sizes).
#' @param parallel Logical. Should parallel processing be enabled? (default FALSE)
#' @param feature_funcs A list of functions to use for RFE (default: rfFuncs).
#' @param handle_categorical Logical. Should categorical variables be one-hot encoded? (default FALSE)
#' @param return_final_model Logical. Should the final model be trained and returned? (default FALSE)
#' @param cross_validate_final_model Logical. Should cross-validation be applied to the final model? (default TRUE)
#' @param early_stop Logical. Should early stopping based on resampling results be enabled? (default FALSE)
#' @param model_method Character string specifying the modeling method for final training (default "rf").
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{OptimalNumberOfVariables}{The optimal number of features selected.}
#'   \item{OptimalVariables}{A character vector of the optimal variable names.}
#'   \item{VariableImportance}{Details of variable importance from RFE.}
#'   \item{ResamplingResults}{The resampling results from RFE.}
#'   \item{FinalModel}{The final trained model (if requested), otherwise NULL.}
#' }
#'
#' @examples
#' \dontrun{
#' data <- data.frame(response = rnorm(100),
#'                    predictor1 = rnorm(100),
#'                    predictor2 = rnorm(100))
#' results <- fs_recursivefeature(data, response_var_index = 1, seed = 123,
#'                                control_params = list(method = "cv", number = 5),
#'                                sizes = c(1,2), parallel = FALSE,
#'                                return_final_model = TRUE, model_method = "lm")
#' }
fs_recursivefeature <- function(data, response_var_index, seed = 123, 
                                control_params = list(method = "cv", number = 5),
                                sizes = NULL, parallel = FALSE, feature_funcs = rfFuncs,
                                handle_categorical = FALSE, return_final_model = FALSE,
                                cross_validate_final_model = TRUE, early_stop = FALSE,
                                model_method = "rf") {
  # Validate response variable index
  response_var_index <- as.integer(response_var_index)
  if (response_var_index < 1 || response_var_index > ncol(data)) {
    stop("Invalid response_var_index. It should be an integer between 1 and the number of columns in data.")
  }
  
  # Validate control parameters
  validate_control_params(control_params)
  
  # Split the data into training and testing sets
  data_split <- split_data(data, response_var_index, seed)
  trainData <- data_split$trainData
  testData <- data_split$testData
  
  # Handle categorical variables if requested (applied separately to train and test sets)
  if (handle_categorical) {
    trainData <- handle_categorical_variables(trainData)
    testData  <- handle_categorical_variables(testData)
  }
  
  # Set default sizes if not provided
  if (is.null(sizes)) {
    # Exclude the response column from the count
    sizes <- seq(1, ncol(trainData) - 1)
  }
  
  # Set up parallel processing if requested
  if (parallel) {
    cores <- max(1, detectCores() - 1)
    cl <- makeCluster(cores)
    registerDoParallel(cl)
    # Ensure cluster shutdown when function exits
    on.exit(stopCluster(cl))
  }
  
  # Perform Recursive Feature Elimination (RFE)
  rfeProfile <- perform_rfe(trainData, response_var_index, sizes, control_params, feature_funcs, parallel, early_stop)
  
  # Extract optimal features and variable importance information
  optimal_num_vars <- rfeProfile$optsize
  optimal_vars <- rfeProfile$optVariables
  var_importance <- rfeProfile$variables
  
  final_model <- NULL
  if (return_final_model) {
    # Combine train and test sets for final model training
    full_data <- rbind(trainData, testData)
    final_model <- train_final_model(full_data, optimal_vars, response_var_index, control_params, model_method, cross_validate_final_model)
  }
  
  return(list(
    OptimalNumberOfVariables = optimal_num_vars,
    OptimalVariables = optimal_vars,
    VariableImportance = var_importance,
    ResamplingResults = rfeProfile$results,
    FinalModel = final_model
  ))
}

