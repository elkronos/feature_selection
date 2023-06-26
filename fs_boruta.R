library(Boruta)
library(caret)
library(data.table)
library(doParallel)

#' Select Features Using Boruta Algorithm
#'
#' This function selects features from a data frame or data table using the Boruta algorithm.
#' It first removes the target variable from the data and then runs the Boruta algorithm. 
#' If cutoff_features is specified, it limits the number of selected features. 
#' It then removes variables that are highly correlated with each other if cutoff_cor is specified.
#'
#' @param data A data frame or data table.
#' @param target_var A string indicating the name of the target variable.
#' @param seed An optional integer value to use as the random seed.
#' @param doTrace An integer value indicating the verbosity level of the Boruta algorithm (0 = no output, 1 = prints which attributes have been confirmed, 2 = also prints tentative attributes).
#' @param maxRuns An integer value indicating the maximum number of iterations to run the Boruta algorithm.
#' @param num_cores An optional integer value indicating the number of cores to use for parallel processing.
#' @param cutoff_features An optional integer value indicating the maximum number of features to select.
#' @param cutoff_cor An optional numeric value indicating the maximum correlation coefficient to allow between features.
#'
#' @return A list containing the selected feature indices and the Boruta object.
#'
#' @examples
#' # Load iris dataset
#' data(iris)
#'
#' # Define the target variable
#' target_var <- "Species"
#'
#' # Run the fs_boruta function
#' result <- fs_boruta(iris, target_var, doTrace = 0, num_cores = 2, cutoff_features = 10, cutoff_cor = 0.7)
#'
#' # Print the selected features
#' print(result$selected_features)
#'
#' @importFrom Boruta Boruta findRejectedAttributes getSelectedAttributes
#' @importFrom caret findCorrelation
#' @importFrom data.table as.data.table
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @export
fs_boruta <- function(data, target_var, seed = NULL, doTrace = 1, maxRuns = 250, num_cores = NULL, cutoff_features = NULL, cutoff_cor = 0.7) {
  # convert input data to data.table object
  dt <- as.data.table(data)
  
  # create a vector of the dependent variable
  y <- dt[[target_var]]
  
  # remove the target variable from the data.table
  dt <- dt[, !target_var, with = FALSE]
  
  # run the Boruta algorithm
  if (!is.null(num_cores)) {
    registerDoParallel(num_cores)
    on.exit(stopImplicitCluster())
  } 
  boruta_obj <- Boruta(dt, y, doTrace = doTrace, maxRuns = maxRuns, seed = seed)
  
  # get the selected features
  selected_features <- getSelectedAttributes(boruta_obj, withTentative = FALSE)
  
  # reduce the number of features
  if (!is.null(cutoff_features) && length(selected_features) > cutoff_features) {
    rejected_features <- findRejectedAttributes(boruta_obj)
    selected_features <- setdiff(selected_features, rejected_features)
  }
  
  # remove any variables that are too correlated with each other
  if (!is.null(cutoff_cor)) {
    correlation_matrix <- cor(dt[, selected_features, with = FALSE])
    keep_features <- findCorrelation(correlation_matrix, cutoff = cutoff_cor, verbose = FALSE)
    selected_features <- selected_features[keep_features]
  }
  
  # check if any variables were removed
  if (length(keep_features) == 0) {
    cat("All variables were kept\n")
  }
  
  # return the selected features
  return(list(selected_features = selected_features, boruta_obj = boruta_obj))
}