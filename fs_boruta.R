#' Select Features Using Boruta Algorithm
#'
#' This function selects features from a data table using the Boruta algorithm.
#'
#' @param data A data frame or data table.
#' @param target_var A string indicating the name of the target variable.
#' @param seed An optional integer value to use as the random seed.
#' @param doTrace An integer value indicating the verbosity level of the Boruta algorithm.
#' @param maxRuns An integer value indicating the maximum number of iterations to run the Boruta algorithm.
#' @param num_cores An optional integer value indicating the number of cores to use for parallel processing.
#' @param cutoff_features An optional integer value indicating the maximum number of features to select.
#' @param cutoff_cor An optional numeric value indicating the maximum correlation coefficient to allow between features.
#'
#' @return A list containing the selected feature names and the Boruta object.
#'
#' @examples
#' # Load iris dataset
#' data(iris)
#'
#' # Run Boruta feature selection
#' boruta_select(iris[, -5], "Species", doTrace = 0)
#'
#' @importFrom Boruta Boruta findRejectedAttributes getSelectedAttributes
#' @importFrom caret findCorrelation
#' @importFrom data.table as.data.table
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom utils prompt
#' @export
boruta_select <- function(data, target_var, seed = NULL, doTrace = 1, maxRuns = 250, num_cores = NULL, cutoff_features = NULL, cutoff_cor = 0.7) {
  # convert input data to data.table object
  dt <- as.data.table(data)
  
  # create a vector of the dependent variable
  y <- dt[[target_var]]
  
  # remove the target variable from the data.table
  dt <- dt[, !target_var, with = FALSE]
  
  # run the Boruta algorithm
  if (!is.null(num_cores)) {
    registerDoParallel(num_cores)
    boruta_obj <- Boruta(dt, y, doTrace = doTrace, maxRuns = maxRuns, seed = seed)
    stopImplicitCluster()
  } else {
    boruta_obj <- Boruta(dt, y, doTrace = doTrace, maxRuns = maxRuns, seed = seed)
  }
  
  # get the selected features
  selected_features <- getSelectedAttributes(boruta_obj, withTentative = FALSE)
  
  # reduce the number of features
  if (!is.null(cutoff_features)) {
    if (length(selected_features) > cutoff_features) {
      rejected_features <- findRejectedAttributes(boruta_obj)
      selected_features <- selected_features[!(selected_features %in% rejected_features)]
    }
  }
  
  # remove any variables that are too correlated with each other
  keep_features <- findCorrelation(dt[, selected_features, with = FALSE], cutoff = cutoff_cor, verbose = FALSE)
  
  # check if any variables were removed
  if (length(keep_features) == 0) {
    cat("All variables were kept\n")
    return(list(selected_features = colnames(dt)[selected_features], boruta_obj = boruta_obj))
  }
  
  # return the selected features
  return(list(selected_features = colnames(dt)[selected_features][keep_features], boruta_obj = boruta_obj))
}