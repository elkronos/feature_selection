#' Correlation-based feature selection
#'
#' This function selects features from a data set based on their correlation with
#' other features. It returns a list containing the correlation matrix and the
#' selected variables.
#'
#' @param data A data frame or matrix containing the data.
#' @param threshold The threshold above which to select features based on their
#'   correlation. Must be a value between 0 and 1.
#' @param method The correlation method to use, either "pearson" or "spearman".
#'
#' @return A list containing two elements: the correlation matrix and the
#'   selected variables.
#'
#' @examples
#' # Load the mtcars data set
#' data(mtcars)
#'
#' # Default is Pearson
#' corr_vars <- correlation_select(mtcars, 0.7)
#' print(corr_vars$corr_matrix)
#' print(corr_vars$selected_vars)
#'
#' # Specify spearman
#' corr_vars <- correlation_select(mtcars, 0.7, method = "spearman")
#' print(corr_vars$corr_matrix)
#' print(corr_vars$selected_vars)
#'
fs_correlation <- function(data, threshold, method = "pearson") {
  # Calculate the correlation matrix
  if (method == "pearson") {
    corr_matrix <- cor(data, method = "pearson")
  } else if (method == "spearman") {
    corr_matrix <- cor(data, method = "spearman")
  } else {
    stop("Invalid correlation method. Please specify 'pearson' or 'spearman'.")
  }
  
  # Find the absolute correlation values above the threshold
  high_corr_vars <- which(abs(corr_matrix) > threshold, arr.ind = TRUE)
  
  # Remove duplicate correlations and self-correlations
  high_corr_vars <- high_corr_vars[high_corr_vars[,1] < high_corr_vars[,2],]
  
  # Check if there are any selected variables
  if (nrow(high_corr_vars) == 0) {
    message("No variables meet the correlation threshold.")
    return(list(corr_matrix = as.data.frame(corr_matrix), selected_vars = data))
  }
  
  # Extract the selected variables from the data frame
  selected_vars <- unique(c(high_corr_vars))
  selected_data <- data[,selected_vars]
  
  # Save the correlation matrix and selected variables as data frames in a list
  result <- list(corr_matrix = as.data.frame(corr_matrix), selected_vars = selected_data)
  return(result)
}