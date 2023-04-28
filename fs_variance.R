#' Apply variance thresholding to a dataset
#'
#' This function applies variance thresholding to a dataset, removing features
#' whose variances are below a certain threshold.
#'
#' @param data A numeric matrix or data frame containing the input data
#' @param threshold A numeric value specifying the variance threshold
#' @return A numeric matrix containing the thresholded data
#' @examples
#' # Create a sample dataset with 5 features and 10 observations
#' data <- matrix(rnorm(50), ncol = 5)
#' colnames(data) <- paste0("Feature", 1:5)
#'
#' # Apply variance thresholding with a threshold of 0.5
#' thresholded_data <- variance_threshold(data, 0.5)
#'
#' # Check the dimensions of the thresholded dataset
#' dim(thresholded_data)
#' @export
variance_threshold <- function(data, threshold) {
  # Set seed to ensure reproducibility
  set.seed(123)
  # Get the number of rows in the input data
  n <- nrow(data)
  
  # Calculate the variances for each feature using matrix multiplication
  variances <- apply(data, 2, var)
  
  # Remove features with variances below the threshold
  data <- data[, variances >= threshold]
  
  return(data)
}