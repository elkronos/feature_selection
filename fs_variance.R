#' Apply variance thresholding to a numeric dataset
#'
#' This function applies variance thresholding to a numeric dataset, removing features
#' whose variances are not above a certain threshold. The function returns a warning 
#' if no features have variance above the threshold and returns NULL. It will also
#' fail with an appropriate error message if the inputs are not as expected, and 
#' converts data frames to matrices if necessary for calculations.
#'
#' @param data A numeric matrix or data frame containing the input data. All columns of
#'             data frames should be numeric.
#' @param threshold A non-negative numeric value specifying the variance threshold. Features
#'                  with variances above this threshold are kept in the dataset.
#'
#' @return A numeric matrix containing the thresholded data. If the data was originally a
#'         data frame, it will be returned as a matrix. If no features have variance above 
#'         the threshold, the function returns NULL.
#'
#' @examples
#' # Create a sample dataset with 10 features and 100 observations
#' set.seed(123)
#' n_features <- 10
#' n_observations <- 100
#' data <- matrix(0, ncol = n_features, nrow = n_observations)
#' for (i in 1:n_features) {
#'   if (i %% 2 == 0) {
#'     data[, i] <- rnorm(n_observations, mean = 0, sd = 0.1)
#'   } else {
#'     data[, i] <- rnorm(n_observations, mean = 0, sd = 1)
#'   }
#' }
#' colnames(data) <- paste0("Feature", 1:n_features)
#'
#' # Apply variance thresholding with a threshold of 0.5
#' thresholded_data <- fs_variance(data, 0.5)
#'
#' # Check the dimensions of the thresholded dataset
#' dim(thresholded_data)
#'
#' @export
fs_variance <- function(data, threshold) {
  # Check threshold input
  if (!is.numeric(threshold) || length(threshold) != 1 || threshold < 0 || !is.finite(threshold)) {
    stop("Threshold should be a non-negative numeric value and finite.")
  }
  
  # Check data input
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data should be a matrix or a data frame.")
  }
  
  if (is.data.frame(data)) {
    if (!all(sapply(data, is.numeric))) {
      stop("All columns of data frame should be numeric.")
    }
    col_names <- colnames(data)  # Preserve column names
    data <- as.matrix(data)  # Convert to matrix for calculations
    colnames(data) <- col_names  # Re-assign column names
  }
  
  # Calculate the variances for each feature
  variances <- apply(data, 2, var)
  
  # Check if any variances exceed the threshold
  if (all(variances <= threshold)) {
    warning("No features have variance above the threshold.")
    return(NULL)
  }
  
  # Remove features with variances below the threshold
  data <- data[, variances > threshold, drop = FALSE]
  
  return(data)
}