library(data.table)
library(lubridate)

#' Calculate entropy.
#'
#' This function calculates the entropy of a given vector.
#'
#' @param x A vector of numeric values.
#'
#' @return The entropy value.
#'
#' @importFrom data.table rle
#' @importFrom lubridate is.Date year month day
#'
#' @examples
#' entropy(c(1, 1, 2, 2, 2, 3))
#'
#' @export
entropy <- function(x) {
  x_sorted <- sort(x)
  freq <- rle(x_sorted)$lengths
  prob <- freq / length(x)
  -sum(prob * log2(prob))
}

#' Calculate the number of bins using Freedman-Diaconis rule.
#'
#' This function calculates the number of bins for histogram bins using the Freedman-Diaconis rule or Sturges' rule as a fallback.
#'
#' @param x A vector of numeric values.
#'
#' @return The number of bins.
#'
#' @importFrom stats IQR ceiling log2
#'
#' @examples
#' calculate_bins(c(1, 2, 3, 4, 5, 6))
#'
#' @export
calculate_bins <- function(x) {
  n <- length(x)
  iqr_x <- IQR(x)
  iqr_x <- ifelse(iqr_x == 0, 1, iqr_x)  # Ensure non-zero IQR
  fd_bin_width <- 2 * iqr_x / (n^(1/3))  # Freedman-Diaconis rule
  sturges_bins <- ceiling(log2(n) + 1)    # Sturges' rule
  
  # Use Freedman-Diaconis rule if it gives a valid bin number, else use Sturges' rule
  bins <- ifelse((max(x) - min(x)) / fd_bin_width > 1, ceiling((max(x) - min(x)) / fd_bin_width), sturges_bins)
  
  return(bins)
}

#' Calculate information gain.
#'
#' This function calculates the information gain for each variable in a given data frame.
#'
#' @param df A data frame.
#' @param target The name of the target variable in the data frame.
#'
#' @return A data frame with variable names and corresponding information gains.
#'
#' @importFrom data.table data.table nrow
#' @importFrom lubridate is.Date year month day
#' @import entropy calculate_bins
#'
#' @examples
#' df <- data.frame(A = sample(1:10, 100, replace = TRUE), 
#'                  B = sample(c("yes", "no"), 100, replace = TRUE), 
#'                  C = seq(as.Date("2001/1/1"), by = "month", length.out = 100),
#'                  target = sample(1:2, 100, replace = TRUE))
#' information_gain(df, "target")
#'
#' @export
fs_infogain <- function(df, target) {
  
  df <- data.table(df)
  
  # Extract date features
  date_cols <- which(sapply(df, lubridate::is.Date))
  for(col in names(df)[date_cols]) {
    df <- df[, paste0(col, "_year") := lubridate::year(df[[col]])]
    df <- df[, paste0(col, "_month") := lubridate::month(df[[col]])]
    df <- df[, paste0(col, "_day") := lubridate::day(df[[col]])]
  }
  
  info_gain <- vector()
  column_names <- names(df)
  entropy_target <- entropy(df[[target]])
  
  for(col in column_names) {
    
    if(col != target){
      
      # Check if the column is numeric
      if(is.numeric(df[[col]])) {
        n_bins <- calculate_bins(df[[col]])
        df[[col]] <- cut(df[[col]], breaks = n_bins, include.lowest = TRUE, labels = FALSE)
      }
      
      levels <- unique(df[[col]])
      entropy_attribute <- 0
      
      for(level in levels) {
        subset <- df[df[[col]] == level, ]
        weight <- nrow(subset) / nrow(df)
        entropy_attribute <- entropy_attribute + weight * entropy(subset[[target]])
      }
      
      gain <- entropy_target - entropy_attribute
      info_gain <- c(info_gain, gain)
    }
  }
  
  result <- data.frame(Variable = column_names[column_names != target], InfoGain = info_gain)
  return(result)
}

#' Calculate information gain for multiple data frames.
#'
#' This function calculates the information gain for each variable in multiple data frames and combines the results into a single data frame.
#'
#' @param dfs_list A list of data frames.
#'
#' @return A data frame containing the variable names, information gains, and the origin of the variables.
#'
#' @importFrom data.table rbindlist
#' @import information_gain
#'
#' @examples
#' # Create a list of data frames
#' df1 <- data.frame(A = sample(1:10, 100, replace = TRUE), 
#'                   B = sample(c("yes", "no"), 100, replace = TRUE), 
#'                   C = seq(as.Date("2001/1/1"), by = "month", length.out = 100),
#'                   target = sample(1:2, 100, replace = TRUE))
#' df2 <- data.frame(D = sample(1:10, 200, replace = TRUE), 
#'                   E = sample(c("yes", "no"), 200, replace = TRUE), 
#'                   F = seq(as.Date("2002/1/1"), by = "month", length.out = 200),
#'                   target = sample(1:2, 200, replace = TRUE))
#' df3 <- data.frame(G = sample(1:10, 150, replace = TRUE), 
#'                   H = sample(c("yes", "no"), 150, replace = TRUE), 
#'                   I = seq(as.Date("2003/1/1"), by = "month", length.out = 150),
#'                   target = sample(1:2, 150, replace = TRUE))
#' dfs_list <- list(df1, df2, df3)
#' calculate_information_gain_multiple(dfs_list)
#'
#' @export
calculate_information_gain_multiple <- function(dfs_list) {
  
  # Initialize an empty data frame to store the results
  all_results <- data.frame()
  
  # Process each data table in the list
  for(i in seq_along(dfs_list)) {
    
    # Get the current data table
    df <- dfs_list[[i]]
    
    # Apply the information_gain function
    result <- information_gain(df, "target")
    
    # Add a column to indicate the origin of the variables
    result$Origin <- paste0("Data_Table_", i)
    
    # Combine the results
    all_results <- rbind(all_results, result)
  }
  
  # Return the combined results
  return(all_results)
}