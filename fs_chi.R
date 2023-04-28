#' Perform feature selection using chi-square test
#' 
#' This function performs feature selection on a data frame using the chi-square
#' test to identify the categorical features that are statistically significant
#' with respect to the target variable.
#'
#' @param data A data frame containing the feature and target columns
#' @param target_col A character vector containing the name of the target column
#' @param sig_level A numeric value indicating the significance level for the chi-square test
#' @param correct A logical value indicating whether to apply a correction for continuity to the chi-square test
#'
#' @return A character vector containing the names of the statistically significant categorical features
#' 
#' @examples
#' # Create a data frame with categorical variables for feature and target columns
#' data <- data.frame(
#'     feature_1 = factor(c("A", "B", "C", "A", "B", "C", "A", "B", "C")),
#'     feature_2 = factor(c("X", "X", "Y", "Y", "X", "Y", "X", "Y", "Y")),
#'     feature_3 = factor(c("M", "M", "M", "N", "N", "M", "M", "N", "N")),
#'     target = factor(c("Yes", "No", "Yes", "Yes", "Yes", "No", "No", "No", "Yes"))
#' )
#' 
#' # Call the chi_square_feature_selection function with the example dataset
#' selected_features <- chi_square_select(data, "target")
#' 
#' # Print the selected features
#' print(selected_features)
#'
#' @import stats
#' @importFrom dplyr mutate_if
#' @export
# Load the necessary package
library(stats)
library(dplyr)
# Define a function that performs feature selection using chi-square test
fs_chi <- function(data, target_col, sig_level = 0.05, correct = TRUE) {

  # Convert non-numeric columns to factors
  data <- data %>% mutate_if(is.character, as.factor)
  
  # Identify the categorical feature columns in the data frame
  feature_cols <- setdiff(names(data), target_col)
  categorical_feature_cols <- sapply(feature_cols, function(col) is.factor(data[[col]]))
  feature_cols <- feature_cols[categorical_feature_cols]
  
  # Compute the contingency table between each feature and the target variable
  contingency_tables <- lapply(feature_cols, function(feature_col) {
    table(data[[feature_col]], data[[target_col]])
  })
  
  # Compute the chi-square test statistic and p-value for each feature
  chi_square_results <- lapply(contingency_tables, function(table) {
    res <- chisq.test(table, correct = correct)
    cat("Feature", names(table), "p-value:", res$p.value, "\n")
    res$p.value
  })
  
  # Perform multiple hypothesis correction using the Bonferroni correction
  adj_sig_level <- sig_level / length(feature_cols)
  
  # Identify the features that are statistically significant
  sig_features <- feature_cols[chi_square_results < adj_sig_level]
  
  return(sig_features)
}