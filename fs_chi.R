library(stats)
library(dplyr)
library(purrr)

#' Perform feature selection using chi-square test
#' 
#' This function performs feature selection on a data frame using the chi-square
#' test to identify the categorical features that are statistically significant
#' with respect to the target variable. The function also handles cases where
#' the minimum frequency in the contingency table for a feature is less than 5,
#' in which case a warning is issued and the chi-square test is not performed for that feature.
#'
#' @param data A data frame containing the feature and target columns
#' @param target_col A character vector containing the name of the target column
#' @param sig_level A numeric value indicating the significance level for the chi-square test (default 0.05)
#' @param correct A logical value indicating whether to apply a correction for continuity to the chi-square test (default TRUE)
#'
#' @return A list with:
#' * `significant_features`: A character vector containing the names of the statistically significant categorical features
#' * `p_values`: A numeric vector with the p-values of the chi-square tests for each feature
#' 
#' @examples
#' \dontrun{
#' # Set seed for reproducibility
#' set.seed(123)
#'
#' # Create a vector with Yes and No repeated 250 times each
#' target <- factor(rep(c("Yes", "No"), each = 250))
#'
#' # Create a feature that is directly associated with the target variable
#' feature_1 <- factor(c(rep(c("A", "B"), times = c(200, 50)), rep(c("A", "B", "C"), times = c(50, 100, 100))))
#'
#' # Create a feature that is not associated with the target variable
#' feature_2 <- factor(sample(c("X", "Y"), 500, replace = TRUE))
#'
#' # Create a feature that is weakly associated with the target variable
#' feature_3 <- factor(c(rep(c("M", "N"), times = c(150, 100)), rep(c("M", "N"), times = c(100, 150))))
#'
#' # Combine all the features and target into a data frame
#' data <- data.frame(feature_1 = feature_1, feature_2 = feature_2, feature_3 = feature_3, target = target)
#'
#' # Use the fs_chi function on this dataset
#' results <- fs_chi(data, "target")
#'
#' # Print the significant features and their p-values
#' print(results$significant_features)
#' print(results$p_values)
#' }
#' @import stats
#' @importFrom dplyr mutate_if
#' @importFrom purrr map map_dbl set_names
#' @export
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
    if (min(table) < 5) {
      message(paste("Frequency less than 5 detected in the contingency table for feature", names(table), 
                    ". Chi-Square test may not be accurate, setting p-value as NA."))
      return(NA)
    } else {
      res <- chisq.test(table, correct = correct)
      message(paste("Feature", names(table), "p-value:", res$p.value))
      return(res$p.value)
    }
  })
  
  # Perform multiple hypothesis correction using the Bonferroni correction
  adj_sig_level <- sig_level / length(feature_cols)
  
  # Identify the features that are statistically significant
  sig_features <- feature_cols[!is.na(chi_square_results) & unlist(chi_square_results) < adj_sig_level]
  
  return(list(significant_features = sig_features, p_values = setNames(chi_square_results, feature_cols)))
}