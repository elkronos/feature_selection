#' Compute information gain for a given feature and target variable
#'
#' This function computes the information gain (IG) of a given feature on a
#' target variable. The feature can be either numeric or categorical.
#'
#' @param data A data frame containing the data.
#' @param feature A character string specifying the name of the feature column.
#' @param target A character string specifying the name of the target column.
#' @param bins An integer specifying the number of bins to use if the feature is numeric. Default is 4.
#' @param type A character string specifying the type of the target variable. Must be either "numeric" or "categorical". Default is "categorical".
#'
#' @return The information gain of the feature on the target variable.
#'
#' @examples
#' df <- data.frame(age = rnorm(100, 35, 10),
#'                  salary = rnorm(100, 50000, 10000),
#'                  promoted = sample(c("yes", "no"), 100, replace = TRUE))
#' IG(df, "age", "promoted", bins = 4, type = "numeric")
#'
#' df <- data.frame(gender = sample(c("male", "female"), 100, replace = TRUE),
#'                  transaction_type = sample(c("online", "in-store", "ATM"), 100, replace = TRUE))
#' IG(df, "gender", "transaction_type", type = "categorical")
#'
#' @importFrom dplyr group_by summarise group_by_at
#' @importFrom stats cut
#' @importFrom stats entropy
#' @importFrom base nrow
library(dplyr)
info_gain_stat <- function(data, feature, target, bins = 4, type = "categorical") {
  #Strip out rows where feature is NA
  data <- data[!is.na(data[, feature]),]
  
  # Check that target is of the correct type
  if (type == "numeric") {
    if (is.numeric(data[[target]])) {
      stop("Input target must be a categorical variable")
    }
  } else {
    if (type == "categorical" && !is.character(data[[target]])) {
      stop("Input target must be a character or factor variable")
    } else if (type != "categorical" && !is.factor(data[[target]])) {
      stop("Input target must be a factor variable")
    }
  }
  
  #compute entropy for the parent
  e0 <- entropy(data[, target])
  
  if(type == "numeric") {
    # Check that feature is numeric
    if (!is.numeric(data[[feature]])) {
      stop("Input feature must be a numeric column")
    }
    data$cat <- cut(data[[feature]], breaks = bins, labels = c(1:bins))
    #use dplyr to compute e and p for each value of the feature
    dd_data <- data %>% group_by(cat) %>% summarise(e = entropy(get(target)), 
                                                    n = length(get(target)),
                                                    min = min(get(feature)),
                                                    max = max(get(feature))
    )
    dd_data$p <- dd_data$n / nrow(data)
  } else {
    # Check that feature is not numeric
    if (is.numeric(data[[feature]])) {
      stop("Input feature must be a categorical variable")
    }
    #use dplyr to compute e and p for each value of the feature
    dd_data <- data %>% group_by_at(feature) %>% summarise(e = entropy(get(target)), 
                                                           n = length(get(target))
    )
    dd_data$p <- dd_data$n / nrow(data)
  }
  
  #compute IG
  IG <- e0 - sum(dd_data$p * dd_data$e)
  
  return(IG)
}