#' Random forest prediction function
#'
#' This function applies random forest algorithm for classification or regression task. 
#' It has options for specifying the number of trees, importance computation, 
#' feature selection, data preprocessing, and parallel computing.
#'
#' @param data A data frame with columns for features and a column for the target variable.
#' @param target A string indicating the name of the column in the \code{data} that represents the target variable.
#' @param type A string indicating the type of the prediction task. Should be "classification" or "regression".
#' @param ntree An integer indicating the number of trees to grow in the random forest.
#' @param importance A logical value indicating whether importance of features should be computed.
#' @param sample_size An integer indicating the number of observations to sample from \code{data}.
#' @param feature_select A function that takes a data frame as input and returns a data frame with selected features.
#' @param preprocess A function that takes a data frame as input and returns a processed data frame.
#' @param n_cores An integer indicating the number of cores to use for parallel computing.
#'
#' @return A vector of predictions for the target variable.
#'
#' @examples
#' data(iris)
#' class_pred <- rf_select(iris, "Species", "classification")
#' print(head(class_pred))
#'
#' data(mtcars)
#' num_pred <- rf_select(mtcars, "mpg", "regression")
#' print(head(num_pred))
#'
#' @export
fs_randomforest <- function(data, target, type, ntree = 500, importance = TRUE, 
                      sample_size = NULL, feature_select = NULL, 
                      preprocess = NULL, n_cores = NULL) {
  
  target_idx <- match(target, names(data))
  
  if(type == "classification") {
    data[[target]] <- factor(data[[target]])
  }
  
  # Apply preprocessing, if specified
  if(!is.null(preprocess)) {
    data <- preprocess(data)
  }
  
  # Apply feature selection, if specified
  if(!is.null(feature_select)) {
    data <- feature_select(data)
  }
  
  # Sample data, if specified
  if(!is.null(sample_size)) {
    data <- data %>% sample_n(sample_size)
  }
  
  # Use parallel computing, if specified
  if(!is.null(n_cores)) {
    registerDoParallel(cores = n_cores)
    rf <- foreach(ntree = rep(floor(ntree/n_cores), n_cores), .combine = combine) %dopar% {
      randomForest(x = data[, -target_idx],
                   y = data[[target]],
                   ntree = ntree, importance = importance)
    }
    stopImplicitCluster()
  } else {
    rf <- randomForest(x = data[, -target_idx],
                       y = data[[target]],
                       ntree = ntree, importance = importance)
  }
  
  if(type == "classification") {
    predict(rf, newdata = data, type = "class")
  } else if(type == "regression") {
    predict(rf, newdata = data)
  } else {
    stop("Invalid type parameter. Choose 'classification' or 'regression'")
  }
}