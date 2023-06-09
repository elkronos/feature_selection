#' Elastic Net Regression with Cross-Validation
#'
#' This function performs elastic net regression with cross-validation, using the glmnet package.
#'
#' @param data A data frame containing the response variable and predictor variables.
#' @param formula A formula specifying the model, with the response variable on the left-hand side and the predictor variables on the right-hand side.
#' @param alpha A numeric vector of values between 0 and 1 that specifies the mixing parameter between the L1 and L2 penalties in the elastic net model.
#' @param trControl A trainControl object specifying the cross-validation method and number of folds.
#' @param use_pca A logical value indicating whether principal component analysis (PCA) should be performed on the predictor variables before fitting the model.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{coef}{A vector of coefficients for the best model.}
#'   \item{alpha}{The mixing parameter between the L1 and L2 penalties for the best model.}
#'   \item{lambda}{The regularization parameter for the best model.}
#' }
#'
#' @examples
#' # Set the seed for reproducibility
#' set.seed(123)
#'
#' # Generate 100 observations and 10 predictors
#' n <- 100
#' p <- 10
#' x <- matrix(rnorm(n * p), nrow = n)
#' colnames(x) <- paste0("x", 1:p)
#'
#' # Generate the response variable with some noise
#' beta <- c(1.5, -2, 0, 0, 0.8, 0, -1.2, 0.5, 0.3, -1)
#' y <- x %*% beta + rnorm(n)
#'
#' # Combine the data into a data frame
#' data <- data.frame(y, x)
#'
#' # Use elastic net with 5-fold cross-validation and y as the response variable
#' elastic_select(data, y ~ .,
#'                trControl = trainControl(method = "cv", number = 5))
#'
#' @importFrom glmnet glmnet
#' @importFrom caret trainControl
#' @importFrom doParallel makeCluster registerDoParallel stopCluster
#' @importFrom stats prcomp model.matrix model.response
#' @export
# Load the necessary packages
library(glmnet)
library(caret)
library(doParallel)
fs_elastic <- function(data, formula, alpha = seq(0, 1, by = 0.1), trControl = trainControl(method = "cv", number = 5), use_pca = FALSE) {
  
  # Extract the response and predictor variables from the formula
  y <- model.response(model.frame(formula, data))
  x <- model.matrix(formula, data)[,-1]
  
  # Handle missing values if necessary
  complete_cases <- complete.cases(x, y)
  y <- y[complete_cases]
  x <- x[complete_cases, ]
  
  # Define the grid of lambda values to search over
  lambda <- 10^seq(-3, 3, length = 100)
  
  # Use doParallel to run the cross-validation and grid search in parallel
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  
  # Load necessary libraries on each core
  clusterEvalQ(cl, {
    library(caret)
    library(glmnet)
  })
  
  # Fit the elastic net models using cross-validation and grid search
  cv_fit <- foreach(a = alpha, .combine = "c", .packages = c("caret", "glmnet")) %dopar% {
    # Perform PCA if use_pca is TRUE
    if (use_pca) {
      pca <- prcomp(x, scale. = TRUE)
      x_pca <- pca$x[, 1:3]
    } else {
      x_pca <- x
    }
    
    # Create a tuneGrid for each alpha
    tuneGrid <- expand.grid(alpha = a, lambda = lambda)
    
    fit <- train(x_pca, y,
                 method = "glmnet",
                 tuneGrid = tuneGrid,
                 trControl = trControl)
    
    list(RMSE = fit$results$RMSE[which.min(fit$results$RMSE)], model = fit)
  }
  
  # Stop the parallelization
  stopCluster(cl)
  
  # Get the model with the best performance
  best_model <- cv_fit[[which.min(sapply(cv_fit, function(cv) cv$RMSE))]]$model
  
  # Return the coefficients and alpha and lambda values of the best model
  list(coef = coef(best_model$finalModel, s = best_model$bestTune$lambda),
       alpha = best_model$bestTune$alpha,
       lambda = best_model$bestTune$lambda)
}