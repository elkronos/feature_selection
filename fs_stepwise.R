library(caret)
library(MASS)
library(leaps)

#' Stepwise regression with cross-validation
#'
#' Fits a stepwise regression model with backward, forward, or both stepwise
#' selection, and performs cross-validation to evaluate model performance.
#'
#' @param data A data.frame containing the variables to be used in the regression.
#' @param dependent_var The name of the dependent variable as a string.
#' @param step_type The type of stepwise selection to perform: "backward", "forward", or "both".
#'
#' @return A list containing the following items:
#' \itemize{
#' \item \code{result}: A data.frame containing the results of the cross-validation.
#' \item \code{best_tune}: The value of \code{nvmax} that gives the best performance.
#' \item \code{final_model_summary}: A summary of the final model.
#' \item \code{coefficients}: The coefficients of the final model.
#' }
#'
#' @examples
#' data(swiss)
#' stepwise_regression(swiss, "Fertility", "backward")
#' stepwise_regression(swiss, "Fertility", "forward")
#' stepwise_regression(swiss, "Fertility", "both")
#'
#' @importFrom stats lm regsubsets stepAIC
#' @importFrom caret trainControl train
#' @export
fs_stepwise <- function(data, dependent_var, step_type = "both"){
  
  if(!is.data.frame(data)){
    stop("Input 'data' should be a data frame")
  }
  
  dep_var <- deparse(substitute(dependent_var))
  
  if(!(dep_var %in% colnames(data))){
    stop("Dependent variable is not a valid column in the data frame")
  }
  
  set.seed(123)
  
  formula <- reformulate(".", response = dep_var)
  full.model <- lm(formula, data = data)
  
  step.model <- stepAIC(full.model, direction = step_type, 
                        trace = FALSE)
  
  summary(step.model)
  
  # Set 'nvmax' to the number of predictors
  nvmax <- ncol(data) - 1
  models <- regsubsets(formula, data = data, nvmax = nvmax,
                       method = "seqrep")
  
  summary(models)
  
  train.control <- trainControl(method = "cv", number = 10)
  
  # Map step_type to method
  method <- switch(step_type,
                   "both" = "leapSeq",
                   "backward" = "leapBackward",
                   "forward" = "leapForward",
                   stop("Invalid step_type")
  )
  
  step.model <- train(formula, data = data,
                      method = method, 
                      tuneGrid = data.frame(nvmax = 1:nvmax),
                      trControl = train.control
  )
  
  result <- step.model$results
  best_tune <- step.model$bestTune
  final_model_summary <- summary(step.model$finalModel)
  
  # Get the variable importance from the final model
  importance <- varImp(step.model)
  
  return(list(result = result, best_tune = best_tune, 
              final_model_summary = final_model_summary, 
              importance = importance))
}