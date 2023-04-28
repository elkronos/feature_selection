#' Feature Selection for Bayesian Models
#'
#' @title fs_bayes
#'
#' @description This function performs feature selection for Bayesian models using the brms package.
#'   It generates all possible combinations of predictor columns and fits a Bayesian model for each
#'   combination. The best model is selected based on the lowest WAIC value. The function also
#'   calculates model quality metrics such as MAE and RMSE.
#'
#' @param data A data frame containing the response and predictor variables.
#' @param response_col A character string specifying the name of the response column.
#' @param predictor_cols A character vector specifying the names of the predictor columns.
#' @param date_col An optional character string specifying the name of the date column. If provided, the
#'   function will add a 'week' feature to the data set.
#' @param prior An optional brms prior object to be passed to the brm function.
#' @param brm_args A list of additional arguments to be passed to the brm function.
#'
#' @return A list containing the best model and the modified data with fitted values and residuals.
#'
#' @importFrom brms brm gaussian waic loo
#' @importFrom tidyverse mutate
#' @importFrom parallel detectCores
#'
#' @examples
#' # Generate synthetic data
#' set.seed(123)
#'
#' n <- 1000
#' date_seq <- seq(as.Date("2000-01-01"), by = "day", length.out = n)
#' sales <- rnorm(n, mean = 100, sd = 20)
#' duration <- rnorm(n, mean = 50, sd = 10)
#' company <- factor(sample(LETTERS[1:3], n, replace = TRUE))
#'
#' data <- tibble(
#'   date = date_seq,
#'   sales = sales,
#'   duration = duration,
#'   company = company
#' )
#'
#' # Test the function with the synthetic data
#' result <- fs_bayes(data, "sales", c("duration", "company"), "date")
#'
#' # Access the best model and modified data
#' best_model <- result$Model
#' data_with_fitted_values <- result$Data
#'
#' # Display best model
#' print(best_model)
#'
#' # Display data with fitted values and residuals
#' head(data_with_fitted_values)
#'
#' @seealso
#' \url{https://cran.r-project.org/web/packages/brms/index.html}
#' \url{https://cran.r-project.org/web/packages/tidyverse/index.html}
#' \url{https://cran.r-project.org/web/packages/parallel/index.html}
#' \url{https://cran.r-project.org/web/packages/loo/index.html}
#'
#' @references
#' Bürkner, P. C. (2017). brms: An R Package for Bayesian Multilevel Models Using Stan.
#'   Journal of Statistical Software, 80(1), 1-28. doi:10.18637/jss.v080.i01
#'
#' @export
# Load packages
library(brms)
library(tidyverse)
library(parallel)
library(loo)
# Save function
fs_bayes <- function(data, response_col, predictor_cols, date_col,
                     prior = NULL, brm_args = list()) {
  # Add date-related features if provided
  if (!is.null(date_col)) {
    data <- data %>% mutate(week = as.integer(format(!!sym(date_col), "%U")))
    predictor_cols <- c(predictor_cols, "week")
  }
  
  # Generate all possible combinations of predictor columns
  predictor_combinations <- unlist(lapply(seq_along(predictor_cols),
                                          function(i) combn(predictor_cols, i, simplify = FALSE)), recursive = FALSE)
  
  best_model <- NULL
  best_waic <- Inf
  
  # Number of cores for parallel processing
  n_cores <- detectCores(logical = FALSE)
  
  # Iterate through models with different combinations of predictor variables
  for (predictor_comb in predictor_combinations) {
    predictors <- paste(predictor_comb, collapse = " + ")
    formula_str <- paste0(response_col, " ~ ", predictors)
    formula_sales <- as.formula(formula_str)
    
    # Fit the Bayesian model with increased number of iterations
    model <- tryCatch({
      do.call(brm, modifyList(list(
        formula = formula_sales,
        data = data,
        family = gaussian(),
        prior = prior,
        cores = n_cores,
        iter = 4000
      ), brm_args))
    }, warning = function(w) {
      return(NULL)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(model)) {
      cat("Model:", formula_str, "encountered a warning or error.\n\n")
      next
    }
    
    # Calculate WAIC and LOO for model comparison
    loo_est <- loo(model)
    waic_est <- waic(model)
    
    cat("Model:", formula_str, "\n")
    cat("WAIC:", waic_est$estimates["waic"], "\n")
    cat("LOO:", loo_est$estimates["loo"], "\n\n")
    
    # Update the best model if the current one has a lower WAIC and is not NA
    if (!is.na(waic_est$estimates["waic"]) && waic_est$estimates["waic"] < best_waic) {
      best_model <- model
      best_waic <- waic_est$estimates["waic"]
    }
  }
  
  cat("Best Model:", as.character(formula(best_model)), "\n")
  cat("Best WAIC:", best_waic, "\n")
  
  # Extract the fitted values and residuals from the best model
  fitted_values <- fitted(best_model)
  residuals <- resid(best_model)
  
  # Add the fitted values and residuals to the data frame
  data$fitted_values <- fitted_values
  data$residuals <- residuals
  
  # Calculate model quality metrics
  data$abs_residuals <- abs(residuals)
  data$residuals <- residuals
  
  # Calculate model quality metrics
  data$abs_residuals <- abs(residuals)
  data$squared_residuals <- residuals^2
  mae <- mean(data$abs_residuals)
  rmse <- sqrt(mean(data$squared_residuals))
  
  cat("MAE:", mae, "\n")
  cat("RMSE:", rmse, "\n")
  
  return(list("Model" = best_model, "Data" = data))
}