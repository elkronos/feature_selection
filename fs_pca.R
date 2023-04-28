#' Perform PCA on Data
#'
#' This function performs principal component analysis (PCA) on a given dataset and returns the PCA results,
#' including the principal component loadings, scores, the proportion of variance explained by each principal component,
#' and a data table with the principal component scores and the labels.
#'
#' @param data A data frame containing the data to be analyzed.
#' @param chunk_size An integer indicating the number of rows to include in each chunk of the data. Default is 1000.
#'
#' @return A list containing the following components:
#' \itemize{
#' \item pc_loadings: a matrix of principal component loadings.
#' \item pc_scores: a matrix of principal component scores.
#' \item var_explained: a vector of the proportion of variance explained by each principal component.
#' \item pca_df: a data table of principal component scores and labels.
#' }
#'
#' @importFrom data.table as.data.table
#' @importFrom dplyr select where is.character is.factor names first pull mutate
#' @import ggplot2 ggtitle xlab ylab geom_point scale_color_brewer
#' @importFrom stats prcomp summary
#'
#' @examples
#' data(iris)
#' perform_pca(iris)
#'
#' @export
library(data.table)
library(tidyverse)
perform_pca <- function(data, chunk_size = 1000) {
  
  # Identify label column
  label_col <- data %>% select(where(is.character) | where(is.factor)) %>% names() %>% first()
  
  # Split the data into chunks
  chunks <- split(data, ceiling(seq_along(data[, 1])/chunk_size))
  
  # Apply PCA to each chunk of data
  pca_results <- lapply(chunks, function(chunk) {
    
    # Perform PCA on the chunk
    pca <- prcomp(select(chunk, -{{label_col}}), scale. = TRUE)
    
    # Get the proportion of variance explained by each principal component
    var_explained <- round(summary(pca)$importance[2,], 2)
    
    # Get the PC loadings
    pc_loadings <- round(pca$rotation[,1:2], 2)
    colnames(pc_loadings) <- paste0("PC", 1:2)
    
    # Get the PC scores
    pc_scores <- round(pca$x[,1:2], 2)
    colnames(pc_scores) <- paste0("PC", 1:2)
    
    # Create a data table with the PC scores and the labels
    pca_df <- pc_scores %>% as.data.table() %>% 
      mutate(Label = chunk %>% pull({{label_col}}))
    
    # Return the PCA results
    list(pc_loadings = pc_loadings,
         pc_scores = pc_scores,
         var_explained = var_explained,
         pca_df = pca_df)
  })
  
  # Combine the results into a single data table
  pc_loadings <- do.call(rbind, lapply(pca_results, `[[`, "pc_loadings"))
  pc_scores <- do.call(rbind, lapply(pca_results, `[[`, "pc_scores"))
  var_explained <- unlist(lapply(pca_results, `[[`, "var_explained"))
  pca_df <- do.call(rbind, lapply(pca_results, `[[`, "pca_df"))
  
  # Plot the PC scores, colored by the labels
  pca_plot <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Label)) + 
    geom_point() +
    scale_color_brewer(palette = "Set1") +
    ggtitle("PCA Plot") +
    xlab(paste0("PC1 (", var_explained[1]*100, "%)")) +
    ylab(paste0("PC2 (", var_explained[2]*100, "%)"))
  
  # Display the plot
  print(pca_plot)
  
  # Return the PCA results
  return(list(pc_loadings = pc_loadings,
              pc_scores = pc_scores,
              var_explained = var_explained,
              pca_df = pca_df))
}