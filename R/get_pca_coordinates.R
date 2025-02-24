#' @title Extract PCA Coordinates
#' @name get_pca_coordinates
#' @description Extracts PCA coordinates and related information from multivariate data
#'
#' @param df A data frame containing the variables for PCA analysis
#' @param var_names Optional character vector specifying the names of variables to be used in PCA. 
#'                 If NULL, all numeric columns except 'Group' and 'SampleID' will be used.
#' @param Group Optional character vector specifying the levels of groups. 
#'             If NULL (default), all groups in the data will be used.
#'
#' @return A list containing:
#'         - coordinates: Data frame with sample coordinates (PC1, PC2, PC3) and group information
#'         - variance: Vector with explained variance for PC1, PC2 and PC3
#'         - loadings: Data frame with variable loadings
#'         - adonis: PERMANOVA test results
#' @export
#'
#' @importFrom vegan rda adonis2 vegdist
#' @importFrom dplyr select all_of
#'
#' @examples
#' # Create example data
#' set.seed(123)
#' example_data <- data.frame(
#'   SampleID = paste0("Sample_", 1:30),
#'   Group = rep(c("Group1", "Group2"), each = 15),
#'   Var1 = c(rnorm(15, mean = 10, sd = 2), rnorm(15, mean = 12, sd = 2)),
#'   Var2 = c(rnorm(15, mean = 15, sd = 3), rnorm(15, mean = 18, sd = 3)),
#'   Var3 = c(rnorm(15, mean = 5, sd = 1), rnorm(15, mean = 7, sd = 1))
#' )
#' 
#' # Run PCA analysis with all variables
#' pca_results <- get_pca_coordinates(example_data)
#' 
#' # Access results
#' head(pca_results$coordinates)  # View sample coordinates
#' pca_results$variance          # View explained variance
#' pca_results$loadings         # View variable loadings
#' pca_results$adonis          # View PERMANOVA results
#' 
#' # Run PCA with specific variables and groups
#' pca_results2 <- get_pca_coordinates(
#'   df = example_data,
#'   var_names = c("Var1", "Var2"),
#'   Group = c("Group1", "Group2")
#' )
#'
get_pca_coordinates <- function(df, var_names = NULL, Group = NULL) {
  # Input validation
  if (!is.data.frame(df)) {
    stop("'df' must be a data frame")
  }
  
  if (!all(c("Group", "SampleID") %in% colnames(df))) {
    stop("Data frame must contain 'Group' and 'SampleID' columns")
  }
  
  # If Group is NULL, use all groups in the data
  if(is.null(Group)) {
    Group <- unique(df$Group)
  }
  
  # Filter data to include only specified groups
  df <- df[df$Group %in% Group, ]
  
  # Check if data is empty after filtering
  if(nrow(df) == 0) {
    stop("No valid data after filtering groups")
  }
  
  # Automatically get variable names if not specified
  if (is.null(var_names)) {
    var_names <- setdiff(colnames(df), c("Group", "SampleID"))
  }
  
  # Validate that selected columns are numeric
  non_numeric_cols <- names(which(sapply(df[var_names], function(x) !is.numeric(x))))
  if (length(non_numeric_cols) > 0) {
    stop("The following variables are not numeric: ", paste(non_numeric_cols, collapse = ", "))
  }
  
  # Perform PCA analysis
  pca_result <- summary(vegan::rda(dplyr::select(df, dplyr::all_of(var_names)), scale=T))
  
  # Determine number of PCs available
  n_pcs <- min(3, ncol(pca_result$sites))
  
  # Extract sample coordinates
  coordinates <- data.frame(
    SampleID = df$SampleID,
    Group = df$Group,
    PC1 = pca_result$sites[,1]
  )
  if (n_pcs >= 2) coordinates$PC2 = pca_result$sites[,2]
  if (n_pcs >= 3) coordinates$PC3 = pca_result$sites[,3]
  
  # Extract explained variance
  variance <- pca_result$cont$importance[2,1:n_pcs] * 100
  names(variance) <- paste0("PC", 1:n_pcs)
  
  # Extract variable loadings
  loadings <- data.frame(
    Variable = rownames(pca_result$species),
    PC1 = pca_result$species[,1]
  )
  if (n_pcs >= 2) loadings$PC2 = pca_result$species[,2]
  if (n_pcs >= 3) loadings$PC3 = pca_result$species[,3]
  
  # Perform PERMANOVA analysis
  adonis_result <- vegan::adonis2(
    vegan::vegdist(dplyr::select(df, dplyr::all_of(var_names)), method="bray") ~ Group, 
    data=df
  )
  
  # Return results as a list
  return(list(
    coordinates = coordinates,
    variance = variance,
    loadings = loadings,
    adonis = adonis_result
  ))
}