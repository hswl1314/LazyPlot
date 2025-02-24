#' Perform PCA Analysis and Get Coordinates
#'
#' This function performs Principal Component Analysis (PCA) on the input data
#' and returns the PCA coordinates along with additional information.
#'
#' @param df A data frame containing the input data. Must have 'Group' and 'SampleID' columns.
#' @param var_names Optional vector of variable names to use for PCA. If NULL, all numeric columns except 'Group' and 'SampleID' are used.
#' @param Group Optional vector of group names to include in the analysis. If NULL, all groups are used.
#'
#' @return A list containing:
#'   \item{coordinates}{Data frame with PCA coordinates for each sample}
#'   \item{variance}{Vector of explained variance for each principal component}
#'   \item{loadings}{Matrix of variable loadings}
#'   \item{adonis}{Results of PERMANOVA analysis}
#'
#' @importFrom stats prcomp
#' @importFrom vegan adonis2
#' @export
#'
#' @examples
#' # Create example data
#' set.seed(123)
#' example_data <- data.frame(
#'   SampleID = paste0("Sample_", 1:30),
#'   Group = rep(c("Control", "Treatment1", "Treatment2"), each = 10),
#'   Var1 = rnorm(30, mean = 10, sd = 2),
#'   Var2 = rnorm(30, mean = 15, sd = 3),
#'   Var3 = rnorm(30, mean = 5, sd = 1)
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
#'   Group = c("Control", "Treatment1")
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
  
  # Check if there are at least 2 groups in the input data
  if(length(unique(df$Group)) < 2) {
    stop("Input data must contain at least 2 groups")
  }
  
  # If Group is NULL, use all groups in the data
  if(is.null(Group)) {
    Group <- unique(df$Group)
  } else {
    # Validate that specified groups exist in the data
    if(!all(Group %in% df$Group)) {
      stop("Some specified groups do not exist in the data")
    }
    # Check if at least 2 groups are specified
    if(length(Group) < 2) {
      stop("At least 2 groups must be specified")
    }
  }
  
  # Filter data by specified groups
  df <- df[df$Group %in% Group, ]
  
  # If var_names is NULL, use all numeric columns except Group and SampleID
  if (is.null(var_names)) {
    var_names <- colnames(df)[sapply(df, is.numeric)]
  } else {
    # Validate that specified variables exist in the data
    if (!all(var_names %in% colnames(df))) {
      stop("Some specified variables do not exist in the data")
    }
  }
  
  # Extract numeric data for PCA
  pca_data <- df[, var_names, drop = FALSE]
  
  # Check if there are any NA values
  if (any(is.na(pca_data))) {
    stop("Data contains NA values")
  }
  
  # Perform PCA
  pca_result <- prcomp(pca_data, scale. = TRUE)
  
  # Calculate explained variance
  explained_var <- (pca_result$sdev^2) / sum(pca_result$sdev^2) * 100
  
  # Get PCA coordinates
  coordinates <- as.data.frame(pca_result$x)
  coordinates$SampleID <- df$SampleID
  coordinates$Group <- df$Group
  
  # Perform PERMANOVA
  dist_matrix <- dist(pca_data, method = "euclidean")
  adonis_result <- adonis2(dist_matrix ~ Group, data = df)
  
  # Return results
  return(list(
    coordinates = coordinates,
    variance = explained_var,
    loadings = pca_result$rotation,
    adonis = adonis_result
  ))
}