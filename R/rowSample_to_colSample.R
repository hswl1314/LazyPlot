#' Convert Sample Rows to Sample Columns
#' 
#' @title Convert a gene expression matrix from row-sample to column-sample format
#' @description Transforms a data frame where samples are rows back to a format
#'     where samples are columns.
#' 
#' @param data A data frame with samples as rows and genes as columns, plus an ID column
#' @param id_col A character string specifying the name of the ID column (default: "SampleID")
#' @return A data frame with genes as rows and samples as columns
#' @export
#' @importFrom magrittr %>%
#' @importFrom tibble column_to_rownames
#' 
#' @examples
#' B <- data.frame(
#'   SampleID = c("Sample1", "Sample2", "Sample3"),
#'   Gene1 = c(10, 12, 14),
#'   Gene2 = c(15, 18, 16),
#'   Gene3 = c(20, 22, 24)
#' )
#' rowSample_to_colSample(B)
rowSample_to_colSample <- function(data, id_col = "SampleID") {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  if (!(id_col %in% colnames(data))) {
    stop(sprintf("Column '%s' not found in the data frame", id_col))
  }
  
  # Perform the transformation
  transformed_data <- data %>%
    column_to_rownames(id_col) %>%
    t() %>%
    as.data.frame()
  
  return(transformed_data)
}