#' Convert Sample Columns to Sample Rows
#' 
#' @title Convert a gene expression matrix from column-sample to row-sample format
#' @description Transforms a data frame where samples are columns into a format
#'     where samples are rows, with an additional ID column.
#' 
#' @param data A data frame with genes as rows and samples as columns
#' @param id_col A character string specifying the name for the ID column (default: "SampleID")
#' @return A data frame with samples as rows and genes as columns, plus an ID column
#' @export
#' @importFrom magrittr %>%
#' @importFrom tibble rownames_to_column
#' 
#' @examples
#' A <- data.frame(
#'   Sample1 = c(10, 15, 20),
#'   Sample2 = c(12, 18, 22),
#'   Sample3 = c(14, 16, 24)
#' )
#' rownames(A) <- c("Gene1", "Gene2", "Gene3")
#' colSample_to_rowSample(A)
#' colSample_to_rowSample(A, id_col = "Sample_Name")
colSample_to_rowSample <- function(data, id_col = "SampleID") {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  if (is.null(rownames(data))) {
    warning("Input data frame has no rownames")
  }
  
  transformed_data <- data %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column(id_col)
  
  return(transformed_data)
}