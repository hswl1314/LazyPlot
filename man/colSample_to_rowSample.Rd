% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colSample_to_rowSample.R
\name{colSample_to_rowSample}
\alias{colSample_to_rowSample}
\title{Convert a gene expression matrix from column-sample to row-sample format}
\usage{
colSample_to_rowSample(data, id_col = "SampleID")
}
\arguments{
\item{data}{A data frame with genes as rows and samples as columns}

\item{id_col}{A character string specifying the name for the ID column (default: "SampleID")}
}
\value{
A data frame with samples as rows and genes as columns, plus an ID column
}
\description{
Transforms a data frame where samples are columns into a format
where samples are rows, with an additional ID column.
}
\details{
Convert Sample Columns to Sample Rows
}
\examples{
A <- data.frame(
  Sample1 = c(10, 15, 20),
  Sample2 = c(12, 18, 22),
  Sample3 = c(14, 16, 24)
)
rownames(A) <- c("Gene1", "Gene2", "Gene3")
colSample_to_rowSample(A)
colSample_to_rowSample(A, id_col = "Sample_Name")
}
