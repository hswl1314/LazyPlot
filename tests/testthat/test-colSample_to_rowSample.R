test_that("colSample_to_rowSample basic functionality works", {
  # Create test data
  test_data <- data.frame(
    Sample1 = c(10, 15),
    Sample2 = c(12, 18)
  )
  rownames(test_data) <- c("Gene1", "Gene2")
  
  # Run function with default id_col
  result <- colSample_to_rowSample(test_data)
  
  # Basic structure tests
  expect_equal(nrow(result), 2)  # Should have 2 rows (samples)
  expect_equal(ncol(result), 3)  # Should have 3 columns (SampleID + 2 genes)
  expect_equal(colnames(result)[1], "SampleID")  # First column should be SampleID
  expect_true(all(c("Gene1", "Gene2") %in% colnames(result)))  # Should have gene columns
  expect_equal(result$SampleID, c("Sample1", "Sample2"))  # Check sample names
})

test_that("colSample_to_rowSample works with custom id_col", {
  # Create test data
  test_data <- data.frame(
    Sample1 = c(10, 15),
    Sample2 = c(12, 18)
  )
  rownames(test_data) <- c("Gene1", "Gene2")
  
  # Run function with custom id_col
  result <- colSample_to_rowSample(test_data, id_col = "Sample_Name")
  
  # Check custom id_col
  expect_equal(colnames(result)[1], "Sample_Name")
  expect_equal(result$Sample_Name, c("Sample1", "Sample2"))
})

test_that("colSample_to_rowSample preserves data values", {
  # Create test data
  test_data <- data.frame(
    Sample1 = c(10, 15),
    Sample2 = c(12, 18)
  )
  rownames(test_data) <- c("Gene1", "Gene2")
  
  # Run function
  result <- colSample_to_rowSample(test_data)
  
  # Check if values are preserved
  expect_equal(as.numeric(result[1, c("Gene1", "Gene2")]), c(10, 15))
  expect_equal(as.numeric(result[2, c("Gene1", "Gene2")]), c(12, 18))
})

test_that("colSample_to_rowSample handles errors correctly", {
  # Test non-data.frame input
  expect_error(colSample_to_rowSample(matrix(1:4, 2, 2)))
  
  # Test data.frame without rownames
  test_data <- data.frame(
    Sample1 = c(10, 15),
    Sample2 = c(12, 18)
  )
  expect_warning(colSample_to_rowSample(test_data))
})

test_that("colSample_to_rowSample handles special cases", {
  # Test single column data
  test_data <- data.frame(Sample1 = c(10, 15))
  rownames(test_data) <- c("Gene1", "Gene2")
  result <- colSample_to_rowSample(test_data)
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 3)  # SampleID + 2 genes
  
  # Test single row data
  test_data <- data.frame(Sample1 = 10, Sample2 = 12)
  rownames(test_data) <- "Gene1"
  result <- colSample_to_rowSample(test_data)
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 2)  # SampleID + 1 gene
})

test_that("colSample_to_rowSample maintains data types", {
  # Create test data with different data types
  test_data <- data.frame(
    Sample1 = c(10.5, 15.7),
    Sample2 = c(12.3, 18.9)
  )
  rownames(test_data) <- c("Gene1", "Gene2")
  
  # Run function
  result <- colSample_to_rowSample(test_data)
  
  # Check if numeric values remain numeric
  expect_true(is.numeric(result$Gene1))
  expect_true(is.numeric(result$Gene2))
})