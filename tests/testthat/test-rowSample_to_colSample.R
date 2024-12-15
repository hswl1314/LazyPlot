test_that("rowSample_to_colSample basic functionality works", {
  # Create test data
  test_data <- data.frame(
    SampleID = c("Sample1", "Sample2"),
    Gene1 = c(10, 12),
    Gene2 = c(15, 18)
  )
  
  # Run function
  result <- rowSample_to_colSample(test_data)
  
  # Basic structure tests
  expect_equal(ncol(result), 2)  # Should have 2 columns (samples)
  expect_equal(nrow(result), 2)  # Should have 2 rows (genes)
  expect_equal(colnames(result), c("Sample1", "Sample2"))
  expect_true(all(c("Gene1", "Gene2") %in% rownames(result)))
  
  # Check data values
  expect_equal(as.numeric(result["Gene1", ]), c(10, 12))
  expect_equal(as.numeric(result["Gene2", ]), c(15, 18))
})

test_that("rowSample_to_colSample works with custom id_col", {
  # Create test data with custom ID column
  test_data <- data.frame(
    CustomID = c("Sample1", "Sample2"),
    Gene1 = c(10, 12),
    Gene2 = c(15, 18)
  )
  
  # Run function with custom id_col
  result <- rowSample_to_colSample(test_data, id_col = "CustomID")
  
  # Check structure and values
  expect_equal(colnames(result), c("Sample1", "Sample2"))
  expect_equal(rownames(result), c("Gene1", "Gene2"))
})

test_that("rowSample_to_colSample handles errors correctly", {
  # Test invalid input
  expect_error(rowSample_to_colSample(matrix(1:4, 2, 2)))
  
  # Test missing ID column
  test_data <- data.frame(
    WrongID = c("Sample1", "Sample2"),
    Gene1 = c(10, 12)
  )
  expect_error(rowSample_to_colSample(test_data))
  
  # Test NULL input
  expect_error(rowSample_to_colSample(NULL))
  
  # Test empty data frame
  expect_error(rowSample_to_colSample(data.frame()))
})

test_that("rowSample_to_colSample handles special cases", {
  # Test single gene case
  test_data <- data.frame(
    SampleID = c("Sample1", "Sample2"),
    Gene1 = c(10, 12)
  )
  result <- rowSample_to_colSample(test_data)
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 2)
  
  # Test single sample case
  test_data <- data.frame(
    SampleID = "Sample1",
    Gene1 = 10,
    Gene2 = 15
  )
  result <- rowSample_to_colSample(test_data)
  expect_equal(ncol(result), 1)
  expect_equal(nrow(result), 2)
})

test_that("rowSample_to_colSample maintains data types", {
  # Test with different data types
  test_data <- data.frame(
    SampleID = c("Sample1", "Sample2"),
    Gene1 = c(10.5, 12.3),  # numeric
    Gene2 = c(15L, 18L)     # integer
  )
  
  result <- rowSample_to_colSample(test_data)
  
  # Check if numeric values remain numeric
  expect_true(is.numeric(result["Gene1", ]))
  expect_true(is.numeric(result["Gene2", ]))
})

test_that("rowSample_to_colSample handles NA values", {
  # Test with NA values
  test_data <- data.frame(
    SampleID = c("Sample1", "Sample2"),
    Gene1 = c(10, NA),
    Gene2 = c(NA, 18)
  )
  
  result <- rowSample_to_colSample(test_data)
  
  # Check NA handling
  expect_true(is.na(result["Gene1", "Sample2"]))
  expect_true(is.na(result["Gene2", "Sample1"]))
})

test_that("rowSample_to_colSample preserves row order", {
  # Test row order preservation
  test_data <- data.frame(
    SampleID = c("Sample2", "Sample1", "Sample3"),  # Deliberately unordered
    Gene1 = c(12, 10, 14),
    Gene2 = c(18, 15, 21)
  )
  
  result <- rowSample_to_colSample(test_data)
  
  # Check if order is preserved
  expect_equal(colnames(result), c("Sample2", "Sample1", "Sample3"))
})