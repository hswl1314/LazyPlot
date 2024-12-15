test_that("draw_pca works with basic input", {
  # Create test data
  set.seed(123)
  df_test <- data.frame(
    SampleID = paste0("Sample_", 1:20),
    Group = rep(c("A", "B"), each = 10),
    Var1 = rnorm(20),
    Var2 = rnorm(20),
    Var3 = rnorm(20)
  )
  
  # Test basic functionality
  p <- draw_pca(df_test)
  expect_s3_class(p, "ggplot")
})

test_that("draw_pca handles custom parameters", {
  set.seed(123)
  df_test <- data.frame(
    SampleID = paste0("Sample_", 1:30),
    Group = rep(c("A", "B", "C"), each = 10),
    Var1 = rnorm(30),
    Var2 = rnorm(30)
  )
  
  # Test with custom parameters
  p <- draw_pca(
    df = df_test,
    Group = c("A", "B"),
    colors = c("red", "blue"),
    point_size = 4,
    show_stats = FALSE,
    top_n_vars = 0
  )
  
  expect_s3_class(p, "ggplot")
})

test_that("draw_pca handles errors appropriately", {
  # Test with non-numeric variables
  df_test <- data.frame(
    SampleID = paste0("Sample_", 1:10),
    Group = rep(c("A", "B"), each = 5),
    Var1 = letters[1:10],
    Var2 = rnorm(10)
  )
  
  expect_error(draw_pca(df_test))
  
  # Test with invalid group specification
  df_test$Var1 <- rnorm(10)
  expect_error(draw_pca(df_test, Group = c("C", "D")))
})