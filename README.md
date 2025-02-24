# LazyPlot

[![CRAN status](https://www.r-pkg.org/badges/version/LazyPlot)](https://CRAN.R-project.org/package=LazyPlot)

LazyPlot is an R package that provides easy-to-use functions for Principal Component Analysis (PCA) visualization and data transformation. It features comprehensive PCA plots with integrated density distributions and statistical analysis.

## Features

- Comprehensive PCA visualization with density plots and box plots
- Automatic PERMANOVA statistical testing
- Flexible data transformation between row-sample and column-sample formats
- Customizable plot aesthetics including colors, point sizes, and confidence ellipses
- Built-in PCA range checking functionality

## Installation

You can install LazyPlot from GitHub using:



```r
# install.packages("devtools")
devtools::install_github("hswl1314/LazyPlot")
```

## Usage

### Basic PCA Plot
```r
library(LazyPlot)
```

```r
# Create example data
set.seed(123)
example_data <- data.frame(
SampleID = paste0("Sample_", 1:30),
Group = rep(c("Control", "Treatment"), each = 15),
Var1 = c(rnorm(15, mean = 10, sd = 2), rnorm(15, mean = 12, sd = 2)),
Var2 = c(rnorm(15, mean = 15, sd = 3), rnorm(15, mean = 18, sd = 3)),
Var3 = c(rnorm(15, mean = 5, sd = 1), rnorm(15, mean = 7, sd = 1))
)
```

```r
# Create PCA plot
draw_pca(example_data)
```

### Customized PCA Plot
```r
Customize plot appearance
draw_pca(
df = example_data,
colors = c("#1F77B4", "#FF7F0E"),
point_size = 4,
top_n_vars = 3,
ellipse_alpha = 0.3,
show_stats = TRUE
)
```

### Check PCA Ranges
```r
# Check PCA data ranges before plotting
ranges <- pca_check(example_data)
```

### Data Format Transformation
```r
Convert from column-sample to row-sample format
gene_data <- data.frame(
Sample1 = c(10, 15, 20),
Sample2 = c(12, 18, 22),
Sample3 = c(14, 16, 24)
)
rownames(gene_data) <- c("Gene1", "Gene2", "Gene3")
row_sample_data <- colSample_to_rowSample(gene_data)
```

```r
# Convert back to column-sample format
col_sample_data <- rowSample_to_colSample(row_sample_data)
```

## Getting PCA Coordinates
```r
# Get PCA coordinates and analysis results
pca_results <- get_pca_coordinates(example_data)
```

# Get PCA coordinates and analysis results
```r
pca_results <- get_pca_coordinates(example_data)
# Access results
head(pca_results$coordinates) # Sample coordinates
pca_results$variance # Explained variance
pca_results$loadings # Variable loadings
pca_results$adonis # PERMANOVA results
```
