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




# install.packages("devtools")

```r
devtools::install_github("hswl1314/LazyPlot")
```

## Usage

### Basic PCA Plot
```r
library(LazyPlot)
```

# Create example data
```r
set.seed(123)
example_data <- data.frame(
SampleID = paste0("Sample_", 1:30),
Group = rep(c("Control", "Treatment"), each = 15),
Var1 = c(rnorm(15, mean = 10, sd = 2), rnorm(15, mean = 12, sd = 2)),
Var2 = c(rnorm(15, mean = 15, sd = 3), rnorm(15, mean = 18, sd = 3)),
Var3 = c(rnorm(15, mean = 5, sd = 1), rnorm(15, mean = 7, sd = 1))
)
```

# Create PCA plot
```r
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


