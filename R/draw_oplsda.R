#' @title Draw OPLS-DA Plot with Statistics
#' @name draw_oplsda
#' @description Creates an OPLS-DA plot with confidence ellipses and statistics
#' for visualizing group differences in multivariate data.
#' 
#' @param df A data frame containing the variables for OPLS-DA analysis
#' @param var_names Optional character vector specifying the names of variables to be used
#' @param Group Optional character vector specifying the levels of groups
#' @param group_labels Optional character vector for custom group labels
#' @param colors Optional vector of colors for groups
#' @param x_limits Optional numeric vector of length 2 specifying x-axis limits
#' @param y_limits Optional numeric vector of length 2 specifying y-axis limits
#' @param show_stats Logical, whether to show model statistics (default: TRUE)
#' @param point_size Numeric, size of points in the plot (default: 3)
#' @param ellipse_alpha Numeric, transparency of confidence ellipses (default: 0.2)
#' @param ellipse_level Numeric, confidence level for ellipses (default: 0.95)
#'
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @import grDevices
#' @import stats
#' @importFrom ropls opls
#' @importFrom utils installed.packages install.packages
#' @importFrom stats rnorm scale
#'
#' @return A list containing:
#' \itemize{
#'   \item plot - ggplot object of the OPLS-DA plot
#'   \item model - OPLS-DA model object
#'   \item scores - data frame of score values
#'   \item vip - data frame of VIP scores
#' }
#' @export
#'
#' @usage draw_oplsda(df, var_names = NULL, Group = NULL, group_labels = NULL,
#'                    colors = NULL, x_limits = NULL, y_limits = NULL,
#'                    show_stats = TRUE, point_size = 3,
#'                    ellipse_alpha = 0.2, ellipse_level = 0.95)
#'
#' @examples
#' \dontrun{
#' # Create example data
#' set.seed(123)
#' n_samples <- 60
#' example_data <- data.frame(
#'   SampleID = paste0("Sample_", 1:n_samples),
#'   Group = rep(c("Control", "Treatment"), each = 30)
#' )
#' 
#' # Add 10 simulated metabolite variables
#' for(i in 1:10) {
#'   example_data[paste0("Metabolite_", i)] <- c(
#'     rnorm(30, mean = 10, sd = 2),  # Control group
#'     rnorm(30, mean = 15, sd = 2)   # Treatment group
#'   )
#' }
#' 
#' # Basic usage
#' result <- draw_oplsda(example_data)
#' 
#' # Custom colors and labels
#' result <- draw_oplsda(example_data,
#'                      colors = c("blue", "red"),
#'                      group_labels = c("Control Group", "Treatment Group"))
#' }
NULL

#' @keywords internal
.load_required_packages <- function() {
  required_packages <- c("ropls", "ggplot2", "dplyr", "scales", "grDevices")
  
  missing_packages <- required_packages[!required_packages %in% utils::installed.packages()[,"Package"]]
  
  if(length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    utils::install.packages(missing_packages)
  }
  
  for(pkg in required_packages) {
    if(!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Package", pkg, "is required but could not be loaded."))
    }
    library(pkg, character.only = TRUE)
  }
}

draw_oplsda <- function(df, var_names = NULL, Group = NULL, group_labels = NULL,
                       colors = NULL,
                       x_limits = NULL,
                       y_limits = NULL,
                       show_stats = TRUE,
                       point_size = 3,
                       ellipse_alpha = 0.2,
                       ellipse_level = 0.95) {
  
  .load_required_packages()
  
  if (!is.data.frame(df)) {
    stop("'df' must be a data frame")
  }
  
  if (!all(c("Group", "SampleID") %in% colnames(df))) {
    stop("Data frame must contain 'Group' and 'SampleID' columns")
  }
  
  if(is.null(Group)) {
    Group <- unique(df$Group)
  }
  
  df <- df[df$Group %in% Group, ]
  
  if (is.null(var_names)) {
    var_names <- setdiff(colnames(df), c("Group", "SampleID"))
  }
  
  if(is.null(colors)) {
    default_colors <- c("#2B8BC34E", "#4E0C664E", "#FF7F0EFF", "#D62728FF", "#9467BDFF")
    colors <- grDevices::colorRampPalette(default_colors)(length(Group))
  }
  
  if(!is.null(group_labels)) {
    if(length(group_labels) != length(Group)) {
      stop("Number of group labels must match number of groups")
    }
    df$Group <- factor(df$Group, levels = Group, labels = group_labels)
  } else {
    df$Group <- factor(df$Group, levels = Group)
  }
  
  X <- as.matrix(df[, var_names])
  Y <- df$Group
  
  oplsda_model <- ropls::opls(X, Y,
                             predI = 1,
                             orthoI = 1,
                             log10L = FALSE,
                             crossvalI = nrow(X),
                             scaleC = "pareto",
                             permI = 200)
  
 plot_data <- data.frame(
    o1 = stats::scale.default(oplsda_model@scoreMN),
    orthogonal = stats::scale.default(oplsda_model@orthoScoreMN),
    Group = Y
  )
  
  t1_weight <- sprintf("%.1f%%", oplsda_model@modelDF[1,1] * 100)
  to1_weight <- sprintf("%.1f%%", oplsda_model@modelDF[2,1] * 100)
  R2X <- sum(oplsda_model@modelDF[1:2,1])
  R2Y <- sum(oplsda_model@modelDF[1:2,3])
  Q2Y <- sum(oplsda_model@modelDF[1:2,6])
  
  subtitle <- sprintf("R2X=%.3f  R2Y=%.3f  Q2Y=%.3f", R2X, R2Y, Q2Y)
  
  p <- ggplot(plot_data, aes(x = .data$o1, 
                            y = .data$orthogonal, 
                            color = .data$Group)) +
    geom_point(aes(shape = .data$Group), size = point_size) +
    stat_ellipse(aes(fill = .data$Group),
                 alpha = ellipse_alpha,
                 level = ellipse_level,
                 geom = "polygon") +
    theme_classic() +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    labs(
      title = "OPLS-DA",
      subtitle = if(show_stats) subtitle else NULL,
      x = paste0("o1 (", t1_weight, ")"),
      y = paste0("Orthogonal (", to1_weight, ")")
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 10)
    )
  
  if(!is.null(x_limits)) {
    p <- p + ggplot2::scale_x_continuous(limits = x_limits)
  }
  if(!is.null(y_limits)) {
    p <- p + ggplot2::scale_y_continuous(limits = y_limits)
  }
  
  return(list(
    plot = p,
    model = oplsda_model,
    scores = plot_data,
    vip = data.frame(
      variable = var_names,
      VIP = oplsda_model@vipVn
    )
  ))
}

# Example usage
if(FALSE) {  # This block won't run automatically
  # Create example data
  set.seed(123)
  n_samples <- 60
  example_data <- data.frame(
    SampleID = paste0("Sample_", 1:n_samples),
    Group = rep(c("Control", "Treatment"), each = 30)
  )
  
  # Add 10 simulated metabolite variables
  for(i in 1:10) {
    example_data[paste0("Metabolite_", i)] <- c(
      rnorm(30, mean = 10, sd = 2),  # Control group
      rnorm(30, mean = 15, sd = 2)   # Treatment group
    )
  }
  
  # Run analysis with default parameters
  result <- draw_oplsda(example_data)
  
  # View results
  print(result$plot)
  print(result$vip)
  
  # Example with custom parameters
  result_custom <- draw_oplsda(example_data,
                              colors = c("blue", "red"),
                              group_labels = c("Control Group", "Treatment Group"),
                              point_size = 4,
                              ellipse_alpha = 0.3)
  
  print(result_custom$plot)
} 