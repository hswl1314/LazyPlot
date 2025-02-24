 #' @title Draw PLS-DA Plot with Statistics
#' @name draw_plsda
#' @description Creates a PLS-DA plot with confidence ellipses and statistics
#' for visualizing multiple group differences in multivariate data.
#'
#' @param df A data frame containing the variables for PLS-DA analysis
#' @param var_names Optional character vector specifying the names of variables to be used. 
#'                 If NULL, all numeric columns except 'Group' and 'SampleID' will be used.
#' @param Group Optional character vector specifying the levels of groups. 
#'             If NULL (default), all groups in the data will be used.
#' @param group_labels Optional character vector for custom group labels
#' @param colors Optional vector of colors for groups
#' @param x_limits Optional numeric vector of length 2 specifying x-axis limits
#' @param y_limits Optional numeric vector of length 2 specifying y-axis limits
#' @param show_stats Logical, whether to show model statistics (default: TRUE)
#' @param point_size Numeric, size of points in the plot (default: 3)
#' @param ellipse_alpha Numeric, transparency of confidence ellipses (default: 0.2)
#' @param ellipse_level Numeric, confidence level for ellipses (default: 0.95)
#'
#' @importFrom ggplot2 ggplot aes geom_point stat_ellipse theme_classic scale_fill_manual scale_color_manual labs theme element_text scale_x_continuous scale_y_continuous
#' @importFrom grDevices colorRampPalette
#' @importFrom stats as.formula
#' @importFrom rlang .data
#'
#' @return A list containing the ggplot object and model results
#' @export
draw_plsda <- function(df, var_names = NULL, Group = NULL, group_labels = NULL,
                      colors = NULL,
                      x_limits = NULL,
                      y_limits = NULL,
                      show_stats = TRUE,
                      point_size = 3,
                      ellipse_alpha = 0.2,
                      ellipse_level = 0.95) {
  
  # Input validation
  if (!is.data.frame(df)) {
    stop("'df' must be a data frame")
  }
  
  if (!all(c("Group", "SampleID") %in% colnames(df))) {
    stop("Data frame must contain 'Group' and 'SampleID' columns")
  }
  
  # If Group is NULL, use all groups in the data
  if(is.null(Group)) {
    Group <- unique(df$Group)
  }
  
  # Filter data to include only specified groups
  df <- df[df$Group %in% Group, ]
  
  # Automatically get variable names if not specified
  if (is.null(var_names)) {
    var_names <- names(df)[sapply(df, is.numeric)]  # Only get numeric columns
    var_names <- setdiff(var_names, c("Group", "SampleID"))
  }

  # Validate that all var_names exist in the dataframe
  if (!all(var_names %in% colnames(df))) {
    stop("Some specified variable names not found in data frame")
  }
  
  # Auto-generate colors if not specified
  if(is.null(colors)) {
    default_colors <- c("#2B8BC34E", "#4E0C664E", "#FF7F0EFF", "#D62728FF", "#9467BDFF")
    colors <- colorRampPalette(default_colors)(length(Group))
  }
  
  # Process group labels
  if(!is.null(group_labels)) {
    if(length(group_labels) != length(Group)) {
      stop("Number of group labels must match number of groups")
    }
    df$Group <- factor(df$Group, levels = Group, labels = group_labels)
  } else {
    df$Group <- factor(df$Group, levels = Group)
  }
  
  # Prepare data matrix for PLS-DA
  X <- as.matrix(df[, var_names])
  Y <- df$Group
  
  # Perform PLS-DA
  plsda_model <- ropls::opls(X, Y,
                            predI = 2,     # Use 2 predictive components
                            orthoI = 0,    # No orthogonal components for PLS-DA
                            log10L = FALSE,
                            crossvalI = nrow(X),
                            scaleC = "pareto",
                            permI = 200)
  
  # Get model statistics
  t1_weight <- sprintf("%.1f%%", plsda_model@modelDF[1,1] * 100)
  t2_weight <- sprintf("%.1f%%", plsda_model@modelDF[2,1] * 100)
  R2X <- sum(plsda_model@modelDF[,1])
  R2Y <- sum(plsda_model@modelDF[,3])
  Q2Y <- sum(plsda_model@modelDF[,6])
  
  subtitle <- sprintf("R2X=%.3f  R2Y=%.3f  Q2Y=%.3f", R2X, R2Y, Q2Y)
  
  # Extract scores and create plot data
  plot_data <- data.frame(
    t1 = as.vector(plsda_model@scoreMN[,1]),
    t2 = as.vector(plsda_model@scoreMN[,2]),
    Group = Y
  )
  
  # Create plot
  p <- ggplot(plot_data, aes(x = .data$t1, 
                            y = .data$t2, 
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
      title = "PLS-DA",
      subtitle = if(show_stats) subtitle else NULL,
      x = paste0("t1 (", t1_weight, ")"),
      y = paste0("t2 (", t2_weight, ")")
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 10)
    )
  
  # Set axis limits if provided
  if(!is.null(x_limits)) {
    p <- p + scale_x_continuous(limits = x_limits)
  }
  if(!is.null(y_limits)) {
    p <- p + scale_y_continuous(limits = y_limits)
  }
  
  # Return results
  return(list(
    plot = p,
    model = plsda_model,
    scores = plot_data,
    vip = data.frame(
      variable = var_names,
      VIP = plsda_model@vipVn
    )
  ))
}

#' @title Create Example Data for PLS-DA Analysis
#' @name create_example_data
#' @description Creates example data with multiple groups for demonstrating PLS-DA analysis
#'
#' @param n_samples Number of samples per group to generate (default: 30)
#' @return A data frame containing example data
#' @importFrom stats rnorm
#' @export
create_example_data <- function(n_samples = 30) {
  set.seed(123)  # Set random seed for reproducibility
  
  # Create data for three groups
  group1 <- data.frame(
    SampleID = paste0("Sample_A", 1:n_samples),
    Group = "Control",
    Variable1 = rnorm(n_samples, mean = 5, sd = 1),
    Variable2 = rnorm(n_samples, mean = 10, sd = 2),
    Variable3 = rnorm(n_samples, mean = 15, sd = 1.5)
  )
  
  group2 <- data.frame(
    SampleID = paste0("Sample_B", 1:n_samples),
    Group = "Treatment1",
    Variable1 = rnorm(n_samples, mean = 7, sd = 1),
    Variable2 = rnorm(n_samples, mean = 13, sd = 2),
    Variable3 = rnorm(n_samples, mean = 18, sd = 1.5)
  )
  
  group3 <- data.frame(
    SampleID = paste0("Sample_C", 1:n_samples),
    Group = "Treatment2",
    Variable1 = rnorm(n_samples, mean = 3, sd = 1),
    Variable2 = rnorm(n_samples, mean = 8, sd = 2),
    Variable3 = rnorm(n_samples, mean = 12, sd = 1.5)
  )
  
  # Combine data
  example_data <- rbind(group1, group2, group3)
  return(example_data)
}

# Example usage
if(FALSE) {  # Set to FALSE to prevent accidental execution
  # Create example data
  example_data <- create_example_data()
  
  # Basic usage
  result <- draw_plsda(example_data)
  print(result$plot)
  
  # Custom usage example
  result_custom <- draw_plsda(
    df = example_data,
    var_names = c("Variable1", "Variable2", "Variable3"),
    group_labels = c("Control Group", "Treatment Group 1", "Treatment Group 2"),
    colors = c("#1f77b4", "#ff7f0e", "#2ca02c"),
    point_size = 4,
    ellipse_alpha = 0.3
  )
  print(result_custom$plot)
  
  # View VIP values
  print(result$vip)
}