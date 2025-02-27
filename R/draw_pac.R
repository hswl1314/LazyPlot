#' @title Draw PCA Plot with Density and Box Plots
#' @name draw_pca
#' @description Creates a comprehensive PCA plot with density distributions and box plots
#' for visualizing group differences in multivariate data.
#'
#' @param df A data frame containing the variables for PCA analysis
#' @param var_names Optional character vector specifying the names of variables to be used in PCA. 
#'                 If NULL, all numeric columns except 'Group' and 'SampleID' will be used.
#' @param Group Optional character vector specifying the levels of groups. 
#'             If NULL (default), all groups in the data will be used.
#' @param group_labels Optional character vector for custom group labels
#' @param colors Optional vector of colors for groups
#' @param x_limits Optional numeric vector of length 2 specifying x-axis limits
#' @param y_limits Optional numeric vector of length 2 specifying y-axis limits
#' @param show_stats Logical, whether to show PERMANOVA results (default: TRUE)
#' @param point_size Numeric, size of points in the plot (default: 3)
#' @param top_n_vars Numeric, number of top contributing variables to show (default: 5, 0 for no variables)
#' @param ellipse_alpha Numeric, transparency of confidence ellipses (default: 0.2)
#' @param ellipse_level Numeric, confidence level for ellipses (default: 0.95)
#'
#' @return A ggplot object containing the combined PCA plot
#' @export
#'
#' @importFrom ggplot2 ggplot geom_point geom_segment geom_hline aes
#' @importFrom ggplot2 geom_vline geom_path geom_polygon scale_color_manual
#' @importFrom ggplot2 scale_fill_manual labs scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 theme theme_minimal element_blank element_line annotate
#' @importFrom ggplot2 geom_density geom_boxplot coord_flip xlab ylab unit arrow theme_classic
#' @importFrom dplyr mutate select filter arrange row_number all_of desc
#' @importFrom magrittr %>%
#' @importFrom vegan rda adonis2 vegdist
#' @importFrom ggpubr theme_classic2 stat_compare_means get_legend
#' @importFrom ggrepel geom_text_repel
#' @importFrom patchwork plot_layout
#' @importFrom ellipse ellipse
#' @importFrom grDevices colorRampPalette
#' @importFrom stats cov
#' @importFrom utils combn
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic usage with all groups
#' set.seed(123)
#' df_example <- data.frame(
#'   SampleID = paste0("Sample_", 1:60),
#'   Group = rep(c("Control", "Treatment1", "Treatment2"), each = 20),
#'   Var1 = rnorm(60, mean = 10, sd = 2),
#'   Var2 = rnorm(60, mean = 15, sd = 3),
#'   Var3 = rnorm(60, mean = 20, sd = 4)
#' )
#' 
#' # Use all groups automatically
#' pca_plot1 <- draw_pca(df = df_example)
#'
#' # Example 2: Specify groups and customize appearance
#' pca_plot2 <- draw_pca(
#'   df = df_example,
#'   Group = c("Control", "Treatment1"),  # Use only two groups
#'   colors = c("#1F77B4", "#FF7F0E"),
#'   point_size = 4,
#'   show_stats = TRUE
#' )
#' }

# Add global variables declaration
utils::globalVariables(c("PC1", "PC2", "contrib", "func", "group"))

draw_pca <- function(df, var_names = NULL, Group = NULL, group_labels = NULL, 
                    colors = NULL,
                    x_limits = NULL, 
                    y_limits = NULL,
                    show_stats = TRUE,
                    point_size = 3,
                    top_n_vars = 5,
                    ellipse_alpha = 0.2,
                    ellipse_level = 0.95) {
  
  # Input validation
  if (!is.data.frame(df)) {
    stop("'df' must be a data frame")
  }
  
  if (!all(c("Group", "SampleID") %in% colnames(df))) {
    stop("Data frame must contain 'Group' and 'SampleID' columns")
  }
  
  if (!is.numeric(point_size) || point_size <= 0) {
    stop("'point_size' must be a positive number")
  }
  
  if (!is.numeric(top_n_vars) || top_n_vars < 0) {
    stop("'top_n_vars' must be a non-negative number")
  }
  
  if (!is.numeric(ellipse_alpha) || ellipse_alpha < 0 || ellipse_alpha > 1) {
    stop("'ellipse_alpha' must be between 0 and 1")
  }
  
  if (!is.numeric(ellipse_level) || ellipse_level <= 0 || ellipse_level >= 1) {
    stop("'ellipse_level' must be between 0 and 1")
  }
  
  # If Group is NULL, use all groups in the data
  if(is.null(Group)) {
    Group <- unique(df$Group)
  }
  
  # Filter data to include only specified groups
  df <- df[df$Group %in% Group, ]
  
  # Check if data is empty after filtering
  if(nrow(df) == 0) {
    stop("No valid data after filtering groups")
  }
  
  # Automatically get variable names if not specified
  if (is.null(var_names)) {
    var_names <- setdiff(colnames(df), c("Group", "SampleID"))
  }
  
  # Validate that selected columns are numeric
  non_numeric_cols <- names(which(sapply(df[var_names], function(x) !is.numeric(x))))
  if (length(non_numeric_cols) > 0) {
    stop("The following variables are not numeric: ", paste(non_numeric_cols, collapse = ", "))
  }
  
  # Auto-generate colors if not specified
  if(is.null(colors)) {
    default_colors <- c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF", "#D62728FF", "#9467BDFF")
    colors <- grDevices::colorRampPalette(default_colors)(length(Group))
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
  
  # Perform PCA analysis
  pca_result <- summary(vegan::rda(dplyr::select(df, dplyr::all_of(var_names)), scale=T))
  
  # Extract sample coordinates
  sample_coords <- data.frame(pca_result$sites) %>%
    dplyr::mutate(Group = df$Group)
  
  # Calculate data ranges and set plot limits
  x_range <- range(sample_coords$PC1)
  y_range <- range(sample_coords$PC2)
  
  if(is.null(x_limits)) {
    x_margin <- diff(x_range) * 0.2
    x_limits <- c(x_range[1] - x_margin, x_range[2] + x_margin)
  }
  if(is.null(y_limits)) {
    y_margin <- diff(y_range) * 0.2
    y_limits <- c(y_range[1] - y_margin, y_range[2] + y_margin)
  }
  
  # Extract and process species scores
  species_scores <- data.frame(pca_result$species) %>%
    dplyr::mutate(func = factor(rownames(pca_result$species), 
                               levels = var_names,
                               labels = var_names))
  
  # Calculate variable contributions
  var_contrib <- data.frame(
    var = var_names,
    contrib = sqrt(species_scores$PC1^2 + species_scores$PC2^2)
  ) %>%
    dplyr::arrange(dplyr::desc(contrib)) %>%
    dplyr::mutate(rank = dplyr::row_number())
  
  # Filter top contributing variables
  if(top_n_vars > 0) {
    top_vars <- var_contrib$var[1:min(top_n_vars, nrow(var_contrib))]
    species_filtered <- species_scores %>%
      dplyr::filter(func %in% top_vars)
  }
  
  # Perform PERMANOVA analysis
  adonis_result <- vegan::adonis2(vegan::vegdist(dplyr::select(df, dplyr::all_of(var_names)), 
                                                method="bray") ~ Group, data=df)
  
  # Calculate confidence ellipses
  ellipse_data <- lapply(levels(df$Group), function(grp) {
    subset <- dplyr::filter(sample_coords, Group == grp)
    mean_data <- colMeans(subset[, c("PC1", "PC2")])
    cov_matrix <- stats::cov(subset[, c("PC1", "PC2")])
    ellipse_points <- ellipse::ellipse(cov_matrix, 
                                     centre = mean_data, 
                                     level = ellipse_level)
    data.frame(Group = grp, ellipse_points)
  })
  ellipse_data <- do.call(rbind, ellipse_data)
  
  # Set annotation position
  annotation_x <- x_limits[2]
  annotation_y <- y_limits[2]
  
  # Create main PCA plot
  main_plot <- ggplot2::ggplot() +
    ggplot2::geom_point(data = sample_coords, 
                        ggplot2::aes(x = PC1, y = PC2, fill = Group), 
                        size = point_size, color = "transparent", shape = 21)
  
  # Add variable arrows and labels if requested
  if(top_n_vars > 0) {
    main_plot <- main_plot +
      ggplot2::geom_segment(data = species_filtered, 
                           ggplot2::aes(x = 0, y = 0, xend = -1.25 * PC1, 
                                      yend = 1.25 * PC2),
                           arrow = ggplot2::arrow(angle = 22.5, 
                                                length = ggplot2::unit(0.25, "cm"), 
                                                type = "closed")) +
      ggrepel::geom_text_repel(data = species_filtered, 
                              ggplot2::aes(x = -1.275 * PC1, y = 1.275 * PC2, 
                                         label = func), 
                              size = 3.8)
  }
  
  # Add plot elements
  main_plot <- main_plot +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
    ggplot2::geom_polygon(data = ellipse_data,
                         ggplot2::aes(x = PC1, y = PC2, group = Group, 
                                    fill = Group),
                         alpha = ellipse_alpha) +
    ggplot2::geom_path(data = ellipse_data, 
                       ggplot2::aes(x = PC1, y = PC2, group = Group), 
                       color = rep(colors, each = 100),
                       linetype = "dashed") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(x = sprintf("PC1 (%.2f%%)", pca_result$cont$importance[2,1] * 100),
                  y = sprintf("PC2 (%.2f%%)", pca_result$cont$importance[2,2] * 100)) +
    ggplot2::scale_x_continuous(limits = x_limits) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggpubr::theme_classic2() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(color = "black"),
                   axis.ticks = ggplot2::element_blank())
  
  # Add PERMANOVA results if requested
  if(show_stats) {
    main_plot <- main_plot + 
      ggplot2::annotate("text", 
                       label = sprintf("PERMANOVA\nR^2 = %.3f\np = %.3f", 
                                     adonis_result$R2[1], 
                                     adonis_result$`Pr(>F)`[1]),
                       x = annotation_x,
                       y = annotation_y,
                       size = 3.5,
                       hjust = 1,
                       vjust = 1)
  }
  
  # Extract legend and create legend-free version
  legend <- ggpubr::get_legend(main_plot)
  main_plot_no_legend <- main_plot + ggplot2::theme(legend.position = "none")
  
  # Create density plots
  density_plot_x <- ggplot2::ggplot(data = sample_coords) +
    ggplot2::geom_density(ggplot2::aes(x = PC1, fill = Group), 
                         alpha = ellipse_alpha,
                         color = 'black', 
                         position = 'identity', 
                         show.legend = FALSE) +
    ggplot2::scale_fill_manual(values=colors) +
    ggplot2::scale_x_continuous(limits = x_limits) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())
  
  density_plot_y <- ggplot2::ggplot(data = sample_coords) +
    ggplot2::geom_density(ggplot2::aes(x = PC2, fill = Group), 
                         alpha = ellipse_alpha,
                         color = 'black', 
                         position = 'identity', 
                         show.legend = FALSE) +
    ggplot2::scale_fill_manual(values=colors) +
    ggplot2::scale_x_continuous(limits = y_limits) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()) +
    ggplot2::coord_flip()
  
  # Prepare data for box plots
  pca_data <- data.frame(
    PC1 = sample_coords$PC1,
    PC2 = sample_coords$PC2,
    group = sample_coords$Group
  )
  
  # Generate pairwise comparisons
  pairwise_comparisons <- utils::combn(levels(df$Group), 2, simplify = FALSE)
  
  # Create box plots
  boxplot_x <- ggplot2::ggplot(pca_data, ggplot2::aes(x = group, y = PC1, 
                                                      colour = group)) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   legend.position = "none",
                   axis.ticks = ggplot2::element_blank()) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::coord_flip() +
    ggpubr::stat_compare_means(comparisons = pairwise_comparisons, label = "p.signif")
  
  boxplot_y <- ggplot2::ggplot(pca_data, ggplot2::aes(x = group, y = PC2, 
                                                      colour = group)) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   legend.position = "none",
                   axis.ticks = ggplot2::element_blank()) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggpubr::stat_compare_means(comparisons = pairwise_comparisons, label = "p.signif")
  
  # Combine all plots using patchwork
  design <- "166
            266
            345"
  
  combined_plot <- boxplot_x + density_plot_x + main_plot_no_legend + 
    density_plot_y + boxplot_y + legend + 
    patchwork::plot_layout(design = design, widths=c(5,1,1), heights=c(1,1,5))
  
  return(combined_plot)
}