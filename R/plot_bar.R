#' Generate a bar plot with error bars, jitter points, and Tukey's letters
#'
#' @param df a data frame
#' @param var a target variable to plot
#' @param group a grouping variable like Group or Treatment
#' @param xlab x-axis label
#' @param ylab y-axis label
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' plot_bar(df, Rainfall, Location, "Location", "Precipitation (mm)")
plot_bar <- function(df, var, group, xlab, ylab) {
  library(ggplot2)
  library(dplyr)
  library(multcompView)
  library(RColorBrewer)

  # Calculate mean and standard deviation for each group
  df_summary <- df %>%
    group_by(!!sym(group)) %>%
    summarise(mean = mean(!!sym(var)), sd = sd(!!sym(var)), max_val = max(!!sym(var)))

  # Perform ANOVA
  aov_res <- aov(as.formula(paste(var, "~", group)), data = df)

  # Perform Tukey's HSD test
  tukey_res <- TukeyHSD(aov_res)

  # Generate grouping letters
  tukey_letters <- multcompLetters4(aov_res, tukey_res)
  df_summary$letters <- tukey_letters[[group]]$Letters

  plot <- ggplot(data = df_summary, aes_string(x = group, y = "mean", fill = group)) +
    geom_bar(stat = "identity") +
    # Add jitter points
    geom_jitter(data = df, aes_string(x = group, y = var), width = 0.1, alpha = 0.3, size = 2) +
    # Add error bars using mean and standard deviation
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
    # Annotate bars with grouping letters above the max value
    geom_text(aes(label = letters, y = max_val + 1), vjust = 0) +
    xlab(xlab) +
    ylab(ylab) +
    scale_fill_brewer(palette = "Dark2") +
    theme_bw()+
    # remove legend
    theme(legend.position = "none")

  return(plot)
}
