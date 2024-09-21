#' Generate a bar plot with error bars, jitter points, and Tukey's letters
#'
#' @param df a data frame
#' @param var a target variable to plot
#' @param group a grouping variable like Group or Treatment
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @import RColorBrewer
#' @importFrom stats TukeyHSD aov as.formula sd
#' @importFrom rlang sym
#' @importFrom multcompView multcompLetters4
#' @importFrom ggplot2 ggplot aes_string geom_bar geom_jitter geom_errorbar geom_text scale_fill_brewer theme_bw
#' @importFrom dplyr group_by summarise %>%
#' @importFrom stats TukeyHSD aov as.formula sd
#' @importFrom rlang sym .data
#' @importFrom multcompView multcompLetters4
#' @return a ggplot object
#' @export
#' @examples
#' set.seed(123)
#' df <- data.frame(var = c(rnorm(9, 10, 4), rnorm(9, 20, 6), rnorm(9, 30, 9)),
#'                 group = rep(c("Ctrl", "Pla", "DoSM"), each = 9))
#' plot_bar(df, "var", "group", "Treatment", "Cured number of patients")
plot_bar <- function(df, var, group, xlab, ylab) {
  # Calculate mean and standard deviation for each group
  df_summary <- df %>%
    dplyr::group_by(!!rlang::sym(group)) %>%
    dplyr::summarise(mean = mean(!!rlang::sym(var)), sd = sd(!!rlang::sym(var)), max_val = max(!!rlang::sym(var)))

  # Perform ANOVA
  aov_res <- stats::aov(stats::as.formula(paste(var, "~", group)), data = df)

  # Perform Tukey's HSD test
  tukey_res <- stats::TukeyHSD(aov_res)

  # Generate grouping letters
  tukey_letters <- multcompView::multcompLetters4(aov_res, tukey_res)
  df_summary$letters <- tukey_letters[[group]]$Letters

  plot <- ggplot2::ggplot(data = df_summary, ggplot2::aes_string(x = group, y = "mean", fill = group)) +
    ggplot2::geom_bar(stat = "identity") +
    # Add jitter points
    ggplot2::geom_jitter(data = df, ggplot2::aes_string(x = group, y = var), width = 0.1, alpha = 0.3, size = 2) +
    # Add error bars using mean and standard deviation
    ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
    # Annotate bars with grouping letters above the max value
    ggplot2::geom_text(ggplot2::aes(label = letters, y = .data$max_val + 1), vjust = 0) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::scale_fill_brewer(palette = "Dark2") +
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = "none")

  return(plot)
}
