#' Animated Line Plot
#'
#' Creates an animated line plot of the given data.
#'
#' @param data The data to be visualized.
#' @param x The variable that is plotted on the x-axis.
#' @param y The variable that is plotted on the y-axis.
#' @import ggplot2
#' @import gganimate
#' @examples
#' # Example dataset
#' set.seed(123)
#' data <- data.frame(
#'   time = 1:100,
#'   value = cumsum(rnorm(100))
#' )
#'
#' # Create an animated line plot
#' animated_lineplot(data, x = "time", y = "value")
#' @export
animated_lineplot <- function(data, x, y) {
  if (!all(c(x, y) %in% names(data))) {
    stop("Variables 'x' and 'y' must be column names in 'data'.")
  }
  library(ggplot2)
  library(gganimate)

  p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), group = 1)) +
    geom_line(color = "darkblue") +
    labs(title = "Animated Line Plot", x = x, y = y) +
    theme_minimal()

  p <- p + transition_reveal(!!sym(x))

  animate(p, nframes = 200, fps = 5)
}

#' Boxplot with mean
#'
#' Creates a boxplot of the given data and shows the mean.
#'
#' @param data The data to be visualized.
#' @param x The variable for which the boxplot will be created.
#' @importFrom graphics boxplot abline
#' @examples
#' # Example dataset
#' set.seed(123)
#' data <- data.frame(value = rnorm(100))
#'
#' # Create a boxplot with mean line
#' boxplot_with_mean(data, "value")
#'
#' @export
boxplot_with_mean <- function(data, x) {
  if (!x %in% names(data)) {
    stop("Variable 'x' must be a column name in 'data'.")
  }

  boxplot(data[[x]], main = "Boxplot", xlab = x, col = "green", outcol = "orange", pch = 19)

  mean_value <- mean(data[[x]], na.rm = TRUE)

  abline(h = mean_value, col = "blue", lwd = 2, lty = 2) # Blue dashed line for mean
}

#' Scatterplot with regressionline
#'
#' Creates a scatterplot of the given data and adds a regression line.
#'
#' @param data A dataframe with the data to be visualized.
#' @param x The variable plotted on the x-axis (as a string).
#' @param y The variable plotted on the y-axis (as a string).
#' @param main (optional) The title of the plot. Defaults to "Scatterplot with Regression Line".
#' @examples
#' # Example dataset
#' set.seed(123)
#' data <- data.frame(x = rnorm(100), y = rnorm(100))
#'
#' # Create a scatterplot with a regression line
#' scatterplot_with_regression(data, x = "x", y = "y", main = "Example Scatterplot with Regression Line")
#' @export
scatterplot_with_regression <- function(data, x, y, main = "Scatterplot with Regression Line") {
  if (!all(c(x, y) %in% names(data))) {
    stop("Variables 'x' and 'y' must be column names in 'data'.")
  }

  plot(data[[x]], data[[y]], main = main, xlab = x, ylab = y, col = "blue")
  abline(lm(data[[y]] ~ data[[x]]), col = "red", lwd = 3)
}

#' datavisualizationpackager: A Package for three different datavisualization
#'
#' This datavisualization package contains three distinct functions for data visualization:
#' animated_lineplot: Generates an animated line plot of your data,
#' boxplot_with_mean: Creates a boxplot overlaid with a line representing the mean of your data,
#' scatterplot_with_regression: Produces a scatterplot with the corresponding regression line of your data.
#'
#' @name datavisualizationpackager
#' @keywords internal
#' @section Functions:
#' \itemize{
#' \item \code{\link{animated_lineplot}}: Generates an animated line plot of your data.
#' \item \code{\link{boxplot_with_mean}}: Creates a boxplot overlaid with a line representing the mean of your data.
#' \item \code{\link{scatterplot_with_regression}}: Produces a scatterplot with the corresponding regression line of your data.
#' }
#'
"_PACKAGE"
