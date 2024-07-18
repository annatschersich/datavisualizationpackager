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
