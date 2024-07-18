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

