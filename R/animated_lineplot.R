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

