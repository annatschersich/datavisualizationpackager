library(testthat)
library(ggplot2)
library(datavisualizationpackager)

test_that("scatterplot_with_regression works correctly", {
  # Example dataset
  set.seed(123)
  data <- data.frame(x = rnorm(100), y = rnorm(100))

  # Test if the function runs without errors
  expect_silent(suppressWarnings(scatterplot_with_regression(data, x = "x", y = "y")))

  # Test invalid column names
  expect_error(scatterplot_with_regression(data, x = "a", y = "b"),
               "Variables 'x' and 'y' must be column names in 'data'.")
})
test_that("scatterplot_with_regression creates plot with title", {
  # Example dataset
  set.seed(123)
  data <- data.frame(x = rnorm(100), y = rnorm(100))

  # Capture plot output with a custom title
  plot_output <- recordPlot({
    scatterplot_with_regression(data, x = "x", y = "y", main = "Custom Title")
  })

  # Test if the plot has been created successfully
  expect_true(!is.null(plot_output), "Plot should be created successfully.")

  # Capture plot output with another custom title
  plot_output_2 <- recordPlot({
    scatterplot_with_regression(data, x = "x", y = "y", main = "Another Title")
  })

  # Test if plots with different titles are different
  expect_false(identical(plot_output, plot_output_2), "Plots with different titles should be different.")
})
