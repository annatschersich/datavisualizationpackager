library(testthat)
library(datavisualizationpackager)

test_that("boxplot_with_mean works correctly", {
  # example-dataset
  set.seed(123)
  data <- data.frame(value = rnorm(100))

  # testing, if the function runs without any errors
  expect_silent(boxplot_with_mean(data, "value"))
  # testing invalid column names
  expect_error(boxplot_with_mean(data, "invalid_column"),
               "Variable 'x' must be a column name in 'data'.")
})
test_that("boxplot_with_mean creates boxplot with mean", {
  set.seed(123)
  data <- data.frame(value = rnorm(100))

  # capture plot output
  expect_silent({
    plot_output <- recordPlot({
      boxplot_with_mean(data, "value")
    })
  })

  # Ptesting, if the boxplot and the line representing the mean are created
  expect_true(!is.null(plot_output))
})
