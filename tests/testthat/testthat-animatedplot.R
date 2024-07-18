library(testthat)
library(ggplot2)
library(gganimate)
library(datavisualizationpackager)

test_that("animated_lineplot works correctly", {
  # Example dataset
  set.seed(123)
  data <- data.frame(
    time = 1:100,
    value = cumsum(rnorm(100))
  )

  # Test if the function runs without errors or unexpected messages
  expect_silent({
    suppressMessages({
      animated_lineplot(data, x = "time", y = "value")
    })
  })
  # Test invalid column names
  expect_error(animated_lineplot(data, x = "invalid_time", y = "invalid_value"),
               "Variables 'x' and 'y' must be column names in 'data'.")
})

test_that("animated_lineplot loads the necessary packages", {
  # Test if ggplot2 and gganimate are loaded
  expect_true("package:ggplot2" %in% search())
  expect_true("package:gganimate" %in% search())
})
