% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datavisualizationpackager.R
\name{animated_lineplot}
\alias{animated_lineplot}
\title{Animated Line Plot}
\usage{
animated_lineplot(data, x, y)
}
\arguments{
\item{data}{The data to be visualized.}

\item{x}{The variable that is plotted on the x-axis.}

\item{y}{The variable that is plotted on the y-axis.}
}
\description{
Creates an animated line plot of the given data.
}
\examples{
# Example dataset
set.seed(123)
data <- data.frame(
  time = 1:100,
  value = cumsum(rnorm(100))
)

# Create an animated line plot
animated_lineplot(data, x = "time", y = "value")
}
