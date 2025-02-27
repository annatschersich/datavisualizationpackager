% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datavisualizationpackager.R
\name{boxplot_with_mean}
\alias{boxplot_with_mean}
\title{Boxplot with mean}
\usage{
boxplot_with_mean(data, x)
}
\arguments{
\item{data}{The data to be visualized.}

\item{x}{The variable for which the boxplot will be created.}
}
\description{
Creates a boxplot of the given data and shows the mean.
}
\examples{
# Example dataset
set.seed(123)
data <- data.frame(value = rnorm(100))

# Create a boxplot with mean line
boxplot_with_mean(data, "value")

}
