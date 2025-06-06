% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heatmapFromPoints.R
\name{heatmapFromPoints}
\alias{heatmapFromPoints}
\title{Creates a heatmap from a set of points.}
\usage{
heatmapFromPoints(data, plim = NULL, plot.pal = TRUE, ...)
}
\arguments{
\item{data}{A data frame containing the coordinates of the points with columns named 
`x` and `y`.}

\item{plim}{Optional vector of length two specifying the plotting limits for the density 
values. If provided, values outside this range will be adjusted.}

\item{plot.pal}{A logical value indicating whether to plot the color palette alongside 
the heatmap (default is TRUE).}

\item{...}{Additional graphical parameters to customize the image.}
}
\value{
A matrix of density values corresponding to the heatmap.
}
\description{
Generates a heatmap based on the density of points in a given dataset. 
It utilizes kernel density estimation to visualize the concentration of points, 
allowing for adjustments in color palette and plotting limits.
}
\examples{
# Generate example data
set.seed(123)
data <- data.frame(x = rnorm(1000), y = rnorm(1000))

# Create a heatmap from the points
heatmap_result <- heatmapFromPoints(data)

}
