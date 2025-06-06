% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distanceTraveled.R
\name{distanceTraveled}
\alias{distanceTraveled}
\title{Calculates the total distance traveled through a series of points.}
\usage{
distanceTraveled(points)
}
\arguments{
\item{points}{A matrix or data frame where each row represents a point with its 
coordinates (e.g., x and y).}
}
\value{
A numeric value representing the total distance traveled through the points.
}
\description{
Computes the cumulative distance between consecutive points in a given 
set of coordinates. It sums the Euclidean distances between each pair of adjacent 
points, providing the total distance traveled along the path defined by the points.
}
\examples{
# Define a set of points
points <- matrix(c(0, 0, 1, 1, 2, 0), ncol = 2, byrow = TRUE)
# Calculate the distance traveled
total_distance <- distanceTraveled(points)
# Print the total distance
print(total_distance)

}
