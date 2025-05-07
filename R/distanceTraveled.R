#' Calculates the total distance traveled through a series of points.
#'
#' Computes the cumulative distance between consecutive points in a given 
#' set of coordinates. It sums the Euclidean distances between each pair of adjacent 
#' points, providing the total distance traveled along the path defined by the points.
#'
#' @param points A matrix or data frame where each row represents a point with its 
#'   coordinates (e.g., x and y).
#'
#' @return A numeric value representing the total distance traveled through the points.
#'
#' @examples
#' # Define a set of points
#' points <- matrix(c(0, 0, 1, 1, 2, 0), ncol = 2, byrow = TRUE)
#' # Calculate the distance traveled
#' total_distance <- distanceTraveled(points)
#' # Print the total distance
#' print(total_distance)
#'
#' @export
distanceTraveled <- function(points) {
  n <- nrow(points)
  sumDistance <- 0

  for (i in 1:(n - 1)) {
    distance <- sqrt(sum((points[i, ] - points[i + 1, ])^2))
    sumDistance <- sumDistance + distance
  }
  return(sumDistance)
}
