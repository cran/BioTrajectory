#' Finds the nearest targets to a set of points within a specified radius.
#'
#' Calculates the distances between a set of points and target locations. 
#' It identifies the nearest target for each point and checks if the distance is within 
#' a specified radius. If a target is found within the radius, its index and distance 
#' are returned; otherwise, -1 is returned for both.
#'
#' @param points A matrix or data frame containing the coordinates of the points with 
#'   rows representing points.
#' @param targets A matrix or data frame containing the coordinates of the target locations 
#'   with rows representing targets.
#' @param r A numeric value specifying the radius within which to consider targets.
#'
#' @return A data frame with two columns:
#' \item{nt}{Index of the nearest target for each point. If no target is found within the 
#'   radius, this will be -1.}
#' \item{d}{Distance to the nearest target. If no target is found within the radius, this 
#'   will be -1.}
#'
#' @examples
#' # Define a set of points and targets
#' points <- matrix(c(1, 2, 3, 4), ncol = 2)
#' targets <- matrix(c(2, 3, 5, 6), ncol = 2)
#' radius <- 2
#'
#' # Find the nearest targets
#' nearest_results <- nearestTarget(points, targets, radius)
#'
#' # Print the results
#' print(nearest_results)
#' @export

nearestTarget <- function(points, targets, r) {
  distances <- as.matrix(dist(rbind(points, targets)))
  nPoints <- nrow(points)
  nTargets <- nrow(targets)
  distances <- distances[1:nPoints, (nPoints + 1):(nPoints + nTargets)]

  nn <- function(x, r) {
    nearestIndex <- which.min(x)
    nearestDistance <- x[nearestIndex]
    if (nearestDistance <= r) {
      c(nearestIndex, nearestDistance)
    } else {
      c(-1, -1)
    }
  }
  results <- apply(distances, 1, nn, r=r)
  results <- t(results) %>% as.data.frame()
  colnames(results) <- c("nt", "d")
  return(results)
}
