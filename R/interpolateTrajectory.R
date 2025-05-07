#' Interpolates a trajectory
#'
#' Given a dataset of points, it interpolates to generate
#' intermediate points between them.
#'
#' @param data A data frame with at least two columns: `x` and `y`, representing
#' the coordinates of the points.
#' @param n An integer indicating the number of intermediate points to generate
#' between each pair of points. Default is 4.
#' @return A data frame containing the `x` and `y` coordinates, which includes
#' both the original points and the interpolated points.
#' @examples
#' # Create an example data frame
#' data <- data.frame(x = c(1, 2, 3), y = c(1, 4, 9))
#' # Interpolate the trajectory
#' interpolated <- interpolateTrajectory(data, n = 4)
#' print(interpolated)
#' @export

interpolateTrajectory <- function(data, n = 4) {
  N <- nrow(data)
  res <- c(data$x[1], data$y[1])
  for (i in 1:(N - 1)) {
    data1 <- data[i + 0:1, ]
    if (data1$x[1] != data1$x[2]) {
      xyI <- approx(data1, n = 2 + n)
      if (data1$x[1] > data1$x[2]) {
        xyI$x <- rev(xyI$x)
        xyI$y <- rev(xyI$y)
      }
    } else {
      xyI <- data.frame(
        x = rep(data1$x[1], 2 + n),
        y = seq(data1$y[1], data1$y[2], length.out = 2 + n)
      )
    }
    temp <- cbind(head(xyI$x, -1), head(xyI$y, -1))
    colnames(temp) <- c("x", "y")
    res <- rbind(res, temp)
  }
  res <- rbind(tail(res, -1), tail(data, 1))
  return(res)
}
