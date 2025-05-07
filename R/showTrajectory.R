#' Displays a trajectory plot from a set of coordinates.
#'
#' Visualizes the trajectory represented by a series of points. The function can display 
#' the full trajectory or a subset based on a specified step size. It supports customizable 
#' color palettes for enhanced visualization.
#'
#' @param data A data frame containing the trajectory coordinates with columns `x` and `y`.
#' @param stepSize An integer specifying the interval for plotting segments of the trajectory. 
#'   If set to 0, the entire trajectory is plotted. Default is 0.
#' @param pal A color palette to be used for plotting the trajectory segments.
#'
#' @return A plot displaying the trajectory based on the provided coordinates and settings.
#'
#' @examples
#' # Generate example trajectory data
#' path <- system.file('extdata/track.txt', package='BioTrajectory')
#' data <- BioTrajectory::readtrackData(path)
#' data <- na.omit(data)
#' palette <- grDevices::colorRampPalette(c("purple","blue","cyan","yellow","orange","red"))
#' # Show the full trajectory
#' showTrajectory(data, stepSize = 0, pal = palette)
#' # Show the trajectory with a step size of 36
#' showTrajectory(data, stepSize = 36, pal = palette)
#'
#' @export

showTrajectory <- function(data, stepSize = 0, pal) {
  colorScheme<-colorRampPalette(c("purple","blue","cyan","yellow","orange","red"))(nrow(data))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar)) 
  if (stepSize == 0) {
    layout(matrix(c(1, 2), nrow = 1, byrow = TRUE), c(3, 1), 3, TRUE)
    par(mai = c(0.8, 0.8, 0.4, 0))
    plot(data, pch = 19, col = colorScheme, xlab = "x", ylab = "y", main="Trajectory")
    par(mai = c(0.8, 0, 0.4, 0.8))
    legendPlot(seq_along(data$x), colorScheme)
  } else {
    k <- ceiling(nrow(data) / stepSize)
    m <- findM(k)
    xlim <- range(data$x)
    ylim <- range(data$y)
    par(mfrow = m, mai = c(0, 0, 0, 0), oma=c(0,0,1,0))
    for (i in 1:(nrow(data) - 1)) {
      if (i %% stepSize == 1) {
        plot(xlim, ylim,
          bty = "o", type = "n",
          axes = FALSE, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
      }
      segments(data$x[i], data$y[i], data$x[i + 1], data$y[i + 1],
        col = colorScheme[i], lwd = 10 * (i %% stepSize) / stepSize
      )
      abline(v = xlim[1], h = ylim[1], lwd = 1)
    }
    mtext(paste(c("Trajectory ", stepSize, " steps"), collapse=""), outer = TRUE)
  }
}
