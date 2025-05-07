#' Creates a heatmap from a set of points.
#'
#' Generates a heatmap based on the density of points in a given dataset. 
#' It utilizes kernel density estimation to visualize the concentration of points, 
#' allowing for adjustments in color palette and plotting limits.
#'
#' @param data A data frame containing the coordinates of the points with columns named 
#'   `x` and `y`.
#' @param plim Optional vector of length two specifying the plotting limits for the density 
#'   values. If provided, values outside this range will be adjusted.
#' @param plot.pal A logical value indicating whether to plot the color palette alongside 
#'   the heatmap (default is TRUE).
#' @param ... Additional graphical parameters to customize the image.
#'
#' @return A matrix of density values corresponding to the heatmap.
#'
#' @examples
#' # Generate example data
#' set.seed(123)
#' data <- data.frame(x = rnorm(1000), y = rnorm(1000))
#'
#' # Create a heatmap from the points
#' heatmap_result <- heatmapFromPoints(data)
#'
#' @export

heatmapFromPoints <- function(data, plim = NULL, plot.pal = TRUE, ...) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar)) 
  pal <- rev(heat.colors(100))
  if (plot.pal) layout(matrix(c(1, 2), nrow = 1, byrow = TRUE), c(3, 1), 3, TRUE)
  par(mai = c(0.8, 0.8, 0.4, 0))
  dens <- kde2d(data$x, data$y, n = 100)
  dens$z <- dens$z * nrow(data)
  if (!is.null(plim)) {
    if (any(dens$z <= plim[1])) {
      dens$z[dens$z < plim[1]] <- plim[1]
    } else {
      dens$z[1] <- plim[1]
    }
    if (any(dens$z >= plim[2])) {
      dens$z[dens$z >= plim[2]] <- plim[2]
    } else {
      dens$z[2] <- plim[2]
    }
  }
  x <- dens$x
  y <- dens$y
  image(x, y, dens$z, col = pal, main="Heatmap", ...)
  if (plot.pal) {
    par(mai = c(0.8, 0, 0.4, 0.8))
    legendPlot(dens$z, pal)
  }
  return(invisible(dens$z))
}