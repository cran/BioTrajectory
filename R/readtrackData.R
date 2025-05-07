#' Reads tracking data from a specified file.
#'
#' Reads a text file containing tracking data, where each line represents 
#' coordinates. If a line contains "null", it adds NA values for that entry. Optionally, 
#' it can remove rows with NA values.
#'
#' @param file A character string specifying the path to the file containing the tracking data.
#' @param na.rm A logical value indicating whether to remove rows with NA values (default is FALSE).
#'
#' @return A data frame with two columns (`x` and `y`) containing the coordinates read from 
#'   the file. If `na.rm` is TRUE, rows with NA values are omitted.
#'
#' @examples
#' # Read tracking data from a file
#' path <- system.file('extdata/track.txt', package='BioTrajectory')
#' tracking_data <- readtrackData(path, na.rm = TRUE)
#' # Print the resulting data frame
#' print(tracking_data)
#'
#' @export

readtrackData <- function(file, na.rm = FALSE) {
  lines <- readLines(file)
  data <- data.frame(x = NA, y = NA)
  for (i in seq_along(lines)) {
    linea <- lines[i]
    if (linea == "null") {
      data <- rbind(data, c(NA, NA))
    } else {
      coords <- as.numeric(unlist(strsplit(linea, " ")))
      data <- rbind(data, coords)
    }
  }
  data <- data[-1, ]
  if (na.rm) {
    data <- na.omit(data)
    rownames(data) <- 1:nrow(data)
  }
  return(data)
}
