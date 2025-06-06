% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selFrame.R
\name{selFrame}
\alias{selFrame}
\title{Image Frame Selector and Viewer}
\usage{
selFrame(image_files)
}
\arguments{
\item{image_files}{A vector of file paths to the images that will be displayed in the viewer.}
}
\value{
The index of the selected image.
}
\description{
This function provides an interactive image frame viewer where the user can navigate through a sequence of images 
and select a specific one. The viewer uses buttons for navigation (previous and next) and an option to select an image.
The function uses the `rp.control` to create a window for the interface, and `grid` for displaying images.
}
\details{
The function displays a sequence of images from the provided file paths, allowing the user to navigate between them 
using "Previous" and "Next" buttons. The images are displayed using `grid.raster` from the `grid` package. A "Select" button 
allows the user to select the current image, and the function returns the index of the selected image.
}
\examples{
\donttest{
# Not run:
path <- system.file('extdata/frames/', package='BioTrajectory')
image_files <- list.files(path, pattern = "\\\\.png$", full.names = TRUE)
selFrame(image_files)
}

}
