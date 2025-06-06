% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readImage.R
\name{readImage}
\alias{readImage}
\title{Read and Resize an Image}
\usage{
readImage(path, resizeRows)
}
\arguments{
\item{path}{The file path of the image to be read.}

\item{resizeRows}{The desired number of rows (height) to resize the image. 
The aspect ratio of the image will be maintained during resizing.}
}
\value{
A cimg object containing the resized image.
}
\description{
This function reads an image from a file path and resizes it to a specified number of rows. 
It supports several image formats, including JPG, JPEG, PNG, TIFF, and TIF. The function
also converts the image into a suitable format for further processing.
}
\details{
The function detects the image format based on the file extension. It currently supports 
the following formats: JPG, JPEG, PNG, TIFF, and TIF. If the image has more than 3 
dimensions (such as an RGBA image with an alpha channel), the alpha channel is discarded.
The image is resized only if its height exceeds the specified `resizeRows`.
}
\examples{
# Example usage
img_path <- system.file('extdata/data.tiff', package='BioTrajectory')
img <- readImage(img_path, resizeRows = 500)
plot(img)  # Visualizes the resized image

}
