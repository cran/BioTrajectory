% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/videoToFrames.r
\name{videoToFrames}
\alias{videoToFrames}
\title{Extract Frames from a Video and Save as Images}
\usage{
videoToFrames(videoPath, outputDir, fps=15)
}
\arguments{
\item{videoPath}{Path to the video file.}

\item{outputDir}{Path to the directory where the extracted images will be saved.}

\item{fps}{The number of frames shown per second in the video. By default, it extracts 15 frames per second.}
}
\value{
A list of file paths to the generated images.
}
\description{
This function is a wrapper that simplifies extracting frames from a video. It processes the video file and saves the frames as PNG images in the specified directory. By default, it extracts 15 frames per second.
}
\examples{
\donttest{
# Not run:
videoPath <- system.file('extdata/video.mp4', package='BioTrajectory')
outputDir <- system.file('extdata/frames/', package='BioTrajectory')
images <- videoToFrames(videoPath, outputDir, fps=15)
print(images)  # Displays the paths to the generated images
}
}
