#' Extract Frames from a Video and Save as Images
#'
#' This function takes a video file, processes it, and saves the frames as individual images in the specified directory. The images are saved in PNG format by default, and at a rate of 1 frame per second.
#'
#' @param videoPath Path to the video file.
#' @param outputDir Path to the directory where the extracted images will be saved.
#'
#' @return A list of file paths to the generated images.
#' 
#' @examples
#' \donttest{
#' # Not run:
#' videoPath <- system.file('extdata/video.mp4', package='BioTrajectory')
#' outputDir <- system.file('extdata/frames/', package='BioTrajectory')
#' images <- videoToFrames(videoPath, outputDir)
#' print(images)  # Displays the paths to the generated images
#' }
#' @export

videoToFrames <- function(videoPath, outputDir) {
  av_video_images(videoPath, destdir = outputDir, format = "png", fps = 15)
  listImages <- list.files(outputDir, pattern = "\\.(jpg|jpeg|png|tiff)$", full.names = TRUE)
  return(listImages)
}
