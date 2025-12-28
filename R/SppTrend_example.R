#' @title Open the SppTrend example
#'
#' @description This function locates and opens the PDF example included in the package.
#'
#' @return Opens the PDF in the default system viewer.
#' @export
#'
SppTrend_example <- function() {
  file_name <- "SppTrend_example.pdf"
  path <- system.file("extdata", file_name, package = "SppTrend")
  if (path == "") {
    stop(paste("The file", file_name, "could not be found in extdata. Please check your installation."))
  }
  utils::browseURL(path)
}
