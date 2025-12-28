#' @title Open the SppTrend user guide
#'
#' @description This function locates and opens the PDF user guide included in the package.
#'
#' @return Opens the PDF in the default system viewer.
#' @export
#'
SppTrend_guide <- function() {
  path <- system.file("extdata", "SppTrend_example.pdf", package = "SppTrend")
  if (path == "") {
    stop("The guide PDF could not be found. Please check your installation.")
  }
  utils::browseURL(path)
}
