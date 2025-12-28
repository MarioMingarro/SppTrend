#' @title Open the SppTrend example
#'
#' @description This function locates and opens the PDF example included in the package.
#'
#' @param format A `character` string, either "pdf" or "html" (default).
#'
#' @return Opens the PDF or html in the default system viewer.
#' @export
#'
SppTrend_example <- function(format = "html") {
  format <- tolower(format)
  if (!(format %in% c("pdf", "html"))) {
    stop("Invalid format. Please use 'pdf' or 'html'.")
  }
  file_name <- paste0("SppTrend_example.", format)
  path <- system.file("extdata", file_name, package = "SppTrend")
  if (path == "") {
    stop(paste("The file", file_name, "could not be found in extdata. Please check your installation."))
  }
  utils::browseURL(path)
}
