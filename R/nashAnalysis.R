#' @title Nash Analysis
#' @description Create plots visualizing nash convergence of a given REMIND run
#'
#' @author Falk Benke
#'
#' @param gdx file path to a gdx file (default fulldata.gdx)
#' @param outputFile file name to save the html dashboard
#'
#' @importFrom rmarkdown render
#'
#' @export
nashAnalysis <- function(gdx = "fulldata.gdx", outputFile = NULL) {

  if (!file.exists(gdx)) {
    warning("Gdx file not found.")
    return()
  }

  markdownPath <- system.file("markdown", "nashAnalysis.Rmd", package = "remind2")

  if (is.null(outputFile)) {
    outputFile <- file.path(getwd(), "Nash Analysis.html")
  }

  rmarkdown::render(markdownPath, output_file = outputFile, params = list(gdx = gdx))
}
