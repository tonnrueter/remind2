#' Render CompareScenarios2.Rmd
#'
#' Renders the CompareScenarios2.Rmd. In the Rmd, Scenario- and historical .mif-files are loaded. Plots are created from
#' this data. The result may be rendered to PDF or html.
#'
#' @param mif character(n). Path to one or more mif-files.
#' @param hist character(1). Path to one mif-file containing historical data.
#' @param y numerical(n). Years for which scenario data will be shown.
#' @param y_hist numerical(n). Years for which historical data will be shown.
#' @param y_bar numerical(n). Years to be shown in bar plots.
#' @param reg character(n) or NULL. Regions to be shown. NULL for all.
#' @param outputFile character(1). File name of the output document to be created (without extension).
#' @param outputDir character(1). The directory where the output document and intermediary files are created.
#' @param outputFormat character(1). "html_document" or "pdf_document".
#' @param ... Further arguments to be passed to rmarkdown::render().
#' @value The value returned by rmarkdown::render() is returned.
#' @author Christof Schoetz
#' @export
compareScenarios2 <- function(mif,
  hist,
  y = c(seq(2005, 2060, 5), seq(2070, 2100, 10)),
  y_hist = c(seq(1960, 2020, 1), seq(2025, 2100, 5)),
  y_bar = c(2010, 2030, 2050, 2100),
  reg = NULL,
  mainReg = "World",
  outputDir = getwd(),
  outputFile = "CompareScenarios2",
  outputFormat = "html_document",
  ...) {
  yaml_params <- list(
    mif = mif,
    hist = hist,
    y = y,
    y_hist = y_hist,
    y_bar = y_bar,
    reg = reg,
    mainReg = mainReg)
  rmarkdown::render(
    system.file("markdown/compareScenarios2/cs2_main.Rmd", package = "remind2"),
    intermediates_dir = outputDir,
    output_dir = outputDir,
    output_file = outputFile,
    output_format = outputFormat,
    params = yaml_params,
    envir = new.env(),
    ...)
}
