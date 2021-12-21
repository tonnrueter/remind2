#' Render CompareScenarios2.Rmd
#'
#' Renders the CompareScenarios2.Rmd. In the Rmd, Scenario- and historical .mif-files are loaded. Plots are created from
#' this data. The result may be rendered to PDF or html.
#'
#' @param ... YAML parameters.
#' @param outputFile character(1). File name of the output document to be created (without extension).
#' @param outputDir character(1). The directory where the output document and intermediary files are created.
#' @param outputFormat character(1). "html_document", "pdf_document", or "rmd".
#' @return The value returned by rmarkdown::render() is returned.
#' @author Christof Schoetz
#' @export
compareScenarios2 <- function(
  ...,
  outputDir = getwd(),
  outputFile = "CompareScenarios2",
  outputFormat = "html_document"
  ) {
  yamlParams <- list(...)
  if (identical(tolower(outputFormat), "rmd")) {
    return(.compareScenarios2_rmd(yamlParams, outputDir, outputFile))
  }
  rmarkdown::render(
    system.file("markdown/compareScenarios2/cs2_main.Rmd", package = "remind2"),
    intermediates_dir = outputDir,
    output_dir = outputDir,
    output_file = outputFile,
    output_format = outputFormat,
    params = yamlParams,
    envir = new.env())
}

.compareScenarios2_rmd <- function(yamlParams, outputDir, outputFile) {
  system.file("markdown/compareScenarios2/cs2_main.Rmd", package = "remind2") %>% 
    ymlthis:::read_rmd() %>% 
    yaml::yaml.load(handlers=list(r = \(x) ymlthis::yml_params_code(!!rlang::parse_expr(x)))) %>% 
    ymlthis::as_yml() ->
    baseYaml
  baseYaml %>% 
    ymlthis::yml_pluck("params") ->
    newYamlParams
  newYamlParams[names(yamlParams)] <- yamlParams
  baseYaml %>% 
    ymlthis::yml_replace(params = newYamlParams) ->
    yaml
  pathDir <- file.path(outputDir, paste0(outputFile, "_Rmd"))
  if (!dir.exists(pathDir)) dir.create(pathDir)
  system.file("markdown/compareScenarios2", package = "remind2") %>% 
    dir(full.names = TRUE) %>% 
    grep(pattern="cs2_main\\.Rmd$", invert=TRUE, value=TRUE) %>% 
    file.copy(pathDir)
  ymlthis::use_rmarkdown(
    yaml,
    path = file.path(pathDir, "cs2_main.Rmd"),
    template = system.file("markdown/compareScenarios2/cs2_main.Rmd", package = "remind2"),
    include_yaml = FALSE)
}

