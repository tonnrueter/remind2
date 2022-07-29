#' @importFrom utils globalVariables
globalVariables(".")

getProjectPath <- function(project = "remind") {
  possibleProjectLocations <- file.path(c("//clusterfs.pik-potsdam.de", "/p/projects"), project)
  sel <- which(file.exists(possibleProjectLocations))[1]
  if (is.na(sel)) stop("Cannot determine a path to projects on the cluster.")
  possibleProjectLocations[sel]
}

#' @importFrom dplyr bind_cols
getNewsestModeltests <- function(namePattern = "^SSP2EU-AMT-") {

  modeltestOutPath <- file.path(getProjectPath(), "modeltests/output")
  entries <- dir(modeltestOutPath)
  allRunNames <-
    entries %>%
    grep(
      x = .,
      pattern = "^[a-zA-Z0-9-]+_\\d{4}-\\d{2}-\\d{2}_\\d{2}\\.\\d{2}\\.\\d{2}$",
      value = TRUE)
  newest <-
    allRunNames %>%
    strsplit("_", fixed = TRUE) %>%
    unlist() %>%
    matrix(nrow = 3, dimnames = list(c("name", "date", "time"), NULL)) %>%
    t() %>%
    as_tibble() %>%
    bind_cols(runName = allRunNames) %>%
    mutate(date = as.Date(.data$date)) %>%
    filter(date >= max(.data$date) - 3) # day of newest run until 3 days before
  selectedRuns <-
    newest %>%
    filter(grepl(x = .data$name, pattern = .env$namePattern)) %>%
    mutate(path = file.path(.env$modeltestOutPath, .data$runName))

  return(selectedRuns)
}

cs2InputPaths <- function(outputDirs) {
  names <- lucode2::getScenNames(outputDirs)
  # conditional for _adjustedPolicyCosts.mif not needed anymore since REMIND PR #881.
  path <- list(
    run = outputDirs,
    mifScen = file.path(outputDirs, paste0("REMIND_generic_", names, ".mif")),
    mifHist = file.path(outputDirs[1], "historical.mif"),
    cfgScen = file.path(outputDirs, "config.Rdata"),
    cfgDefault = file.path(outputDirs[1], "../../config/default.cfg")
  )
  lapply(path, normalizePath)
}

#' Load compareScenarios2 Data
#'
#' Load data from mif files into R-objects as used in \link[=compareScenarios2]{compareScenarios2()}.
#'
#' @param cfgScen,cfgDefault See section "YAML Parameters" in \link[=compareScenarios2]{compareScenarios2()}.
#' @param envir \code{environment}. The environment where the loaded data is put into.
#' @inheritParams compareScenarios2
#' @examples
#' \dontrun{
#' loadCs2Data(
#'   c("path/to/Base.mif", "path/to/NDC.mif"),
#'   "path/to/historical.mif")
#' }
#' @export
loadCs2Data <- function(
    mifScen,
    mifHist,
    cfgScen = NULL,
    cfgDefault = NULL,
    envir = globalenv()
) {

  folder <- tempdir()
  outputFile <- format(Sys.time(), "cs2_%Y%m%d-%H%M%S")
  compareScenarios2(
    mifScen = mifScen,
    mifHist = mifHist,
    cfgScen = cfgScen,
    cfgDefault = cfgDefault,
    outputDir = folder,
    outputFormat = "pdf",
    outputFile = outputFile,
    sections = NULL,
    envir = envir,
    quiet = TRUE)
  file.remove(file.path(folder, paste0(outputFile, ".pdf")))
  return(invisible(NULL))
}

#' Load Modeltest Results
#'
#' The newest model tests are collected from the cluster and copied into a temporary folder (by default). Then the \link[=compareScenarios2]{compareScenarios2()} data loading procedure is used to load this data into the users environment.
#'
#' @param namePattern \code{character(1)}. A regular expression to filter the modeltest run names.
#' @param folder \code{character(1)}. A folder to copy the modeltest data to.
#' @inheritParams loadCs2Data
#' @examples
#' \dontrun{
#' loadModeltest()
#'
#' ssp1 <- new.env()
#' ssp2eu <- new.env()
#' loadModeltest(ssp1, "^SSP1-AMT-")
#' loadModeltest(ssp2eu, "^SSP2EU-AMT-")
#' ssp1$data
#' ssp2eu$data
#' }
#' @export
loadModeltest <- function(
    envir = globalenv(),
    namePattern = "^SSP2EU-AMT-",
    folder = tempdir()
) {

  stopifnot(is.environment(envir))
  stopifnot(is.character(namePattern) && length(namePattern) == 1)
  stopifnot(is.character(folder) && length(folder) == 1)

  cat("Obtaining modeltest paths.\n")

  modeltests <- getNewsestModeltests(namePattern)
  path <- cs2InputPaths(modeltests$path)
  tmpPath <- list(
    mifScen = file.path(folder, paste0(modeltests$name, ".mif")),
    mifHist = file.path(folder, "historical.mif"),
    cfgScen = file.path(folder, paste0(modeltests$name, ".Rdata")),
    cfgDefault = file.path(folder, "default.cfg"))

  copyFile <- function(from, to) {
    cat("Copying\n  ", paste(from, collapse = "\n  "),
        "\nto\n  ", paste(to, collapse = "\n  "),
        "\n",
        sep="")
    file.copy(from, to, overwrite = TRUE)
  }

  copyFile(path$mifScen, tmpPath$mifScen)
  copyFile(path$mifHist, tmpPath$mifHist)
  copyFile(path$cfgScen, tmpPath$cfgScen)
  copyFile(path$cfgDefault, tmpPath$cfgDefault)

  cat(
    "Loading cs2 data into ",
    if (environmentName(envir) == "") "user specified environment", environmentName(envir),
    ".\n", sep="")

  loadCs2Data(
    mifScen = tmpPath$mifScen,
    mifHist = tmpPath$mifHist,
    cfgScen = tmpPath$cfgScen,
    cfgDefault = tmpPath$cfgDefault,
    envir = envir)

  cat("Done.\n")

  return(invisible(NULL))
}
