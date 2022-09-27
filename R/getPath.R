#' Get Paths to Certain Files in the REMIND Directory
#'
#' \code{getMifScenPath}: get path to the scenarios' reporting mifs.
#' \code{getMifHistPath}: get path to the scenarios' historical.mif.
#' \code{getCfgScenPath}: get path to the scenarios' config.Rdata.
#' \code{getCfgDefaultPath}: get path to REMIND's  default.cfg.
#'
#'
#' @param outputDirs A character vector of paths to output folders of REMIND runs.
#' @param mustWork \code{logical(1)}. Throw error if file not available?
#' @rdname getPath
#' @export
getMifScenPath <- function(outputDirs, mustWork = FALSE) {
  scenNames <- lucode2::getScenNames(outputDirs)
  path <- file.path(outputDirs, paste0("REMIND_generic_", scenNames, ".mif"))
  return(normalizePath(path, mustWork = mustWork))
}

#' @rdname getPath
#' @export
getMifHistPath <- function(outputDirs, mustWork = FALSE) {
  path <- file.path(outputDirs, "historical.mif")
  normalizePath(path, mustWork = mustWork)
}

#' @rdname getPath
#' @export
getCfgScenPath <- function(outputDirs, mustWork = FALSE) {
  path <- file.path(outputDirs, "config.Rdata")
  normalizePath(path, mustWork = mustWork)
}

#' @param remindDir A single string. The path to the remind directory.
#' @rdname getPath
#' @export
getCfgDefaultPath <- function(remindDir = ".", mustWork = FALSE) {
  path <- file.path(remindDir, "config/default.cfg")
  normalizePath(path, mustWork = mustWork)
}
