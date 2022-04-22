#' take two REMIND scenario-config*.csv files and print the difference,
#' comparing it to a default.cfg
#'
#' @param fileList vector containing two csv file paths. If empty, user can select
#' @param configfile path to configfile. If empty, uses './default.cfg'
#' @param row.names column in csv used for row.names. Use NULL for mapping files
#' @param renamedColumns vector with old and new column names such as c("old1" = "new1", "old2" = "new2"))
#' @param renamedRows vector with old and new row names such as c("old3" = "new3", "old4" = "new4"))
#' @param printit boolean switch (default: TRUE) whether function prints its output
#' @author Oliver Richters
#' @examples
#'
#'  \dontrun{
#'     compareScenConf(fileList = c("scenario_config1.csv", "scenario_config2.csv"),
#'     configfile = "default.cfg",
#'     renamedColumns = c("old1" = "new1", "old2" = "new2"),
#'     renamedRows = c("old3" = "new3", "old3" = "new4"))
#'   }
#'
#' @return list with $allwarnings and $out
#' @importFrom utils read.csv2
#' @export
compareScenConf <- function(fileList = "", configfile = "default.cfg", row.names = 1,
                            renamedColumns = NULL, renamedRows = NULL, printit = TRUE) {
  m <- c()
  folder <- getwd()
  if (length(fileList) != 2) {
    cat("Specify folder with .csv files. The script also searches in subdirectories.\n")
    cat(paste0("Press enter to use current: ", getwd(), ".\n"))
    folder <- getLine()
    if (folder == "") folder <- getwd()
    fileList <- c(Sys.glob(file.path(paste0(folder, "/scenario_config*.csv"))),
                  Sys.glob(file.path(paste0(folder, "/*/scenario_config*.csv"))),
                  Sys.glob(file.path(paste0(folder, "/config/*/scenario_config*.csv")))
    )
    cat("\nPlease choose old and new file:\n")
    cat(paste(seq_along(fileList), fileList, sep = ": "), sep = "\n")
    identifier <- as.numeric(strsplit(getLine(), ",")[[1]])
    if (length(identifier) < 2) stop("You must choose two files!")
    fileList <- fileList[identifier]
  }
  if (! file.exists(configfile)) {
    if (file.exists(paste0("config/", configfile))) {
      configfile <- paste0("config/", configfile)
    } else if (file.exists(paste0(folder, "/", configfile))) {
      configfile <- paste0(folder, "/", configfile)
    } else if (file.exists(paste0(folder, "/config/", configfile))) {
      configfile <- paste0(folder, "/config/", configfile)
    } else {
      m <- c(m, paste0("No configfile ", configfile, " found in ", getwd(), " or ", folder, "."))
      configfile <- FALSE
    }
  }
  m <- c(m, "", paste0("File comparison: ", fileList[[1]], " -> ", fileList[[2]]))
  if (! isFALSE(configfile)) {
    m <- c(m, paste("Using configfile", configfile, "as default."))
    cfg <- list() # to avoid 'no visible binding' error
    source(configfile, local = TRUE)
    if (!length(cfg)) stop(paste0("In ", configfile, ", no 'cfg' data was found."))
    # enable script to match default data not in gms
    try(cfg$gms[["output"]] <- paste0(cfg$output, collapse = ","))
    try(cfg$gms[["model"]] <- cfg$model)
    try(cfg$gms[["regionmapping"]] <- cfg$regionmapping)
    try(cfg$gms[["inputRevision"]] <- cfg$inputRevision)
  }

  settings1 <- read.csv2(fileList[[1]], stringsAsFactors = FALSE, row.names = row.names,
                         comment.char = "#", na.strings = "", dec = ".")
  settings2 <- read.csv2(fileList[[2]], stringsAsFactors = FALSE, row.names = row.names,
                         comment.char = "#", na.strings = "", dec = ".")

  # rename columns and rows in old file to new names after some checks
  allwarnings <- checkrowscolumns(settings1, settings2, renamedColumns, renamedRows)
  names(settings1)[names(settings1) %in% names(renamedColumns)] <-
    renamedColumns[intersect(names(renamedColumns), unlist(names(settings1)))]
  rownames(settings1)[rownames(settings1) %in% names(renamedRows)] <-
    renamedRows[intersect(names(renamedRows), unlist(rownames(settings1)))]

  # print comparison
  scenarios <- unique(c(rownames(settings1), rownames(settings2)))
  switchnames <- sort(unique(c(names(settings1), names(settings2))))
  m <- c(m, paste0("Columns deleted: ", paste(setdiff(switchnames, names(settings2)), collapse = ", ")))
  m <- c(m, paste0("Columns added:   ", paste(setdiff(switchnames, names(settings1)), collapse = ", ")))
  m <- c(m, paste0("Renamed columns: ", paste(names(renamedColumns), renamedColumns, sep = " -> ", collapse = ", ")))
  m <- c(m, paste0("Renamed rows:    ", paste(names(renamedRows), renamedRows, sep = " -> ", collapse = ", ")))
  for (s in scenarios) {
    if (s %in% rownames(settings1) & s %in% rownames(settings2)) {
      # scenario name, oldname -> newname if renamed
      if (! all(identical(toString(settings1[s, ]), toString(settings2[s, ])))) {
        m <- c(m, paste0(ifelse(s %in% renamedRows, paste(names(which(renamedRows == s)), "-> "), ""), s, ":"))
        for (c in intersect(names(settings1), names(settings2))) {
          # print only if different, if description was changed print only this fact
          if (! identical(toString(settings1[s, c]), toString(settings2[s, c]))) {
            m <- c(m, paste0("  ", ifelse(c %in% renamedColumns, paste(names(which(renamedColumns == c)), "-> "), ""),
                   c, ": ", ifelse(c == "description", "was changed", paste0(settings1[s, c], " -> ", settings2[s, c])),
                   ifelse(isFALSE(configfile), "", paste0(" (default: ", cfg$gms[[c]], ")"))))
          }
        }
      }
    } else {
      m <- c(m, paste0(s, ifelse(s %in% rownames(settings1), " was deleted.", " was added.")))
    }
  }
  if (printit) {
    message(paste0(m, collapse = "\n"), "\n");
    if (length(allwarnings) > 0) warning(paste0(allwarnings, collapse = "\n"), "\n")
    return(list(allwarnings = allwarnings))
  } else {
    return(list(allwarnings = allwarnings, out = m))
  }
}

checkrowscolumns <- function(settings1, settings2, renamedColumns, renamedRows) {
  allwarnings <- c()
  if (any(! names(renamedColumns) %in% names(settings1))) {
    allwarnings <- c(allwarnings, "oldColNotIn1" = paste(
      "Old column name(s) not present in first file:", setdiff(names(renamedColumns), names(settings1))))
  }
  if (any(names(renamedColumns) %in% names(settings2))) {
    allwarnings <- c(allwarnings, "oldColAlsoIn2" = paste(
      "Old column name(s) also present in second file:", setdiff(names(renamedColumns), names(settings2))))
  }
  if (any(! renamedColumns %in% names(settings2))) {
    allwarnings <- c(allwarnings, "newColNotIn2" = paste(
      "New column name(s) not present in second file:", setdiff(renamedColumns, names(settings2))))
  }
  if (any(renamedColumns %in% names(settings1))) {
    allwarnings <- c(allwarnings, "newColAlsoIn1" = paste(
      "New column name(s) also present in first file: ", setdiff(renamedColumns, names(settings1))))
  }
  if (any(! names(renamedRows) %in% rownames(settings1))) {
    allwarnings <- c(allwarnings, "newRowNotIn1" = paste(
      "Old row name(s) not present in first file:", setdiff(names(renamedRows), rownames(settings1))))
  }
  if (any(names(renamedRows) %in% rownames(settings2))) {
    allwarnings <- c(allwarnings, "oldRowAlsoIn2" = paste(
      "Old row name(s) also present in second file:", setdiff(names(renamedRows), rownames(settings2))))
  }
  if (any(! renamedRows %in% rownames(settings2))) {
    allwarnings <- c(allwarnings, "newRowNotIn2" = paste(
      "New row name(s) not present in second file: ", setdiff(renamedRows, rownames(settings2))))
  }
  if (any(renamedRows %in% rownames(settings1))) {
    allwarnings <- c(allwarnings, "newRowAlsoIn1" = paste(
      "New row name(s) also present in first file: ", setdiff(renamedRows, rownames(settings1))))
  }
  return(allwarnings)
}
