#' take two REMIND scenario-config*.csv files and print the difference,
#' comparing it to a default.cfg
#'
#' @param fileList vector containing two csv file paths as c(oldfile, newfile). If empty, user can select
#' @param configfile path to configfile. If empty, uses './default.cfg'
#' @param row.names column in csv used for row.names. Use NULL for mapping files
#' @param renamedCols vector with old and new column names such as c("old1" = "new1", "old2" = "new2"))
#' @param renamedRows vector with old and new row names such as c("old3" = "new3", "old3" = "new4", "old5" = "new5"))
#'        the "old" name can also remain in the new file, if you generated a variant
#' @param printit boolean switch (default: TRUE) whether function prints its output
#' @author Oliver Richters
#' @examples
#'
#'  \dontrun{
#'     compareScenConf(fileList = c("scenario_config_old.csv", "scenario_config_new.csv"),
#'     configfile = "default.cfg",
#'     renamedCols = c("old1" = "new1", "old2" = "new2"),
#'     renamedRows = c("old3" = "new3", "old4" = "new4"))
#'   }
#'
#' @return list with $allwarnings and $out
#' @importFrom utils read.csv2
#' @export
compareScenConf <- function(fileList = "", configfile = "default.cfg", row.names = 1,
                            renamedCols = NULL, renamedRows = NULL, printit = TRUE) {
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
    cat("\nPlease choose old and new file, separated with comma:\n")
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

  # for mapping files
  if (is.null(row.names)) {
    rownames(settings1) <- make.unique(paste0(settings1[, 1], ": ", settings1[, 2]))
    rownames(settings2) <- make.unique(paste0(settings2[, 1], ": ", settings2[, 2]))
  }

  # rename columns and rows in old file to new names after some checks
  allwarnings <- checkRowsCols(settings1, settings2, renamedCols, renamedRows)
  for (colname in intersect(names(settings1), names(renamedCols)))
    names(settings1)[names(settings1) == colname] <- renamedCols[colname]

  # print comparison
  scenarios <- unique(c(rownames(settings2), rownames(settings1)))
  switchnames <- sort(unique(c(names(settings1), names(settings2))))
  deletedCols <- setdiff(switchnames, names(settings2))
  addedCols <- setdiff(switchnames, names(settings1))
  jointCols <- intersect(names(settings1), names(settings2))
  m <- c(m, paste0("Columns deleted: ", ifelse(length(deletedCols) > 0, paste(deletedCols, collapse = ", "), "-")))
  m <- c(m, paste0("Columns added:   ", ifelse(length(addedCols) > 0, paste(addedCols, collapse = ", "), "-")))
  m <- c(m, paste0("Renamed columns: ", ifelse(length(renamedCols) > 0,
                   paste(names(renamedCols), renamedCols, sep = " -> ", collapse = ", "), "-")))
  m <- c(m, paste0("Renamed rows:    ", ifelse(length(renamedRows) > 0,
                   paste(names(renamedRows), renamedRows, sep = " -> ", collapse = ", "), "-")))

  settings1[, addedCols] <- ""
  m <- c(m, "", "Changes in the scenarios:")
  for (s in scenarios) {
    if (s %in% intersect(c(rownames(settings1), renamedRows), rownames(settings2))) {
      # scenario name, oldname -> newname if renamed
      sold <- if (s %in% renamedRows) names(renamedRows[s == renamedRows]) else s
      if (! all(identical(toString(settings1[sold, jointCols]), toString(settings2[s, jointCols])))) {
        m <- c(m, paste0("~ ", ifelse(s %in% renamedRows, paste(sold, "-> "), ""), s, ":"))
        for (c in jointCols) {
          # print only if different, if description was changed print only this fact
          if (! identical(toString(settings1[sold, c]), toString(settings2[s, c]))) {
            m <- c(m, paste0("    ", ifelse(c %in% renamedCols, paste(names(which(renamedCols == c)), "-> "), ""), c,
                   ": ", ifelse(c == "description", "was changed", paste0(settings1[sold, c], " -> ", settings2[s, c])),
                   ifelse(isFALSE(configfile) || is.null(cfg$gms[[c]]), "", paste0(" (default: ", cfg$gms[[c]], ")"))))
          }
        }
      }
    } else if (! s %in% names(renamedRows)) {
      m <- c(m, ifelse(s %in% rownames(settings1), paste0("- ", s, " was deleted."), paste0("+ ", s, " was added.")))
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

checkRowsCols <- function(settings1, settings2, renamedCols, renamedRows) {
  allwarnings <- c()
  if (any(! names(renamedCols) %in% names(settings1))) {
    allwarnings <- c(allwarnings, "oldColNotIn1" = paste(
      "Old column name(s) not present in first file:", setdiff(names(renamedCols), names(settings1))))
  }
  if (any(names(renamedCols) %in% names(settings2))) {
    allwarnings <- c(allwarnings, "oldColAlsoIn2" = paste(
      "Old column name(s) also present in second file:", setdiff(names(renamedCols), names(settings2))))
  }
  if (any(! renamedCols %in% names(settings2))) {
    allwarnings <- c(allwarnings, "newColNotIn2" = paste(
      "New column name(s) not present in second file:", setdiff(renamedCols, names(settings2))))
  }
  if (any(renamedCols %in% names(settings1))) {
    allwarnings <- c(allwarnings, "newColAlsoIn1" = paste(
      "New column name(s) also present in first file: ", setdiff(renamedCols, names(settings1))))
  }
  if (any(! names(renamedRows) %in% rownames(settings1))) {
    allwarnings <- c(allwarnings, "newRowNotIn1" = paste(
      "Old row name(s) not present in first file:", setdiff(names(renamedRows), rownames(settings1))))
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
