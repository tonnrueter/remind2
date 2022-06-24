#' Delete "+" in Variable Names. Optionally Save in File with Suffix
#' "_withoutPlus"
#'
#' The function can take a multiple of input formats. It replaces "|+|", "|++|",
#' "|+++|", ... by "|" in variable names and in this "deletes" the
#' "plus-notation".
#'
#' @param mif A path to a mif-file (might be created by confGDX2MIF), a magpie
#'   object containing reporting, a quitte object, or a character vector or
#'   factor of variable names.
#' @param writemif A single truth value. Only used when mif is a file path.
#'   Should the new mif with the adjusted names be saved? If yes the new name
#'   will have the suffix "_withoutPlus.mif".
#' @return If mif is a file path and \code{writemif} is \code{FALSE}, a MAgPIE
#'   object. If mif is not a file path the result is the object \code{mif} with
#'   replaced variable names.
#'
#' @examples
#' deletePlus(c(
#'   "ab|cd", # "ab|cd"
#'   "de|+|fgh", # "de|fgh"
#'   "i|j|++|k|l|++++|mno|p", # "i|j|k|l|mno|p"
#'   "++|q+|+r|+p+|u+v|+", # "++|q+|+r|+p+|u+v|+"
#'   "w|+|+|x")) # "w|x"
#' @export
deletePlus <- function(mif, writemif = FALSE) {
  modifyVariableNames(
    mif,
    modify = .deletePlusString,
    writeMifSuffix = if (writemif) "_withoutPlus" else NULL
  )
}

.deletePlusString <- function(s) {
  pattern <- "\\|\\++\\|"
  replacement <- "|"
  while (any(grepl(pattern, s))) {
    s <- gsub(pattern, replacement, s)
  }
  return(s)
}

#' Substitute Placeholders in Variable Names.
#'
#' @param x A path to a mif-file (might be created by confGDX2MIF), a quitte
#'   object, or a character vector or factor of variable names.
#' @param writeMifSuffix \code{NULL} or a suffix for the filename if the result
#'   should be written as a mif-file.
#' @param substitutions A named character vector of placeholders with
#'   replacements as names.
#' @return If \code{x} is not a file path the result is the object \code{x} with
#'   replaced variable names.
#' @examples
#' substituePlaceholder(c(
#'   "3$dot$5", # "3.5"
#'   "3$dot5", # "3$dot5"
#'   "3.5", # "3.5"
#'   "$dot$dot$dot$dot$dot$")) # ".dot.dot."
#' @export
substituePlaceholder <- function(
    x, writeMifSuffix = NULL,
    substitutions = c(
      "." = "$dot$"
  )) {
  modifyVariableNames(
    x,
    modify = .substituePlaceholderString,
    writeMifSuffix = writeMifSuffix,
    substitutions = substitutions
  )
}

.substituePlaceholderString <- function(s, substitutions) {
  for (i in seq_along(substitutions)) {
    placeholder <- substitutions[i]
    replacement <- names(substitutions)[i]
    while (any(grepl(placeholder, s, fixed = TRUE))) {
      s <- gsub(placeholder, replacement, s, fixed = TRUE)
    }
  }

  return(s)
}

#' @param x A path to a mif-file (might be created by confGDX2MIF), a magpie
#'   object containing reporting, a quitte object, or a character vector or
#'   factor of variable names.
#' @param writeMifSuffix \code{NULL} or a suffix for the file name if the result
#'   should be written as a mif-file.
#' @param modify A function that modifies a character vector.
#' @param ... Further arguments for the modify function.
#' @return If \code{x} is a file path and \code{writeMifSuffix} is \code{NULL},
#'   a MAgPIE object. If \code{x} is not a file path the result is the object
#'   \code{x} with replaced variable names.
modifyVariableNames <- function(x, modify, writeMifSuffix = NULL, ...) {

  # Does x refer to a file?
  if (is.character(x) && length(x) == 1 && file.exists(x)) {

    # Should the output be written to a file?
    if (is.character(writeMifSuffix) == TRUE) {

      # read in mif-reporting
      mifData <- read.csv(x, sep = ";", check.names = FALSE)

      # drop the empty last column of .mif format
      if (all(is.na(mifData[, ncol(mifData)]))) {
        mifData[, ncol(mifData)] <- ""
      }

      mifData$Variable <- modify(mifData$Variable, ...)

      # save reporting with new names under a new name
      oldName <- gsub("\\.mif$", "", x)
      newName <- paste0(oldName, writeMifSuffix, ".mif")
      write.table(mifData, file = newName, quote = FALSE, sep = ";", row.names = FALSE)

    } else {

      # read in mif-reporting
      mifData <- read.report(x, as.list = FALSE)
      getNames(mifData) <- modify(getNames(mifData), ...)
      return(mifData)
    }

  } else { # x does not refer to a file.

    if (is.magpie(x)) {
      getNames(x) <- modify(getNames(x), ...)
      return(x)
    }

    if (quitte::is.quitte(x)) {
      levels(x$variable) <- modify(levels(x$variable), ...)
      return(x)
    }

    if (is.factor(x)) {
      levels(x) <- modify(levels(x), ...)
      return(x)
    }

    if (is.character(x)) {
      x <- modify(x, ...)
      return(x)
    }

    stop("Cannot handle input format.")
  }
}

