#' Delete "+" in Variable Names. Optionally Save in File with Suffix
#' "_withoutPlus"
#'
#' The function can take a multiple of input formats. It replaces "|+|", "|++|",
#' "|+++|", ... by "|" in variable names and in this "deletes" the
#' "plus-notation".
#'
#' @param mif A path to a mif-file (might be created by confGDX2MIF), a magpie
#'   object containing reporting, a quitte object, or a character vector of
#'   variable names.
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
#' @importFrom utils write.table
deletePlus <- function(mif, writemif = FALSE) {

  # Does mif refer to a file?
  if (is.character(mif) && length(mif) == 1 && file.exists(mif)) {
    if (writemif == TRUE) {

      # read in mif-perorting
      rep <- read.csv(mif, sep = ";", check.names = FALSE)

      # drop the empty last column of .mif format
      if (all(is.na(rep[, ncol(rep)]))) {
        rep[, ncol(rep)] <- ""
      }

      rep$Variable <- .deletePlusString(rep$Variable)

      # save reporting with new names under a new name
      oldName <- gsub("\\.mif$", "", mif)
      newName <- paste0(oldName, "_withoutPlus.mif")
      write.table(rep, file = newName, quote = FALSE, sep = ";", row.names = FALSE)

    } else {

      # read in mif-reporting
      rep <- read.report(mif, as.list = FALSE)

      getNames(rep) <- .deletePlusString(getNames(rep))

      return(rep)
    }
  } else { # mif does not refer to a file.
    if (is.magpie(mif)) {
      getNames(mif) <- .deletePlusString(getNames(mif))
      return(mif)
    }
    if (quitte::is.quitte(mif)) {
      levels(mif$variable) <- .deletePlusString(levels(mif$variable))
      return(mif)
    }
    if (is.character(mif)) {
      mif <- .deletePlusString(mif)
      return(mif)
    }
    stop("Cannot handle input format of argument mif.")
  }
}

.deletePlusString <- function(s) {
  pattern <- "\\|\\++\\|"
  replacement <- "|"
  while (any(grepl(pattern, s))) {
    s <- gsub(pattern, replacement, s)
  }
  return(s)
}
