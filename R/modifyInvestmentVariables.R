#' Modify Investment Variables
#'
#' a helper to adapt investment variables in reporting after changes in
#' https://github.com/remindmodel/remind/pull/1238
#'
#' for a given magclass object, calculates the value of each timestep
#' as the average of this timestep and the next timestep
#' the data for the last timestep in the magclass object remains unchanged
#'
#' @param x a magclass object to be manipulated
#' @param ref an optional magclass object to be used for fixing values before 'startYear'
#' @param startYear years before will be overwritten with values from 'ref'
#'
#'
#' @author Falk Benke
modifyInvestmentVariables <- function(x, ref = NULL, startYear = NULL) {

  # obtain 't+1'
  idx <- c(tail(getYears(x, as.integer = TRUE), -1), tail(getYears(x, as.integer = TRUE), 1))

  # retrieve values for 't+1'  and assign to 't'
  tmp <- x[, idx, ]
  getItems(tmp, dim = 2) <- getItems(x, dim = 2)

  # calculate average of 't' and 't'+1
  x <- (x + tmp) / 2

  if (!is.null(ref)) {
    fixedYears <- getYears(x)[getYears(x, as.integer = TRUE) < startYear]

    if (length(fixedYears) == 0) {
      return(x)
    }

    ref <- modifyInvestmentVariables(ref)
    joinedNames <- intersect(getNames(x), getNames(ref))
    joinedRegions <- intersect(getItems(ref, dim = 1), getItems(x, dim = 1))
    x[joinedRegions, fixedYears, joinedNames] <- ref[joinedRegions, fixedYears, joinedNames]
  }

  return(x)
}
