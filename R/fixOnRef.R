# Fix on reference

#' for a given magclass object, reset values for years smaller than a start year
#' to the values of a reference gdx

#' @param x a magclass object produced during reporting to be manipulated
#' @param gdx_ref the reference gdx to be used for overwriting values
#' @param startYear years before will be overwritten with values from gdx_ref
#' @param reportFunc reporting function to be used to generate the variables
#' from the reference gdx
#' @param reportArgs list of arguments to be passed to the reporting function,
#' 'gdx' and 'gdx_ref' must not be added, as they are set by the function
#'
#' @author Falk Benke
fixOnRef <- function(x, gdx_ref, startYear, reportFunc, reportArgs = list()) {

  if (is.null(gdx_ref)) {
    return(x)
  }

  fixedYears <- getYears(x)[getYears(x, as.integer = TRUE) < startYear]

  if (length(fixedYears) == 0) {
    return(x)
  }

  fName <- as.character(substitute(reportFunc))

  message(paste0(fName, " loads price for < ", startYear, " from gdx_ref."))
  ref <- try(
    do.call(reportFunc, c(gdx = gdx_ref, reportArgs, gdx_ref = NULL))
  )
  if (!inherits(ref, "try-error")) {
    joinedNames <- intersect(getNames(x), getNames(ref))
    joinedRegions <- intersect(getItems(ref, dim = 1), getItems(x, dim = 1))
    x[joinedRegions, fixedYears, joinedNames] <- ref[joinedRegions, fixedYears, joinedNames]
  } else {
    message(paste0("failed to run ", fName, " on gdx_ref"))
  }

  return(x)
}
