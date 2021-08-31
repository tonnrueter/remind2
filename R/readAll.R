#' readAll
#'
#' (This function was copied from magpie::read_all to remove the magpie dependency.)
#' Function to read in input from multiple gdx files.
#'
#' @param pathToGdx A vector or list. Can contain either the filenames of gdx files or GDX lists as created by readGDX.
#'                  If it is named, the names will also be used for the output)
#' @param func The output function that should be executed on the gdx files. E.g. emissions
#' @param asList If TRUE, the output will be a list of length(gdx). If FALSE, read_all tries to store everything in one
#'               magpie object.
#' @param ... Additional arguments passed to func.
#' @return A list of magpie objects (as.list=TRUE) or one magpie object (as.list=FALSE) with the output returned by func
#'         for all the gdx files
#' @author Markus Bonsch
#' @examples
#' \dontrun{
#' gdxPaths <- c(baseline = "fulldata1.gdx", policy = "fulldata2.gdx")
#' croparea <- read_all(gdxPaths, func = croparea, level = "glo", crop_aggr = TRUE, asList = TRUE)
#' }
#' @importFrom magclass ndata mbind
readAll <- function(pathToGdx, func, asList = TRUE, ...) {
  if (!is.list(pathToGdx)) {
    pathToGdx <- as.list(pathToGdx)
  }
  if (identical(names(pathToGdx), c(
    "aliases", "sets", "equations",
    "parameters", "variables"
  ))) {
    pathToGdx <- list(pathToGdx)
  }
  out <- list()
  for (i in seq_along(pathToGdx)) {
    out[[i]] <- func(pathToGdx[[i]], ...)
    if (!is.null(names(pathToGdx))) {
      names(out)[i] <- names(pathToGdx)[i]
    }
  }
  if (!all(lapply(out, ndata) == ndata(out[[1]]))) {
    stop("ERROR: different data dimensions. Can't readAll")
  }
  if (asList) {
    return(out)
  } else if (length(out) == 1) {
    out <- setNames(out[[1]], paste(names(out), getNames(out[[1]]),
      sep = "."
    ))
    getNames(out) <- sub("\\.$", "", getNames(out))
    getNames(out) <- sub("^\\.", "", getNames(out))
    return(out)
  } else {
    inp <- out
    if (is.null(names(inp))) {
      names(inp) <- seq_along(inp)
    }
    out <- NULL
    for (i in seq_along(inp)) {
      tmp <- inp[[i]]
      getNames(tmp) <- paste(names(inp)[i], getNames(tmp),
        sep = "."
      )
      out <- mbind(out, tmp)
    }
    getNames(out) <- sub("\\.$", "", getNames(out))
    return(out)
  }
}
