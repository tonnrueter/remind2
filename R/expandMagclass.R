#' Expand Magclass Object
#'
#' A helper to that restricts and expands x to the size of ref.
#'
#' @param x a magclass object to be modified
#' @param ref a magclass object used as a reference for the modification
#' @param fill value to be set in new dimensions
#'
#' @export
expandMagclass <- function(x, ref, fill = 0) {
  # extend the object to the union of the both objects
  r <- new.magpie(
    cells_and_regions = union(getRegions(x), getRegions(ref)),
    years = union(getYears(x), getYears(ref)),
    names = union(getNames(x), getNames(ref)),
    fill = fill,
    sets = names(dimnames(ref))
  )
  # copy over values from x
  r[getRegions(x), getYears(x), getNames(x)] <- x

  # restrict object to dimensions of ref
  r <- r[getRegions(ref), getYears(ref), getNames(ref)]
  return(r)
}
