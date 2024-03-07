# TODO: add documentation

expandMagclass <- function(x, ref, fill = 0, includeNames = TRUE) {
  r <- new.magpie(
    cells_and_regions = union(getRegions(x), getRegions(ref)),
    years = union(getYears(x), getYears(ref)),
    names = if (includeNames) union(getNames(x), getNames(ref)) else getNames(x),
    fill = fill,
    sets = names(dimnames(ref))
  )
  r[getRegions(x), getYears(x), getNames(x)] <- x
  r <- r[getRegions(ref), getYears(ref), ]
  if (includeNames) {
    r <- r[, , getNames(ref)]
  }
  return(r)
}
