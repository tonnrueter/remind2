expandMagclass <- function(x, ref, fill = 0) {
  r <- new.magpie(
    cells_and_regions = union(getRegions(x), getRegions(ref)),
    years = union(getYears(x), getYears(ref)),
    names = union(getNames(x), getNames(ref)),
    fill = fill,
    sets = names(dimnames(ref))
  )
  r[getRegions(x), getYears(x), getNames(x)] <- x
  return(r)
}
