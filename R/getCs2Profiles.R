#' Get the List of cs2 Profiles
#'
#' A list of arguments to \code{\link{compareScenarios2}} is called a (cs2-)
#' profile. This function loads a list of such profiles from a file. The default
#' argument is set such that the default profiles file of REMIND is loaded.
#'
#' @param path \code{character(1)}. Path to the profiles file.
#' @return A named list of profiles. The names are the names of the respective
#'   profile. Each profile is a named list. Those names correspond to arguments
#'   of \code{\link{compareScenarios2}}.
#' @author Christof Schoetz
#' @examples
#' \dontrun{
#' profiles <- getCs2Profiles() # load from default location
#' profiles <- getCs2Profiles(path = "path/to/profiles.json")
#' }
#' @export
getCs2Profiles <- function(path = "./scripts/cs2/profiles.json") {
  path <- normalizePath(path, mustWork = TRUE)
  profiles <- jsonlite::read_json(path, simplifyVector = FALSE)
  # Remove entries starting with "_". They are treated as comments.
  profiles <- profiles[!startsWith(names(profiles), "_")]
  return(profiles)
}
