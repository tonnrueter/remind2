#' Read in GDX and extract climate assessment variables
#'
#' Read climate assessment variables from GDX file
#'
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#'
#' @author Tonn Rüter
#' @examples
#' \dontrun{
#' reportClimate(gdx)
#' }
#'
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass setNames mbind
reportClimate <- function(gdx) {
  tryCatch(
    # Climate assessment is turned of per default. Try reading climate asessment data from GDX, but fail
    # silently if none are available.
    {
      variables <- c(
        pm_globalMeanTemperature = "Temperature|Global Mean (K)",
        p15_forc_magicc = "Forcing (W/m2)"
      )
      return(mbind(lapply(names(variables), function(gamsVariable) {
        setNames(readGDX(gdx, gamsVariable, restore_zeros = FALSE), variables[[gamsVariable]])
      })))
    },
    # Catch warnings & errors to not confuse the log file
    warning = function(e) {
      return(NULL)
    },
    error = function(e) {
      return(NULL)
    }
  )
}