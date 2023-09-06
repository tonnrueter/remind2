#' checkVariablesNames checks remind2 reporting on inconsistency in variable names
#'
#' Pass a vector of variable names including the units. Get warnings if inconsistencies are found
#' for the reporting
#' @param vars vector with variable names and units such as "PE (EJ)"
#' @author Oliver Richters
#' @export
checkVariableNames <- function(vars) {

  barspace <- grep("[\\| ]{2}|^[\\| ]|[\\| ]$", vars, value = TRUE)
  if (length(barspace) > 0) {
    warning("These variable names have wrong bars and spaces: ", paste(barspace, collapse = ", "))
  }

  NAvar <- grep("[\\|\\( ]NA[\\|\\) ]|^NA", vars, value = TRUE)
  NAvar <- NAvar[! grepl("^Services and Products\\|Transport\\|non-LDV\\|S", NAvar)] # unit NA, but ok, see issue #408
  if (length(NAvar) > 0) {
    warning("These variables and units contain NA: ", paste(NAvar, collapse = ", "))
  }

  noUnit <- grep(" \\(.*\\)$", vars, value = TRUE, invert = TRUE)
  if (length(noUnit) > 0) {
    warning("These variables have no units: ", paste(noUnit, collapse = ", "))
  }
}
