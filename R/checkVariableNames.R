#' checkVariablesNames checks remind2 reporting on inconsistency in variable names
#'
#' Pass a vector of variable names including the units. Get warnings if inconsistencies are found
#' for the reporting
#' @param vars vector with variable names and units such as "PE (EJ)"
#' @importFrom piamInterfaces checkVarNames
#' @author Oliver Richters

checkVariableNames <- function(vars) {
  exception <- "^Services and Products\\|Transport\\|non-LDV\\|S" # unit NA, but ok, see issue #408
  vars <- grep(exception, vars, value = TRUE, invert = TRUE)
  checkVarNames(vars)
}
