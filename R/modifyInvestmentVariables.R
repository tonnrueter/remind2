#' Modify Investment Variables
#'
#' a helper to adapt investment variables in reporting after changes in https://github.com/remindmodel/remind/pull/1238
#'
#' calculates the value of each timestep for a given maglcass object
#' as the average of this timestep and the next timestep (last timestep remains unchanged)
#'
#'
#' @author Falk Benke
modifyInvestmentVariables <- function(x) {
  # obtain 't+1'
  idx <- c(tail(getYears(x, as.integer = TRUE), -1), tail(getYears(x, as.integer = TRUE), 1))
  # retrieve values for 't+1'  and assign to 't'
  tmp <- x[, idx, ]
  getItems(tmp, dim = 2) <- getItems(x, dim = 2)
  # calculate average of 't' and 't'+1
  out <- (x + tmp) / 2
  return(out)
}
