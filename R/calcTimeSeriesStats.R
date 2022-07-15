#' Calculate Statistics of Time Series
#'
#' Take a quitte object and calculate given summary statistics of the time
#' series of given variables in the quitte object. Return the result for each
#' model, scenario, region, variable, and statistic in a data frame.
#'
#' @param data A quitte object.
#' @param var character(n). The names of the variables to calculate the
#'   statistics on.
#' @param stats A named list of functions with two arguments. Each function will
#'   be called with the vector of values as the first argument and the vector of
#'   corresponding periods as the second argument.
#' @param from,to Optional variables to filter the time series by period.
#' @return A data frame with columns model, scenario, region, variable, value,
#'   statistic, unit. "value" contains the calculated statistics. "statistic" is
#'   name of the statistics, i.e., the name of the function in the argument
#'   \code{stats}. The "unit" is the unit of the original variable and not
#'   necessarily the correct unit for the calculated statistics.
#' @examples
#'   \dontrun{
#'     calcTimeSeriesStats(
#'       data, \
#'       "Emi|CO2",
#'       stats = list("Net 0 Period" = \(v, p) p[which(v <= 0)[1]]))
#'   }
#' @importFrom dplyr all_of %>% filter distinct group_by summarize mutate
#'   left_join bind_rows
#' @importFrom rlang .env .data
#' @export
calcTimeSeriesStats <- function(
    data,
    var,
    stats,
    from = -Inf,
    to = 2100) {

  stopifnot(quitte::is.quitte(data))
  stopifnot(is.list(stats))
  stopifnot(length(stats) > 0)
  stopifnot(!is.null(names(stats)))
  stopifnot(is.character(var))
  stopifnot(is.numeric(from) && is.numeric(to))
  stopifnot(length(from) == 1 && length(to) == 1)

  idCols <- c("model", "scenario", "region", "variable")
  d <- data %>%
    filter(.data$variable %in% .env$var, .data$period >= .env$from, .data$period <= .env$to)
  if (nrow(d) == 0) {
    warning("No data after filtering for `var`, `from`, and `to`.")
  }
  unit <- d %>% distinct(across(all_of(.env$idCols)), .data$unit)

  res <- NULL
  for (i in seq_along(stats)) {
    res <- d %>%
      group_by(across(all_of(.env$idCols))) %>%
      summarize(value = stats[[i]](.data$value, .data$period), .groups = "drop") %>%
      mutate(statistic = names(stats)[i]) %>%
      left_join(unit, by = idCols) %>%
      bind_rows(res)
  }
  return(res)
}
