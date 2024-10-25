#' Modify Investment Variables
#'
#' Timesteps for investment variables represent a different year range than
#' general REMIND reporting. This function transforms investment data to the
#' general REMIND timesteps representation.
#'
#' See also: https://github.com/remindmodel/remind/pull/1238.
#'
#' For investment variables, the timesteps represent the years as follows:
#' * 5-year timesteps: t represents t .. t-4, e.g. for 2005: 2001-2005
#  * 10-year timesteps: t represents t .. t-9, e.g. for 2070: 2061-2070
#  * 20-year timesteps: t represents t .. t-19, e.g. for 2150: 2131-2150
#'
#' In REMIND, the timesteps represent the years around the timestep, taking
#' into account the length of the period between t-1..t as well as t..t+1
#' @seealso \code{\link{quitte::remind_timesteps}}
#'
#' @param x a magclass object to be manipulated, must have timesteps in 'ttot'
#' @param ref an optional magclass object to be used for fixing values before 'startYear'
#' @param startYear years before will be overwritten with values from 'ref'
#'
#'
#' @author Falk Benke
#'
#'
modifyInvestmentVariables <- function(x, ref = NULL, startYear = NULL) {

  ttot <- c(seq(1900, 2060, 5), seq(2070, 2110, 10), 2130, 2150)

  if (!setequal(getYears(x, as.integer = TRUE), ttot)) {
    stop("Timesteps must equal to 'ttot'")
  }

  # interpretation of investment timesteps for 'ttot'
  investTs <- rbind(
    # first year is 1898, as 1990 represents 1898 to 1902 in REMIND
    data.frame(year = seq(1898, 2110, 1)) %>%
      mutate("period" = ifelse(
        .data$year <= 2060,
        ifelse(.data$year %% 5 == 0, .data$year, .data$year + 5 - (.data$year %% 5)),
        ifelse(.data$year %% 10 == 0, .data$year, .data$year + 10 - (.data$year %% 10))
      )),
    # highest year is 2167, as 2150 represents 2140 - 2167 in REMIND
    data.frame(year = seq(2111, 2167, 1)) %>%
      mutate("period" = ifelse(.data$year <= 2130, 2130, 2150))
  )

  # interpretation of REMIND timesteps for 'ttot'
  remindTs <-
    rbind(
      # mapping for 2005 - 2150 from quitte
      quitte::remind_timesteps,
      # enhance the logic to cover all years starting from 1898 (e.g. period 2000 represents 1998 - 2002)
      data.frame(year = seq(1898, 2002, 1), weight = 1) %>%
        mutate("period" = ifelse(.data$year %% 10 %in% c(1, 2, 8, 9), round(.data$year, digits = -1),
          ifelse(.data$year %% 10 %in% c(6, 7), .data$year - (.data$year %% 5),
            ifelse(.data$year %% 10 %in% c(3, 4), .data$year + 5 - (.data$year %% 5), .data$year)
          )
        ))
    )

  investTs <- investTs %>%
    mutate(
      "year" = paste0("y", .data$year),
      "period" = paste0("y", .data$period),
    )

  remindTs <- remindTs %>%
    mutate(
      "year" = paste0("y", .data$year),
      "period" = paste0("y", .data$period),
    )

  # map investment timesteps to yearly timesteps
  x <- toolAggregate(x, dim = 2, rel = investTs, from = "period", to = "year", verbosity = 2)

  w <- remindTs %>%
    select(-"period") %>%
    # period x weight combos should be distinct for this to work
    distinct() %>%
    as.magpie()

  x <- toolAggregate(x, dim = 2, rel = remindTs, weight = w, from = "year", to = "period")

  if (!is.null(ref)) {
    joinedYears <- intersect(getYears(x, as.integer = TRUE), getYears(ref, as.integer = TRUE))
    fixedYears <- joinedYears[joinedYears < startYear]
    if (length(fixedYears) == 0) {
      return(x)
    }
    ref <- modifyInvestmentVariables(ref)
    joinedNames <- intersect(getNames(x), getNames(ref))
    joinedRegions <- intersect(getItems(ref, dim = 1), getItems(x, dim = 1))
    x[joinedRegions, fixedYears, joinedNames] <- ref[joinedRegions, fixedYears, joinedNames]
  }

  return(x)
}
