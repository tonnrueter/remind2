#' Test Ranges on MAgPIE Object
#'
#' @param data A [`magpie`][magclass::maglcass] object
#' @param regex A character vector of regular expressions, or a list of such
#'     vectors, for selecting variables from `data` to test.
#' @param low A numerical lower bound, or a list of such bounds, to test the
#'     selected variables against.  If `regex` is a list, the variables in each
#'     list entry are tested against the corresponding entry in `low` (which is
#'     recycled if it is shorter then `regex`).
#' @param up A numerical upper bound, or a list of such bounds, to test the
#'     selected variables against.  If `regex` is a list, the variables in each
#'     list entry are tested against the corresponding entry in `up` (which is
#'     recycled if it is shorter then `regex`).
#' @param warn.missing Boolean indication whether a `regex` matching no
#'     variables in `data` should be ignored (default) or cause a warning.
#'
#' @return `NULL`.  `test_ranges()` is called for its side effects of issuing
#'     errors.
#'
#' @importFrom dplyr %>% filter distinct pull
#' @importFrom quitte magclass_to_tibble
#' @importFrom rlang !! sym
#' @importFrom tidyr unite

suppressPackageStartupMessages(
  { stopifnot(require(dplyr, quietly = TRUE))
    stopifnot(require(quitte, quietly = TRUE))
    stopifnot(require(tidyr, quietly = TRUE))
    stopifnot(require(rlang, quietly = TRUE))
  })

test_ranges <- function(data, regex, low = NULL, up = NULL,
                        warn.missing = FALSE) {
  if (!is.list(regex))
    regex <- list(regex)

  if (!is.list(low))
    low <- list(low)

  if (!is.list(up))
    up <- list(up)

  low <- rep_len(low, length(regex))
  up  <- rep_len(up,  length(regex))

  .test <- function(data_variables, low, up) {
    variable_name <- tail(strsplit(names(dimnames(data_variables))[[3]], '.',
                                   fixed = TRUE)[[1]],
                          n = 1)

    low_data <- if (!is.null(low) && any(data_variables < low)) {
      data_variables %>%
        magclass_to_tibble() %>%
        filter(value < low) %>%
        distinct(!!sym(variable_name), .keep_all = TRUE) %>%
        unite('text', everything(), sep = '   ') %>%
        pull('text')
    }
    else {
      character()
    }

    up_data <- if (!is.null(up) && any(data_variables > up)) {
      data_variables %>%
        magclass_to_tibble() %>%
        filter(value > up) %>%
        distinct(!!sym(variable_name), .keep_all = TRUE) %>%
        unite('text', everything(), sep = '   ') %>%
        pull('text')
    }
    else {
      character()
    }

    if (length(low_data))
      low_data <- c(paste('variables exceeding lower limit', low),
                    low_data)
    if (length(up_data))
      up_data <- c(paste('variables exceeding upper limit', up),
                   up_data)

    return(list(low_data, up_data))
  }

  data_names <- tail(getNames(data, fulldim = TRUE), 1)[[1]]
  msg <- list()
  for (i in seq_along(regex)) {
    for (r in regex[[i]]) {

      variables <- grep(r, data_names, value = TRUE)
      if (warn.missing && 0 == length(variables)) {
        warning('No variables match regex "', r, '"')
      }

      msg <- append(
        msg,
        .test(data[,,variables], low[[i]], up[[i]]) %>%
          Filter(function(x) { 0 != length(x) }, x = .) %>%
          lapply(paste, collapse = '\n')
      )
    }
  }

  if (length(msg))
    stop(paste('range error\n', msg, collapse = '\n'))
}
