#' Variable Names as Hierarchical List
#'
#' Take a character vector of variables names with hierarchical structure
#' indicated by \code{|} in the name and convert it into a hierarchical
#' structure of named lists.
#'
#' @seealso \code{\link{createVarListHtml}} for creating an HTML-file of such a list.
#' @param x A character vector of variable names, a quitte object, or a
#'   character vectors of paths to mif files.
#' @param entry A string determining the entries of all leafs and the nodes
#'   which represent variables in \code{vars}. \code{"NULL"} puts \code{NULL}
#'   into the leafs of the resulting list. \code{"name"} places the full name of
#'   an existing variable as entry \code{nm} in the respective node.
#'   \code{"INFO"} puts a list of further information about this node in each
#'   node with existing variables, including \code{details} if specified.
#' @param usePlus \code{logical(1)}. If \code{FALSE}, removes \code{|+|, |++|,
#'   ...} from variable names.
#' @param details \code{NULL} or a data frame with a column \code{name}. The
#'   entries of further columns of \code{details} are put into the INFO nodes.
#' @return A hierarchical named list.
#' @author Christof Schoetz
#' @examples
#' vars <- c(
#'   "Emi|GHG|CO2", "Emi|GHG|CH4", "Emi|NOX",
#'   "FE", "FE|Buildings", "FE|Industry", "FE|Transport")
#' v <- variablesAsList(vars)
#' \dontrun{View(v)}
#' v <- variablesAsList(vars, entry = "name")
#' \dontrun{mip::showLinePlots(data, v$Emi$GHG$nm)}
#' details <- data.frame(name = vars, nr = seq_along(vars))
#' v <- variablesAsList(vars, entry = "INFO", details = details)
#' \dontrun{View(v)}
#'
#' \dontrun{
#' loadModeltest()
#' v <- variablesAsList(data, entry = "INFO")
#' View(v)
#' # Include structure induced by |+|,|++|, ...:
#' vp <- variablesAsList(data, entry = "INFO", usePlus = TRUE)
#' View(vp)
#' }
#'
#' \dontrun{View(variablesAsList("path/to/scenario.mif"))}
#' @export
variablesAsList <- function(
    x,
    entry = c("NULL", "name", "INFO"),
    usePlus = FALSE,
    details = NULL
) {

  stopifnot(identical(usePlus, TRUE) || identical(usePlus, FALSE))
  stopifnot(is.character(entry))
  stopifnot(is.null(details) || (is.data.frame(details)) && "name" %in% names(details))

  entry <- match.arg(entry)

  # Read x to define data and vars.
  data <- NULL
  vars <- NULL
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x) && length(x) < 100 && all(file.exists(x))) { # x are files
    data <- quitte::read.quitte(x)
  } else if (is.character(x)) {
    vars <- x
  } else {
    data <- quitte::as.quitte(x)
  }
  if (is.null(vars)) {
    if ("varplus" %in% names(data))
      vars <- data$varplus
    else if ("variable" %in% data)
      vars <- data$variable
    else
      stop("Object data does not contain a variable column.")
  }
  if (!usePlus) {
    vars <- deletePlus(vars)
  }
  vars <- as.character(vars)
  stopifnot(is.character(vars))
  stopifnot(is.null(data) || quitte::is.quitte(data))

  # Create a data frame summary from vars and data containing all relevant info for further processing.
  if (entry == "INFO") {
    message("This might take a minute or two...")

    if (is.null(data)) {
      df <- data.frame(name = vars)
    } else {
      df <- data
      df$name <- vars
      df$variable <- NULL
      df$varplus <- NULL
      df$value <- NULL
      df$period <- NULL
    }
    counts <- dplyr::count(df, .data$name)
    df <- distinct(df)
    summary <-
      df %>%
      group_by(.data$name) %>%
      summarize(across(everything(), ~ list(as.character(unique(.x))))) %>%
      left_join(counts, by = "name")
    if (!is.null(details)) {
      summary <- left_join(summary, details, by = "name")
    }
  } else {
    summary <- data.frame(name = vars)
  }

  splitList <- strsplit(summary$name, "|", fixed = TRUE)
  maxLen <- max(vapply(splitList, length, integer(1)))
  varsMatrix <- t(vapply(splitList, function(x) x[seq_len(maxLen)], character(maxLen)))

  # Function to recursively parse variable names and create node value.
  .splitMatrixAsList <- function(mat, prefix) {

    # Set the value of the node.
    if (prefix %in% summary$name) {
      nodeValue <- switch(
        entry,
        "NULL" = NULL,
        "name" = list(nm = prefix),
        "INFO" = {
          info <- filter(summary, .data$name == .env$prefix) # Exactly one row.
          info <- lapply(info, function(x) if(is.list(x) && length(x) == 1) x[[1]] else x)
          names(info) <- names(summary)
          list(INFO = info)
        }
      )
    } else {
      nodeValue <- NULL
    }

    # Termination condition.
    if (length(mat) == 0 || all(is.na(mat))) return(nodeValue)

    # Process subcategories.
    lst <- split(mat[, -1, drop = FALSE], mat[, 1])
    lst <- lapply(lst, matrix, ncol = NCOL(mat) - 1)
    newPrefixes <- paste0(prefix, if (nchar(prefix) > 0) "|", names(lst))
    resSubCategories <- Map(.splitMatrixAsList, lst, newPrefixes)

    return(c(nodeValue, resSubCategories))
  }

  res <- .splitMatrixAsList(varsMatrix, prefix = "")
  return(res)
}
