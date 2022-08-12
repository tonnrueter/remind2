#' Variable Names as Hierarchical List
#'
#' Take a character vector of variables names with hierarchical structure
#' indicated by \code{|} in the name and convert it into a hierarchical
#' structure of named lists.
#'
#' @param x A character vector of variable names, a quitte object, or a path to
#'   a mif file.
#' @param entry A string determining the entries of all leafs and the nodes
#'   which represent variables in \code{vars}. \code{"NULL"} puts \code{NULL}
#'   into the leafs of the resulting list. \code{"name"} places the full name of
#'   an existing variable as entry \code{nm} in the respective node.
#'   \code{"INFO"} puts a list of further information about this node in each
#'   node with existing variables, which requires \code{x} to be a mif file or a
#'   quitte object.
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
#'
#' \dontrun{
#' loadModeltest()
#' v <- variablesAsList(data, entry = "INFO")
#' View(v)
#' # Include structure induced by |+|,|++|, ...:
#' vp <- variablesAsList(data$varplus, entry = "INFO")
#' View(vp)
#' }
#'
#' \dontrun{View(variablesAsList("path/to/scenario.mif"))}
#' @export
variablesAsList <- function(x, entry = c("NULL", "name", "INFO")) {

  entry <- match.arg(entry)

  # Read x to define data and vars.
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    if (length(x) == 1 && file.exists(x)) {
      data <- quitte::read.quitte(x, na.strings = c("UNDF", "NA", "N/A", "n_a", "Inf", "-Inf"))
      vars <- data$variable
    } else {
      vars <- x
      data <- NULL
    }
  } else {
    data <- quitte::as.quitte(data)
    vars <- data$variable
  }

  if (entry == "INFO" && is.null(data)) {
    stop("For entry=\"INFO\", full data must be provided. Pass a quitte object or the path to a mif file.")
  }

  if (entry == "INFO") {
    message("This might take a minute or two...")
  }

  # convert factors to character vectors
  if (!is.null(data)) {
    data$model <- as.character(data$model)
    data$scenario <- as.character(data$scenario)
    data$region <- as.character(data$region)
    data$unit <- as.character(data$unit)
  }

  varsTable <- table(vars)
  uniqueVars <- names(varsTable)
  splitList <- strsplit(uniqueVars, "|", fixed = TRUE)
  maxLen <- max(vapply(splitList, length, integer(1)))
  varsMatrix <- t(vapply(splitList, function(x) x[seq_len(maxLen)], character(maxLen)))

  # Function to recursively parse variable names and create node value.
  .splitMatrixAsList <- function(mat, prefix) {

    # Set the value of the node.
    if (prefix %in% uniqueVars) {
      nodeValue <- switch(
        entry,
        "NULL" = NULL,
        "name" = list(nm = prefix),
        "INFO" = {
          sel <- data$variable == prefix
          list(INFO = list(
            nm = prefix,
            count = unname(varsTable[prefix]),
            model = unique(data$model[sel]),
            scenario = unique(data$scenario[sel]),
            region = unique(data$region[sel]),
            unit = unique(data$unit[sel])))
        })
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
