#' gets characters (line) from the terminal or from a connection
#'
#' @return string

getLine <- function() {
  if (interactive()) {
    s <- readline()
  } else {
    con <- file("stdin")
    defer({
      close(con)
    })
    s <- readLines(con, 1, warn = FALSE)
  }
  return(s)
}
