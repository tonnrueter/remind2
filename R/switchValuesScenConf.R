#' searches all scenario config files of REMIND directory for a switch and prints the occurences
#'
#' @param switchname string with switch that is searched in scenario config files.
#'                   If NULL, prints all switches with only one value in main.gms and scenario config files
#' @param directory path to REMIND directory
#' @author Oliver Richters
#' @importFrom gms readDefaultConfig
#' @return values used in default and all scenario configs
#'         if switchname = NULL, then list of all unique values
#' @export
switchValuesScenConf <- function(switchname = NULL, directory = ".") {
  csvfiles <- Sys.glob(c(file.path(directory, "scenario_config*.csv"),
                            file.path(directory, "config", "scenario_config*.csv"),
                            file.path(directory, "config", "*", "scenario_config*.csv")))
  maingmsdirectory <- directory
  if (all(file.exists(file.path(directory, c("main.gms", file.path("..", "main.gms")))) == c(FALSE, TRUE))) {
    maingmsdirectory <- normalizePath(file.path(directory, ".."))
  }
  if (file.exists(file.path(maingmsdirectory, "main.gms"))) {
    cfg <- readDefaultConfig(maingmsdirectory)
    mainswitchnames <- sort(names(cfg$gms))
    duplo <- mainswitchnames[duplicated(mainswitchnames)]
    message("\n### Duplicated switches in main.gms: ", paste(duplo, collapse = ", "))
  } else {
    message("\n### No main.gms found in directory ", maingmsdirectory, ". Please specify REMIND directory.")
    return(NULL)
  }
  if (is.null(switchname)) {
    message("\n### Printing all switches with only one value in main.gms and ",
            length(csvfiles), " scenario config files:")
    switchesUniqueValue <- NULL
    for (switchname in mainswitchnames) {
      results <- suppressMessages(switchValuesScenConf(switchname))
      if (length(results) == 1) {
        switchesUniqueValue[switchname] <- results
        message(switchname, " <- ", results)
      }
    }
    return(switchesUniqueValue)
  } else {
    message("\n### Check values of ", switchname, " in scenario config files:")
    results <- NULL
    if (switchname %in% names(cfg$gms)) {
      message("\n### Default from main.gms: ", cfg$gms[switchname])
      results <- c(results, unlist(cfg$gms[switchname]))
    } else {
      message("\n### Switch not found in main.gms")
    }
    if (length(csvfiles) == 0) {
      message("\n### No scenario_config*.csv file found in directory ", directory, ".")
    } else {
      for (config.file in csvfiles) {
        settings <- read.csv2(config.file, stringsAsFactors = FALSE, row.names = 1, comment.char = "#", na.strings = "")
        if (switchname %in% names(settings)) {
          message("# ", config.file, "\n", paste(sort(unique(settings[, switchname])), collapse = ", "))
          results <- c(results, unique(settings[, switchname]))
        }
      }
      if (length(results) > 0) {
        message("\n### Values used in ", length(csvfiles), " scenario config files: ",
                paste(sort(unique(results)), collapse = ", "))
      } else {
        message("\n### Not found in ", length(csvfiles), " scenario config files.")
      }
    }
    return(sort(unique(results)))
  }
}
