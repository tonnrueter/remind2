# uncomment to skip test
# skip("Skip GDX test")

# Check REMIND output. dt is a data.table in *wide* format,
# i.e., variables are columns. `eqs` is a list of equations of the form
# list(LHS = "RHS", ...). The scope determines if the equations
# should be checked for regions ("regional"), only globally ("world") or
# both ("all"). Sensitivity determines the allowed offset when comparing
# LHS to RHS
library(dplyr)
library(gdx)
library(piamInterfaces)

checkEqs <- function(dt, eqs, gdxPath = NULL, scope = "all", sens = 1e-8) {
  if (scope == "regional") {
    dt <- dt[all_regi != "World"]
  } else if (scope == "world") {
    dt <- dt[all_regi == "World"]
  }

  for (LHS in seq_along(names(eqs))) {
    exp <- parse(text = eqs[[LHS]])
    dt[, total := eval(exp), by = .(all_regi, ttot, scenario, model)]

    dt[, diff := total - get(names(eqs)[LHS])]
    if (nrow(dt[abs(diff) > sens]) > 0) {
      fail(paste(c(gdxPath, paste("Check on data integrity failed for", names(eqs)[LHS]),
                   gsub("`", "", unlist(strsplit(eqs[[LHS]], "`+`", TRUE)))),
                 collapse = "\n"))
    }
  }
}

# please add variable tests below
checkIntegrity <- function(out, gdxPath = NULL) {
  dt <- rmndt::magpie2dt(out)
  stopifnot(!(c("total", "diff") %in% unique(dt[["variable"]])))
  dtWide <- data.table::dcast(dt, ... ~ variable)
  myList <- mip::extractVariableGroups(unique(dt[["variable"]]), keepOrigNames = TRUE)
  myList <- lapply(myList, FUN = function(x) paste0("`", x, "`"))
  myList <- lapply(myList, paste, collapse = "+")
  # remove from the tests the variables whose totals cannot be found
  chck <- grep(" \\(.*.\\)$", names(myList), invert = TRUE)
  if (length(chck) > 0) {
    warning(paste0("For this group the corresponding total could not be found and the summation check ",
                   "will not be performed: \n", myList[chck], "\n\n"))
  }
  myList <- myList[grep(" \\(.*.\\)$", names(myList))]

  checkEqs(dtWide, myList, gdxPath)
}


test_that("Test if REMIND reporting is produced as it should and check data integrity", {
  skip_if_not(as.logical(gdxrrw::igdx(silent = TRUE)), "gdxrrw is not initialized properly")

  # add GDXs for comparison here:
  gdxPaths <- NULL

  if (length(gdxPaths) == 0) {
    defaultGdxPath <- file.path(tempdir(), "fulldata.gdx")
    if (!file.exists(defaultGdxPath)) {
      utils::download.file("https://rse.pik-potsdam.de/data/example/remind2_test-convGDX2MIF_fulldata.gdx",
                           defaultGdxPath, mode = "wb", quiet = TRUE)
    }
    gdxPaths <- defaultGdxPath
  }

  # finds for each AMT scenario the most recent, successfully converged GDX
  .findAMTgdx <- function(gdxPaths = NULL) {
    didremindfinish <- function(fulldatapath) {
      logpath <- paste0(stringr::str_sub(fulldatapath, 1, -14), "/full.log")
      return(file.exists(logpath) &&
               any(grep("*** Status: Normal completion", readLines(logpath, warn = FALSE), fixed = TRUE)))
    }
    gdx <- Sys.glob("/p/projects/remind/modeltests/output/*/fulldata.gdx")
    stamp <- lapply(gdx, stringr::str_sub, -32, -14) %>% strptime(format = "%Y-%m-%d_%H.%M.%S") %>% as.numeric
    gdx <- data.frame(list(gdx = gdx, stamp = stamp))
    gdx <- gdx[Sys.time() - gdx$stamp < 30 * 24 * 60 * 60 & ! is.na(gdx$stamp), ]
    gdx <- gdx[unlist(lapply(gdx$gdx, didremindfinish)), ]
    gdx <- gdx[order(gdx$stamp), ]
    datetimepattern <- "_[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}"
    gdx$scenario <- sub(datetimepattern, "", basename(dirname(as.vector(gdx$gdx))))
    for (scenarioname in unique(gdx$scenario)) {
      gdxPaths <- c(gdxPaths, as.character(tail(gdx[gdx$scenario == scenarioname, ]$gdx, n = 1)))
    }
    return(gdxPaths)
  }

  # uncomment to add current calibration gdxes
  # gdxPaths <- c(gdxPaths, Sys.glob("/p/projects/remind/inputdata/CESparametersAndGDX/*.gdx"))
  # uncomment to add debugging example gdx files
  # gdxPaths <- c(gdxPaths, Sys.glob("/p/projects/remind/debugging/gdx-examples/*.gdx"))
  # uncomment to add gdx files from most recent AMT runs
  # gdxPaths <- c(gdxPaths, .findAMTgdx())

  numberOfMifs <- 0
  for (gdxPath in gdxPaths) {
    numberOfMifs <- numberOfMifs + 1
    message("Running convGDX2MIF(", gdxPath, ")...")
    mifContent <- convGDX2MIF(gdxPath, gdx_ref = gdxPath)
    message("Checking integrity of created MIF...")
    checkIntegrity(mifContent, gdxPath)
    magclass::write.report(
      x = magclass::collapseNames(mifContent),
      file = file.path(tempdir(), paste0(numberOfMifs, ".mif")),
      scenario = paste0(magclass::getItems(mifContent, dim = "scenario"), numberOfMifs),
      model = "REMIND"
    )
  }
  # create a second file, so we can actually check the comparison code
  if (numberOfMifs == 1) {
    numberOfMifs <- numberOfMifs + 1
    magclass::write.report(
      x = magclass::collapseNames(mifContent),
      file = file.path(tempdir(), paste0(numberOfMifs, ".mif")),
      scenario = paste0(magclass::getItems(mifContent, dim = "scenario"), numberOfMifs),
      model = "REMIND"
    )
  }

  message("Checking compareScenarios...")
  myMifs <- file.path(tempdir(), paste0(seq_len(numberOfMifs), ".mif"))
  histMif <- file.path(tempdir(), "historical.mif")
  if (!file.exists(histMif)) {
    utils::download.file("https://rse.pik-potsdam.de/data/example/historical.mif", histMif, quiet = TRUE)
  }
  capture.output( # Do not show stdout text.
    compareScenarios2(
      mifScen = myMifs,
      mifHist = histMif,
      outputFormat = "pdf",
      outputFile = "cs2_test",
      outputDir = tempdir(),
      sections = 0)) # Render only the info section.
  expect_true(file.exists(file.path(tempdir(), "cs2_test.pdf")))
  unlink(tempdir(), recursive = TRUE)
  tempdir(TRUE)
})


magiccVars <- c(
  "Concentration|CH4 (ppb)",
  "Concentration|CO2 (ppm)",
  "Concentration|N2O (ppb)",
  "Forcing (W/m2)",
  "Forcing|Aerosol (W/m2)",
  "Forcing|Aerosol|BC (W/m2)",
  "Forcing|Aerosol|Cloud Indirect (W/m2)",
  "Forcing|Aerosol|OC (W/m2)",
  "Forcing|Aerosol|Other (W/m2)",
  "Forcing|Aerosol|Sulfate Direct (W/m2)",
  "Forcing|Albedo Change and Mineral Dust (W/m2)",
  "Forcing|CH4 (W/m2)",
  "Forcing|CO2 (W/m2)",
  "Forcing|F-Gases (W/m2)",
  "Forcing|Kyoto Gases (W/m2)",
  "Forcing|Montreal Gases (W/m2)",
  "Forcing|N2O (W/m2)",
  "Forcing|Other (W/m2)",
  "Forcing|Tropospheric Ozone (W/m2)",
  "Temperature|Global Mean (K)"
)

test_that("Test if REMIND reporting produces mandatory variables for NGFS reporting", {

  # for now, we enforce this test locally to ensure that remind2 reportings do
  # not accidentally mess up the reporting
  skip_on_ci()

  gdxPath <- file.path(tempdir(), "fulldata.gdx")
  utils::download.file("https://rse.pik-potsdam.de/data/example/remind2_test-NGFS_fulldata.gdx",
                       gdxPath, mode = "wb", quiet = TRUE)

  mif <- convGDX2MIF(gdxPath, gdx_ref = gdxPath)

  computedVariables <- getItems(mif, dim = 3.3)

  computedVariables <- gsub("\\(\\)", "(unitless)", computedVariables)

  templateVariables <- unique(
    piamInterfaces::getREMINDTemplateVariables("AR6"),
    piamInterfaces::getREMINDTemplateVariables("AR6_NGFS")
  )

  missingVariables <- setdiff(templateVariables, computedVariables)

  # MAGICC variables in template are not created here and therefore not considered
  missingVariables <- setdiff(missingVariables, magiccVars)

  if (length(missingVariables) > 0) {
    warning(
      "The following variables are expected in the piamInterfaces package,
          but cannot be found in the reporting generated: ",
      paste(missingVariables, collapse = ",\n ")
    )
  }
  expect_true(length(missingVariables) == 0)
  unlink(tempdir(), recursive = TRUE)
  tempdir(TRUE)
})


