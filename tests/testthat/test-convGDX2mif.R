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

test_that("Test if REMIND reporting is produced as it should and check data integrity", {
  skip_if_not(as.logical(gdxrrw::igdx(silent = TRUE)), "gdxrrw is not initialized properly")

  # add GDXs for comparison here:
  gdxPaths <- NULL

  if (length(gdxPaths) == 0) {
    defaultGdxPath <- file.path(tempdir(), "fulldata.gdx")
    if (!file.exists(defaultGdxPath)) {
      utils::download.file("https://rse.pik-potsdam.de/data/example/remind2_test-convGDX2MIF_fulldata.gdx",
        defaultGdxPath,
        mode = "wb", quiet = TRUE
      )
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
    stamp <- lapply(gdx, stringr::str_sub, -32, -14) %>%
      strptime(format = "%Y-%m-%d_%H.%M.%S") %>%
      as.numeric()
    gdx <- data.frame(list(gdx = gdx, stamp = stamp))
    gdx <- gdx[Sys.time() - gdx$stamp < 30 * 24 * 60 * 60 & !is.na(gdx$stamp), ]
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
    message("Running convGDX2MIF(", gdxPath, ") and checking integrity ..")
    mifContent <- convGDX2MIF(gdxPath, gdx_refpolicycost = gdxPath)

    sumChecks <- piamInterfaces::checkSummations(
      mifFile = mifContent, outputDirectory = NULL, summationsFile = "extractVariableGroups") %>%
      mutate(!!sym("diff") := abs(!!sym("diff"))) %>%
      filter(!!sym("diff") > 0.00001)

    if (nrow(sumChecks) > 0) {
      warning("Some summation checks have failed! Run piamInterfaces::checkSummations")
    }

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

  suppressWarnings(
    capture.output( # Do not show stdout text.
      compareScenarios2(
        mifScen = myMifs,
        mifHist = histMif,
        outputFormat = "pdf",
        outputFile = "cs2_test",
        outputDir = tempdir(),
        sections = 0
      )
    ) # Render only the info section.
  )
  expect_true(file.exists(file.path(tempdir(), "cs2_test.pdf")))
  unlink(tempdir(), recursive = TRUE)
  tempdir(TRUE)
})
