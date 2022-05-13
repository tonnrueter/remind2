# uncomment to skip test
# skip("Skip GDX test")

# Check REMIND output. dt is a data.table in *wide* format,
# i.e., variables are columns. `eqs` is a list of equations of the form
# list(LHS = "RHS", ...). The scope determines if the equations
# should be checked for regions ("regional"), only globally ("world") or
# both ("all"). Sensitivity determines the allowed offset when comparing
# LHS to RHS
checkEqs <- function(dt, eqs, scope = "all", sens = 1e-8) {
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
      fail(paste(c(paste("Check on data integrity failed for", names(eqs)[LHS]),
                 gsub('`', '', unlist(strsplit(eqs[[LHS]], '`+`', TRUE)))),
                 collapse = '\n' ))
    }
  }
}

# please add variable tests below
checkIntegrity <- function(out) {
  dt <- rmndt::magpie2dt(out)
  stopifnot(!(c("total", "diff") %in% unique(dt[["variable"]])))
  dtWide <- data.table::dcast(dt, ... ~ variable)
  myList <- mip::extractVariableGroups(unique(dt[["variable"]]), keepOrigNames = TRUE)
  myList <- lapply(myList, FUN = function(x) paste0("`", x, "`"))
  myList <- lapply(myList, paste, collapse = "+")
  # remove from the tests the variables whose totals cannot be found
  chck <- grep(" \\(.*.\\)$", names(myList), invert = T)
  if (length(chck) > 0) {
    warning(paste0("For this group the corresponding total could not be found and the summation check ",
                   "will not be performed: \n", myList[chck], "\n\n"))
  }
  myList <- myList[grep(" \\(.*.\\)$", names(myList))]

  checkEqs(dtWide, myList)
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

  numberOfMifs <- 0
  for (gdxPath in gdxPaths) {
    numberOfMifs <- numberOfMifs + 1
    message("Running convGDX2MIF(", gdxPath, ")...")
    mifContent <- convGDX2MIF(gdxPath)
    message("Checking integrity of created MIF...")
    checkIntegrity(mifContent)
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
