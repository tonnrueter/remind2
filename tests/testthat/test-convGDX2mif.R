## uncomment to skip test
skip("Skip GDX test")

## Check REMIND output. dt is a data.table in *wide* format,
## i.e., variables are columns. `eqs` is a list of equations of the form
## list(LHS = "RHS", ...). The scope determines if the equations
## should be checked for regions ("regional"), only globally ("world") or
## both ("all"). Sensitivity determines the allowed offset when comparing
## LHS to RHS
check_eqs <- function(dt, eqs, scope = "all", sens = 1e-8) {
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
      fail(sprintf("Check on data integrity failed for %s", names(eqs)[LHS]))
    }
  }
}

test_that("Test if REMIND reporting is produced as it should and check data integrity", {
  skip_if_not(as.logical(gdxrrw::igdx(silent = TRUE)))
  dir.create("test-convGDX2mif-data", showWarnings = FALSE)

  ## add GDXs for comparison here:
  my_gdxs <- NULL
  if (length(my_gdxs) == 0) {
    if (!file.exists(file.path("test-convGDX2mif-data", "fulldata.gdx"))) {
      utils::download.file(
        "https://rse.pik-potsdam.de/data/example/fulldata_REMIND21.gdx",
        file.path("test-convGDX2mif-data", "fulldata.gdx"),
        mode = "wb"
      )
    }
  }
  my_gdxs <- list.files("test-convGDX2mif-data", "*.gdx", full.names = TRUE)

  ## please add variable tests below
  check_integrity <- function(out) {
    dt <- rmndt::magpie2dt(out)
    stopifnot(!(c("total", "diff") %in% unique(dt[["variable"]])))
    dt_wide <- data.table::dcast(dt, ... ~ variable)
    mylist <- mip::extractVariableGroups(unique(dt[["variable"]]), keepOrigNames = TRUE)
    mylist <- lapply(mylist, FUN = function(x) {
      return(paste0("`", x, "`"))
    })
    mylist <- lapply(mylist, paste, collapse = "+")
    # remove from the tests the variables whose totals cannot be found
    chck <- grep(" \\(.*.\\)$", names(mylist), invert = T)
    if (length(chck) > 0) {
      warning(paste0("For this group the corresponding total could not be found and the summation check ",
                     "will not be performed: \n", mylist[chck], "\n\n"))
    }
    mylist <- mylist[grep(" \\(.*.\\)$", names(mylist))]

    check_eqs(dt_wide, mylist)
  }

  ## test compareScenarios
  check_compscen <- function() {
    ## save mif with random filename

    my_mifs <- list.files("test-convGDX2mif-data", "*.mif", full.names = TRUE)
    histmif <- file.path("test-convGDX2mif-data", "historical.mif")
    my_mifs <- my_mifs[my_mifs != histmif]
    if (!file.exists(histmif)) {
      utils::download.file("https://rse.pik-potsdam.de/data/example/historical.mif", histmif)
    }
    compareScenarios(my_mifs, histmif, fileName = file.path("test-convGDX2mif-data", "scenarioComparison.pdf"))
  }

  n <- 0
  for (i in my_gdxs) {
    n <- n + 1
    cat(paste0(i, "\n"))
    a <- convGDX2MIF(i)
    print("Check integrity.")
    check_integrity(a)
    magclass::write.report2(
      x = magclass::collapseNames(a),
      file = file.path("test-convGDX2mif-data", sprintf("%s.mif", stringi::stri_rand_strings(1, 5))),
      scenario = paste0(getItems(a, dim = "scenario"), n),
      model = "REMIND"
    )
    cat("\n")
  }
  if (length(my_gdxs) == 1) {
    magclass::write.report2(
      x = magclass::collapseNames(a),
      file = file.path("test-convGDX2mif-data", sprintf("%s.mif", stringi::stri_rand_strings(1, 5))),
      scenario = paste0(magclass::getItems(a, dim = "scenario"), 2),
      model = "REMIND"
    )
  }

  print("Check compareScenarios.")
  check_compscen()
})
