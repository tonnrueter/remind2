context("REMIND reporting")

library(gdx)
library(data.table)
library(doParallel)

## Check REMIND output. dt is a data.table in *wide* format,
## i.e., variables are columns. `eqs` is a list of equations of the form
## list(LHS = "RHS", ...). The scope determines if the equations
## should be checked for regions ("regional"), only globally ("world") or
## both ("all"). Sensitivity determines the allowed offset when comparing
## LHS to RHS
check_eqs <- function(dt, eqs, scope="all", sens=1e-10){
  if(scope == "regional"){
    dt <- dt[all_regi != "World"]
  }else if(scope == "world"){
    dt <- dt[all_regi == "World"]
  }

  for(LHS in names(eqs)){
    exp <- parse(text=eqs[[LHS]])
    dt[, total := eval(exp), by=.(all_regi, ttot, scenario, model)]

    dt[, diff := total - get(LHS)]
    if(nrow(dt[abs(diff) > sens]) > 0){
      fail(
        sprintf("Check on data integrity failed for %s", LHS))
    }
    ## expect_equal(dt[["total"]], dt[[LHS]], tolerance = sens)
  }

}

test_that("Test if REMIND reporting is produced as it should and check data integrity", {

  ## uncomment to skip test
  ## success("Skip GDX test")

  testgdx_folder <- "../testgdxs/"

  ## add GDXs for comparison here:
  my_gdxs <- list.files("../testgdxs/", "*.gdx", full.names = TRUE)
  if(length(my_gdxs) == 0){
    dir.create(testgdx_folder, showWarnings = FALSE)
    download.file("https://rse.pik-potsdam.de/data/example/fulldata_REMIND21.gdx",
                  file.path(testgdx_folder, "fulldata.gdx"), mode="wb")
  }
  my_gdxs <- list.files("../testgdxs/", "*.gdx", full.names = TRUE)

  ## please add variable tests below
  check_integrity <- function(out){
    dt <- rmndt::magpie2dt(out)
    stopifnot(!(c("total", "diff") %in% unique(dt[["variable"]])))
    dt_wide <- data.table::dcast(dt, ... ~ variable)

    check_eqs(
      dt_wide,
      list(
        `FE|Transport|+|Liquids (EJ/yr)` = "`FE|Transport|Liquids|+|Biomass (EJ/yr)` + `FE|Transport|Liquids|+|Fossil (EJ/yr)`"
      ))

  }

  foreach (i = my_gdxs) %dopar% {
    cat(paste0(i,"\n"))
    a <- convGDX2MIF(i)
    check_integrity(a)
    cat("\n")
  }

})
