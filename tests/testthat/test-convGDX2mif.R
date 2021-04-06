context("REMIND reporting")

library(gdx)
library(data.table)
library(stringi)

## uncomment to skip test
## skip("Skip GDX test")

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

  testgdx_folder <- "../testgdxs/"
  testmif_folder <- "../testmifs/"

  dir.create(testmif_folder, showWarnings = FALSE)

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
    mylist <- mip::extractVariableGroups(unique(dt[["variable"]]),keepOrigNames = TRUE)
    mylist <- lapply(mylist, FUN=function(x){return(paste0("`",x,"`"))})
    mylist <- lapply(mylist, paste, collapse = "+")
    # remove from the tests the variables whose totals cannot be found
    chck <- grep(" \\(.*.\\)$",names(mylist),invert = T)
    if (length(chck)>0) warning(paste0("For this group the corresponding total could not be found and the summation check will not be performed: \n",mylist[chck],"\n\n"))
    mylist <- mylist[grep(" \\(.*.\\)$",names(mylist))]

    check_eqs(dt_wide,mylist)

  }

  ## test compareScenarios
  check_compscen <- function(){
    ## save mif with random filename

    my_mifs <- list.files(testmif_folder, "*.mif", full.names = TRUE)
    histmif <- file.path(testmif_folder, "historical.mif")
    my_mifs <- my_mifs[my_mifs != histmif]
    if(!file.exists(histmif))
      download.file("https://rse.pik-potsdam.de/data/example/historical.mif", histmif)
    compareScenarios(my_mifs, histmif, fileName=file.path(testmif_folder, "scenarioComparison.pdf"))

  }

  for(i in my_gdxs) {
    cat(paste0(i,"\n"))
    a <- convGDX2MIF(i)
    print("Check integrity.")
    check_integrity(a)
    write.report(
      a, file.path(testmif_folder, sprintf("%s.mif", stri_rand_strings(1, 5))))
    cat("\n")
  }

  print("Check compareScenarios.")
  check_compscen()

})
