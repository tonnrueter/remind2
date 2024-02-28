#' Read in GDX and write *.mif short reporting for REMIND-MAgPIE coupling
#'
#' Read in information from GDX file and create the *.mif reporting
#' using only reporting functions that calculate the variables that
#' are relevant for the REMIND-MAgPIE coupling
#'
#'
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @param file name of the mif file which will be written. If no name is
#' provided a magpie object containing all the reporting information is
#' returned
#' @param scenario scenario name that is used in the *.mif reporting
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)
#' @author David Klein
#' @examples
#'
#' \dontrun{convGDX2MIF_REMIND2MAgPIE(gdx,file="REMIND_generic_default.csv",scenario="default")}
#'
#' @export

convGDX2MIF_REMIND2MAgPIE <- function(gdx, file = NULL, scenario = "default",
                        t = c(seq(2005, 2060, 5), seq(2070, 2110, 10),
                              2130, 2150)) {
   # Define region subsets
  regionSubsetList <- toolRegionSubsets(gdx)
  # ADD EU-27 region aggregation if possible
  if("EUR" %in% names(regionSubsetList)){
    regionSubsetList <- c(regionSubsetList, list(
      "EU27" = c("ENC", "EWN", "ECS", "ESC", "ECE", "FRA","DEU", "ESW")
    ))
  }

  # reporting of variables that need variables from different other report functions
  message("running reportPrices...")
  output <- reportPrices(gdx = gdx, regionSubsetList = regionSubsetList, t = t)[, t, ] # needs output from reportSE, reportFE, reportEmi, reportExtraction, reportMacroEconomy

  # Add dimension names "scenario.model.variable"
  magclass::getSets(output)[3] <- "variable"
  output <- magclass::add_dimension(output, dim = 3.1, add = "model",    nm = "REMIND")
  output <- magclass::add_dimension(output, dim = 3.1, add = "scenario", nm = scenario)

  # either write the *.mif or return the magpie object
  if(!is.null(file)) {
    magclass::write.report(output, file = file, ndigit = 7)
    # write same reporting without "+" or "++" in variable names
    deletePlus(file, writemif = TRUE)
  } else {
    return(output)
  }
}
