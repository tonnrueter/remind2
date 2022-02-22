#' Computes the employment values (jobs) across different sectors
#' @param pathToMIF A mif file putput from a REMIND run
#' @param improvements Either "None", "CEEW", "Dias", "Rutovitz_aus","Solar_found" or "All". Use "All" for all improvements.
#' @param subtype Subtype for how shares of solar rooftop, wind offshore, and small hydro are assumed in the future. Options "current", "irena", and "expert". See calcDspvShare for more information.
#' @param share_manf Either "current" or "local". Current implies current shares of world manufacture remain same until 2050, current means that in 2050 all countries manufacture required components locally.
#' @param multiplier controls how the regional multiplier for non-oecd countries changes with time.
#' @param decline How should the employment factors change over time? "capcosts" means according to capital costs. "static" means it doesn't change
#' @description This function returns a magpie object containing the reporting the jobs for different technologies
#' @return A magpie object
#' @author Aman Malik
#' @examples
#' \dontrun{

#' runEmployment(pathToMIF, improvements = "All", multiplier = "own", subtype = "expert", share_manf = "local", decline = "capcosts")
#' }
#' @importFrom madrat calcOutput
#' @importFrom magclass getNames add_columns getItems
#' @export

reportEmployment <- function(pathToMIF, improvements, multiplier, subtype, share_manf, decline) {
  
  input_mif <- remind2::readReportingMIF(pathToMIF = "") # read as data frame
  input_mif_mp <- as.magpie(input_mif) # convert to magpie object
  input_mif_mp <- collapseDim(input_mif_mp) # remove dimensions with same values, i.e., "model" and "scenario"
  input_mif_mp <- collapseDim(input_mif_mp,dim = 3.2) # remove "unit" dimension
  
  # use setConfig(forcecache=TRUE) to make the code run faster
  # employment factors
  x <- madrat::calcOutput("Employmentfactors", improvements = improvements, multiplier = multiplier)
  getNames(x) <- gsub("Wind onshore","Wind|Onshore",getNames(x))
  getNames(x) <- gsub("Wind offshore","Wind|Offshore",getNames(x))
  x <- x[, , "HP", pmatch = TRUE, invert = TRUE] # excluding (at the moment) jobs in combined heat and power plants
  
  ## DECLINE FACTORS------------------------
  if (decline == "capcosts") {
    

    # capital costs and OM fixed costs evolution over time for different techs, used to calculated the decline factor
   # report_tech <- reportTechnology(gdx)
    #report_tech <- collapseNames(report_tech)
    
    # variables needed - capital and fixed costs of technologies
    var <- c("Tech|Electricity|Coal|Pulverised Coal w/o CC|Capital Costs",
             "Tech|Electricity|Gas|Combined Cycle w/o CC|Capital Costs",
             "Tech|Electricity|Hydro|Capital Costs",
             "Tech|Electricity|Nuclear|Capital Costs",
             "Tech|Electricity|Solar|PV|Capital Costs",
             "Tech|Electricity|Solar|CSP|Capital Costs",
             "Tech|Electricity|Wind|Onshore|Capital Costs",
             "Tech|Electricity|Wind|Offshore|Capital Costs",
             "Tech|Electricity|Biomass|Combined Heat and Power w/o CC|Capital Costs",
             "Tech|Electricity|Storage|Battery|For PV|Capital Costs",
             "Tech|Electricity|Oil|DOT|Capital Costs",
             "Tech|Electricity|Geothermal|Capital Costs",
             
             "Tech|Electricity|Coal|Pulverised Coal w/o CC|OM Cost|fixed",
             "Tech|Electricity|Gas|Combined Cycle w/o CC|OM Cost|fixed",
             "Tech|Electricity|Hydro|OM Cost|fixed",
             "Tech|Electricity|Nuclear|OM Cost|fixed",
             "Tech|Electricity|Solar|PV|OM Cost|fixed",
             "Tech|Electricity|Solar|CSP|OM Cost|fixed",
             "Tech|Electricity|Wind|Onshore|OM Cost|fixed",
             "Tech|Electricity|Wind|Offshore|OM Cost|fixed",
             "Tech|Electricity|Biomass|Combined Heat and Power w/o CC|OM Cost|fixed",
             "Tech|Electricity|Oil|DOT|OM Cost|fixed",
             "Tech|Electricity|Geothermal|OM Cost|fixed",
             "Tech|Electricity|Storage|Battery|For PV|OM Cost|fixed", 
             NULL
    )
    # only capital costs
    report_tech <- input_mif_mp[,,var]
    cap_costs <- report_tech[getRegions(x), seq(2015, 2050, 5), "Capital Costs",pmatch=TRUE]# selecting years until 2050 and removing "World" region

    cap_costs <- cap_costs[, , ] / setYears(cap_costs[, "y2020", ], NULL) # costs relative to 2020
    
    # Evolution of employment factor (CI & Manf) with time depends on capital costs
    # the job intensity decreases with capital costs for techs, relative to 2015
    var_in_x <- getItems(x, dim = 3.1) # variables from employment factor magpie object
    var_in_rem <- c("Coal", "Gas", "Nuclear", "Biomass", "Hydro", "Hydro", "Wind|Onshore", "Wind|Offshore", "Solar|PV",
                    "Geothermal", "Solar|CSP", "Oil", "Solar|PV", "Storage") # variables from remind (order matters)
    var_comb <- paste(var_in_x, var_in_rem, sep = ".") # concatenating the two for looping to work
    
    for (i in var_comb) {
      x[getRegions(x), , sub("\\..*", "", i)][, , c("CI", "Manf")] <- x[getRegions(x), , sub("\\..*", "", i)][, , c("CI", "Manf")] * setNames(cap_costs[getRegions(x), getYears(x), sub(".*\\.", "", i), pmatch = TRUE], NULL)
    }
    # OM fixed costs used to calculate  decline factors for O&M employment factors
    fixed_costs <- report_tech[getRegions(x), seq(2015, 2050, 5), "fixed",pmatch=TRUE]# selecting years until 2050 and removing "World" region

    fixed_costs <- fixed_costs[, , ] / setYears(fixed_costs[, "y2020", ], NULL) # costs relative to 2020
    for (i in var_comb) {
      x[getRegions(x), , sub("\\..*", "", i)][, , "OM"] <- x[getRegions(x), , sub("\\..*", "", i)][, , "OM"] * setNames(fixed_costs[getRegions(x), getYears(x), sub(".*\\.", "", i), pmatch = TRUE], NULL)
    }
    
    # decline factors for coal fuel supply until 2050 are projected depending on
    # historical patterns and convergence between some countries.
    # See calcLabourProductivity for more information.
    coal_ef <- calcOutput("CoalLabourProductivity", subtype = "Employment_factor")
    coal_ef <- coal_ef / setYears(coal_ef[, 2020, ], NULL)
    coal_ef[which(coal_ef == "NaN")] <- 0
    x[, getYears(x), "Coal.Fuel_supply"] <- x[, getYears(x), "Coal.Fuel_supply"] * setNames(coal_ef[, getYears(x), ], NULL)
  }
  # if Employment factors don't change
  if (decline == "static") {
    # do nothing, as employment factors have already been read
  }
  
  ## Other external parameters--------------------------

  # share of rooftop in total spv, wind offshore in total wind, and hydro small in total hydro
  share <- calcOutput("DspvShare")
  
  ## ----------------------------------------------
  
  ## New and Absolute capacity from REMIND---------------------------------
  # filtering required capacity variables i.e., only new and existing coal capacities
#  remind_output <- reportCapacity(gdx)

  # selecting variables only related to power or electricity
  rem_filter <- input_mif_mp[,,c("Cap|Electricity"), pmatch = TRUE]
  rem_filter <- rem_filter[, , c("Cumulative", "Idle", "Total Cap", "CC", "Hydrogen", 
                                    "Estimated", "Non-Biomass", "For Wind", "For PV", "GT"), pmatch = TRUE, invert = TRUE] # removing cumulative and idle variables in capacity

  rem_filter <- rem_filter[, , c("Cap|Electricity", # total sum of all techs and not needed
                                 "New Cap|Electricity",
                                 "Cap|Electricity|Wind",
                                 "New Cap|Electricity|Wind",
                                 "Cap|Electricity|Solar", # is sum of pv and csp and not needed
                                 "New Cap|Electricity|Solar"), invert = TRUE]
 

  
  # filtering required regions
  rem_filter <- rem_filter[getRegions(x), seq(2015, 2050, 5), ]
  
  ## disaggregating certain variables/adding new variables
  cols_to_add <- c("Solar|PV-utility", "Solar|PV-rooftop", "Hydro-large", "Hydro-small")
  for (j in cols_to_add) {
    rem_filter <- add_columns(rem_filter, addnm = paste0("New Cap|Electricity|", j), dim = 3.1)
    rem_filter <- add_columns(rem_filter, addnm = paste0("Cap|Electricity|", j), dim = 3.1)
  }
  
  # adding values to the new variables
  rem_filter[, , "New Cap|Electricity|Solar|PV-rooftop"] <-
    rem_filter[, , "New Cap|Electricity|Solar|PV"] * share[, , "spv"]
  rem_filter[, , "New Cap|Electricity|Solar|PV-utility"] <-
    rem_filter[, , "New Cap|Electricity|Solar|PV"] * (1 - share[, , "spv"])
  
  rem_filter[, , "Cap|Electricity|Solar|PV-rooftop"] <-
    rem_filter[, , "Cap|Electricity|Solar|PV"] * share[, , "spv"]
  rem_filter[, , "Cap|Electricity|Solar|PV-utility"] <-
    rem_filter[, , "Cap|Electricity|Solar|PV"] * (1 - share[, , "spv"])
  
  rem_filter[, , "New Cap|Electricity|Hydro-small"] <- 
    rem_filter[, , "New Cap|Electricity|Hydro"] * share[, , "hydro"]
  rem_filter[, , "New Cap|Electricity|Hydro-large"] <-
    rem_filter[, , "New Cap|Electricity|Hydro"] * (1 - share[, , "hydro"])
  
  rem_filter[, , "Cap|Electricity|Hydro-small"] <-
    rem_filter[, , "Cap|Electricity|Hydro"] * share[, , "hydro"]
  rem_filter[, , "Cap|Electricity|Hydro-large"] <-   
    rem_filter[, , "Cap|Electricity|Hydro"] * (1 - share[, , "hydro"])
  
  # removing "Hydro" and "Solar" broad categories
  rem_filter <- rem_filter[,,c("New Cap|Electricity|Hydro","Cap|Electricity|Hydro",
                               "New Cap|Electricity|Solar|PV","Cap|Electricity|Solar|PV"),invert=TRUE]

  # added/installed capacity from REMIND, for years > 2010
  added_cap <- rem_filter[getRegions(x), getYears(rem_filter) > 2010, "New Cap", pmatch = TRUE] # only new capacities
  
  added_cap_sum <- dimSums(added_cap, dim = 1) # Total World added capacity for each tech
  
  # existing/operating capacity from REMIND
  cap_tot <- rem_filter[, , "New Cap", invert = TRUE, pmatch = TRUE]
  
  ## -----------------------------------------------------
  
  #  share of exports to world installed capacity
  prod_share <- calcOutput("ProdShares")
  prod_share <- prod_share[, "y2019", ] # use 2019 as 2018 values have NAs for important countries
  cols_to_add <- c("Solar|PV-utility", "Solar|PV-rooftop", "Wind|Offshore", "Wind|Onshore")
  prod_share  <- add_columns(prod_share, addnm = cols_to_add, dim = 3.1)
  prod_share[, , c("Solar|PV-utility", "Solar|PV-rooftop")] <- prod_share[, , "spv"]
  prod_share[, , c("Wind|Offshore", "Wind|Onshore")] <- prod_share[, , "wind"]
  prod_share <- prod_share[,,c("spv","wind"),invert=TRUE]
  
  # share of world capacity addition by region, only for solar and PV
  shr_GLO_add <- added_cap[, , c("Solar|PV","Wind"), pmatch = TRUE] / added_cap_sum[, , c("Solar|PV","Wind"), pmatch = TRUE]
  prod_share_tmp <- new.magpie(getRegions(x), c(2015, 2020, 2050), names = c("Solar|PV-utility", "Solar|PV-rooftop", "Wind|Offshore", "Wind|Onshore"))
  
  if (share_manf == "local" || share_manf == "current") {
    ## prod shares calculate local manufacture of wind and solar components in 2050. Change is linear.
    # assigning 2050 regional share values as prod shares
    prod_share_tmp[, "y2050", ] <- as.numeric(shr_GLO_add[, "y2050", getNames(prod_share_tmp), pmatch = TRUE])
    # 2015 and 2020 values same as 2019 values
    prod_share_tmp[, c(2015, 2020), ] <- prod_share[, , getNames(prod_share_tmp)]
    # from 2020 to 2050, linearly interpolate
    prod_share <- time_interpolate(prod_share_tmp, seq(2025, 2045, 5), integrate_interpolated_years = TRUE)
    if (share_manf == "current") {
      prod_share[, , ] <- prod_share[, 2020, ] # shares remain same throughout = 2020
    }
  }
  # Obtaining fuel supply variables
#  remind_prod <- reportExtraction(gdx)["GLO", , , invert = TRUE] # remove GLO
  vars <- c("PE|Production|Biomass",
            "PE|Production|Gross|Coal",
            "PE|Production|Gross|Oil",
            "PE|Production|Gross|Gas",
            "PE|Production|Gross|Uranium [Energy]"
  )
  remind_prod <- input_mif_mp[getRegions(x), getYears(x), vars] # period >2010 and only select variables
  
  pe_trad <- input_mif_mp[getRegions(x), getYears(x), "PE|Biomass|Traditional"]
  
  # removing the traditional biomass component, where biomass isn't sold, thus no employment
  # in conventional sense
  remind_prod[, , "PE|Production|Biomass"] <-  remind_prod[, , "PE|Production|Biomass"] - setNames(pe_trad[, , ], NULL)
  
  # getNames(remind_prod) <- gsub(pattern = "\\[Energy\\]",replacement = "",x = getNames(remind_prod))
 # getNames(remind_prod) <- gsub(pattern = "  \\(EJ\\/yr\\)", replacement = " \\(EJ\\/yr\\)", x = getNames(remind_prod)) # removing space
  getNames(remind_prod) <- gsub(pattern = "Uranium \\[Energy\\]", replacement = "Nuclear", x = getNames(remind_prod)) # removing space
  
  ## Calculating jobs--------------------------------
  techs <- c("Solar|PV-utility", "Solar|PV-rooftop", "Hydro-large", "Hydro-small",
             "Wind|Offshore", "Wind|Onshore", "Coal", "Biomass", "Gas", "Storage", "Oil", "Nuclear", "Geothermal", "Solar|CSP")
  techs_exp <-  c("Solar|PV-utility", "Solar|PV-rooftop", "Wind|Offshore", "Wind|Onshore") # technologies with export component
  
  # cons <- new.magpie(getRegions(x),names = techs)
  # average construction time for different technologies
  # cons[] <- rep(c(1,1,4,3,4,2,5,2,2,1,2,10,2,2),each=12)
  
  # 1. Manufacturing jobs for techs with export component.
  
  # 1a) Total manf jobs (per region) for these techs are equal to total world addition X share of
  # world exports (for that region)
  
  jobs_manf <- new.magpie(getRegions(x), getYears(x), techs, fill = 0)
  for (i in techs_exp) {
    # manf <- paste0(i,".","Manf")
    jobs_manf[, , i] <- added_cap_sum[, getYears(x), i, pmatch = TRUE] * 1000
    jobs_manf[, getYears(x), i] <-  (jobs_manf[, getYears(x), i] * setNames(x[, , i][, , "Manf"], NULL) * prod_share[, , i])
  }
  
  for (i in setdiff(techs, techs_exp)) {
    # manf <- paste0(i,".","Manf")
    jobs_manf[, , i] <- added_cap[, getYears(x), i, pmatch = TRUE] * 1000
    jobs_manf[, getYears(x), i] <- (jobs_manf[, getYears(x), i] *  setNames(x[, getYears(x), i, pmatch = TRUE][, , "Manf"], NULL))
  }
  
  jobs_manf <- add_dimension(jobs_manf, dim = 3.2, add = "type", nm = "Manf")
  getSets(jobs_manf) <- c("region", "year", "variable", "type")
  
  # 2. Construction and Installation jobs
  jobs_ci <- new.magpie(getRegions(x), getYears(x), techs, fill = 0)
  
  
  for (i in techs) {
    # inst <- paste0(i,".","CI")
    jobs_ci[, , i] <- added_cap[, getYears(x), i, pmatch = TRUE] * 1000
    # jobs_ci[,,i] <-  (jobs_ci[,getYears(x),i] *  setNames(x[,,i,pmatch=TRUE][,,"CI"],NULL))
    jobs_ci[, , i] <-  jobs_ci[, getYears(x), i] *  setNames(x[, , i, pmatch = TRUE][, , "CI"], nm = NULL)
    
  }
  
  jobs_ci <- add_dimension(jobs_ci, dim = 3.2, add = "type", nm = "CI")
  getSets(jobs_ci) <- c("region", "year", "variable", "type")
  
  
  ## 3. Jobs in O & M
  # jobs = existing capacity per tech * emp factor per tech
  jobs_om <- new.magpie(getRegions(x), getYears(x), techs)
  for (i in techs) {
    # inst <- paste0(i,".","OM")
    jobs_om[, , i] <- cap_tot[, getYears(x), i, pmatch = TRUE] * 1000
    jobs_om[, , i] <-  jobs_om[, , i] *  setNames(x[, , i, pmatch = TRUE][, , "OM"], NULL)
  }
  jobs_om <- add_dimension(jobs_om, dim = 3.2, add = "type", nm = "OM")
  getSets(jobs_om) <- c("region", "year", "variable", "type")
  
  ## 4. Jobs in Fuel supply
  # jobs = total production (in EJ/yr) per fuel * employment factor per fuel (Jobs/PJ)
  fuels <- c("Coal", "Gas", "Biomass", "Oil")
  jobs_prod <- new.magpie(getRegions(x), getYears(x), techs, fill = 0)
  for (i in fuels) {
    # fs <- paste0(i,".","Fuel_supply")
    jobs_prod[, , i] <- remind_prod[, getYears(x), i, pmatch = TRUE] * 1000
    jobs_prod[, , i] <-  jobs_prod[, , i] * setNames(x[, , i][, , "Fuel_supply"], NULL)
  }
  jobs_prod <- add_dimension(jobs_prod, dim = 3.2, add = "type", nm = "Fuel_supply")
  getSets(jobs_prod) <- c("region", "year", "variable", "type")
  
  ## Jobs in Fuel supply, nuclear which are given in terms of SE
#  rem_se_nuc <- reportSE(gdx)[regions, getYears(x), "Nuclear", pmatch = TRUE] * 277777.778 # EJ to GWh
  rem_se_nuc <- input_mif_mp[getRegions(x),,"SE|Electricity|+|Nuclear"]* 277777.778 # EJ to GWh
  
  jobs_prod[, , "Nuclear.Fuel_supply"] <- setNames(rem_se_nuc[,getYears(x) , ], NULL) * x[,getYears(x) , "Nuclear.Fuel_supply"]
  
  ####### put all together #############
  
  jobs <- mbind(jobs_ci, jobs_om, jobs_prod, jobs_manf)
  
  jobs <- as.data.frame(jobs)
  return (jobs)
  
}
