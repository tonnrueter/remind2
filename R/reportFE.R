#' Read in GDX and calculate final energy, used in convGDX2MIF.R for the
#' reporting
#'
#' Read in final energy information from GDX file, information used in
#' convGDX2MIF.R for the reporting
#'
#'
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)
#'
#' @author Renato Rodrigues, Christoph Bertram, Antoine Levesque
#' @examples
#'
#'   \dontrun{reportFE(gdx)}
#'
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass new.magpie mselect getRegions getYears mbind setNames
#'                      dimSums getNames<- as.data.frame as.magpie
#' @importFrom dplyr filter %>% mutate select inner_join group_by summarise left_join full_join
#'                   ungroup rename
#' @importFrom quitte inline.data.frame revalue.levels
#' @importFrom utils tail
#'
#'
#'

reportFE <- function(gdx, regionSubsetList = NULL,
                     t = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130,
                           2150)) {
  fedie_bioshare <- fepet_bioshare <- prodFE <- prodSE <- se_Gas <- se_Liq <-
    all_enty <- all_enty1 <- all_te <- all_in <- NULL

  ####### conversion factors ##########
  TWa_2_EJ     <- 31.536

  out <- NULL

  ####### Core Variables ##########

  # ---- read in needed data

  # ---- sets
  se2fe <- readGDX(gdx,"se2fe")
  entyFe2Sector <- readGDX(gdx, "entyFe2Sector")
  sector2emiMkt <- readGDX(gdx, "sector2emiMkt")

  demFemapping <- entyFe2Sector %>%
    full_join(sector2emiMkt, by = 'emi_sectors') %>%
    # rename such that all_enty1 always signifies the FE carrier like in
    # vm_demFeSector
    rename(all_enty1 = all_enty) %>%
    left_join(se2fe, by = 'all_enty1') %>%
    select(-all_te)

  #sety <- readGDX(gdx,c("entySe","sety"),format="first_found")

  # ---- parameter
  p_eta_conv <- readGDX(gdx, c("pm_eta_conv"), restore_zeros = FALSE,format="first_found")[,t,]

  # ---- variables
  vm_prodSe <- readGDX(gdx,name=c("vm_prodSe","v_seprod"),field="l",restore_zeros=FALSE,format="first_found")[,t,]*TWa_2_EJ
  #vm_prodSe <- mselect(vm_prodSe,all_enty1=sety)
  vm_prodFe  <- readGDX(gdx,name=c("vm_prodFe"),field="l",restore_zeros=FALSE,format="first_found")[,t,]*TWa_2_EJ
  #vm_prodFe  <- vm_prodFe[se2fe]
  vm_demFeSector <- readGDX(gdx,name=c("vm_demFeSector"),field="l",format="first_found",restore_zeros=FALSE)[,t,]*TWa_2_EJ
  vm_demFeSector[is.na(vm_demFeSector)] <- 0

  # only retain combinations of SE, FE, sector, and emiMkt which actually exist in the model (see qm_balFe)
  vm_demFeSector <- vm_demFeSector[demFemapping]

  #adding transport gas empty object to keep support to transport complex module
  if(all(grep("fegat", getItems(vm_demFeSector,3)) == 0)){
    extended_vm_demFeSector <- new.magpie(getItems(vm_demFeSector,1),getItems(vm_demFeSector,2),c(getItems(vm_demFeSector,3),"segabio.fegat.trans.ETS","segafos.fegat.trans.ETS","segasyn.fegat.trans.ETS","segabio.fegat.trans.ES","segafos.fegat.trans.ES","segasyn.fegat.trans.ES","segabio.fegat.trans.other","segafos.fegat.trans.other","segasyn.fegat.trans.other"),fill=0, sets=getSets(vm_demFeSector))
    extended_vm_demFeSector[,,c(getItems(vm_demFeSector,3))] <- vm_demFeSector[,,c(getItems(vm_demFeSector,3))]
    vm_demFeSector <- extended_vm_demFeSector
  }

  # only retain combinations of SE, FE, te which actually exist in the model (qm_balFe)
  vm_prodFe <- vm_prodFe[se2fe]

  # FE demand per industry subsector
  o37_demFeIndSub <- readGDX(gdx, "o37_demFeIndSub", restore_zeros = FALSE,
                             format = "first_found", react = 'silent')
  if (!(is.null(o37_demFeIndSub) | 0 == length(o37_demFeIndSub))) {
    o37_demFeIndSub <- o37_demFeIndSub[,t,]
    o37_demFeIndSub[is.na(o37_demFeIndSub)] <- 0
    # convert to EJ
    o37_demFeIndSub <- o37_demFeIndSub * TWa_2_EJ
  }




  # temporary backwards compatability: this can be removed, once a new test gdx after March 2021 is used
  if ("seliqsyn" %in% getNames(vm_prodFe, dim=1)) {
    seliq <- c("seliqfos","seliqbio","seliqsyn")
    segas <- c("segafos","segabio","segasyn")
  } else {
    seliq <- c("seliqfos","seliqbio")
    segas <- c("segafos","segabio")
  }
  ##


  ####### Realisation specific Variables ##########

  # Define current realisation for the different modules
  module2realisation <- readGDX(gdx, "module2realisation")
  rownames(module2realisation) <- module2realisation$modules

  find_real_module <- function(module_set, module_name){
    return(module_set[module_set$modules == module_name,2])
  }

  tran_mod = find_real_module(module2realisation,"transport")
  indu_mod = find_real_module(module2realisation,"industry")
  buil_mod = find_real_module(module2realisation,"buildings")
  cdr_mod  = find_real_module(module2realisation,"CDR")


  # ---- FE total production ------
  out <- mbind(out,

    #total
    setNames((dimSums(vm_prodFe,dim=3,na.rm=T)), "FE (EJ/yr)"),

    #Liquids
    setNames(dimSums(vm_prodFe[,,seliq],dim=3,na.rm=T),                                                "FE|+|Liquids (EJ/yr)"),
    setNames(dimSums(vm_prodFe[,,"seliqbio"],dim=3,na.rm=T),                                                              "FE|Liquids|+|Biomass (EJ/yr)"),
    setNames(dimSums(vm_prodFe[,,"seliqfos"],dim=3,na.rm=T),                                                              "FE|Liquids|+|Fossil (EJ/yr)"),
    setNames(dimSums(mselect(vm_prodFe, all_enty="seliqsyn") ,dim=3,na.rm=T),                                             "FE|Liquids|+|Hydrogen (EJ/yr)"),

    #Solids
    setNames(dimSums(vm_prodFe[,,c("sesobio","sesofos")],dim=3,na.rm=T),                                                  "FE|+|Solids (EJ/yr)"),
    setNames(dimSums(vm_prodFe[,,"sesobio"],dim=3,na.rm=T),                                                               "FE|Solids|+|Biomass (EJ/yr)"),
    setNames(p_eta_conv[,,"tdbiosos"] * dimSums(mselect(vm_prodSe,all_enty1="sesobio",all_te="biotrmod"),dim=3,na.rm=T),  "FE|Solids|Biomass|+|Modern (EJ/yr)"),
    setNames(p_eta_conv[,,"tdbiosos"] * dimSums(mselect(vm_prodSe,all_enty1="sesobio",all_te="biotr"),dim=3,na.rm=T),     "FE|Solids|Biomass|+|Traditional (EJ/yr)"),
    setNames(dimSums(vm_prodFe[,,"sesofos"],dim=3,na.rm=T),                                                               "FE|Solids|+|Fossil (EJ/yr)"),
    setNames(p_eta_conv[,,"tdfossos"] * dimSums(mselect(vm_prodSe,all_enty1="sesofos",all_enty="pecoal"),dim=3,na.rm=T),  "FE|Solids|Fossil|+|Coal (EJ/yr)"),

    #Gases
    setNames(dimSums(vm_prodFe[,,segas],dim=3,na.rm=T),                                                  "FE|+|Gases (EJ/yr)"),
    setNames(dimSums(vm_prodFe[,,"segabio"],dim=3,na.rm=T),                                                               "FE|Gases|+|Biomass (EJ/yr)"),
    setNames(dimSums(vm_prodFe[,,"segafos"],dim=3,na.rm=T),                                                               "FE|Gases|+|Fossil (EJ/yr)"),
     setNames(dimSums(mselect(vm_prodFe, all_enty="segasyn") ,dim=3,na.rm=T),                                             "FE|Gases|+|Hydrogen (EJ/yr)"),


    # electricity
    setNames(dimSums(vm_prodFe[,,c("feels","feelt")],dim=3,na.rm=T),                                                      "FE|+|Electricity (EJ/yr)"),

    # heat
    setNames(dimSums(vm_prodFe[,,"sehe.fehes.tdhes"],dim=3,na.rm=T),                                                      "FE|+|Heat (EJ/yr)"),

    # hydrogen
    setNames(dimSums(vm_prodFe[,,c("feh2s","feh2t")],dim=3,na.rm=T),                                                      "FE|+|Hydrogen (EJ/yr)"),

    #Emission markets
    setNames((dimSums(mselect(vm_demFeSector, all_emiMkt="ES")  ,dim=3,na.rm=T)),                                         "FE|ESR (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector, all_emiMkt="ETS")  ,dim=3,na.rm=T)),                                        "FE|ETS (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector, all_emiMkt="other")  ,dim=3,na.rm=T)),                                      "FE|Outside ETS and ESR (EJ/yr)")
  )

  getSets(out, fulldim=FALSE)[3] <- "data"

  # ---- FE sector demand ------



  #FE per sector and per emission market (ETS and ESR)
  out <- mbind(out,

    #industry
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="indst")  ,dim=3,na.rm=T)),                                      "FE|++|Industry (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|Industry|++|ESR (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                    "FE|Industry|++|ETS (EJ/yr)"),

    # industry liquids
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",emi_sectors="indst")  ,dim=3,na.rm=T)),                    "FE|Industry|+|Liquids (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqbio",emi_sectors="indst")  ,dim=3,na.rm=T)),"FE|Industry|Liquids|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqfos",emi_sectors="indst")  ,dim=3,na.rm=T)),"FE|Industry|Liquids|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="indst")  ,dim=3,na.rm=T)),"FE|Industry|Liquids|+|Hydrogen (EJ/yr)"),

    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|Industry|ESR|+|Liquids (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqbio",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Liquids|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqfos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Liquids|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Liquids|+|Hydrogen (EJ/yr)"),

    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                    "FE|Industry|ETS|+|Liquids (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqbio",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Liquids|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqfos",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Liquids|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Liquids|+|Hydrogen (EJ/yr)"),

    # industry solids
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",emi_sectors="indst")  ,dim=3,na.rm=T)),                    "FE|Industry|+|Solids (EJ/yr)"),

    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesobio",emi_sectors="indst")  ,dim=3,na.rm=T)), "FE|Industry|Solids|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesofos",emi_sectors="indst")  ,dim=3,na.rm=T)), "FE|Industry|Solids|+|Fossil (EJ/yr)"),

    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|Industry|ESR|+|Solids (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesobio",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Solids|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesofos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Solids|+|Fossil (EJ/yr)"),

    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                   "FE|Industry|ETS|+|Solids (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesobio",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Solids|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesofos",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Solids|+|Fossil (EJ/yr)"),

    # industry gases
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="indst")  ,dim=3,na.rm=T)),                    "FE|Industry|+|Gases (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="indst"),dim=3,na.rm=T)),  "FE|Industry|Gases|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="indst") ,dim=3,na.rm=T)),  "FE|Industry|Gases|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="indst") ,dim=3,na.rm=T)),  "FE|Industry|Gases|+|Hydrogen (EJ/yr)"),

    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|Industry|ESR|+|Gases (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Gases|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Gases|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Industry|ESR|Gases|+|Hydrogen (EJ/yr)"),

    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                  "FE|Industry|ETS|+|Gases (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Gases|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Gases|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="indst", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Industry|ETS|Gases|+|Hydrogen (EJ/yr)"),

    # industry electricity
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="indst")  ,dim=3,na.rm=T)),                   "FE|Industry|+|Electricity (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),  "FE|Industry|ESR|+|Electricity (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Industry|ETS|+|Electricity (EJ/yr)"),

    # industry heat
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="indst")  ,dim=3,na.rm=T)),                   "FE|Industry|+|Heat (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),  "FE|Industry|ESR|+|Heat (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Industry|ETS|+|Heat (EJ/yr)"),

    # industry hydrogen
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="indst")  ,dim=3,na.rm=T)),                   "FE|Industry|+|Hydrogen (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="indst", all_emiMkt="ES")  ,dim=3,na.rm=T)),  "FE|Industry|ESR|+|Hydrogen (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="indst", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Industry|ETS|+|Hydrogen (EJ/yr)"),


    # transport
    ## all transport variables include bunkers, except when expressly written otherwise in the variable name
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="trans")  ,dim=3,na.rm=T)),                     "FE|++|Transport (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),    "FE|Transport|++|ESR (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T)), "FE|Transport|++|Outside ETS and ESR (EJ/yr)"),

    # transport liquids
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),emi_sectors="trans")  ,dim=3,na.rm=T)),                     "FE|Transport|+|Liquids (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqbio",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|Liquids|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqfos",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|Liquids|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqsyn",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|Liquids|+|Hydrogen (EJ/yr)"),

    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|Transport|ESR|+|Liquids (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqbio",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|ESR|Liquids|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqfos",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|ESR|Liquids|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqsyn",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|ESR|Liquids|+|Hydrogen (EJ/yr)"),


    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T)),                    "FE|Transport|Outside ETS and ESR|+|Liquids (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqbio",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|Liquids|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqfos",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|Liquids|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet","fedie"),all_enty="seliqsyn",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|Liquids|+|Hydrogen (EJ/yr)"),

    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),all_enty="seliqbio",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|LDV|Liquids|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),all_enty="seliqfos",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|LDV|Liquids|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),all_enty="seliqsyn",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|LDV|Liquids|+|Hydrogen (EJ/yr)"),

    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqbio",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Liquids|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqfos",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Liquids|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqsyn",emi_sectors="trans")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Liquids|+|Hydrogen (EJ/yr)"),

    # transport gases
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",emi_sectors="trans")  ,dim=3,na.rm=T)),                    "FE|Transport|+|Gases (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segabio",emi_sectors="trans"),dim=3,na.rm=T)),  "FE|Transport|Gases|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segafos",emi_sectors="trans") ,dim=3,na.rm=T)),  "FE|Transport|Gases|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segasyn",emi_sectors="trans") ,dim=3,na.rm=T)),  "FE|Transport|Gases|+|Hydrogen (EJ/yr)"),

    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|Transport|ESR|+|Gases (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segabio",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|ESR|Gases|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segafos",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|ESR|Gases|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segasyn",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|ESR|Gases|+|Hydrogen (EJ/yr)"),

    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T)),                   "FE|Transport|Outside ETS and ESR|+|Gases (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segabio",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|Gases|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segafos",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|Gases|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segasyn",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|Gases|+|Hydrogen (EJ/yr)"),

    # transport electricity
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feelt",emi_sectors="trans")  ,dim=3,na.rm=T)),                     "FE|Transport|+|Electricity (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feelt",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),    "FE|Transport|ESR|+|Electricity (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feelt",emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|+|Electricity (EJ/yr)"),

    # transport hydrogen
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2t",emi_sectors="trans")  ,dim=3,na.rm=T)),                     "FE|Transport|+|Hydrogen (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2t",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),    "FE|Transport|ESR|+|Hydrogen (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2t",emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T)), "FE|Transport|Outside ETS and ESR|+|Hydrogen (EJ/yr)"),


    # Buildings
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="build")  ,dim=3,na.rm=T)),                   "FE|++|Buildings (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)),  "FE|Buildings|++|ESR (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="build", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Buildings|++|ETS (EJ/yr)"),

    # buildings liquids
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",emi_sectors="build")  ,dim=3,na.rm=T)),                     "FE|Buildings|+|Liquids (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqbio",emi_sectors="build")  ,dim=3,na.rm=T)), "FE|Buildings|Liquids|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqfos",emi_sectors="build")  ,dim=3,na.rm=T)), "FE|Buildings|Liquids|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="build")  ,dim=3,na.rm=T)), "FE|Buildings|Liquids|+|Hydrogen (EJ/yr)"),

    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|Buildings|ESR|+|Liquids (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqbio",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Liquids|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqfos",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Liquids|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Liquids|+|Hydrogen (EJ/yr)"),


    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",emi_sectors="build", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                    "FE|Buildings|ETS|+|Liquids (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqbio",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Liquids|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqfos",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Liquids|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehos",all_enty="seliqsyn",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Liquids|+|Hydrogen (EJ/yr)"),

    # buildings solids
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",emi_sectors="build")  ,dim=3,na.rm=T)),                    "FE|Buildings|+|Solids (EJ/yr)"),

    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesobio",emi_sectors="build")  ,dim=3,na.rm=T)), "FE|Buildings|Solids|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesofos",emi_sectors="build")  ,dim=3,na.rm=T)), "FE|Buildings|Solids|+|Fossil (EJ/yr)"),

    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|Buildings|ESR|+|Solids (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesobio",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Solids|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesofos",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Solids|+|Fossil (EJ/yr)"),

    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",emi_sectors="build", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                   "FE|Buildings|ETS|+|Solids (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesobio",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Solids|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fesos",all_enty="sesofos",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Solids|+|Fossil (EJ/yr)"),

    # buildings gases
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="build")  ,dim=3,na.rm=T)),                    "FE|Buildings|+|Gases (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="build"),dim=3,na.rm=T)),  "FE|Buildings|Gases|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="build") ,dim=3,na.rm=T)),  "FE|Buildings|Gases|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="build") ,dim=3,na.rm=T)),  "FE|Buildings|Gases|+|Hydrogen (EJ/yr)"),

    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|Buildings|ESR|+|Gases (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Gases|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Gases|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Buildings|ESR|Gases|+|Hydrogen (EJ/yr)"),

    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="build", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Buildings|ETS|+|Gases (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Gases|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Gases|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="build", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|Buildings|ETS|Gases|+|Hydrogen (EJ/yr)"),

    # buildings electricity
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="build")  ,dim=3,na.rm=T)),                   "FE|Buildings|+|Electricity (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)),  "FE|Buildings|ESR|+|Electricity (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="build", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Buildings|ETS|+|Electricity (EJ/yr)"),

    # buildings heat
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="build")  ,dim=3,na.rm=T)),                   "FE|Buildings|+|Heat (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)),  "FE|Buildings|ESR|+|Heat (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="build", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Buildings|ETS|+|Heat (EJ/yr)"),

    # buildings hydrogen
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="build")  ,dim=3,na.rm=T)),                   "FE|Buildings|+|Hydrogen (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="build", all_emiMkt="ES")  ,dim=3,na.rm=T)),  "FE|Buildings|ESR|+|Hydrogen (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="build", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|Buildings|ETS|+|Hydrogen (EJ/yr)"),

    # CDR
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="CDR")  ,dim=3,na.rm=T)),                  "FE|++|CDR (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,emi_sectors="CDR", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|CDR|+++|ETS (EJ/yr)"),

    # CDR liquids
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",emi_sectors="CDR")  ,dim=3,na.rm=T)),                     "FE|CDR|+|Liquids (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",all_enty="seliqbio",emi_sectors="CDR")  ,dim=3,na.rm=T)), "FE|CDR|Liquids|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",all_enty="seliqfos",emi_sectors="CDR")  ,dim=3,na.rm=T)), "FE|CDR|Liquids|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",all_enty="seliqsyn",emi_sectors="CDR")  ,dim=3,na.rm=T)), "FE|CDR|Liquids|+|Hydrogen (EJ/yr)"),

    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",emi_sectors="CDR", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                    "FE|CDR|ETS|+|Liquids (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",all_enty="seliqbio",emi_sectors="CDR", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|CDR|ETS|Liquids|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",all_enty="seliqfos",emi_sectors="CDR", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|CDR|ETS|Liquids|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fedie",all_enty="seliqsyn",emi_sectors="CDR", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|CDR|ETS|Liquids|+|Hydrogen (EJ/yr)"),
    # CDR gases
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="CDR")  ,dim=3,na.rm=T)),                   "FE|CDR|+|Gases (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="CDR"),dim=3,na.rm=T)), "FE|CDR|Gases|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="CDR") ,dim=3,na.rm=T)), "FE|CDR|Gases|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="CDR") ,dim=3,na.rm=T)), "FE|CDR|Gases|+|Hydrogen (EJ/yr)"),

    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",emi_sectors="CDR", all_emiMkt="ETS")  ,dim=3,na.rm=T)),                   "FE|CDR|ETS|+|Gases (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segabio",emi_sectors="CDR", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|CDR|ETS|Gases|+|Biomass (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segafos",emi_sectors="CDR", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|CDR|ETS|Gases|+|Fossil (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegas",all_enty="segasyn",emi_sectors="CDR", all_emiMkt="ETS") ,dim=3,na.rm=T)), "FE|CDR|ETS|Gases|+|Hydrogen (EJ/yr)"),

    # CDR electricity
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="CDR")  ,dim=3,na.rm=T)),                   "FE|CDR|+|Electricity (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feels",emi_sectors="CDR", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|CDR|ETS|+|Electricity (EJ/yr)"),

    # CDR hydrogen
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="CDR")  ,dim=3,na.rm=T)),                   "FE|CDR|+|Hydrogen (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2s",emi_sectors="CDR", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|CDR|ETS|+|Hydrogen (EJ/yr)"),

    # CDR heat
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="CDR")  ,dim=3,na.rm=T)),                   "FE|CDR|+|Heat (EJ/yr)"),
    setNames((dimSums(mselect(vm_demFeSector,all_enty1="fehes",emi_sectors="CDR", all_emiMkt="ETS")  ,dim=3,na.rm=T)), "FE|CDR|ETS|+|Heat (EJ/yr)")

  )

  # TODO: align these variables in transport complex and edge_esm!
  # quick fix: to avoid duplicates of this variable with the version with a "+" below in case of transport complex: only calculate this in case of edge_esm being used:

  if (tran_mod != "complex"){

  out <- mbind(out,
               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),emi_sectors="trans")  ,dim=3,na.rm=T)),                     "FE|Transport|LDV|Liquids (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),emi_sectors="trans")  ,dim=3,na.rm=T)),                     "FE|Transport|non-LDV|Liquids (EJ/yr)"))


  }




  ### FE carriers from specific PE origin

  p_share_coal_liq <- dimSums(mselect(vm_prodSe, all_enty="pecoal", all_enty1="seliqfos"), dim=3) / dimSums(mselect(vm_prodSe, all_enty1="seliqfos"), dim=3, na.rm = T)
  p_share_oil_liq <- dimSums(mselect(vm_prodSe, all_enty="peoil", all_enty1="seliqfos"), dim=3) / dimSums(mselect(vm_prodSe, all_enty1="seliqfos"), dim=3, na.rm = T)
  p_share_gas_liq <- dimSums(mselect(vm_prodSe, all_enty="pegas", all_enty1="seliqfos"), dim=3) / dimSums(mselect(vm_prodSe, all_enty1="seliqfos"), dim=3, na.rm = T)


  p_share_coal_liq[is.na(p_share_coal_liq)] <- 0
  p_share_oil_liq[is.na(p_share_oil_liq)] <- 0
  p_share_gas_liq[is.na(p_share_gas_liq)] <- 0

  p_share_ngas_gas <- dimSums(mselect(vm_prodSe, all_enty="pegas", all_enty1="segafos"), dim=3) / dimSums(mselect(vm_prodSe, all_enty1="segafos"), dim=3, na.rm = T)
  p_share_coal_gas <- dimSums(mselect(vm_prodSe, all_enty="pecoal", all_enty1="segafos"), dim=3) / dimSums(mselect(vm_prodSe, all_enty1="segafos"), dim=3, na.rm = T)

  p_share_ngas_gas[is.na(p_share_ngas_gas)] <- 0
  p_share_coal_gas[is.na(p_share_coal_gas)] <- 0

  # origin of fossil liquids and gases
  out <- mbind(out,

                # industry fossil liquids
                setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1="fehos",emi_sectors="indst")*p_share_oil_liq  ,dim=3,na.rm=T)), "FE|Industry|Liquids|Fossil|+|Oil (EJ/yr)"),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1="fehos",emi_sectors="indst")*p_share_gas_liq  ,dim=3,na.rm=T)), "FE|Industry|Liquids|Fossil|+|Gas (EJ/yr)"),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1="fehos",emi_sectors="indst")*p_share_coal_liq ,dim=3,na.rm=T)), "FE|Industry|Liquids|Fossil|+|Coal (EJ/yr)"),

                # buildings fossil liquids
                setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1="fehos",emi_sectors="build")*p_share_oil_liq  ,dim=3,na.rm=T)), "FE|Buildings|Liquids|Fossil|+|Oil (EJ/yr)"),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1="fehos",emi_sectors="build")*p_share_gas_liq  ,dim=3,na.rm=T)), "FE|Buildings|Liquids|Fossil|+|Gas (EJ/yr)"),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1="fehos",emi_sectors="build")*p_share_coal_liq ,dim=3,na.rm=T)), "FE|Buildings|Liquids|Fossil|+|Coal (EJ/yr)"),

                # transport fossil liquids
                setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1=c("fepet","fedie"),emi_sectors="trans")*p_share_oil_liq  ,dim=3,na.rm=T)), "FE|Transport|Liquids|Fossil|+|Oil (EJ/yr)"),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1=c("fepet","fedie"),emi_sectors="trans")*p_share_gas_liq  ,dim=3,na.rm=T)), "FE|Transport|Liquids|Fossil|+|Gas (EJ/yr)"),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1=c("fepet","fedie"),emi_sectors="trans")*p_share_coal_liq ,dim=3,na.rm=T)), "FE|Transport|Liquids|Fossil|+|Coal (EJ/yr)"),

                # industry fossil gases
                setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1="fegas",emi_sectors="indst")*p_share_ngas_gas  ,dim=3,na.rm=T)), "FE|Industry|Gases|Fossil|+|Natural Gas (EJ/yr)"),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1="fegas",emi_sectors="indst")*p_share_coal_gas  ,dim=3,na.rm=T)), "FE|Industry|Gases|Fossil|+|Coal (EJ/yr)"),

                # buildings fossil gases
                setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1="fegas",emi_sectors="build")*p_share_ngas_gas  ,dim=3,na.rm=T)), "FE|Buildings|Gases|Fossil|+|Natural Gas (EJ/yr)"),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1="fegas",emi_sectors="build")*p_share_coal_gas  ,dim=3,na.rm=T)), "FE|Buildings|Gases|Fossil|+|Coal (EJ/yr)"),

                # transport fossil gases
                setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1="fegat",emi_sectors="trans")*p_share_ngas_gas  ,dim=3,na.rm=T)), "FE|Transport|Gases|Fossil|+|Natural Gas (EJ/yr)"),
                setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1="fegat",emi_sectors="trans")*p_share_coal_gas  ,dim=3,na.rm=T)), "FE|Transport|Gases|Fossil|+|Coal (EJ/yr)"),

               # total fossil liquids
               setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1=c("fehos","fepet","fedie"))*p_share_oil_liq  ,dim=3,na.rm=T)), "FE|Liquids|Fossil|+|Oil (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1=c("fehos","fepet","fedie"))*p_share_gas_liq  ,dim=3,na.rm=T)), "FE|Liquids|Fossil|+|Gas (EJ/yr)"),
               setNames((dimSums(mselect(vm_demFeSector,all_enty="seliqfos",all_enty1=c("fehos","fepet","fedie"))*p_share_coal_liq ,dim=3,na.rm=T)), "FE|Liquids|Fossil|+|Coal (EJ/yr)")


               # # total fossil gases, TODO: does not add up, check another time
               # setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1=c("fegas","fegat"))*p_share_ngas_gas  ,dim=3,na.rm=T)), "FE|Gases|Fossil|+|Natural Gas (EJ/yr)"),
               # setNames((dimSums(mselect(vm_demFeSector,all_enty="segafos",all_enty1=c("fegas","fegat"))*p_share_coal_gas  ,dim=3,na.rm=T)), "FE|Gases|Fossil|+|Coal (EJ/yr)")

  )

  # ---- BUNKERS ----
  # creating additional variables with and without bunkers for the transport sector

  ## ESR variables correspond to transport without bunkers by definition
  var_without_Bunkers <- mbind(
    lapply(
      getNames(out)[grep("FE\\|Transport\\|ESR", getNames(out))],
      function(x) {
        setNames(out[,,x],gsub("FE\\|Transport\\|ESR","FE\\|Transport\\|w/o Bunkers",x))
      }
    )
  )
  ## Other emission market variables correspond to transport with bunkers by definition
  var_with_Bunkers <- mbind(
    lapply(
      getNames(out)[grep("FE\\|Transport\\|Outside ETS and ESR", getNames(out))],
      function(x) {
        setNames(out[,,x],gsub("FE\\|Transport\\|Outside ETS and ESR","FE\\|Transport\\|Bunkers",x))
      }
    )
  )
  out <- mbind(out,var_without_Bunkers,var_with_Bunkers)
  ##
  out <- mbind(out,
    setNames(out[,,"FE|Transport|++|ESR (EJ/yr)"], "FE|Transport|w/o Bunkers (EJ/yr)"),
    setNames(out[,,"FE|Transport|++|Outside ETS and ESR (EJ/yr)"], "FE|Transport|Bunkers (EJ/yr)")
  )
  out <- mbind(out,
               setNames(out[,,"FE (EJ/yr)"] - out[,,"FE|Transport|Bunkers (EJ/yr)"], "FE|w/o Bunkers (EJ/yr)")
  )



  # ---- read in needed data

  # ---- sets

  # ---- parameter
  pm_cesdata <- readGDX(gdx,"pm_cesdata")[,t,]

  # ---- variables
  if((buil_mod %in% c("services_putty", "services_with_capital"))||(tran_mod == "edge_esm") )
    vm_demFeForEs <- readGDX(gdx,name = c("vm_demFeForEs"), field="l", restore_zeros=FALSE,format= "first_found",react = "silent")[,t,]*TWa_2_EJ
  #vm_demFeForEs = vm_demFeForEs[fe2es]
  # CES nodes, convert from TWa to EJ
  vm_cesIO <- readGDX(gdx, name=c("vm_cesIO"), field="l", restore_zeros=FALSE,format= "first_found")[,t,]*TWa_2_EJ

  # ---- transformations
  # Correct for offset quantities in the transition between ESM and CES for zero quantities
  if (any(grep('\\.offset_quantity$', getNames(pm_cesdata)))) {
    pf <- paste0(getNames(vm_cesIO), '.offset_quantity')
    offset <- collapseNames(pm_cesdata[,,pf]) * TWa_2_EJ
    vm_cesIO <- vm_cesIO + offset[,t,getNames(vm_cesIO)]
  }


  # ---- Buildings Module ----

  p36_floorspace <- readGDX(gdx, "p36_floorspace", react = "silent")[, t, ]
  if (!is.null(p36_floorspace)) {
    out <- mbind(out, setNames(p36_floorspace, "Energy Service|Buildings|Floor Space (bn m2/yr)"))
  }

  if (buil_mod == "simple") {
    # PPF in REMIND and the respective reporting variables
    carrierBuild <- c(
      feelcb  = "FE|Buildings|non-Heating|Electricity|Conventional (EJ/yr)",
      feelrhb = "FE|Buildings|Heating|Electricity|Resistance (EJ/yr)",
      feelhpb = "FE|Buildings|Heating|Electricity|Heat pumps (EJ/yr)",
      feheb   = "FE|Buildings|Heating|District Heating (EJ/yr)",
      fesob   = "FE|Buildings|Heating|Solids (EJ/yr)",
      fehob   = "FE|Buildings|Heating|Liquids (EJ/yr)",
      fegab   = "FE|Buildings|Heating|Gases (EJ/yr)",
      feh2b   = "FE|Buildings|Heating|Hydrogen (EJ/yr)")
    # all final energy (FE) demand in buildings without coventional electricity
    # is summed as heating
    carrierBuildHeating <- tail(carrierBuild, -1)

    # FE demand in buildings for each carrier
    # (electricity split: heat pumps, resistive heating, rest)
    for (c in names(carrierBuild)) {
      out <- mbind(out, setNames(dimSums(vm_cesIO[,, c], dim = 3, na.rm = TRUE),
                                 carrierBuild[c]))
    }

    # sum of heating FE demand
    out <- mbind(out,
      setNames(dimSums(vm_cesIO[,, names(carrierBuildHeating)], dim = 3, na.rm = TRUE),
               "FE|Buildings|Heating (EJ/yr)"))

    # UE demand in buildings for each carrier
    # this buildings realisation only works on a FE level but the UE demand is
    # estimated here assuming the FE-UE efficiency of the basline (from EDGE-B)
    p36_uedemand_build <- readGDX(gdx, "p36_uedemand_build", react = "silent")[, t, ]
    if (!is.null(p36_uedemand_build)) {
      pm_fedemand <- readGDX(gdx, "pm_fedemand")[, t, ]
      feUeEff_build <- p36_uedemand_build[,, names(carrierBuild)] /
        pm_fedemand[,, names(carrierBuild)]
      # assume efficiency for all gases also for H2
      feUeEff_build[,, "feh2b"] <- setNames(feUeEff_build[,, "fegab"], "feh2b")
      # apply efficiency to get UE levels
      uedemand_build <- vm_cesIO[,, names(carrierBuild)] * feUeEff_build
      getItems(uedemand_build, 3) <-
        gsub("^FE", "UE", carrierBuild)[getItems(uedemand_build, 3)]
      out <- mbind(out, uedemand_build)

      # sum of heating UE demand
      out <- mbind(out,
        setNames(dimSums(out[,, gsub("^FE", "UE", carrierBuildHeating)],
                         dim = 3, na.rm = TRUE),
                 "UE|Buildings|Heating (EJ/yr)"))

      # sum of buildings UE demand
      out <- mbind(out,
                   setNames(dimSums(out[,, gsub("^FE", "UE", carrierBuild)],
                                    dim = 3, na.rm = TRUE),
                            "UE|Buildings (EJ/yr)"))
    }
  } else if (buil_mod %in% c("services_putty", "services_with_capital")){

    # sets
    ppfen_build <- readGDX(gdx,c("ppfen_buildings_dyn36","ppfen_buildings_dyn28","ppfen_buildings"),format="first_found", react = "silent")
    esty_build <-  readGDX(gdx,c("esty_dyn36"),format="first_found", react = "silent")

    #var
    v_prodEs <- readGDX(gdx,name = c("vm_prodEs","v_prodEs"), field="l",restore_zeros = F, format = "first_found", react = "silent")[,t,]* TWa_2_EJ

    ces_elec = c(grep("elb$", ppfen_build, value = T),grep("hpb$", ppfen_build, value = T))
    es_elec = c(grep("elb$", esty_build, value = T),grep("hpb$", esty_build, value = T))
    es_solids = c(grep("sob$", esty_build, value = T), grep("stb$", esty_build, value = T))
    es_gas = grep("gab$", esty_build, value = T)
    es_liq = grep("hob$", esty_build, value = T)
    es_heat = grep("heb$", esty_build, value = T)
    es_hydro = grep("h2b$", esty_build, value = T)

    putty_ue = c("uescb","ueshb","uealb","uecwb")

    out <- mbind(out,
                  # Useful Energy
                  setNames(dimSums(vm_cesIO[,,"uealb"],dim=3,na.rm=T),        "UE|Buildings|Appliances and Light (EJ/yr)"),
                  setNames(dimSums(vm_cesIO[,,"uecwb"],dim=3,na.rm=T),        "UE|Buildings|Cooking and Water (EJ/yr)"),
                  setNames(dimSums(vm_cesIO[,,"ueshb"],dim=3,na.rm=T),        "UE|Buildings|Space Heating (EJ/yr)"),
                  setNames(dimSums(vm_cesIO[,,"uescb"],dim=3,na.rm=T),        "UE|Buildings|Space Cooling (EJ/yr)"),

                  setNames(dimSums(vm_cesIO[,,putty_ue],dim=3,na.rm=T),       "UE|Buildings (EJ/yr)"),

                  setNames(dimSums(vm_cesIO[,,"uescb"],dim=3,na.rm=T),        "UE|Buildings|Space Cooling|Electricity (EJ/yr)"),

                  setNames(dimSums(vm_cesIO[,,"uealb"],dim=3,na.rm=T),        "UE|Buildings|Appliances and Light|Electricity (EJ/yr)"),

                  setNames(dimSums(v_prodEs[,,c("uecwsob","uecwstb")],dim=3,na.rm=T),        "UE|Buildings|Cooking and Water|Solids (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"uecwsob"],dim=3,na.rm=T),        "UE|Buildings|Cooking and Water|Solids|Modern (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"uecwstb"],dim=3,na.rm=T),        "UE|Buildings|Cooking and Water|Solids|Traditional (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"uecwelb"],dim=3,na.rm=T),        "UE|Buildings|Cooking and Water|Electricity|Resistance (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"uecwheb"],dim=3,na.rm=T),        "UE|Buildings|Cooking and Water|Heat (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"uecwgab"],dim=3,na.rm=T),        "UE|Buildings|Cooking and Water|Gases (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"uecwhob"],dim=3,na.rm=T),        "UE|Buildings|Cooking and Water|Liquids (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"uecwh2b"],dim=3,na.rm=T),        "UE|Buildings|Cooking and Water|Hydrogen (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"uecwhpb"],dim=3,na.rm=T),        "UE|Buildings|Cooking and Water|Electricity|Heat pumps (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,c("uecwelb","uecwhpb")],dim=3,na.rm=T), "UE|Buildings|Cooking and Water|Electricity (EJ/yr)"),


                  setNames(dimSums(v_prodEs[,,c("ueshsob","ueshstb")],dim=3,na.rm=T),        "UE|Buildings|Space Heating|Solids (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"ueshsob"],dim=3,na.rm=T),        "UE|Buildings|Space Heating|Solids|Modern (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"ueshstb"],dim=3,na.rm=T),        "UE|Buildings|Space Heating|Solids|Traditional (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"ueshelb"],dim=3,na.rm=T),        "UE|Buildings|Space Heating|Electricity|Resistance (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"ueshheb"],dim=3,na.rm=T),        "UE|Buildings|Space Heating|Heat (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"ueshgab"],dim=3,na.rm=T),        "UE|Buildings|Space Heating|Gases (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"ueshhob"],dim=3,na.rm=T),        "UE|Buildings|Space Heating|Liquids (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"ueshh2b"],dim=3,na.rm=T),        "UE|Buildings|Space Heating|Hydrogen (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,"ueshhpb"],dim=3,na.rm=T),        "UE|Buildings|Space Heating|Electricity|Heat pumps (EJ/yr)"),
                  setNames(dimSums(v_prodEs[,,c("ueshelb","ueshhpb")],dim=3,na.rm=T), "UE|Buildings|Space Heating|Electricity (EJ/yr)"),

                  # Final Energy
                  setNames(dimSums(vm_cesIO[,,"fealelb"],dim=3,na.rm=T),        "FE|Buildings|Appliances and Light|Electricity (EJ/yr)"),

                  setNames(dimSums(vm_demFeForEs[,,c("uecwsob","uecwstb")],dim=3,na.rm=T),        "FE|Buildings|Cooking and Water|Solids (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"uecwsob"],dim=3,na.rm=T),        "FE|Buildings|Cooking and Water|Solids|Modern (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"uecwstb"],dim=3,na.rm=T),        "FE|Buildings|Cooking and Water|Solids|Traditional (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"uecwelb"],dim=3,na.rm=T),        "FE|Buildings|Cooking and Water|Electricity|Resistance (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"uecwheb"],dim=3,na.rm=T),        "FE|Buildings|Cooking and Water|Heat (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"uecwgab"],dim=3,na.rm=T),        "FE|Buildings|Cooking and Water|Gases (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"uecwhob"],dim=3,na.rm=T),        "FE|Buildings|Cooking and Water|Liquids (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"uecwh2b"],dim=3,na.rm=T),        "FE|Buildings|Cooking and Water|Hydrogen (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"uecwhpb"],dim=3,na.rm=T),        "FE|Buildings|Cooking and Water|Electricity|Heat pumps (EJ/yr)"),

                  setNames(dimSums(vm_demFeForEs[,,c("uecwelb","uecwhpb")],dim=3,na.rm=T), "FE|Buildings|Cooking and Water|Electricity (EJ/yr)"),

                  setNames(dimSums(vm_cesIO[,,"fescelb"],dim=3,na.rm=T),        "FE|Buildings|Space Cooling|Electricity (EJ/yr)"),

                  setNames(dimSums(vm_demFeForEs[,,c("ueshsob","ueshstb")],dim=3,na.rm=T),        "FE|Buildings|Space Heating|Solids (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"ueshsob"],dim=3,na.rm=T),        "FE|Buildings|Space Heating|Solids|Modern (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"ueshstb"],dim=3,na.rm=T),        "FE|Buildings|Space Heating|Solids|Traditional (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"ueshelb"],dim=3,na.rm=T),        "FE|Buildings|Space Heating|Electricity|Resistance (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"ueshheb"],dim=3,na.rm=T),        "FE|Buildings|Space Heating|Heat (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"ueshgab"],dim=3,na.rm=T),        "FE|Buildings|Space Heating|Gases (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"ueshhob"],dim=3,na.rm=T),        "FE|Buildings|Space Heating|Liquids (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"ueshh2b"],dim=3,na.rm=T),        "FE|Buildings|Space Heating|Hydrogen (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,"ueshhpb"],dim=3,na.rm=T),        "FE|Buildings|Space Heating|Electricity|Heat pumps (EJ/yr)"),
                  setNames(dimSums(vm_demFeForEs[,,c("ueshelb","ueshhpb")],dim=3,na.rm=T), "FE|Buildings|Space Heating|Electricity (EJ/yr)")#,

    )

    out <- mbind(out,
                  setNames(dimSums(out[,,c("UE|Buildings|Space Heating (EJ/yr)","UE|Buildings|Space Cooling (EJ/yr)")],dim=3,na.rm=T), "UE|Buildings|Space Conditioning (EJ/yr)"),

                  setNames(dimSums(out[,,"FE|Buildings|Appliances and Light|Electricity (EJ/yr)"],dim=3,na.rm=T),        "FE|Buildings|Appliances and Light (EJ/yr)"),

                  setNames(dimSums(out[,,c("FE|Buildings|Cooking and Water|Solids (EJ/yr)",
                                            "FE|Buildings|Cooking and Water|Electricity (EJ/yr)",
                                            "FE|Buildings|Cooking and Water|Heat (EJ/yr)",
                                            "FE|Buildings|Cooking and Water|Gases (EJ/yr)",
                                            "FE|Buildings|Cooking and Water|Liquids (EJ/yr)",
                                            "FE|Buildings|Cooking and Water|Hydrogen (EJ/yr)")],dim=3,na.rm=T),        "FE|Buildings|Cooking and Water (EJ/yr)"),

                  setNames(dimSums(out[,,"FE|Buildings|Space Cooling|Electricity (EJ/yr)"],dim=3,na.rm=T),        "FE|Buildings|Space Cooling (EJ/yr)"),

                  setNames(dimSums(out[,,c("FE|Buildings|Space Heating|Solids (EJ/yr)",
                                            "FE|Buildings|Space Heating|Electricity (EJ/yr)",
                                            "FE|Buildings|Space Heating|Heat (EJ/yr)",
                                            "FE|Buildings|Space Heating|Gases (EJ/yr)",
                                            "FE|Buildings|Space Heating|Liquids (EJ/yr)",
                                            "FE|Buildings|Space Heating|Hydrogen (EJ/yr)")],dim=3,na.rm=T),        "FE|Buildings|Space Heating (EJ/yr)")

    )
  }

  # Industry Module ----
  # detailed industry FE reporting

  ## FE demand per industry subsector ----
  # this reporting is only available for GDXs which have the reporting parameter
  # o37_demFeIndSub
  if (!(is.null(o37_demFeIndSub) | 0 == length(o37_demFeIndSub))) {
    # total FE per industry subsector
    out <- mbind(
      out,
      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "steel"), dim = 3),
               "FE|Industry|+++|Steel (EJ/yr)"),

      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "cement"), dim = 3),
               "FE|Industry|+++|Cement (EJ/yr)"),

      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "chemicals"),
                       dim = 3),
               "FE|Industry|+++|Chemicals (EJ/yr)"),

      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "otherInd"), dim = 3),
               "FE|Industry|+++|Other Industry (EJ/yr)"))

    ## FE per industry sector and carrier ----
    ### steel sector ----
    out <- mbind(
      out,
      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "steel",
                               all_enty1 = "feels"), dim = 3),
               "FE|Industry|Steel|+|Electricity (EJ/yr)"),

      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "steel",
                               all_enty1 = "feh2s"), dim = 3 ),
               "FE|Industry|Steel|+|Hydrogen (EJ/yr)"),

      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "steel",
                               all_enty1 = "fesos"), dim = 3),
               "FE|Industry|Steel|+|Solids (EJ/yr)"),

      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "steel",
                               all_enty1 = "fehos"), dim = 3),
               "FE|Industry|Steel|+|Liquids (EJ/yr)"),

      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "steel",
                               all_enty1 = "fegas"), dim = 3),
               "FE|Industry|Steel|+|Gases (EJ/yr)"))

    # more detailed reporting of electricity uses available in subsectors realization
    if (indu_mod == 'subsectors') {
      out <- mbind(
        out,
        setNames(mselect(vm_cesIO, all_in = "feel_steel_primary"),
                 "FE|Industry|Steel|Primary|Electricity (EJ/yr)"),
        setNames(mselect(vm_cesIO, all_in = "feel_steel_secondary"),
                 "FE|Industry|Steel|Secondary|Electricity (EJ/yr)"))

      # mapping of industrial output to energy production factors in CES tree
      ces_eff_target_dyn37 <- readGDX(gdx, "ces_eff_target_dyn37")

      # energy production factors for primary and secondary steel
      en.ppfen.primary.steel <- ces_eff_target_dyn37 %>%
        filter(all_in == "ue_steel_primary") %>%
        pull('all_in1')
      en.ppfen.sec.steel <- ces_eff_target_dyn37 %>%
        filter(all_in == "ue_steel_secondary") %>%
        pull('all_in1')

      # total FE by primary/secondary Steel
      out <- mbind(
        out,
        setNames(dimSums(mselect(vm_cesIO, all_in = en.ppfen.primary.steel),
                         dim = 3),
                 "FE|Industry|Steel|++|Primary (EJ/yr)"),
        setNames(dimSums(mselect(vm_cesIO, all_in = en.ppfen.sec.steel),
                         dim = 3),
                 "FE|Industry|Steel|++|Secondary (EJ/yr)"))
    }

    ### cement sector ----
    out <- mbind(
      out,
      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "cement",
                               all_enty1 = "feels"), dim = 3),
               "FE|Industry|Cement|+|Electricity (EJ/yr)"),

      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "cement",
                               all_enty1 = "feh2s"), dim = 3),
               "FE|Industry|Cement|+|Hydrogen (EJ/yr)"),

      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "cement",
                               all_enty1 = "fesos"), dim = 3),
               "FE|Industry|Cement|+|Solids (EJ/yr)"),

      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "cement",
                               all_enty1 = "fehos"), dim = 3),
               "FE|Industry|Cement|+|Liquids (EJ/yr)"),

      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "cement",
                               all_enty1 = "fegas"), dim = 3),
               "FE|Industry|Cement|+|Gases (EJ/yr)"))

    ### chemicals sector ----
    out <- mbind(
      out,
      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "chemicals",
                               all_enty1 = "feels"), dim = 3),
               "FE|Industry|Chemicals|+|Electricity (EJ/yr)"),

      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "chemicals",
                               all_enty1 = "feh2s"), dim = 3),
               "FE|Industry|Chemicals|+|Hydrogen (EJ/yr)"),

      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "chemicals",
                               all_enty1 = "fesos"), dim = 3),
               "FE|Industry|Chemicals|+|Solids (EJ/yr)"),

      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "chemicals",
                               all_enty1 = "fehos"), dim = 3),
               "FE|Industry|Chemicals|+|Liquids (EJ/yr)"),

      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "chemicals",
                               all_enty1 = "fegas"), dim = 3),
               "FE|Industry|Chemicals|+|Gases (EJ/yr)"))


    # more detailed reporting of electricity uses available in subsectors
    # realization
    if (indu_mod == 'subsectors') {
      out <- mbind(
        out,
        setNames(mselect(vm_cesIO, all_in = "feelwlth_chemicals"),
                 "FE|Industry|Chemicals|Electricity|+|Mechanical work and low-temperature heat (EJ/yr)"),
        setNames(mselect(vm_cesIO, all_in = "feelhth_chemicals"),
                 "FE|Industry|Chemicals|Electricity|+|High-temperature heat (EJ/yr)"))
    }

    ### other industry sector ----
    out <- mbind(
      out,
      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "otherInd",
                               all_enty1 = "feels"), dim = 3),
               "FE|Industry|Other Industry|+|Electricity (EJ/yr)"),

      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "otherInd",
                               all_enty1 = "fehes"), dim = 3),
               "FE|Industry|Other Industry|+|Heat (EJ/yr)"),

      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "otherInd",
                               all_enty1 = "feh2s"), dim = 3),
               "FE|Industry|Other Industry|+|Hydrogen (EJ/yr)"),

      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "otherInd",
                               all_enty1 = "fesos"), dim = 3),
               "FE|Industry|Other Industry|+|Solids (EJ/yr)"),

      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "otherInd",
                               all_enty1 = "fehos"), dim = 3),
               "FE|Industry|Other Industry|+|Liquids (EJ/yr)"),

      setNames(dimSums(mselect(o37_demFeIndSub, secInd37 = "otherInd",
                               all_enty1 = "fegas"), dim = 3),
               "FE|Industry|Other Industry|+|Gases (EJ/yr)"))

    # more detailed reporting of electricity uses available in subsectors
    # realization
    if (indu_mod == 'subsectors') {
      out <- mbind(
        out,
        setNames(mselect(vm_cesIO, all_in = "feelwlth_otherInd"),
                 "FE|Industry|Other Industry|Electricity|+|Mechanical work and low-temperature heat (EJ/yr)"),
        setNames(mselect(vm_cesIO, all_in = "feelhth_otherInd"),
                 "FE|Industry|Other Industry|Electricity|+|High-temperature heat (EJ/yr)"))
    }

    ## Industry Production/Value Added ----
    # reporting of industry production and value added as given by CES nodes
    # (only available in industry subsectors)
    if (indu_mod == 'subsectors') {
      # production and value added
      out <- mbind(
        out,
        # as vm_cesIO was multiplied by TWa_2_EJ in the beginning of the script,
        # needs to be converted back to REMIND units here and then scaled by 1e3
        # for obtaining Mt or billion US$2005
        setNames(mselect(vm_cesIO, all_in = "ue_cement") * 1e3 / TWa_2_EJ,
                 "Production|Industry|Cement (Mt/yr)"),
        setNames(
          mselect(vm_cesIO, all_in = "ue_steel_primary") * 1e3 / TWa_2_EJ,
          "Production|Industry|Steel|Primary (Mt/yr)"),
        setNames(
          mselect(vm_cesIO, all_in = "ue_steel_secondary") * 1e3 / TWa_2_EJ,
          "Production|Industry|Steel|Secondary (Mt/yr)"),
        setNames(mselect(vm_cesIO, all_in = "ue_chemicals") * 1e3 / TWa_2_EJ,
                 "Value Added|Industry|Chemicals (billion US$2005/yr)"),
        setNames(mselect(vm_cesIO, all_in = "ue_otherInd") * 1e3 / TWa_2_EJ,
                 "Value Added|Industry|Other Industry (billion US$2005/yr)"),

        # report CES node of total industry as internal variable (for model
        # diagnostics) to represent total industry activity
        setNames(mselect(vm_cesIO, all_in = "ue_industry"),
                 "Internal|Activity|Industry (arbitrary unit/yr)"))

      # total steel production
      out <- mbind(
        out,
        setNames(  out[,,"Production|Industry|Steel|Primary (Mt/yr)"]
                 + out[,,"Production|Industry|Steel|Secondary (Mt/yr)"],
                 "Production|Industry|Steel (Mt/yr)"))


    }
  }

  #--- Transport reporting ---

  #Realization specific
  if (tran_mod == "complex"){

    #fedie = HDV, fepet = LDV
    #FE|Transport|LDV
    #FE|Transport|non-LDV
    #  FE|Transport|non-LDV|Bunkers
    #  FE|Transport|non-LDV|w/o Bunkers

    #Freight   > non-LDV > non bunker (national goods transportation)
    #                    > bunker (international goods transportation)
    #Passenger > non-LDV > non bunker
    #                    > bunker (international aviation and passenger ships)
    #Passenger > LDV

    v35_demTransType <- readGDX(gdx,name=c("v35_demTransType"),field="l",restore_zeros=FALSE,format="first_found")

    if(!is.null(v35_demTransType)){
      v35_demTransType <- v35_demTransType[,t,]*TWa_2_EJ

      out <- mbind(out,
                   # Transport LDV
                   setNames((dimSums(mselect(v35_demTransType,transType_35="LDV")  ,dim=3,na.rm=T)),                  "FE|Transport|LDV (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fepet",transType_35="LDV")  ,dim=3,na.rm=T)), "FE|Transport|LDV|+|Liquids (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fegas",transType_35="LDV")  ,dim=3,na.rm=T)), "FE|Transport|LDV|+|Gases (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feelt",transType_35="LDV")  ,dim=3,na.rm=T)), "FE|Transport|LDV|+|Electricity (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feh2t",transType_35="LDV")  ,dim=3,na.rm=T)), "FE|Transport|LDV|+|Hydrogen (EJ/yr)"),

                   setNames((dimSums(mselect(v35_demTransType,all_emiMkt="ES",transType_35="LDV")  ,dim=3,na.rm=T)),                  "FE|Transport|LDV|ESR (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fepet",all_emiMkt="ES",transType_35="LDV")  ,dim=3,na.rm=T)), "FE|Transport|LDV|ESR|+|Liquids (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fegas",all_emiMkt="ES",transType_35="LDV")  ,dim=3,na.rm=T)), "FE|Transport|LDV|ESR|+|Gases (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feelt",all_emiMkt="ES",transType_35="LDV")  ,dim=3,na.rm=T)), "FE|Transport|LDV|ESR|+|Electricity (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feh2t",all_emiMkt="ES",transType_35="LDV")  ,dim=3,na.rm=T)), "FE|Transport|LDV|ESR|+|Hydrogen (EJ/yr)"),

                   # Transport nonLDV
                   setNames((dimSums(mselect(v35_demTransType,transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)),                  "FE|Transport|non-LDV (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fedie",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|+|Liquids (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fegas",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|+|Gases (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feelt",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|+|Electricity (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feh2t",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|+|Hydrogen (EJ/yr)"),

                   setNames((dimSums(mselect(v35_demTransType,all_emiMkt="ES",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)),                  "FE|Transport|non-LDV|ESR (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fedie",all_emiMkt="ES",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|ESR|+|Liquids (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fegas",all_emiMkt="ES",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|ESR|+|Gases (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feelt",all_emiMkt="ES",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|ESR|+|Electricity (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feh2t",all_emiMkt="ES",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|ESR|+|Hydrogen (EJ/yr)"),

                   setNames((dimSums(mselect(v35_demTransType,all_emiMkt="other",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)),                  "FE|Transport|non-LDV|Outside ETS and ESR (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fedie",all_emiMkt="other",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Outside ETS and ESR|+|Liquids (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fegas",all_emiMkt="other",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Outside ETS and ESR|+|Gases (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feelt",all_emiMkt="other",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Outside ETS and ESR|+|Electricity (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feh2t",all_emiMkt="other",transType_35=c("nonLDV_noBunkers","nonLDV_Bunkers"))  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Outside ETS and ESR|+|Hydrogen (EJ/yr)"),

                   # Transport non-LDV Bunkers
                   setNames((dimSums(mselect(v35_demTransType,transType_35="nonLDV_Bunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|++|Bunkers (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fedie",transType_35="nonLDV_Bunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Bunkers|+|Liquids (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fegas",transType_35="nonLDV_Bunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Bunkers|+|Gases (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feelt",transType_35="nonLDV_Bunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Bunkers|+|Electricity (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feh2t",transType_35="nonLDV_Bunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Bunkers|+|Hydrogen (EJ/yr)"),

                   setNames((dimSums(mselect(v35_demTransType,all_emiMkt="other",transType_35="nonLDV_Bunkers")  ,dim=3,na.rm=T)),                  "FE|Transport|non-LDV|Bunkers|++|Outside ETS and ESR (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fedie",all_emiMkt="other",transType_35="nonLDV_Bunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Bunkers|Outside ETS and ESR|+|Liquids (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fegas",all_emiMkt="other",transType_35="nonLDV_Bunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Bunkers|Outside ETS and ESR|+|Gases (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feelt",all_emiMkt="other",transType_35="nonLDV_Bunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Bunkers|Outside ETS and ESR|+|Electricity (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feh2t",all_emiMkt="other",transType_35="nonLDV_Bunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|Bunkers|Outside ETS and ESR|+|Hydrogen (EJ/yr)"),

                   # Transport non-LDV w/o Bunkers
                   setNames((dimSums(mselect(v35_demTransType,transType_35="nonLDV_noBunkers")  ,dim=3,na.rm=T)),                  "FE|Transport|non-LDV|++|w/o Bunkers (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fedie",transType_35="nonLDV_noBunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|w/o Bunkers|+|Liquids (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fegas",transType_35="nonLDV_noBunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|w/o Bunkers|+|Gases (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feelt",transType_35="nonLDV_noBunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|w/o Bunkers|+|Electricity (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feh2t",transType_35="nonLDV_noBunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|w/o Bunkers|+|Hydrogen (EJ/yr)"),

                   setNames((dimSums(mselect(v35_demTransType,all_emiMkt="ES",transType_35="nonLDV_noBunkers")  ,dim=3,na.rm=T)),                  "FE|Transport|non-LDV|w/o Bunkers|++|ESR (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fedie",all_emiMkt="ES",transType_35="nonLDV_noBunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|w/o Bunkers|ESR|+|Liquids (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="fegas",all_emiMkt="ES",transType_35="nonLDV_noBunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|w/o Bunkers|ESR|+|Gases (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feelt",all_emiMkt="ES",transType_35="nonLDV_noBunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|w/o Bunkers|ESR|+|Electricity (EJ/yr)"),
                   setNames((dimSums(mselect(v35_demTransType,all_enty="feh2t",all_emiMkt="ES",transType_35="nonLDV_noBunkers")  ,dim=3,na.rm=T)), "FE|Transport|non-LDV|w/o Bunkers|ESR|+|Hydrogen (EJ/yr)")
      )
    }

    p35_pass_FE_share_transp <- readGDX(gdx,"p35_pass_FE_share_transp", restore_zeros = FALSE)[,t,]

    v_demFe <- readGDX(gdx,name=c("v35_demFe","v_demFe"),field="l",restore_zeros=FALSE,format="first_found")[,t,]*TWa_2_EJ

    out <- mbind(out,

       # Passengers Liquids
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),emi_sectors="trans")  ,dim=3,na.rm=T)) +
                (dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),emi_sectors="trans")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp,                     "FE|Transport|Pass|+|Liquids (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),all_enty="seliqbio",emi_sectors="trans")  ,dim=3,na.rm=T)) +
                (dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqbio",emi_sectors="trans")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp, "FE|Transport|Pass|Liquids|+|Biomass (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),all_enty="seliqfos",emi_sectors="trans")  ,dim=3,na.rm=T)) +
                (dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqfos",emi_sectors="trans")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp, "FE|Transport|Pass|Liquids|+|Fossil (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),all_enty="seliqsyn",emi_sectors="trans")  ,dim=3,na.rm=T)) +
                  (dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqsyn",emi_sectors="trans")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp, "FE|Transport|Pass|Liquids|+|Hydrogen (EJ/yr)"),

       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)) +
                  (dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp,                     "FE|Transport|Pass|ESR|+|Liquids (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),all_enty="seliqbio",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)) +
                  (dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqbio",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp, "FE|Transport|Pass|ESR|Liquids|+|Biomass (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),all_enty="seliqfos",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)) +
                  (dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqfos",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp, "FE|Transport|Pass|ESR|Liquids|+|Fossil (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fepet"),all_enty="seliqsyn",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)) +
                  (dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqsyn",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp, "FE|Transport|Pass|ESR|Liquids|+|Hydrogen (EJ/yr)"),

       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp,                     "FE|Transport|Pass|Outside ETS and ESR|+|Liquids (EJ/yr)"), #=bunkers
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqbio",emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp, "FE|Transport|Pass|Outside ETS and ESR|Liquids|+|Biomass (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqfos",emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp, "FE|Transport|Pass|Outside ETS and ESR|Liquids|+|Fossil (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqsyn",emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T))*p35_pass_FE_share_transp, "FE|Transport|Pass|Outside ETS and ESR|Liquids|+|Hydrogen (EJ/yr)"),

       setNames((dimSums(mselect(v_demFe,all_te="apCarPeT")  ,dim=3,na.rm=T)),                                                        "FE|Transport|Pass|Liquids|Road (EJ/yr)" ),

       # Freight Liquids
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),emi_sectors="trans") ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp),                      "FE|Transport|Freight|+|Liquids (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqbio",emi_sectors="trans")  ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp), "FE|Transport|Freight|Liquids|+|Biomass (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqfos",emi_sectors="trans")  ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp), "FE|Transport|Freight|Liquids|+|Fossil (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqsyn",emi_sectors="trans")  ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp), "FE|Transport|Freight|Liquids|+|Hydrogn (EJ/yr)"),

       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp),                     "FE|Transport|Freight|ESR|+|Liquids (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqbio",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp), "FE|Transport|Freight|ESR|Liquids|+|Biomass (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqfos",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp), "FE|Transport|Freight|ESR|Liquids|+|Fossil (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqsyn",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp), "FE|Transport|Freight|ESR|Liquids|+|Hydrogen (EJ/yr)"),

       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),emi_sectors="trans", all_emiMkt="other")  ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp),                    "FE|Transport|Freight|Outside ETS and ESR|+|Liquids (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqbio",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp), "FE|Transport|Freight|Outside ETS and ESR|Liquids|+|Biomass (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqfos",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp), "FE|Transport|Freight|Outside ETS and ESR|Liquids|+|Fossil (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1=c("fedie"),all_enty="seliqsyn",emi_sectors="trans", all_emiMkt="other") ,dim=3,na.rm=T))*(1-p35_pass_FE_share_transp), "FE|Transport|Freight|Outside ETS and ESR|Liquids|+|Hydrogen (EJ/yr)"),

       # Passengers Gases
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|Transport|Pass|+|Gases (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segabio",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|Pass|Gases|+|Biomass (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segafos",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|Pass|Gases|+|Fossil (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segasyn",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|Pass|Gases|+|Hydrogen (EJ/yr)"),

       setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),                    "FE|Transport|Pass|ESR|+|Gases (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segabio",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|Pass|ESR|Gases|+|Biomass (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segafos",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|Pass|ESR|Gases|+|Fossil (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="fegat",all_enty="segasyn",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)), "FE|Transport|Pass|ESR|Gases|+|Hydrogen (EJ/yr)"),

       # Passengers Electricity
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="feelt",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|Transport|Pass|+|Electricity (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="feelt",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|Transport|Pass|ESR|+|Electricity (EJ/yr)"),

       setNames((dimSums(mselect(v_demFe,all_te="apTrnElT")  ,dim=3,na.rm=T)),                                                                 "FE|Transport|Pass|Electricity|Train (EJ/yr)" ),
       setNames((dimSums(mselect(v_demFe,all_te="apCarElT")  ,dim=3,na.rm=T)),                                                                 "FE|Transport|Pass|Electricity|Road (EJ/yr)"),

       # Passengers hydrogen
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2t",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|Transport|Pass|+|Hydrogen (EJ/yr)"),
       setNames((dimSums(mselect(vm_demFeSector,all_enty1="feh2t",emi_sectors="trans", all_emiMkt="ES")  ,dim=3,na.rm=T)),                     "FE|Transport|Pass|ESR|+|Hydrogen (EJ/yr)")
    )



    # transport technologies FE use
    out <- mbind(out,
                 setNames((dimSums(mselect(v_demFe,all_te="apTrnElT")  ,dim=3,na.rm=T)),                          "FE|Transport|Pass|Train|Electricity (EJ/yr)" ),
                 setNames((dimSums(mselect(v_demFe,all_te=c("apCarPeT","apCarH2T","apCarElT"))  ,dim=3,na.rm=T)), "FE|Transport|Pass|Road|LDV (EJ/yr)"),
                 setNames((dimSums(mselect(v_demFe,all_te="apCarH2T")  ,dim=3,na.rm=T)),                          "FE|Transport|Pass|Road|LDV|Hydrogen (EJ/yr)"),
                 setNames((dimSums(mselect(v_demFe,all_te="apCarPeT")  ,dim=3,na.rm=T)),                          "FE|Transport|Pass|Road|LDV|Liquids (EJ/yr)"),
                 setNames((dimSums(mselect(v_demFe,all_te="apCarElT")  ,dim=3,na.rm=T)),                          "FE|Transport|Pass|Road|LDV|Electricity (EJ/yr)")
    )

    # out <- mbind(out,
    #              setNames((dimSums(mselect(v_demFe,all_te=c("apCarDiT","apcarDiEffT","apcarDiEffH2T")),dim=3,na.rm=T)), "FE|Transport|non-LDV (EJ/yr)"),
    #              setNames((dimSums(mselect(v_demFe,all_te="apCarDiT")  ,dim=3,na.rm=T)),                                "FE|Transport|non-LDV|apCarDiT (EJ/yr)"),
    #              setNames((dimSums(mselect(v_demFe,all_te="apcarDiEffT")  ,dim=3,na.rm=T)),                             "FE|Transport|non-LDV|apcarDiEffT (EJ/yr)"),
    #              setNames((dimSums(mselect(v_demFe,all_te="apcarDiEffH2T")  ,dim=3,na.rm=T)),                           "FE|Transport|non-LDV|apcarDiEffH2T (EJ/yr)")
    #              )

    out <- mbind(out,
      # Passengers
      setNames(out[,,"FE|Transport|Pass|+|Liquids (EJ/yr)"]       + out[,,"FE|Transport|Pass|+|Gases (EJ/yr)"]     + out[,,"FE|Transport|Pass|+|Electricity (EJ/yr)"]     + out[,,"FE|Transport|Pass|+|Hydrogen (EJ/yr)"]     , "FE|Transport|Pass (EJ/yr)"),
      setNames(out[,,"FE|Transport|Pass|ESR|+|Liquids (EJ/yr)"]   + out[,,"FE|Transport|Pass|ESR|+|Gases (EJ/yr)"] + out[,,"FE|Transport|Pass|ESR|+|Electricity (EJ/yr)"] + out[,,"FE|Transport|Pass|ESR|+|Hydrogen (EJ/yr)"] , "FE|Transport|Pass|++|ESR (EJ/yr)"),
      setNames(out[,,"FE|Transport|Pass|Outside ETS and ESR|+|Liquids (EJ/yr)"] , "FE|Transport|Pass|++|Outside ETS and ESR (EJ/yr)"),

      # Freight
      setNames(out[,,"FE|Transport|Freight|+|Liquids (EJ/yr)"]       , "FE|Transport|Freight (EJ/yr)"),
      setNames(out[,,"FE|Transport|Freight|ESR|+|Liquids (EJ/yr)"]   , "FE|Transport|Freight|++|ESR (EJ/yr)"),
      setNames(out[,,"FE|Transport|Freight|Outside ETS and ESR|+|Liquids (EJ/yr)"] , "FE|Transport|Freight|++|Outside ETS and ESR (EJ/yr)")
    )

    # BUNKERS

    ## ESR variables correspond to transport without bunkers by definition
    pass_var_without_Bunkers <- mbind(
      lapply(
        getNames(out)[grep("FE\\|Transport\\|Pass\\|ESR", getNames(out))],
        function(x) {
          setNames(out[,,x],gsub("FE\\|Transport\\|Pass\\|ESR","FE\\|Transport\\|Pass\\|w/o Bunkers",x))
        }
      )
    )
    freight_var_without_Bunkers <- mbind(
      lapply(
        getNames(out)[grep("FE\\|Transport\\|Freight\\|ESR", getNames(out))],
        function(x) {
          setNames(out[,,x],gsub("FE\\|Transport\\|Freight\\|ESR","FE\\|Transport\\|Freight\\|w/o Bunkers",x))
        }
      )
    )
    ## Outside ETS and ESR emission market variables correspond to transport with bunkers by definition
    pass_var_with_Bunkers <- mbind(
      lapply(
        getNames(out)[grep("FE\\|Transport\\|Pass\\|Outside ETS and ESR", getNames(out))],
        function(x) {
          setNames(out[,,x],gsub("FE\\|Transport\\|Pass\\|Outside ETS and ESR","FE\\|Transport\\|Pass\\|Bunkers",x))
        }
      )
    )
    freight_var_with_Bunkers <- mbind(
      lapply(
        getNames(out)[grep("FE\\|Transport\\|Freight\\|Outside ETS and ESR", getNames(out))],
        function(x) {
          setNames(out[,,x],gsub("FE\\|Transport\\|Freight\\|Outside ETS and ESR","FE\\|Transport\\|Freight\\|Bunkers",x))
        }
      )
    )
    out <- mbind(out,pass_var_without_Bunkers,freight_var_without_Bunkers,freight_var_with_Bunkers)

    ##
    out <- mbind(out,
                 setNames(out[,,"FE|Transport|Pass|++|ESR (EJ/yr)"],   "FE|Transport|Pass|w/o Bunkers (EJ/yr)"),
                 setNames(out[,,"FE|Transport|Pass|++|Outside ETS and ESR (EJ/yr)"], "FE|Transport|Pass|Bunkers (EJ/yr)"),

                 setNames(out[,,"FE|Transport|Freight|++|ESR (EJ/yr)"],   "FE|Transport|Freight|w/o Bunkers (EJ/yr)"),
                 setNames(out[,,"FE|Transport|Freight|++|Outside ETS and ESR (EJ/yr)"], "FE|Transport|Freight|Bunkers (EJ/yr)")
    )

    # Energy Services
    p35_passLDV_ES_efficiency <- readGDX(gdx,"p35_passLDV_ES_efficiency", restore_zeros = FALSE)[,t,]
    p35_pass_nonLDV_ES_efficiency <- readGDX(gdx,"p35_pass_nonLDV_ES_efficiency", restore_zeros = FALSE)[,t,]
    p35_freight_ES_efficiency <- readGDX(gdx,"p35_freight_ES_efficiency", restore_zeros = FALSE)[,t,]

    out <- mbind(out,
      setNames(dimSums(vm_cesIO[,,"ueLDVt"],dim=3,na.rm=T) * p35_passLDV_ES_efficiency,                                "ES|Transport|Pass|Road|LDV (bn pkm/yr)"),
      setNames(p35_pass_nonLDV_ES_efficiency * p35_pass_FE_share_transp * dimSums(vm_cesIO[,,"ueHDVt"],dim=3,na.rm=T), "ES|Transport|Pass|non-LDV (bn pkm/yr)")
    )

    out <- mbind(out,
      setNames(p35_freight_ES_efficiency * (1-p35_pass_FE_share_transp) * dimSums(vm_cesIO[,,"ueHDVt"],dim=3,na.rm=T), "ES|Transport|Freight (bn tkm/yr)"),
      setNames(out[,,"ES|Transport|Pass|Road|LDV (bn pkm/yr)"] + out[,,"ES|Transport|Pass|non-LDV (bn pkm/yr)"],       "ES|Transport|Pass (bn pkm/yr)")
    )


    # ## Other variables (Kept temporarily for backwards compatibility) (need to double check these)
    # fe2ue <- readGDX(gdx,c("fe2ue", "fe2es"), format = "first_found")
    # LDV35 <- readGDX(gdx, "LDV35")
    #
    # v_demFe <- readGDX(gdx,name=c("v_demFe"),field="l",restore_zeros=FALSE,format="first_found")*TWa_2_EJ
    # v_demFe <- v_demFe[fe2ue]
    #
    # out <- mbind(out,
    #              setNames(dimSums(v_demFe[,,"apTrnElT"],dim=3), "FE|Transport|Pass|Train|Electricity (EJ/yr)" ),
    #              setNames(dimSums(v_demFe[,,LDV35],dim=3),      "FE|Transport|Pass|Road|LDV (EJ/yr)"),
    #              setNames(dimSums(v_demFe[,,"apCarH2T"],dim=3), "FE|Transport|Pass|Road|LDV|Hydrogen (EJ/yr)"),
    #              setNames(dimSums(v_demFe[,,"apCarPeT"],dim=3), "FE|Transport|Pass|Road|LDV|Liquids (EJ/yr)"),
    #              setNames(dimSums(v_demFe[,,"apCarElT"],dim=3), "FE|Transport|Pass|Road|LDV|Electricity (EJ/yr)")
    # )
    #
    # ## load conversion parameters
    # p35_passLDV_ES_efficiency <- readGDX(gdx,"p35_passLDV_ES_efficiency", restore_zeros = FALSE)
    # p35_pass_FE_share_transp <- readGDX(gdx,"p35_pass_FE_share_transp", restore_zeros = FALSE)
    # p35_freight_ES_efficiency <- readGDX(gdx,"p35_freight_ES_efficiency", restore_zeros = FALSE)
    # p35_pass_nonLDV_ES_efficiency <- readGDX(gdx,"p35_pass_nonLDV_ES_efficiency", restore_zeros = FALSE)
    #
    # #choose the CES entries names for transport
    # name_trsp=c("fepet","ueLDVt","fedie","ueHDVt","feelt","ueelTt","fepet_pass_sm","fedie_pass_sm","feelt_pass_sm","fedie_pass_lo","fedie_frgt_sm","feelt_frgt_sm","fedie_frgt_lo")
    # name_trsp=name_trsp[name_trsp%in%getNames(vm_cesIO)]
    #
    # name_trsp_HDV <- c("fedie","ueHDVt")
    # name_trsp_HDV=name_trsp_HDV[name_trsp_HDV%in%getNames(vm_cesIO)]
    # name_trsp_LDV <- c("fepet","ueLDVt")
    # name_trsp_LDV=name_trsp_LDV[name_trsp_LDV%in%getNames(vm_cesIO)]
    # name_trsp_ELT <- c("feelt","ueelTt")
    # name_trsp_ELT=name_trsp_ELT[name_trsp_ELT%in%getNames(vm_cesIO)]
    #
    # out <- mbind(out,
    #              setNames(out[,,"FE|Transport|w/o Bunkers (EJ/yr)"] - out[,,"FE|Transport|Pass|Road|LDV (EJ/yr)"], "FE|Transport|non-LDV (EJ/yr)"),
    #              setNames(dimSums(vm_cesIO[,,name_trsp_LDV],dim=3) * p35_passLDV_ES_efficiency, "ES|Transport|Pass|Road|LDV (bn pkm/yr)"),
    #              setNames(dimSums(vm_cesIO[,,name_trsp_LDV],dim=3), "UE|Transport|LDV (EJ/yr)"),
    #              setNames(dimSums(vm_cesIO[,,name_trsp_HDV],dim=3), "UE|Transport|HDV (EJ/yr)")
    # )
    #
    # out <- mbind(
    #   out,
    #   setNames(p35_pass_FE_share_transp * out[,, "FE|Transport|non-LDV (EJ/yr)"], "FE|Transport|Pass|non-LDV (EJ/yr)"),
    #   setNames(p35_pass_FE_share_transp * out[,, "UE|Transport|HDV (EJ/yr)"], "UE|Transport|Pass|non-LDV (EJ/yr)"),
    #   setNames((1- p35_pass_FE_share_transp) * out[,, "UE|Transport|HDV (EJ/yr)"], "UE|Transport|Freight (EJ/yr)")
    # )
    #
    # out <- mbind(out,
    #              setNames(p35_freight_ES_efficiency * out[,,"UE|Transport|Freight (EJ/yr)"],"ES|Transport|Freight (bn tkm/yr)"),
    #              setNames(p35_pass_nonLDV_ES_efficiency * out[,,"UE|Transport|Pass|non-LDV (EJ/yr)"],"ES|Transport|Pass|non-LDV (bn pkm/yr)")
    # )
    #
    # out <- mbind(out,
    #              setNames(out[,,"ES|Transport|Pass|Road|LDV (bn pkm/yr)"] + out[,,"ES|Transport|Pass|non-LDV (bn pkm/yr)"],"ES|Transport|Pass (bn pkm/yr)")
    # )

  }

  if (tran_mod == "edge_esm") {
    ## define the set that contains fe2es for transport
    fe2es_dyn35 <- readGDX(gdx,c("fe2es_dyn35"), format = "first_found")

    vm_demFeForEs_trnsp = vm_demFeForEs[fe2es_dyn35]

    out <- mbind(out,
      setNames(dimSums(vm_demFeForEs_trnsp[,,"eselt_frgt_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Electricity (EJ/yr)"),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"eselt_pass_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Electricity (EJ/yr)"),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esdie_frgt_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Liquids (EJ/yr)"),
      setNames(dimSums(vm_demFeForEs_trnsp[,,c("esdie_pass_", "espet_pass_"),pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Liquids (EJ/yr)"),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esgat_frgt_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Gases (EJ/yr)"),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esgat_pass_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Gases (EJ/yr)"),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esh2t_pass_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Hydrogen (EJ/yr)"),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esh2t_frgt_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Hydrogen (EJ/yr)"),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"eselt_frgt_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Short-Medium distance|Electricity (EJ/yr)"),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"eselt_pass_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Short-Medium distance|Electricity (EJ/yr)"),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esdie_frgt_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Short-Medium distance|Diesel Liquids (EJ/yr)"),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esdie_pass_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Short-Medium distance|Diesel Liquids (EJ/yr)"),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"espet_pass_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Short-Medium distance|Petrol Liquids (EJ/yr)"),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esdie_frgt_lo",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Long distance|Diesel Liquids (EJ/yr)"),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esdie_pass_lo",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Long distance|Diesel Liquids (EJ/yr)"),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esgat_frgt_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Short-Medium distance|Gases (EJ/yr)"),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esgat_pass_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Short-Medium distance|Gases (EJ/yr)"),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esh2t_pass_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass|Short-Medium distance|Hydrogen (EJ/yr)"),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"esh2t_frgt_sm",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight|Short-Medium distance|Hydrogen (EJ/yr)"),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"_frgt_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Freight (EJ/yr)"),
      setNames(dimSums(vm_demFeForEs_trnsp[,,"_pass_",pmatch=TRUE],dim=3,na.rm=T),"FE|Transport|Pass (EJ/yr)"),
      setNames(dimSums(vm_cesIO[,,"entrp_frgt_",pmatch=TRUE],dim=3,na.rm=T)/TWa_2_EJ * 1e3, # remove EJ conversion factor, conv. trillion to billion tkm
               "ES|Transport|Freight (bn tkm/yr)"),
      setNames(dimSums(vm_cesIO[,,"entrp_pass_",pmatch=TRUE],dim=3,na.rm=T)/TWa_2_EJ * 1e3, # trillion to billion pkm
               "ES|Transport|Pass (bn pkm/yr)"),
      setNames(dimSums(vm_cesIO[,,"entrp_frgt_sm",pmatch=TRUE],dim=3,na.rm=T)/TWa_2_EJ * 1e3, # trillion to billion tkm
               "ES|Transport|Freight|Short-Medium distance (bn tkm/yr)"),
      setNames(dimSums(vm_cesIO[,,"entrp_pass_sm",pmatch=TRUE],dim=3,na.rm=T)/TWa_2_EJ * 1e3, # trillion to billion pkm
               "ES|Transport|Pass|Short-Medium distance (bn pkm/yr)"),
      setNames(dimSums(vm_cesIO[,,"entrp_frgt_lo",pmatch=TRUE],dim=3,na.rm=T)/TWa_2_EJ * 1e3, # trillion to billion tkm
               "ES|Transport|Freight|Long distance (bn tkm/yr)"),
      setNames(dimSums(vm_cesIO[,,"entrp_pass_lo",pmatch=TRUE],dim=3,na.rm=T)/TWa_2_EJ * 1e3, # trillion to billion pkm
               "ES|Transport|Pass|Long distance (bn pkm/yr)"))


    # calculate total diesel and petrol liquids across all modes, needed in reportPrices
    out <- mbind(out,
                 setNames(out[,,"FE|Transport|Pass|Short-Medium distance|Diesel Liquids (EJ/yr)"]+
                          out[,,"FE|Transport|Pass|Long distance|Diesel Liquids (EJ/yr)"] +
                          out[,,"FE|Transport|Freight|Short-Medium distance|Diesel Liquids (EJ/yr)"] +
                          out[,,"FE|Transport|Freight|Long distance|Diesel Liquids (EJ/yr)"],
                          "FE|Transport|Diesel Liquids (EJ/yr)"))

  }


  #--- CDR ---

  if(cdr_mod != "off"){
    vm_otherFEdemand  <- readGDX(gdx,name=c("vm_otherFEdemand"),field="l",format="first_found")[,t,]*TWa_2_EJ

    s33_rockgrind_fedem <- readGDX(gdx,"s33_rockgrind_fedem", react = "silent")
    if (is.null(s33_rockgrind_fedem)){
      s33_rockgrind_fedem  <- new.magpie("GLO",NULL,fill=0)
    }
    v33_grindrock_onfield  <- readGDX(gdx,name=c("v33_grindrock_onfield"),field="l",format="first_found",react = "silent")[,t,]
    if (is.null(v33_grindrock_onfield)){
      v33_grindrock_onfield  <- new.magpie(getRegions(vm_otherFEdemand),getYears(vm_otherFEdemand),fill=0)
    }

    out <- mbind(out,
                 setNames(vm_otherFEdemand[,,"feh2s"],        "FE|CDR|DAC|+|Hydrogen (EJ/yr)"),
                 setNames(vm_otherFEdemand[,,"fegas"],        "FE|CDR|DAC|+|Gases (EJ/yr)"),
                 setNames(vm_otherFEdemand[,,"fehes"],        "FE|CDR|DAC|+|Heat (EJ/yr)"),
                 setNames(vm_otherFEdemand[,,"fedie"],        "FE|CDR|EW|+|Diesel (EJ/yr)"),
                 setNames(s33_rockgrind_fedem*dimSums(v33_grindrock_onfield[,,],dim=3,na.rm=T),        "FE|CDR|EW|+|Electricity (EJ/yr)")
    )
    out <- mbind(out,
                 setNames(out[,,"FE|CDR|+|Electricity (EJ/yr)"] - out[,,"FE|CDR|EW|+|Electricity (EJ/yr)"], "FE|CDR|DAC|+|Electricity (EJ/yr)")
    )
    out <- mbind(out,
                 setNames(out[,,"FE|CDR|DAC|+|Hydrogen (EJ/yr)"] + out[,,"FE|CDR|DAC|+|Gases (EJ/yr)"] + out[,,"FE|CDR|DAC|+|Electricity (EJ/yr)"] + out[,,"FE|CDR|DAC|+|Heat (EJ/yr)"], "FE|CDR|++|DAC (EJ/yr)"),
                 setNames(out[,,"FE|CDR|EW|+|Diesel (EJ/yr)"] + out[,,"FE|CDR|EW|+|Electricity (EJ/yr)"], "FE|CDR|++|EW (EJ/yr)")
    )
  }

  #--- Additional Variables

  out <- mbind(out,
    setNames(out[,,"FE (EJ/yr)"], "FE|Gross with CDR (EJ/yr)"),
    setNames(out[,,"FE (EJ/yr)"] - out[,,"FE|++|CDR (EJ/yr)"], "FE|Net without CDR (EJ/yr)")
  )

  # Add fuel computation
  out = mbind(out,
    setNames(out[,,"FE|++|Buildings (EJ/yr)"] - out[,,"FE|Buildings|+|Electricity (EJ/yr)"] - out[,,"FE|Buildings|+|Heat (EJ/yr)"], "FE|Buildings|Fuels (EJ/yr)"),
    setNames(out[,,"FE|++|Industry (EJ/yr)"]  - out[,,"FE|Industry|+|Electricity (EJ/yr)"]  - out[,,"FE|Industry|+|Heat (EJ/yr)"] , "FE|Industry|Fuels (EJ/yr)"),
    setNames(out[,,"FE|++|Transport (EJ/yr)"] - out[,,"FE|Transport|+|Electricity (EJ/yr)"]                                       , "FE|Transport|Fuels (EJ/yr)"),
    setNames(out[,,"FE (EJ/yr)"]             - out[,,"FE|+|Electricity (EJ/yr)"]           - out[,,"FE|+|Heat (EJ/yr)"]          , "FE|Fuels (EJ/yr)")
  )



  # variables required by the exogains code
  # Disaggregate solids between coal, modern biomass and traditional biomass
  out <-  mbind(out,  setNames(asS4(pmin(out[,,"FE|Solids|Biomass|+|Traditional (EJ/yr)"],out[,,"FE|Buildings|+|Solids (EJ/yr)"]))         ,"FE|Buildings|Solids|Biomass|Traditional (EJ/yr)"))
  out <-  mbind(out,  setNames(out[,,"FE|Solids|Biomass|+|Traditional (EJ/yr)"] - out[,,"FE|Buildings|Solids|Biomass|Traditional (EJ/yr)"] , "FE|Industry|Solids|Biomass|Traditional (EJ/yr)" ))

  share_sol_noTrad_buil = (out[,,"FE|Buildings|+|Solids (EJ/yr)"] - out[,,"FE|Buildings|Solids|Biomass|Traditional (EJ/yr)"]) / (out[,,"FE|+|Solids (EJ/yr)"] - out[,,"FE|Solids|Biomass|+|Traditional (EJ/yr)"] )
  share_sol_noTrad_indu = (out[,, "FE|Industry|+|Solids (EJ/yr)"] - out[,, "FE|Industry|Solids|Biomass|Traditional (EJ/yr)"]) / (out[,,"FE|+|Solids (EJ/yr)"] - out[,,"FE|Solids|Biomass|+|Traditional (EJ/yr)"] )

  out <- mbind(out,
    setNames(out[,,"FE|Solids|Biomass|+|Modern (EJ/yr)"] * share_sol_noTrad_buil, "FE|Buildings|Solids|Biomass|Modern (EJ/yr)"),
    setNames(out[,,"FE|Solids|Fossil|+|Coal (EJ/yr)"]    * share_sol_noTrad_buil, "FE|Buildings|Solids|Coal (EJ/yr)"),
    setNames(out[,,"FE|Solids|Biomass|+|Modern (EJ/yr)"] * share_sol_noTrad_indu,  "FE|Industry|Solids|Biomass|Modern (EJ/yr)"),
    setNames(out[,,"FE|Solids|Fossil|+|Coal (EJ/yr)"]    * share_sol_noTrad_indu,  "FE|Industry|Solids|Coal (EJ/yr)")
  )


  ### temporary (!) industry non-energy use reporting
  # note: only for REMIND-EU SSP2

  if ("DEU" %in% getRegions(vm_prodFe) & indu_mod == 'subsectors') {

      # some initializations required for building library with dplyr operations below
      encar <- data <- value <- value_subsectors <- SSP <- Value_NonEn <- encar <- region <- period <- NULL



      # read in FE industry non-energy use trajectories from industry subsectors run
      df.fe_nechem <- read.csv(system.file("extdata","pm_fe_nechem.cs4r",package = "remind2"),
                               sep = ",", skip = 4, header = F)

      # df.fe_nechem <- read.csv("./inst/extdata/pm_fe_nechem.cs4r",
      #                          sep = ",", skip = 4, header = F)

      colnames(df.fe_nechem) <- c("period", "region","SSP","encar","value_subsectors")
      vars.nechem <- c("FE|Industry|+|Liquids (EJ/yr)",
                       "FE|Industry|+|Gases (EJ/yr)",
                       "FE|Industry|+|Solids (EJ/yr)")

      map.vars.nechem <- c("FE|Industry|+|Liquids (EJ/yr)" = "fehoi_nechem",
                           "FE|Industry|+|Gases (EJ/yr)" = "fegai_nechem",
                           "FE|Industry|+|Solids (EJ/yr)" = "fesoi_nechem")

      map.nonen.vars <- c("fehoi_nechem" = "FE|Non-energy Use|Industry|+|Liquids (EJ/yr)",
                          "fegai_nechem" = "FE|Non-energy Use|Industry|+|Gases (EJ/yr)",
                          "fesoi_nechem" = "FE|Non-energy Use|Industry|+|Solids (EJ/yr)")


      # non-energy use of solids/liquids/gases: min(fehoi,fehoi_nechem),
      # where fehoi would be the liquids of the current run and
      # fehoi_nechem the non-energy use liquids of the reference industry subsectors run
      df.out.nechem <- suppressWarnings(as.quitte(out[,,vars.nechem])) %>%
                        rename( encar = data) %>%
                        # join current FE|Industry|Liquids etc. with non-energy use subsectors data
                        revalue.levels(encar = map.vars.nechem) %>%
                        left_join(df.fe_nechem) %>%
                        mutate( Value_NonEn = ifelse(value >= value_subsectors, value_subsectors, value)) %>%
                        filter( SSP == "SSP2") %>%
                        # map to non-energy use variable names
                        revalue.levels(encar= map.nonen.vars) %>%
                        select(region, period, encar, Value_NonEn)

      out.nechem <- suppressWarnings(as.magpie(df.out.nechem, spatial=1, temporal=2, datacol=4))
      out.nechem <- out.nechem[getRegions(out), getYears(out),]

      # set NAs to zero
      out.nechem[is.na( out.nechem)] <- 0


      # bind FE non-energy use to output object
      out <- mbind(out, out.nechem)



      # add further FE variables needed in ARIADNE
      out <- mbind(out,
                    setNames(out[,,"FE|Non-energy Use|Industry|+|Liquids (EJ/yr)"]
                              + out[,,"FE|Non-energy Use|Industry|+|Gases (EJ/yr)"]
                              + out[,,"FE|Non-energy Use|Industry|+|Solids (EJ/yr)"],
                             "FE|Non-energy Use|Industry (EJ/yr)"))

      out <- mbind(out,
                 setNames(out[,,"FE (EJ/yr)"]
                          - out[,,"FE|Transport|Bunkers (EJ/yr)"]
                          - out[,,"FE|Non-energy Use|Industry (EJ/yr)"],
                          "FE|w/o Non-energy Use w/o Bunkers (EJ/yr)"),
                 setNames(out[,,"FE|++|Industry (EJ/yr)"]
                          - out[,,"FE|Non-energy Use|Industry (EJ/yr)"],
                          "FE|w/o Non-energy Use|Industry (EJ/yr)"),
                 setNames(out[,,"FE|Industry|+|Liquids (EJ/yr)"]
                          - out[,,"FE|Non-energy Use|Industry|+|Liquids (EJ/yr)"],
                          "FE|w/o Non-energy Use|Industry|Liquids (EJ/yr)"),
                 setNames(out[,,"FE|Industry|+|Gases (EJ/yr)"]
                          - out[,,"FE|Non-energy Use|Industry|+|Gases (EJ/yr)"],
                          "FE|w/o Non-energy Use|Industry|Gases (EJ/yr)"),
                 setNames(out[,,"FE|Industry|+|Solids (EJ/yr)"]
                          - out[,,"FE|Non-energy Use|Industry|+|Solids (EJ/yr)"],
                          "FE|w/o Non-energy Use|Industry|Solids (EJ/yr)") )


      # energy carrier split in FE energy use variables
      out <- mbind(out,
                   # FE industry (without feedstocks) liquids: from fossils, biomass, hydrogen
                   setNames(out[,,"FE|w/o Non-energy Use|Industry|Liquids (EJ/yr)"] *
                              out[,,"FE|Industry|Liquids|+|Hydrogen (EJ/yr)"] /
                              out[,,"FE|Industry|+|Liquids (EJ/yr)"],
                            "FE|w/o Non-energy Use|Industry|Liquids|+|Hydrogen (EJ/yr)"),
                   setNames(out[,,"FE|w/o Non-energy Use|Industry|Liquids (EJ/yr)"] *
                              out[,,"FE|Industry|Liquids|+|Biomass (EJ/yr)"] /
                              out[,,"FE|Industry|+|Liquids (EJ/yr)"],
                            "FE|w/o Non-energy Use|Industry|Liquids|+|Biomass (EJ/yr)"),
                   setNames(out[,,"FE|w/o Non-energy Use|Industry|Liquids (EJ/yr)"] *
                              out[,,"FE|Industry|Liquids|+|Fossil (EJ/yr)"] /
                              out[,,"FE|Industry|+|Liquids (EJ/yr)"],
                            "FE|w/o Non-energy Use|Industry|Liquids|+|Fossil (EJ/yr)"),
                   # FE industry (without feedstocks) gases: from fossils, biomass, hydrogen
                   setNames(out[,,"FE|w/o Non-energy Use|Industry|Gases (EJ/yr)"] *
                              out[,,"FE|Industry|Gases|+|Hydrogen (EJ/yr)"] /
                              out[,,"FE|Industry|+|Gases (EJ/yr)"],
                            "FE|w/o Non-energy Use|Industry|Gases|+|Hydrogen (EJ/yr)"),
                   setNames(out[,,"FE|w/o Non-energy Use|Industry|Gases (EJ/yr)"] *
                              out[,,"FE|Industry|Gases|+|Biomass (EJ/yr)"] /
                              out[,,"FE|Industry|+|Gases (EJ/yr)"],
                            "FE|w/o Non-energy Use|Industry|Gases|+|Biomass (EJ/yr)"),
                   setNames(out[,,"FE|w/o Non-energy Use|Industry|Gases (EJ/yr)"] *
                              out[,,"FE|Industry|Gases|+|Fossil (EJ/yr)"] /
                              out[,,"FE|Industry|+|Gases (EJ/yr)"],
                            "FE|w/o Non-energy Use|Industry|Gases|+|Fossil (EJ/yr)"))


      # energy carrier split in FE non-energy use variables
      out <- mbind(out,
                  # split of non-energy use variables to fossil, bio, synfuels
                  # liquids
                   setNames(out[,,"FE|Non-energy Use|Industry|+|Liquids (EJ/yr)"] *
                              out[,,"FE|Industry|Liquids|+|Hydrogen (EJ/yr)"] /
                              out[,,"FE|Industry|+|Liquids (EJ/yr)"],
                            "FE|Non-energy Use|Industry|Liquids|+|Hydrogen (EJ/yr)"),
                  setNames(out[,,"FE|Non-energy Use|Industry|+|Liquids (EJ/yr)"] *
                             out[,,"FE|Industry|Liquids|+|Biomass (EJ/yr)"] /
                             out[,,"FE|Industry|+|Liquids (EJ/yr)"],
                           "FE|Non-energy Use|Industry|Liquids|+|Biomass (EJ/yr)"),
                  setNames(out[,,"FE|Non-energy Use|Industry|+|Liquids (EJ/yr)"] *
                             out[,,"FE|Industry|Liquids|+|Fossil (EJ/yr)"] /
                             out[,,"FE|Industry|+|Liquids (EJ/yr)"],
                           "FE|Non-energy Use|Industry|Liquids|+|Fossil (EJ/yr)"),

                  # gases
                  setNames(out[,,"FE|Non-energy Use|Industry|+|Gases (EJ/yr)"] *
                             out[,,"FE|Industry|Gases|+|Hydrogen (EJ/yr)"] /
                             out[,,"FE|Industry|+|Gases (EJ/yr)"],
                           "FE|Non-energy Use|Industry|Gases|+|Hydrogen (EJ/yr)"),
                  setNames(out[,,"FE|Non-energy Use|Industry|+|Gases (EJ/yr)"] *
                             out[,,"FE|Industry|Gases|+|Biomass (EJ/yr)"] /
                             out[,,"FE|Industry|+|Gases (EJ/yr)"],
                           "FE|Non-energy Use|Industry|Gases|+|Biomass (EJ/yr)"),
                  setNames(out[,,"FE|Non-energy Use|Industry|+|Gases (EJ/yr)"] *
                             out[,,"FE|Industry|Gases|+|Fossil (EJ/yr)"] /
                             out[,,"FE|Industry|+|Gases (EJ/yr)"],
                           "FE|Non-energy Use|Industry|Gases|+|Fossil (EJ/yr)"),

                  # solids
                  setNames(out[,,"FE|Non-energy Use|Industry|+|Solids (EJ/yr)"] *
                             out[,,"FE|Industry|Solids|+|Biomass (EJ/yr)"] /
                             out[,,"FE|Industry|+|Solids (EJ/yr)"],
                           "FE|Non-energy Use|Industry|Solids|+|Biomass (EJ/yr)"),
                  setNames(out[,,"FE|Non-energy Use|Industry|+|Solids (EJ/yr)"] *
                             out[,,"FE|Industry|Solids|+|Fossil (EJ/yr)"] /
                             out[,,"FE|Industry|+|Solids (EJ/yr)"],
                           "FE|Non-energy Use|Industry|Solids|+|Fossil (EJ/yr)"))


      # total FE variables per energy carrier without bunkers and without non-energy use

      out <- mbind(out,
                   # liquids
                   setNames(out[,,"FE|+|Liquids (EJ/yr)"] -
                              out[,,"FE|Transport|Bunkers|+|Liquids (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|+|Liquids (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Liquids (EJ/yr)"),
                   # biomass liquids
                   setNames(out[,,"FE|Liquids|+|Biomass (EJ/yr)"] -
                              out[,,"FE|Transport|Bunkers|Liquids|+|Biomass (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|Liquids|+|Biomass (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Liquids|Biomass (EJ/yr)"),
                   # fossil liquids
                   setNames(out[,,"FE|Liquids|+|Fossil (EJ/yr)"] -
                              out[,,"FE|Transport|Bunkers|Liquids|+|Fossil (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|Liquids|+|Fossil (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Liquids|Fossil (EJ/yr)"),
                   # synthetic liquids
                   setNames(out[,,"FE|Liquids|+|Hydrogen (EJ/yr)"] -
                              out[,,"FE|Transport|Bunkers|Liquids|+|Hydrogen (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|Liquids|+|Hydrogen (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Liquids|Hydrogen (EJ/yr)"),

                   # gases
                   setNames(out[,,"FE|+|Gases (EJ/yr)"] -
                              out[,,"FE|Transport|Bunkers|+|Gases (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|+|Gases (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Gases (EJ/yr)"),
                   # biomass Gases
                   setNames(out[,,"FE|Gases|+|Biomass (EJ/yr)"] -
                              out[,,"FE|Transport|Bunkers|Gases|+|Biomass (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|Gases|+|Biomass (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Gases|Biomass (EJ/yr)"),
                   # fossil Gases
                   setNames(out[,,"FE|Gases|+|Fossil (EJ/yr)"] -
                              out[,,"FE|Transport|Bunkers|Gases|+|Fossil (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|Gases|+|Fossil (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Gases|Fossil (EJ/yr)"),
                   # synthetic Gases
                   setNames(out[,,"FE|Gases|+|Hydrogen (EJ/yr)"] -
                              out[,,"FE|Transport|Bunkers|Gases|+|Hydrogen (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|Gases|+|Hydrogen (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Gases|Hydrogen (EJ/yr)"),

                   # solids
                   setNames(out[,,"FE|+|Solids (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|+|Solids (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Solids (EJ/yr)"),
                   # biomass Solids
                   setNames(out[,,"FE|Solids|+|Biomass (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|Solids|+|Biomass (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Solids|Biomass (EJ/yr)"),
                   # fossil Solids
                   setNames(out[,,"FE|Solids|+|Fossil (EJ/yr)"] -
                              out[,,"FE|Non-energy Use|Industry|Solids|+|Fossil (EJ/yr)"],
                            "FE|w/o Bunkers|w/o Non-energy Use|Solids|Fossil (EJ/yr)"))

  } else {
    # TODO: correct once feedstocks are calculated within the model
    # The variable FE|w/o Non-energy Use|Industry currently contains Non-Energy Use. Non-Energy Use should be subtracted from this variable as soon as feedstocks are calculated within the model.")
    out <- mbind(
      out,
      setNames(out[, , "FE|++|Industry (EJ/yr)"], "FE|w/o Non-energy Use|Industry (EJ/yr)"),
      setNames(out[, , "FE|Industry|+|Solids (EJ/yr)"], "FE|w/o Non-energy Use|Industry|Solids (EJ/yr)"),
      setNames(out[, , "FE|Industry|+|Gases (EJ/yr)"], "FE|w/o Non-energy Use|Industry|Gases (EJ/yr)"),
      setNames(out[, , "FE|Industry|+|Liquids (EJ/yr)"], "FE|w/o Non-energy Use|Industry|Liquids (EJ/yr)")
    )

    out <- add_columns(out, addnm = "FE|Non-energy Use|Industry (EJ/yr)", dim = 3.1, fill = 0)
  }

  # add global values
  out <- mbind(out,dimSums(out,dim=1))
  # add other region aggregations
  if (!is.null(regionSubsetList))
    out <- mbind(out, calc_regionSubset_sums(out, regionSubsetList))


  # add per sector electricity share (for SDG targets)
  out <- mbind(out,
    setNames(out[,,'FE|Buildings|+|Electricity (EJ/yr)'] / out[,,'FE|++|Buildings (EJ/yr)'] * 100, 'FE|Buildings|Electricity|Share (%)'),
    setNames(out[,,'FE|Industry|+|Electricity (EJ/yr)']  / out[,,'FE|++|Industry (EJ/yr)']  * 100, 'FE|Industry|Electricity|Share (%)'),
    setNames(out[,,'FE|Transport|+|Electricity (EJ/yr)'] / out[,,'FE|++|Transport (EJ/yr)'] * 100, 'FE|Transport|Electricity|Share (%)')
  )
  # add per sector fuel share
  out <- mbind(out,
   setNames(out[,,'FE|Buildings|Fuels (EJ/yr)'] / out[,,'FE|++|Buildings (EJ/yr)'] * 100, 'FE|Buildings|Fuels|Share (%)'),
   setNames(out[,,'FE|Industry|Fuels (EJ/yr)']  / out[,,'FE|++|Industry (EJ/yr)']  * 100, 'FE|Industry|Fuels|Share (%)'),
   setNames(out[,,'FE|Transport|Fuels (EJ/yr)'] / out[,,'FE|++|Transport (EJ/yr)'] * 100, 'FE|Transport|Fuels|Share (%)'),
   setNames(out[,,'FE|Fuels (EJ/yr)'] / out[,,'FE (EJ/yr)'] * 100, 'FE|Fuels|Share (%)')
  )

  ## specific energy use (FE per product/value added) ----
  if (all(indu_mod == 'subsectors',
          c('FE|Industry|+++|Cement (EJ/yr)',
            'Production|Industry|Cement (Mt/yr)',
            'FE|Industry|Steel|++|Primary (EJ/yr)',
            'Production|Industry|Steel|Primary (Mt/yr)',
            'FE|Industry|Steel|++|Secondary (EJ/yr)',
            'Production|Industry|Steel|Secondary (Mt/yr)',
            'FE|Industry|+++|Chemicals (EJ/yr)',
            'Value Added|Industry|Chemicals (billion US$2005/yr)',
            'FE|Industry|+++|Other Industry (EJ/yr)',
            'Value Added|Industry|Other Industry (billion US$2005/yr)') %>%
          `%in%`(getNames(out)))) {

    out <- mbind(
      out,
      setNames(
        # EJ/yr / Mt/yr * 1e12 MJ/EJ / (1e6 t/Mt) = MJ/t
        ( out[,,'FE|Industry|+++|Cement (EJ/yr)']
        / out[,,'Production|Industry|Cement (Mt/yr)']
        ) * 1e3,
        'FE|Industry|Specific Energy Consumption|Cement (GJ/t)'),

      setNames(
        # EJ/yr / Mt/yr * 1e12 MJ/EJ / (1e6 t/Mt) = MJ/t
        ( out[,,'FE|Industry|Steel|++|Primary (EJ/yr)']
        / out[,,'Production|Industry|Steel|Primary (Mt/yr)']
        ) * 1e3,
        'FE|Industry|Specific Energy Consumption|Primary Steel (GJ/t)'),

      setNames(
        # EJ/yr / Mt/yr * 1e12 MJ/EJ / (1e6 t/Mt) = MJ/t
        ( out[,,'FE|Industry|Steel|++|Secondary (EJ/yr)']
        / out[,,'Production|Industry|Steel|Secondary (Mt/yr)']
        ) * 1e3,
        'FE|Industry|Specific Energy Consumption|Secondary Steel (GJ/t)'),

      setNames(
        ( out[,,'FE|Industry|+++|Chemicals (EJ/yr)']
        / out[,,'Value Added|Industry|Chemicals (billion US$2005/yr)']
        ) * 1e3,
        'FE|Industry|Specific Energy Consumption|Chemicals (MJ/US$2005)'),

      setNames(
        ( out[,,'FE|Industry|+++|Other Industry (EJ/yr)']
        / out[,,'Value Added|Industry|Other Industry (billion US$2005/yr)']
        ) * 1e3,
        'FE|Industry|Specific Energy Consumption|Other Industry (MJ/US$2005)')
    )
  }

  return(out)
}
