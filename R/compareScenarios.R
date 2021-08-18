#' Read in mif and write comparison.pdf
#'
#' Read in all information from mif file and create
#' the comparison.pdf
#'
#' @param mif a path to one or more mif-files (might be created by confGDX2MIF)
#' @param hist a path to one mif-file containing historical data
#' @param y time span for the data in line plots, default: y=c(seq(2005,2060,5),seq(2070,2100,10))
#' @param y_hist time span for the historical data in the line plots, default: c(seq(1960,2015,1))
#' @param y_bar time slides for bar plots, default: y_bar=c(2010,2030,2050,2100)
#' @param reg region(s) in focus, reg="all_reg" shows all regions if the mifs contain different regions
#' @param mainReg region to be underlined
#' @param fileName name of the pdf, default = "CompareScenarios.pdf"
#' @param sr15marker_RCP if given, show the corresponding marker scenarios (SSP1-5) from the SR15 database in some plots. Requires the sr15data package.
#'
#' @author Lavinia Baumstark
#' @examples
#'
#' \dontrun{compareScenarios(mif_path)}
#'
#' @export
#' @importFrom magclass read.report mbind getRegions new.magpie getYears add_dimension setNames getNames<- time_interpolate
#' @importFrom lusweave swopen swlatex swfigure swclose
#' @importFrom mip mipLineHistorical mipBarYearData plotstyle
#' @importFrom luplot magpie2ggplot2
#' @importFrom ggplot2 facet_grid ggplot geom_col facet_wrap geom_point aes_ geom_ribbon guides guide_legend
#' @importFrom quitte as.quitte
#' @importFrom data.table as.data.table setnames := data.table
#' @importFrom utils installed.packages
#' @importFrom rmndt magpie2dt

compareScenarios <- function(mif, hist,
                             y=c(seq(2005,2060,5),seq(2070,2100,10)),
                             y_hist=c(seq(1960,2020,1), seq(2025,2100,5)),
                             y_bar=c(2010,2030,2050,2100),
                             reg=NULL, mainReg="GLO", fileName="CompareScenarios.pdf",
                             sr15marker_RCP=NULL) {
  
  lineplots_perCap <- function(data, vars, percap_factor, ylabstr,
                               global=FALSE, mainReg_plot=mainReg, per_gdp=FALSE, histdata_plot=NULL){

    ## models for historical data
    histmap = list(
      "Population"="WDI",
      "GDP|PPP"="James_IMF",
      "FE"="IEA",
      "FE|Transport"="IEA",
      "FE|Buildings"="IEA",
      "FE|Industry"="IEA"
    )

    items <- c(vars,
               "Population (million)",
               "GDP|PPP (billion US$2005/yr)")
    var <- as.data.table(as.quitte(data[,, items]))[, "unit" := NULL]

    plain_items <- gsub("(.+) \\(.+\\)", "\\1", items)

    if(!is.null(histdata_plot)){
      if(!all(items %in% getNames(histdata_plot, dim=3))){
        missing <- items[!items %in% getNames(histdata_plot, dim=3)]
        stop(paste("Items missing in historical dataset:",
                   paste(missing, collapse=", ")))
      }else if(!all(plain_items %in% names(histmap))){
        missing <- items[!plain_items %in% names(histmap)]
        stop(paste("No model defined for item in historical dataset:",
                   paste(missing, collapse=", ")))
      }else{
        hist_dt <- as.data.table(as.quitte(histdata_plot[,, items]))
        models <- unlist(histmap[plain_items])
        varhist <- hist_dt[
          model %in% models][ # IEA: energy, IMF: GDP, WDI: Population
          , c("unit", "model") := list(NULL, "REMIND")]
        var <- rbind(var, varhist)
      }
    }

    plain_vars <- gsub("(.+) \\(.+\\)", "\\1", vars)

    variable <- Population <- NULL
    region <- `GDP|PPP` <- model <- value <- scenario <- NULL

    hvar <- data.table::dcast(var, ... ~ variable)

    for(fe in plain_vars){
      hvar[, (fe) := get(fe)/Population*percap_factor]
    }

    if(per_gdp){
      hvar[, `GDP|PPP` := `GDP|PPP`/Population]
      var <- data.table::melt(hvar, id.vars=c("model", "scenario", "region", "period", "GDP|PPP"))
    }else{
      hvar[, `GDP|PPP` := NULL]
      var <- data.table::melt(hvar, id.vars=c("model", "scenario", "region", "period"))
    }

    var <- var[variable != "Population"][
    , variable := factor(variable, levels=plain_vars)]

    highlight_yrs <- c(2030, 2050, 2070)
    highlights <- var[scenario != "historical" & period %in% highlight_yrs]

    reg_cols <- plotstyle(as.character(unique(var$region)))
    reg_labels <- plotstyle(as.character(unique(var$region)), out="legend")

    var <- var[value > 0]
    if(per_gdp){
      if(global){
        p <- ggplot() +
          geom_line(data=var[scenario != "historical" & region == mainReg_plot],
                    aes(x=`GDP|PPP`, y=value, linetype=scenario)) +
          geom_point(data=var[scenario == "historical" & region == mainReg_plot],
                     aes(x=`GDP|PPP`, y=value), shape=4) +
          geom_point(data=highlights[region == mainReg_plot], aes(x=`GDP|PPP`, y=value), shape=1)
      }else{
        p <- ggplot() +
          geom_line(data=var[scenario != "historical" & region != mainReg_plot],
                    aes(x=`GDP|PPP`, y=value, linetype=scenario, color=region)) +
          geom_point(data=var[scenario == "historical" & region != mainReg_plot],
                     aes(x=`GDP|PPP`, y=value, color=region), shape=4) +
          geom_point(data=highlights[region != mainReg_plot], aes(x=`GDP|PPP`, y=value, color=region), shape=1) +
          scale_color_manual(values = reg_cols,  labels = reg_labels)
      }

      p <- p +
        facet_wrap(~ variable, scales="free_y") +
        ylab(ylabstr) +
        xlab("GDP PPP per Cap. (kUS$2005)") +
        expand_limits(y=0) +							
        theme_minimal()

    }else{
      if(global){
        p <- ggplot() +
          geom_line(data=var[scenario != "historical" & region == mainReg_plot],
                    aes(x=period, y=value, linetype=scenario)) +
          geom_point(data=var[scenario == "historical" & region == mainReg_plot],
                     aes(x=period, y=value), shape=4)
      }else{
        p <- ggplot() +
          geom_line(data=var[scenario != "historical" & region != mainReg_plot],
                    aes(x=period, y=value, linetype=scenario, color=region)) +
          geom_point(data=var[scenario == "historical" & region != mainReg_plot],
                     aes(x=period, y=value, color=region), shape=4) +
          scale_color_manual(values = reg_cols,  labels = reg_labels)
      }
      p <- p +
        facet_wrap(~ variable, scales="free_y") +
        xlab("year") +
        ylab(ylabstr) +
        expand_limits(y=0) +
        theme_minimal()

    }
    return(p)
  }


  ## ---- Read data ----
  
  ## read model results
  mifData <- lapply(mif, function(file){read.report(file,as.list=FALSE)})
  mifData0 <- mifData # make a copy
  
  ## check if there are repeated scenario names in mifs to be compared
  # get scenarios in all mif files
  mifScenarios <- c()
  for(i in 1:length(mifData0)){ 
      mifScenarios <-c(mifScenarios, magclass::getNames(mifData0[[i]],fulldim = TRUE)[["scenario"]])
  }
  
  # print a warning message on repeated names
  uniqueScenarioName = unique(unlist(mifScenarios, use.names = FALSE))
  if (length(uniqueScenarioName) < length(mifScenarios)){
    warning(paste0("There are repeated scenario names in comparison! Appending numerical value to repeated names"))
  }
  
  # append numerical value to repeated names
  mifScenarios_new <- c()
  for(i in 1:length(uniqueScenarioName)){
    unique_element = uniqueScenarioName[[i]]
    k = 1
    for(j in 1:length(mifScenarios)){
      if (mifScenarios[[j]] == unique_element){
        mifScenarios_new <-c(mifScenarios_new, paste0(unique_element, "_", k))
        k = k + 1
      }
    }
  }
  
  for(i in 1:length(mifScenarios)){
    magclass::getNames(mifData[[i]],dim=1) <- mifScenarios_new[[i]]
  }
  
  
  # get regions present all mif files
  for(i in 1:length(mifData)){ 
    if(i==1){
      mifRegions <- getRegions(mifData[[i]])
    } else {
      mifRegions <- intersect(mifRegions,getRegions(mifData[[i]]))
    }
  }
  
  ## define regions that should be in the comparison scenario
  if(is.null(reg) || reg == "all_reg"){
    reg <- mifRegions
  } else {
    reg <- intersect(reg,mifRegions)
  }
  
  ## return if no regions found
  if(length(reg) == 0){
    return(warning("There is no intersection between the regions in the mif files and the regions defined in the reg parameter.\nExiting without creating the scenario comparison pdf."))
  }
  
  ## return if the main region if is not available in the current selected regions
  if(!(mainReg %in% reg)){
    return(warning("The main region is not available in all mif files.\nExiting without creating the scenario comparison pdf."))
  }
  
  ## return if after removing the main region there is no remaining region to be plotted
  if(length(reg[!reg == mainReg]) == 0){
    return(warning("There is no remaining region besides the main region.\nExiting without creating the scenario comparison pdf."))
  }
  
  ## merging model results into a single magclass object
  data <- NULL
  for(i in 1:length(mifData)){ 
    data <- mbind(data,mifData[[i]][reg,y,])
  }
  
  ## delete "+" and "++" from variable names
  data <- deletePlus(data)

  ## read historical data
  histData <- read.report(hist,as.list=FALSE)
  y_hist <- intersect(y_hist, getYears(histData, as.integer=TRUE))
  if(all(getRegions(data) %in% getRegions(histData))) {
    histData = histData[getRegions(data),,]
    if ( any(grepl("EDGE_SSP2",getNames(histData)))){
      hist_edge = histData[,union(y_hist,y),]
      histData = histData[,,"EDGE_SSP2", invert = T]
    }
    histData <- histData[,y_hist,]
  } else {
    if(!is.null(reg)){
      ## fill up historical data for additional regions with 0
      dataReg    <- getRegions(data)[-which(getRegions(data) %in% getRegions(histData))]
      dummy_hist <- new.magpie(dataReg,getYears(histData),getNames(histData),fill=NA)
      histData       <- mbind(histData,dummy_hist)
      histData = histData[getRegions(data),,]
      if ( any(grepl("EDGE_SSP2",getNames(histData)))){
        ##EDGE projections are stored in histData. Retrieve them
        hist_edge = histData[,union(y_hist,y),]
        histData = histData[,,"EDGE_SSP2", invert = T]
      }
      histData <- histData[,y_hist,]


    } else {
      stop("historical data do not contain the choosen region")
    }
  }
  
  # copy of historic data replacing 0 with NA
  histData_NA <- histData
  histData_NA[histData_NA == 0] <- NA

  ## ---- Open output-pdf ----

  template <-  c("\\documentclass[a4paper,landscape,twocolumn]{article}",
                 "\\setlength{\\oddsidemargin}{-0.8in}",
                 "\\setlength{\\evensidemargin}{-0.5in}",
                 "\\setlength{\\topmargin}{-0.8in}",
                 "\\setlength{\\parindent}{0in}",
                 "\\setlength{\\headheight}{0in}",
                 "\\setlength{\\topskip}{0in}",
                 "\\setlength{\\headsep}{0in}",
                 "\\setlength{\\footskip}{0.2in}",
                 "\\setlength\\textheight{0.95\\paperheight}",
                 "\\setlength\\textwidth{0.95\\paperwidth}",
                 "\\setlength{\\parindent}{0in}",
                 "\\usepackage{float}",
                 "\\usepackage[bookmarksopenlevel=section,colorlinks=true,linkbordercolor={0.9882353 0.8352941 0.7098039}]{hyperref}",
                 "\\hypersetup{bookmarks=true,pdfauthor={GES group, PIK}}",
                 "\\usepackage{graphicx}",
                 "\\catcode`_=12",
                 "\\usepackage{Sweave}",
                 "\\begin{document}",
                 "<<echo=false>>=",
                 "options(width=110)",
                 "@")

  sw <- swopen(fileName,template = template)
  swlatex(sw,"\\tableofcontents\\newpage")
  
  ## empty page
  swlatex(sw,"\\newpage")
  swlatex(sw,"\\thispagestyle{empty}")
  swlatex(sw,"\\mbox{}")
  swlatex(sw,"\\newpage")
  
  ## ---- ++++ S U M M A R Y ++++ ----
  
  swlatex(sw,"\\section{Summary}")

  ## ---- GHG total ----

  swlatex(sw,"\\subsection{GHG Emissions}")
  
  tot <-"Emi|GHG (Mt CO2eq/yr)"
  items <- c("Emi|CO2|Energy (Mt CO2/yr)",
             "Emi|CO2|Industrial Processes (Mt CO2/yr)",
             "Emi|CO2|Land-Use Change (Mt CO2/yr)",
             "Emi|GHG|CH4 (Mt CO2eq/yr)",
             "Emi|GHG|N2O (Mt CO2eq/yr)",
             "Emi|GHG|F-Gases (Mt CO2eq/yr)",
             "Emi|CO2|non-BECCS CDR (Mt CO2/yr)")
  
  
  var <- data[,,intersect(items,getNames(data,dim=3))]
  p <- mipArea(var[mainReg,,],scales="free_y", total = F)
  p <- p + theme(legend.position="none") + 
           scale_y_continuous("Emi|GHG (Mt CO2eq/yr)") +
           geom_line(data=as.quitte(data[mainReg,,tot]), 
                     mapping=aes(period, value),
                               size=1.3)
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE]) +
        guides(fill=guide_legend(ncol=3))
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y", total = F)
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")
  
  
  ## ---- GHG by sector ----
  
  swlatex(sw,"\\subsection{GHG by sector (w/ gross emissions, excl. BECCS)}")
  
  tot <-"Emi|GHG (Mt CO2eq/yr)"
  items <- c("Emi|GHG|Gross|Energy|Supply|Electricity (Mt CO2eq/yr)",
             "Emi|GHG|Gross|Energy|Supply|Non-electric (Mt CO2eq/yr)",
             "Emi|GHG|Energy|Demand|Transport (Mt CO2eq/yr)",
             "Emi|GHG|Energy|Demand|Buildings (Mt CO2eq/yr)",
             "Emi|GHG|Gross|Energy|Demand|Industry (Mt CO2eq/yr)",
             "Emi|GHG|Industrial Processes (Mt CO2eq/yr)",
             "Emi|GHG|Agriculture (Mt CO2eq/yr)",
             "Emi|GHG|Land-Use Change (Mt CO2eq/yr)",
             "Emi|GHG|Waste (Mt CO2eq/yr)",
             "Emi|GHG|F-Gas (Mt CO2eq/yr)",
             "Emi|CO2|CDR|BECCS (Mt CO2/yr)",
             "Emi|CO2|CDR|DACCS (Mt CO2/yr)",
             "Emi|CO2|CDR|EW (Mt CO2/yr)")
  
  
  var <- data[,,intersect(items,getNames(data,dim=3))]
  p <- mipArea(var[mainReg,,],scales="free_y", total = F)
  p <- p + theme(legend.position="none") + 
    scale_y_continuous("Emi|GHG (Mt CO2eq/yr)") +
    geom_line(data=as.quitte(data[mainReg,,tot]), 
              mapping=aes(period, value),
              size=1.3)
  swfigure(sw,print,p,sw_option="height=3.5,width=7")
  
  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")
  
  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE]) +
    guides(fill=guide_legend(ncol=3))
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y", total = F)
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- CO2 by sector ----

  swlatex(sw,"\\subsection{CO2 by sector (w/ gross emissions, excl. BECCS)}")

  tot <-"Emi|CO2 (Mt CO2/yr)"
  items <- c(
             "Emi|CO2|Land-Use Change (Mt CO2/yr)",
             "Emi|CO2|Industrial Processes (Mt CO2/yr)",
             "Emi|CO2|Energy|Demand|Transport (Mt CO2/yr)",
             "Emi|CO2|Gross|Energy|Demand|Industry (Mt CO2/yr)",
             "Emi|CO2|Energy|Demand|Buildings (Mt CO2/yr)",
             "Emi|CO2|Gross|Energy|Supply|Non-electric (Mt CO2/yr)",
             "Emi|CO2|Gross|Energy|Supply|Electricity (Mt CO2/yr)",
             "Emi|CO2|CDR|BECCS (Mt CO2/yr)",
             "Emi|CO2|CDR|DACCS (Mt CO2/yr)",
             "Emi|CO2|CDR|EW (Mt CO2/yr)")
             
  
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y", total = F)
  p <- p + theme(legend.position="bottom") + 
    scale_y_continuous("Emi|CO2 (Mt CO2/yr)") +
    geom_line(data=as.quitte(data[mainReg,,tot]), 
              mapping=aes(period, value),
              size=1.3)
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE]) +
    guides(fill=guide_legend(ncol=3))
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],total=data[,,tot][mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- CO2 by sector cumulated ----

  tot <- "Emi|CO2|Cumulated (Mt CO2)"
  items <- c(
    "Emi|CO2|Cumulated|Land-Use Change (Mt CO2)",
    "Emi|CO2|Cumulated|Industrial Processes (Mt CO2)",
    "Emi|CO2|Cumulated|Energy|Demand|Transport (Mt CO2)",
    "Emi|CO2|Cumulated|Energy|Demand|Industry (Mt CO2)",
    "Emi|CO2|Cumulated|Energy|Demand|Buildings (Mt CO2)",
    "Emi|CO2|Cumulated|Gross|Energy|Supply|Non-electric (Mt CO2)",
    "Emi|CO2|Cumulated|Gross|Energy|Supply|Electricity (Mt CO2)",
    "Emi|CO2|Cumulated|CDR|BECCS (Mt CO2)",
    "Emi|CO2|Cumulated|CDR|DACCS (Mt CO2)",
    "Emi|CO2|Cumulated|CDR|EW (Mt CO2)")
  
  
  var <- data[,,intersect(items,getNames(data,dim=3))]
  
  
  p <- mipArea(var[mainReg,,],scales="free_y", total = F)
  p <- p + theme(legend.position="bottom") + 
    scale_y_continuous("Emi|CO2|Cumulated (Mt CO2/yr)") +
    geom_line(data=as.quitte(data[mainReg,,tot]), 
              mapping=aes(period, value),
              size=1.3)
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE]) +
    guides(fill=guide_legend(ncol=3))
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],total=data[,,tot][mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")
  

  ## ---- FE by sector ----

  swlatex(sw,"\\subsection{FE by sector}")

  items<- c("FE|CDR (EJ/yr)",
            "FE|Transport (EJ/yr)",
			#"FE|Transport|w/o Bunkers (EJ/yr)",
            "FE|Buildings (EJ/yr)",
            "FE|Industry (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- FE per capita by sector (time domain, area graph)----

  swlatex(sw,"\\subsection{FE per capita (by sector, time domain, area plot)}")

  items<- c(#"FE|CDR (EJ/yr)",
			"FE|Transport (EJ/yr)",
			#"FE|Transport|w/o Bunkers (EJ/yr)",
            "FE|Buildings (EJ/yr)",
            "FE|Industry (EJ/yr)")
  var <- data[,,intersect(items, getNames(data,dim=3))]/data[,, "Population", pmatch=T]*1e3

  p <- mipArea(var[mainReg,,], scales="free_y")
  p <- p + theme(legend.position="none") + ylab("FE p. Cap. (GJ/yr)")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none") + ylab("FE p. Cap. (GJ/yr)")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE]) + ylab("FE p. Cap. (GJ/yr)")
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y") + ylab("FE p. Cap. (GJ/yr)")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- FE per capita by sector (time domain, line graph)----

  swlatex(sw,"\\onecolumn")
  swlatex(sw,"\\subsection{FE per capita (by sector, time domain, line graph)}")

  items<- c(
    "FE (EJ/yr)",
	#"FE|CDR (EJ/yr)",
	"FE|Transport (EJ/yr)",
	#"FE|Transport|w/o Bunkers (EJ/yr)",
    "FE|Buildings (EJ/yr)",
    "FE|Industry (EJ/yr)")

  p <- lineplots_perCap(
    data=data,
    vars=items,
    percap_factor=1e3,
    ylabstr="FE per Cap. (GJ/yr)",
    global=T, histdata_plot=histData)

  if("sr15data" %in% rownames(installed.packages()) & is.character(sr15marker_RCP)){

    variable <- Population <- `FE|Transport` <- `FE|Buildings` <- `FE|Industry` <- NULL
    region <- period <- `GDP|PPP` <- model <- value <- scenario <- FE <- NULL

    ## get marker scenario data
    marker_items <- c(
      "Final Energy",
      paste0("Final Energy|", c("Industry", "Residential and Commercial", "Transportation")),
      "Population")
    sr15dt <- as.data.table(sr15data::sr15data)[
      region == "World"][
    , region := mainReg][
      data.table(period=y), on="period"]
    sr15scens <- data.table(scenario=paste0("SSP", 1:5, "-", sr15marker_RCP),
                            model=c("IMAGE 3.0.1",
                                    "MESSAGE-GLOBIOM 1.0",
                                    "AIM/CGE 2.0",
                                    "GCAM 4.2",
                                    "REMIND-MAgPIE 1.5"))
    markers <- sr15dt[sr15scens, on=c("model", "scenario")][
      variable %in% marker_items][
    , variable := gsub("Final Energy", "FE", variable)][
#  , variable := gsub("CDR", "CDR", variable)][															  
    , variable := gsub("Transportation", "Transport", variable)][
#  , variable := gsub("Transportation", "Transport|w/o Bunkers", variable)][															  
    , variable := gsub("Residential and Commercial", "Buildings", variable)][
    , scenario := gsub("Baseline", "Base", scenario)][, unit := NULL]

    markers_wide <- data.table::dcast(markers, ... ~ variable)

    markers_wide[, `:=`("FE|Transport" = `FE|Transport`/Population*1e3,
						#"FE|Transport|w/o Bunkers" = `FE|Transport|w/o Bunkers`/Population*1e3,							  
                        "FE|Buildings" = `FE|Buildings`/Population*1e3,
                        "FE|Industry" = `FE|Industry`/Population*1e3,
						#"FE|CDR" = `FE|Transport|CDR*1e3,	
                        "FE" = `FE`/Population*1e3), by=c("model")]

    markers <- data.table::melt(markers_wide, id.vars=c("model", "scenario", "region", "period"))[
                             variable != "Population"]

    p <- p + geom_line(data=markers, aes(x=period, y=value, group=scenario, color=scenario),
                       size=2, alpha=1/4)

  }

  swfigure(sw,print,p,sw_option="height=9,width=16")

  ## Second page, with color coded regions

  p <- lineplots_perCap(data, items, 1e3, "FE per Cap. (GJ/yr)", global = F, histdata_plot = histData)
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\twocolumn")


  ## ---- FE per capita by sector (GDP domain)----

  swlatex(sw,"\\onecolumn")
  swlatex(sw,"\\subsection{FE per capita (by sector, GDP)}")

  items<- c(#"FE|CDR (EJ/yr)" ,
			"FE|Transport (EJ/yr)",
			#"FE|Transport|w/o Bunkers (EJ/yr)"
            "FE|Buildings (EJ/yr)",
            "FE|Industry (EJ/yr)")

  p <- lineplots_perCap(data, items, 1e3, "FE per Cap. (GJ/yr)", global = T,
                        per_gdp = T, histdata_plot = histData)
  swfigure(sw,print,p,sw_option="height=9,width=16")

  p <- lineplots_perCap(data, items, 1e3, "FE per Cap. (GJ/yr)", per_gdp = T,
                        histdata_plot = histData)
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\twocolumn")


  ## ---- FE by carrier ----

  swlatex(sw,"\\subsection{FE by carrier}")

  items<- c("FE|Solids (EJ/yr)",
            "FE|Liquids (EJ/yr)",
            "FE|Gases (EJ/yr)",
            "FE|Heat (EJ/yr)",
            "FE|Hydrogen (EJ/yr)",
            "FE|Electricity (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- FE Industry by carrier ----

  swlatex(sw,"\\subsection{FE Industry by carrier}")

  items<- c("FE|Industry|Solids (EJ/yr)",
            "FE|Industry|Liquids (EJ/yr)",
            "FE|Industry|Gases (EJ/yr)",
            "FE|Industry|Heat (EJ/yr)",
            "FE|Industry|Hydrogen (EJ/yr)",
            "FE|Industry|Electricity (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- FE Buildings by carrier ----

  swlatex(sw,"\\subsection{FE Buildings by carrier}")

  items<- c("FE|Buildings|Solids (EJ/yr)",
            "FE|Buildings|Liquids (EJ/yr)",
            "FE|Buildings|Gases (EJ/yr)",
            "FE|Buildings|Heat (EJ/yr)",
            "FE|Buildings|Hydrogen (EJ/yr)",
            "FE|Buildings|Electricity (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- FE Transport by carrier ----

  swlatex(sw,"\\subsection{FE Transport by carrier}")
  items<- c (
    "FE|Transport|Electricity (EJ/yr)",
    "FE|Transport|Hydrogen (EJ/yr)",
    "FE|Transport|Liquids (EJ/yr)",
	#"FE|Transport|Liquids|w/o Bunkers (EJ/yr)",
	#"FE|Transport|Liquids|Bunkers (EJ/yr)",
    "FE|Transport|Gases (EJ/yr)"
  )

  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- FE CDR by carrier ----

  swlatex(sw,"\\subsection{FE CDR by carrier}")

  items<- c("FE|CDR|Liquids (EJ/yr)",
            "FE|CDR|Gases (EJ/yr)",
            "FE|CDR|Hydrogen (EJ/yr)",
            "FE|CDR|Electricity (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")


  ## ---- SE Electricity by carrier ----

  swlatex(sw,"\\subsection{SE Electricity by carrier}")

  items<- c ("SE|Electricity|Coal|w/ CCS (EJ/yr)",
             "SE|Electricity|Coal|w/o CCS (EJ/yr)",
             "SE|Electricity|Oil (EJ/yr)",
             "SE|Electricity|Gas|w/ CCS (EJ/yr)",
             "SE|Electricity|Gas|w/o CCS (EJ/yr)",
             "SE|Electricity|Geothermal (EJ/yr)",
             "SE|Electricity|Hydro (EJ/yr)",
             "SE|Electricity|Nuclear (EJ/yr)",
             "SE|Electricity|Biomass|w/ CCS (EJ/yr)",
             "SE|Electricity|Biomass|w/o CCS (EJ/yr)",
             "SE|Electricity|Solar|CSP (EJ/yr)",
             "SE|Electricity|Solar|PV (EJ/yr)",
             "SE|Electricity|Hydrogen (EJ/yr)",
             "SE|Electricity|Net Imports (EJ/yr)")

  if ("SE|Electricity|Wind|Offshore (EJ/yr)" %in% magclass::getNames(data, dim = 3)) {
    items <- append(items, c( "SE|Electricity|Wind|Onshore (EJ/yr)",
                              "SE|Electricity|Wind|Offshore (EJ/yr)"))
   } else {
     items <- append(items, c( "SE|Electricity|Wind (EJ/yr)"))
     # var[,, "SE|Electricity|Wind|Onshore (EJ/yr)"] <- var[,,"SE|Electricity|Wind (EJ/yr)"]
   }
  
  var <- data[,,intersect(items,getNames(data,dim=3))]

  # correct SE|Electricity|Hydrogen, current value is FE, SE can be calculated by estimating turbine efficiency
  var[,,"SE|Electricity|Hydrogen (EJ/yr)"] <- var[,,"SE|Electricity|Hydrogen (EJ/yr)"] / 0.4
  
  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  if ("SE|Electricity|Wind|Offshore (EJ/yr)" %in% magclass::getNames(data, dim = 3)) {
    
    swlatex(sw,"\\subsubsection{SE Wind Onshore}")
    var0 <- "SE|Electricity|Wind|Onshore (EJ/yr)"
    
    p <-mipLineHistorical(var[mainReg,,var0],
                          ylab=var0,scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(var[,,var0][mainReg,,,invert=TRUE],
                           ylab=var0,scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    swlatex(sw,"\\subsubsection{SE Wind Offshore}")
    var0 <- "SE|Electricity|Wind|Offshore (EJ/yr)"
    p <-mipLineHistorical(var[mainReg,,var0],
                          ylab=var0,scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(var[,,var0][mainReg,,,invert=TRUE],
                           ylab=var0,scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
  }
  ## ---- SE non-electric by carrier ----

  swlatex(sw,"\\subsection{SE non-electric by carrier}")
  ## XXX

  ## ---- PE by sector ----

  swlatex(sw,"\\subsection{PE by sector}")
  ## XXX

  ## ---- PE by carrier ----

  swlatex(sw,"\\subsection{PE by carrier}")

  items <-c("PE|Coal (EJ/yr)",
            "PE|Oil (EJ/yr)",
            "PE|Gas (EJ/yr)",
            "PE|Biomass (EJ/yr)",
            "PE|Nuclear (EJ/yr)",
            "PE|Solar (EJ/yr)",
            "PE|Wind (EJ/yr)",
            "PE|Hydro (EJ/yr)",
            "PE|Geothermal (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- CO2 Prices ----

  swlatex(sw,"\\subsection{CO2 Prices}")

  p <- mipLineHistorical(data[mainReg,,"Price|Carbon (US$2005/t CO2)"],x_hist=NULL,
                         ylab='Price|Carbon [US$2005/t CO2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=8,width=8")
  #p <- mipLineHistorical(data[mainReg,,"Price|Carbon (US$2005/t CO2)"],x_hist=NULL,
  #                       ylab='Price|Carbon_log [US$2005/t CO2]',ybreaks=c(20,30,40,50,60,75,100,200,500,1000,2000,3000),
  #                       ylim=c(0,3000),ylog=TRUE)
  #swfigure(sw,print,p,sw_option="height=4.5,width=7")
  p <- mipLineHistorical(data[,,"Price|Carbon (US$2005/t CO2)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Carbon [US$2005/t CO2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")

  p <- mipLineHistorical(
    data[,,"Price|Carbon (US$2005/t CO2)"][
      mainReg,,,invert=TRUE],
    ylab='Price|Carbon [US$2005/t CO2]',
    scales="free_y",
    plot.priority=c("x_hist","x","x_proj"),
    color.dim="region",
    facet.dim="scenario",
    facet.ncol=2) +
    theme(legend.position="right")
  swfigure(sw,print,p,sw_option="height=9,width=16")
  
  swlatex(sw,"\\twocolumn")
  p <- mipLineHistorical(data[mainReg,,"Price|Carbon|ETS (US$2005/t CO2)"],x_hist=NULL,
                         ylab='Price|Carbon|ETS [US$2005/t CO2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Carbon|ETS (US$2005/t CO2)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Carbon|ETS [US$2005/t CO2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\twocolumn")
  p <- mipLineHistorical(data[mainReg,,"Price|Carbon|ESR (US$2005/t CO2)"],x_hist=NULL,
                         ylab='Price|Carbon|ESR [US$2005/t CO2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Carbon|ESR (US$2005/t CO2)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Carbon|ESR [US$2005/t CO2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\twocolumn")

  if("Policy Cost|Consumption Loss (billion US$2005/yr)" %in% magclass::getNames(data,dim=3)) {
    ## ---- Policy Cost|Consumption Loss ----
    swlatex(sw,"\\subsection{Policy Costs}")

    p <- mipLineHistorical(
      data[mainReg,,"Policy Cost|Consumption Loss (billion US$2005/yr)"],
      x_hist=NULL,
      ylab='Policy Cost|Consumption Loss [billion US$2005/yr]',
      scales="free_y",
      plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")

    p <- mipLineHistorical(
      data[,,"Policy Cost|Consumption Loss (billion US$2005/yr)"][
        mainReg,,,invert=TRUE],
      x_hist=NULL,
      ylab='Policy Cost|Consumption Loss [billion US$2005/yr]',
      scales="free_y",
      plot.priority=c("x_hist","x","x_proj"),
      facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(
      data[mainReg,,"Policy Cost|Consumption Loss|Relative to Reference Consumption (percent)"],
      x_hist=NULL,
      ylab='Policy Cost|Consumption Loss|Relative to Reference Consumption [%]',
      scales="free_y",
      plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(
      data[,,"Policy Cost|Consumption Loss|Relative to Reference Consumption (percent)"][
        mainReg,,,invert=TRUE],
      x_hist=NULL,
      ylab='Policy Cost|Consumption Loss|Relative to Reference Consumption [%]',
      scales="free_y",
      plot.priority=c("x_hist","x","x_proj"),
      facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
  }
  
  if("Policy Cost|GDP Loss (billion US$2005/yr)" %in% magclass::getNames(data,dim=3)) {
    ## ---- Policy Cost|GDP Loss ----
    swlatex(sw,"\\subsection{Policy Costs}")
    
    p <- mipLineHistorical(
      data[mainReg,,"Policy Cost|GDP Loss (billion US$2005/yr)"],
      x_hist=NULL,
      ylab='Policy Cost|GDP Loss [billion US$2005/yr]',
      scales="free_y",
      plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    
    p <- mipLineHistorical(
      data[,,"Policy Cost|GDP Loss (billion US$2005/yr)"][
        mainReg,,,invert=TRUE],
      x_hist=NULL,
      ylab='Policy Cost|GDP Loss [billion US$2005/yr]',
      scales="free_y",
      plot.priority=c("x_hist","x","x_proj"),
      facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
  }

  ## ---- Mitigation Indicators of demand-side transformation in 2050 ----
  ##varis <- c("FE|Industry (EJ/yr)",
  ##           "FE|Buildings (EJ/yr)",
  ##           "FE|Transport (EJ/yr)",
  ##           "FE (EJ/yr)",
  ##           "FE|Industry|Fuels|Share (Percent)",
  ##           "FE|Buildings|Fuels|Share (Percent)",
  ##           "FE|Transport|Fuels|Share (Percent)",
  ##           "FE|Fuels|Share (Percent)",
  ##           "FE|Industry|Fossil Carbon Intensity of fuels (kg CO2/GJ)",
  ##           "FE|Buildings|Fossil Carbon Intensity of fuels (kg CO2/GJ)",
  ##           "FE|Transport|Fossil Carbon Intensity of fuels (kg CO2/GJ)",
  ##           "FE|Fossil Carbon Intensity of fuels (kg CO2/GJ)",
  ##           "Emi|CO2|Energy|Demand|Industry|Gross (Mt CO2/yr)",
  ##           "Emi|CO2|Buildings|Direct and Indirect (Mt CO2/yr)",
  ##           "Emi|CO2|Transport|Demand (Mt CO2/yr)",
  ##           "Emi|CO2|Fossil Fuels and Industry|Demand (Mt CO2/yr)")
  ##
  ##if (all(varis%in%getNames(data,dim=3))) {  # only plot if the variables exist in the data
  ##  swlatex(sw,"\\subsection{Mitigation Indicators of demand-side transformation in 2050}")
  ##
  ##  dataq50 <- as.quitte(data[mainReg,2050,intersect(varis,getNames(data,dim=3))]) # filter data for 2050 and GLO
  ##
  ##  # trick to make rows of the 4x4 grid have constant scaling in the sectoral plots (first 3 of each row)
  ##  varis <- unlist(strsplit(varis,split = " \\("))[seq(1,2*length(varis),2)] # remove units from object varis for matching with column "variable" in quitte object
  ##  dataq50$maxval <- dataq50$value
  ##  dataq50[which(dataq50$variable%in%varis[1:3]),"maxval"] <- max(dataq50[which(dataq50$variable%in%varis[1:3]),"value"])
  ##  dataq50[which(dataq50$variable%in%varis[5:8]),"maxval"] <- 100
  ##  dataq50[which(dataq50$variable%in%varis[9:11]),"maxval"] <- max(dataq50[which(dataq50$variable%in%varis[9:11]),"value"])
  ##  dataq50[which(dataq50$variable%in%varis[13:15]),"maxval"] <- max(dataq50[which(dataq50$variable%in%varis[13:15]),"value"])
  ##
  ##  # add units to variable names
  ##  levels(dataq50$variable)[1:4] <- paste0(levels(dataq50$variable)[1:4],"\n",levels(dataq50$unit)[[1]])
  ##  levels(dataq50$variable)[5:8] <- paste0(levels(dataq50$variable)[5:8],"\n",levels(dataq50$unit)[[2]])
  ##  levels(dataq50$variable)[9:12] <- paste0(levels(dataq50$variable)[9:12],"\n",levels(dataq50$unit)[[3]])
  ##  levels(dataq50$variable)[13:16] <- paste0(levels(dataq50$variable)[13:16],"\n",levels(dataq50$unit)[[4]])
  ##
  ##  # generate plot
  ##  p <- ggplot(dataq50, aes_(x = ~scenario, y = ~value))
  ##  p <- p + geom_col(aes_(fill=~scenario),width =0.5) +
  ##     facet_wrap("variable",scales = "free_y") +
  ##     geom_point(aes_(y=~maxval),alpha=0) # add invisible points to make first 3 items in each row have constant scaling
  ##
  ##  swfigure(sw,print,p,sw_option="height=16,width=16")
  ##}

#### Macro ####
  ## ---- ++++ M A C R O ++++ ----

  swlatex(sw,"\\section{Macro}")

  swlatex(sw,"\\subsection{Consumption}")
  p <- mipLineHistorical(data[mainReg,,"Consumption (billion US$2005/yr)"],x_hist=NULL,
                         ylab='Consumption [billion US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")

  p <- mipLineHistorical(data[,,"Consumption (billion US$2005/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Consumption [billion US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsection{Population}")
  p <- mipLineHistorical(data[mainReg,,"Population (million)"],x_hist=histData[mainReg,,"Population (million)"],
                         ylab='Population [million]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Population (million)"][mainReg,,,invert=TRUE],x_hist=histData[,,"Population (million)"][mainReg,,,invert=TRUE],
                         ylab='Population [million]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsection{GDP - MER}")
  p <- mipLineHistorical(data[mainReg,,"GDP|MER (billion US$2005/yr)"],
                         x_hist=if("GDP|MER (billion US$2005/yr)" %in% getNames(histData,dim=3)) histData[mainReg,,"GDP|MER (billion US$2005/yr)"] else NULL,
                         ylab='GDP|MER [billion US$2005/yr]',
                         scales="free_y",
                         plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"GDP|MER (billion US$2005/yr)"][mainReg,,,invert=TRUE],
                         x_hist=if("GDP|MER (billion US$2005/yr)" %in% getNames(histData,dim=3)) histData[,,"GDP|MER (billion US$2005/yr)"][mainReg,,,invert=TRUE] else NULL,
                         ylab='GDP|MER [billion US$2005/yr]',scales="free_y",
                         plot.priority=c("x_hist","x","x_proj"),
                         facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsection{GDP - PPP}")
  p <- mipLineHistorical(data[mainReg,,"GDP|PPP (billion US$2005/yr)"],x_hist=histData[mainReg,,"GDP|PPP (billion US$2005/yr)"],
                         ylab='GDP|PPP [billion US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"GDP|PPP (billion US$2005/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"GDP|PPP (billion US$2005/yr)"][mainReg,,,invert=TRUE],
                         ylab='GDP|PPP [billion US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsection{GDP - PPP per Capita}")
  gdpcap <- data[,,"GDP|PPP (billion US$2005/yr)"]/data[,,"Population (million)"]*1e3
  gdpcap_hist <- collapseNames(histData[,,"GDP|PPP (billion US$2005/yr)"]/histData[,,"Population (million)"]*1e3, collapsedim=4)

  p <- mipLineHistorical(gdpcap[mainReg,,], x_hist=gdpcap_hist[mainReg,,],
                         ylab='GDP|PPP per Cap. [US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(gdpcap[mainReg,,,invert=TRUE],x_hist=gdpcap_hist[mainReg,,,invert=TRUE],
                         ylab='GDP|PPP per Cap. [US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsection{Capital Stock}")
  p <- mipLineHistorical(data[mainReg,,"Capital Stock|Non-ESM (billion US$2005)"],x_hist=NULL,
                         ylab='Macro-economic Capital Stock [billion US$2005]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")

  p <- mipLineHistorical(data[,,"Capital Stock|Non-ESM (billion US$2005)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Macro-economic Capital Stock [billion US$2005]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsection{Investments}")
  p <- mipLineHistorical(data[mainReg,,"Investments|Non-ESM (billion US$2005/yr)"],x_hist=NULL,
                         ylab='Macro-economic Investments [billion US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")

  p <- mipLineHistorical(data[,,"Investments|Non-ESM (billion US$2005/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Macro-economic Investments [billion US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsection{Interest Rate}")
  p <- mipLineHistorical(data[mainReg,,"Interest Rate (t+1)/(t-1)|Real ()"],x_hist=NULL,
                         ylab='Interest Rate',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")

  p <- mipLineHistorical(data[,,"Interest Rate (t+1)/(t-1)|Real ()"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Interest Rate',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsection{Prices}")

  ## ---- Prices PE ----

  swlatex(sw,"\\subsubsection{PE Prices}")

  p <- mipLineHistorical(data[mainReg,,"Price|Primary Energy|Gas (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Primary Energy|Gas [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Primary Energy|Gas (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Primary Energy|Gas| [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Primary Energy|Oil (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Primary Energy|Oil [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Primary Energy|Oil (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Primary Energy|Oil [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Primary Energy|Coal (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Primary Energy|Coal [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Primary Energy|Coal (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Primary Energy|Coal [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Primary Energy|Biomass|Modern (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Primary Energy|Biomass|Modern [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Primary Energy|Biomass|Modern (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Primary Energy|Biomass|Modern [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Primary Energy|Nuclear (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Primary Energy|Nuclear [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Primary Energy|Nuclear (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Primary Energy|Nuclear [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- Prices SE ----

  swlatex(sw,"\\subsubsection{SE Prices}")

  p <- mipLineHistorical(data[mainReg,,"Price|Secondary Energy|Electricity (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Secondary Energy|Electricity [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Secondary Energy|Electricity (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Secondary Energy|Electricity [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Secondary Energy|Liquids|Fossil (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Secondary Energy|Liquids|Fossil [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Secondary Energy|Liquids|Fossil (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Secondary Energy|Liquids|Fossil [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Secondary Energy|Liquids|Biomass (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Secondary Energy|Liquids|Biomass [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Secondary Energy|Liquids|Biomass (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Secondary Energy|Liquids|Biomass [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(data[mainReg,,"Price|Secondary Energy|Liquids|Hydrogen (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Secondary Energy|Liquids|Hydrogen [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Secondary Energy|Liquids|Hydrogen (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Secondary Energy|Liquids|Hydrogen [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Secondary Energy|Gases|Fossil (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Secondary Energy|Gases|Fossil [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Secondary Energy|Gases|Fossil (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Secondary Energy|Gases|Fossil [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Secondary Energy|Gases|Biomass (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Secondary Energy|Gases|Biomass [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Secondary Energy|Gases|Biomass (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Secondary Energy|Gases|Biomass [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Secondary Energy|Gases|Hydrogen (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Secondary Energy|Gases|Hydrogen [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Secondary Energy|Gases|Hydrogen (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Secondary Energy|Gases|Hydrogen [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Secondary Energy|Solids|Fossil (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Secondary Energy|Solids|Fossil [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Secondary Energy|Solids|Fossil (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Secondary Energy|Solids|Fossil [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Secondary Energy|Solids|Biomass (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Secondary Energy|Solids|Biomass [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Secondary Energy|Solids|Biomass (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Secondary Energy|Solids|Biomass [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Secondary Energy|Hydrogen (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Secondary Energy|Hydrogen [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Secondary Energy|Hydrogen (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Secondary Energy|Hydrogen [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  p <- mipLineHistorical(data[mainReg,,"Price|Secondary Energy|Heat (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Secondary Energy|Heat [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Secondary Energy|Heat (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Secondary Energy|Heat [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- Prices FE

  swlatex(sw,"\\subsubsection{FE Prices}")

  ## ---- Prices FE Liquids ----
  
  p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Transport|Liquids (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Final Energy|Transport|Liquids [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Final Energy|Transport|Liquids (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Final Energy|Transport|Liquids [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Transport|Liquids|HDV (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Final Energy|Transport|Liquids|HDV [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Final Energy|Transport|Liquids|HDV (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Final Energy|Transport|Liquids|HDV [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Transport|Liquids|LDV (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Final Energy|Transport|Liquids|LDV [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Final Energy|Transport|Liquids|LDV (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Final Energy|Transport|Liquids|LDV [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Buildings|Liquids (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Final Energy|Buildings|Liquids [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Final Energy|Buildings|Liquids (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Final Energy|Buildings|Liquids [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Industry|Liquids (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Final Energy|Industry|Liquids [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Final Energy|Industry|Liquids (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Final Energy|Industry|Liquids [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")


## ---- Prices FE Gases ----

  if("Price|Final Energy|Transport|Gases (US$2005/GJ)" %in% getNames(data,dim=3)){
    p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Transport|Gases (US$2005/GJ)"],x_hist=NULL,
                           ylab='Price|Final Energy|Transport|Gases [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Price|Final Energy|Transport|Gases (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Price|Final Energy|Transport|Gases [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
  }
  
  p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Buildings|Gases (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Final Energy|Buildings|Gases [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Final Energy|Buildings|Gases (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Final Energy|Buildings|Gases [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Industry|Gases (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Final Energy|Industry|Gases [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Final Energy|Industry|Gases (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Final Energy|Industry|Gases [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")


## ---- Prices FE Solids ----


  p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Buildings|Solids (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Final Energy|Buildings|Solids [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Final Energy|Buildings|Solids (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Final Energy|Buildings|Solids [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Industry|Solids (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Final Energy|Industry|Solids [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Final Energy|Industry|Solids (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Final Energy|Industry|Solids [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")


## ---- Prices FE Electricity ----

  p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Transport|Electricity (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Final Energy|Transport|Electricity [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Final Energy|Transport|Electricity (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Final Energy|Transport|Electricity [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Buildings|Electricity (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Final Energy|Buildings|Electricity [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Final Energy|Buildings|Electricity (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Final Energy|Buildings|Electricity [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Industry|Electricity (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Final Energy|Industry|Electricity [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Final Energy|Industry|Electricity (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Final Energy|Industry|Electricity [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")


## ---- Prices FE Hydrogen ----

  p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Transport|Hydrogen (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Final Energy|Transport|Hydrogen [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Final Energy|Transport|Hydrogen (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Final Energy|Transport|Hydrogen [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Buildings|Hydrogen (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Final Energy|Buildings|Hydrogen [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Final Energy|Buildings|Hydrogen (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Final Energy|Buildings|Hydrogen [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Industry|Hydrogen (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Final Energy|Industry|Hydrogen [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Final Energy|Industry|Hydrogen (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Final Energy|Industry|Hydrogen [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")


## ---- Prices FE Heat ----

  p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Buildings|Heat (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Final Energy|Buildings|Heat [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Final Energy|Buildings|Heat (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Final Energy|Buildings|Heat [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Price|Final Energy|Industry|Heat (US$2005/GJ)"],x_hist=NULL,
                         ylab='Price|Final Energy|Industry|Heat [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Price|Final Energy|Industry|Heat (US$2005/GJ)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Price|Final Energy|Industry|Heat [US$2005/GJ]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  
  ## ---- Trade ----

  swlatex(sw,"\\subsection{Trade}")

  p <- mipLineHistorical(data[mainReg,,"Trade|Coal (EJ/yr)"],x_hist=histData[mainReg,,"Trade|Coal (EJ/yr)"],
                         ylab='Trade|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Trade|Coal (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"Trade|Coal (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='Trade|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Trade|Oil (EJ/yr)"],x_hist=histData[mainReg,,"Trade|Oil (EJ/yr)"],
                         ylab='Trade|Oil [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Trade|Oil (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"Trade|Oil (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='Trade|Oil [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Trade|Gas (EJ/yr)"],x_hist=histData[mainReg,,"Trade|Gas (EJ/yr)"],
                         ylab='Trade|Gas [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Trade|Gas (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"Trade|Gas (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='Trade|Gas [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Trade|Biomass (EJ/yr)"],x_hist=NULL,
                         ylab='Trade|Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Trade|Biomass (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Trade|Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  # SE trade
  if ("Trade|SE|Hydrogen (EJ/yr)" %in% getNames(data,dim=3)) {
    
    p <- mipLineHistorical(data[mainReg,,"Trade|SE|Hydrogen (EJ/yr)"],x_hist=NULL,
                           ylab='Trade|SE|Hydrogen [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Trade|SE|Hydrogen (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Trade|SE|Hydrogen [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    p <- mipLineHistorical(data[mainReg,,"Trade|SE|Hydrogen (EJ/yr)"],x_hist=NULL,
                           ylab='Trade|SE|Hydrogen [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Trade|SE|Hydrogen (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Trade|SE|Hydrogen [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    p <- mipLineHistorical(data[mainReg,,"Trade|SE|Electricity (EJ/yr)"],x_hist=NULL,
                           ylab='Trade|SE|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Trade|SE|Electricity (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Trade|SE|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    p <- mipLineHistorical(data[mainReg,,"Trade|SE|Liquids|Hydrogen (EJ/yr)"],x_hist=NULL,
                           ylab='Trade|SE|Liquids|Hydrogen [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Trade|SE|Liquids|Hydrogen (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Trade|SE|Liquids|Hydrogen [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    
  }
 

  p <- mipLineHistorical(data[mainReg,,"Trade|Goods (billion US$2005/yr)"],x_hist=NULL,
                         ylab='Trade|Goods [billion US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Trade|Goods (billion US$2005/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Trade|Goods [billion US$2005/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- FE intensity of GDP ----

  swlatex(sw,"\\subsection{FE intensity of GDP}")

  items<- c("FE|Transport (EJ/yr)",
			#"FE|Transport|w/o Bunkers (EJ/yr)",
			#"FE|Transport|Bunkers (EJ/yr)",
            "FE|Buildings (EJ/yr)",
            "FE|Industry (EJ/yr)"#,
			#"FE|CDR (EJ/yr)"
	)
  gdp <- mselect(data, variable="GDP|PPP (billion US$2005/yr)")
  var <- data[,,intersect(items, getNames(data,dim=3))]/gdp*1e3 # EJ/bil.$ -> GJ/$ -> 1e3 MJ/$

  p <- mipArea(var[mainReg,,], scales="free_y")
  p <- p + theme(legend.position="none") + ylab("FE int. of GDP (MJ/US$2005)")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none") + ylab("FE int. of GDP (MJ/US$2005)")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE]) + ylab("FE int. of GDP (MJ/US$2005)")
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y") + ylab("FE int. of GDP (MJ/US$2005)")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  
  swlatex(sw,"\\twocolumn")

  ## ---- FE intensity of GDP (GDP domain)----

  swlatex(sw,"\\onecolumn")
  swlatex(sw,"\\subsection{FE intensity of GDP, linegraph (by GDP)}")

  `FE|Transport (EJ/yr)` <- `FE|Buildings (EJ/yr)` <- `FE|Industry (EJ/yr)` <- NULL
  `GDP|PPP (billion US$2005/yr)` <- `Population (million)` <- year <- GDPpC <- NULL

  items<- c("FE|Transport (EJ/yr)",
            "FE|Buildings (EJ/yr)",
            "FE|Industry (EJ/yr)",
            "GDP|PPP (billion US$2005/yr)",
            "Population (million)")
  var <- data[,,intersect(items, getNames(data,dim=3))]
  dt <- magpie2dt(var)
  dt_hist <- magpie2dt(histData)
  dt_hist <- dt_hist[variable %in% items & model %in% c("IEA", "James_IMF", "WDI")][
    , model := "REMIND"]
  dt <- rbindlist(list(dt, dt_hist))
  hvar <- data.table::dcast(dt, ... ~ variable)

  hvar[, `:=`(
    `FE Intensity Transport`=`FE|Transport (EJ/yr)`/`GDP|PPP (billion US$2005/yr)` * 1e3,
    `FE Intensity Buildings` = `FE|Buildings (EJ/yr)`/`GDP|PPP (billion US$2005/yr)` * 1e3,
    `FE Intensity Industry` = `FE|Industry (EJ/yr)`/`GDP|PPP (billion US$2005/yr)` * 1e3,
    `GDPpC` = `GDP|PPP (billion US$2005/yr)`/`Population (million)`)]

  dt <- data.table::melt(hvar, id.vars=c("model", "scenario", "region", "year", "GDPpC"))

  reg_cols <- plotstyle(as.character(unique(dt$region)))
  reg_labels <- plotstyle(as.character(unique(dt$region)), out="legend")

  dt <- dt[grepl("Intensity", variable)]

  highlight_yrs <- c(2030, 2050, 2070)
  highlights <- dt[scenario != "historical" & year %in% highlight_yrs]

  dt <- dt[value > 0]

  p <- ggplot() +
    geom_line(data=dt[scenario != "historical" & region != mainReg],
              aes(x=GDPpC, y=value, linetype=scenario, color=region)) +
    geom_point(data=dt[scenario == "historical" & region != mainReg],
               aes(x=GDPpC, y=value, color=region), shape=4) +
    geom_point(data=highlights[region != mainReg], aes(x=`GDPpC`, y=value, color=region), shape=1) +
    facet_wrap(~ variable, scales="free_y") +
    ylab("FE Intensity (MJ/US$2005)") +
    xlab("GDP PPP per Cap. (kUS$2005)") +
    scale_y_continuous(limits = c(0, 7.5)) +
    theme_minimal()

  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\twocolumn")


  ## ---- Kaya decomposition ----

  swlatex(sw,"\\subsection{Kaya-Decomposition}")
  ## calculate Kaya-Decomposition
  kaya <- new.magpie(getRegions(data),getYears(data),magclass::getNames(data,dim=1))
  kaya <- add_dimension(kaya,dim=3.2,add="kaya",nm=c("CO2 FF&I/FE","FE/GDP","GDP/Population","Population"))
  for (i in magclass::getNames(data,dim=1)) {
    kaya[,,i] <- calcKayaDecomp(mif=data[,,i])
  }
  p <- magpie2ggplot2(kaya,facet_y="Data2",facet_x="Data1",color="Region",group=NULL,
                      scales="free_y",show_grid=TRUE,ylab='Kaya Decomposition [%]',
                      color_pal=plotstyle(getRegions(kaya)))
  swfigure(sw,print,p,sw_option="height=10,width=9")

  kaya <- new.magpie(getRegions(data),getYears(data),magclass::getNames(data,dim=1))
  kaya <- add_dimension(kaya,dim=3.2,add="kaya",nm=c("CO2 FF&I [Mt CO2/yr]/FE [EJ/yr]","FE [EJ/yr]/GDP [billion US$2005/yr]",
                                                     "GDP [billion US$2005/yr]/Population [million]","Population [million]"))
  for (i in magclass::getNames(data,dim=1)) {
    kaya[,,i] <- calcKayaDecomp(mif=data[,,i],ref_year=NULL)
  }
  p <- magpie2ggplot2(kaya,facet_y="Data2",facet_x="Data1",color="Region",group=NULL,
                      scales="free_y",show_grid=TRUE,ylab='Kaya Decomposition',
                      color_pal=plotstyle(getRegions(kaya)))
  swfigure(sw,print,p,sw_option="height=10,width=9")

  ## ---- ++++ E M I S S I O N S ++++ ----

  swlatex(sw,"\\section{Emissions}")

  ## ---- Emissions GHG Total ----

  swlatex(sw,"\\subsection{GHG - total}")

  targets = c("Emi|GHGtot|target|40% (Mt CO2-equiv/yr)"="-40% by 2030 (vs. 1990)",
              "Emi|GHGtot|target|55% (Mt CO2-equiv/yr)"="-55% by 2030 (vs. 1990)",
              "Emi|GHGtot|target|65% (Mt CO2-equiv/yr)"="-65% by 2030 (vs. 1990)")

  p <- mipLineHistorical(data[mainReg,,"Emi|GHG (Mt CO2eq/yr)"],x_hist=histData[mainReg,,"Emi|GHGtot (Mt CO2-equiv/yr)"],
                         ylab='Total GHG Emisions w/o Bunkers [Mt CO2-equiv/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),
                         hlines=if(all(names(targets) %in% getNames(histData, dim=3)) && !all(is.na(histData[mainReg,2030,names(targets)]))) histData[mainReg,2030,names(targets)] else NULL, 
                         hlines.labels=targets)
  swfigure(sw,print,p,sw_option="height=8,width=8")
  
  p <- mipLineHistorical(data[,,"Emi|GHG (Mt CO2eq/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"Emi|GHGtot (Mt CO2-equiv/yr)"][mainReg,,,invert=TRUE],
                         ylab='Total GHG Emisions w/o Bunkers [Mt CO2-equiv/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3,
                         hlines=if(all(names(targets) %in% getNames(histData, dim=3)) && !all(is.na(histData[mainReg,2030,names(targets)]))) histData[mainReg,2030,names(targets)] else NULL, 
                         hlines.labels=targets)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  
  #GHG total with bunkers
  p <- mipLineHistorical(data[mainReg,,"Emi|GHG|w/ Bunkers (Mt CO2eq/yr)"],x_hist=NULL,
                         ylab='Total GHG Emisions w/ Bunkers [Mt CO2-equiv/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  
  p <- mipLineHistorical(data[,,"Emi|GHG|w/ Bunkers (Mt CO2eq/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Total GHG Emisions w/ Bunkers [Mt CO2-equiv/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  # emission markets
  if(mainReg %in% c("EUR","EU27")){
    swlatex(sw,"\\subsection{GHG - Market}")
    
    swlatex(sw,"\\subsubsection{ETS}")

    targets = c("Emi|GHG|ETS|target|61% (Mt CO2-equiv/yr)"="-61% by 2030 (vs. 2005)")
    
    p <- mipLineHistorical(data[mainReg,,"Emi|GHG|ETS (Mt CO2eq/yr)"],x_hist=histData[mainReg,,"Emi|GHG|ETS (Mt CO2-equiv/yr)"],
                           ylab='Emi|GHG|ETS [Mt CO2-equiv/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
        
    p <- mipLineHistorical(data[mainReg,,"Emi|GHG|ETS (Mt CO2eq/yr)"],x_hist=histData[mainReg,,"Emi|GHG|ETS (Mt CO2-equiv/yr)"],
                           ylab='Emi|GHG|ETS [Mt CO2-equiv/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),
                           hlines=if(all(names(targets) %in% getNames(histData, dim=3) & !is.na(histData[mainReg,2030,names(targets)]))) histData[mainReg,2030,names(targets)] else NULL,
                           hlines.labels=targets)
    swfigure(sw,print,p,sw_option="height=8,width=8")  


    swlatex(sw,"\\subsubsection{ESR}")
    
    targets = c("Emi|GHG|ES|target|40% (Mt CO2-equiv/yr)"="-40% by 2030 (vs. 2005)")
    
    p <- mipLineHistorical(data[mainReg,,"Emi|GHG|ESR (Mt CO2eq/yr)"],x_hist=histData[mainReg,,"Emi|GHG|ES (Mt CO2-equiv/yr)"],
                           ylab='Emi|GHG|ESR [Mt CO2-equiv/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),
hlines=if(all(names(targets) %in% getNames(histData, dim=3) & !is.na(histData[mainReg,2030,names(targets)]))) histData[mainReg,2030,names(targets)] else NULL,
                           hlines.labels=targets)
    swfigure(sw,print,p,sw_option="height=8,width=8")  
    p <- mipLineHistorical(data[,,"Emi|GHG|ESR (Mt CO2eq/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"Emi|GHG|ES (Mt CO2-equiv/yr)"][mainReg,,,invert=TRUE],
                           ylab='Emi|GHG|ESR [Mt CO2-equiv/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3,
                           hlines=if(all(names(targets) %in% getNames(histData, dim=3) & !is.na(histData[mainReg,2030,names(targets)]))) histData[mainReg,,invert=TRUE][,2030,names(targets)] else NULL)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    swlatex(sw,"\\subsubsection{Other - Outside ETS and ESR}")
    p <- mipLineHistorical(data[mainReg,,"Emi|GHG|Outside ETS and ESR (Mt CO2eq/yr)"],
                           ylab='Emi|GHG|Other - outside ETS and ESR [Mt CO2-equiv/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")  
    p <- mipLineHistorical(data[,,"Emi|GHG|Outside ETS and ESR (Mt CO2eq/yr)"][mainReg,,,invert=TRUE],
                           ylab='Emi|GHG|Other - outside ETS and ESR [Mt CO2-equiv/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    swlatex(sw,"\\twocolumn")
  }

  ## ---- Emissions CO2 ----
  
  swlatex(sw,"\\subsection{CO2}")
  
  swlatex(sw,"\\subsubsection{CO2 by sector (net emissions, incl. BECCS)}")
  
  tot <-"Emi|CO2 (Mt CO2/yr)"
  items <- c(
    "Emi|CO2|Land-Use Change (Mt CO2/yr)",
    "Emi|CO2|Industrial Processes (Mt CO2/yr)",
    "Emi|CO2|Energy|Demand|Transport (Mt CO2/yr)",
    "Emi|CO2|Energy|Demand|Industry (Mt CO2/yr)",
    "Emi|CO2|Energy|Demand|Buildings (Mt CO2/yr)",
    "Emi|CO2|Energy|Supply|Non-electric (Mt CO2/yr)",
    "Emi|CO2|Energy|Supply|Electricity w/ couple prod (Mt CO2/yr)",
    "Emi|CO2|CDR|DACCS (Mt CO2/yr)",
    "Emi|CO2|CDR|EW (Mt CO2/yr)")
  
  
  var <- data[,,intersect(items,getNames(data,dim=3))]
  
  
  p <- mipArea(var[mainReg,,],scales="free_y", total = F)
  p <- p + theme(legend.position="bottom") + 
    scale_y_continuous("Emi|CO2 (Mt CO2/yr)") +
    geom_line(data=as.quitte(data[mainReg,,tot]), 
              mapping=aes(period, value),
              size=1.3)
  swfigure(sw,print,p,sw_option="height=3.5,width=7")
  
  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")
  
  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE]) +
    guides(fill=guide_legend(ncol=3))
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],total=data[,,tot][mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")



  swlatex(sw,"\\subsubsection{Total CO2}")
  p <- mipLineHistorical(data[mainReg,,"Emi|CO2 (Mt CO2/yr)"],x_hist=histData[mainReg,,"Emi|CO2 (Mt CO2/yr)"],
                         ylab='Emi|CO2 [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2 (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"Emi|CO2 (Mt CO2/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CO2 [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\subsubsection{Energy and Industrial Processes - Net (incl BECCS)}")
  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Energy and Industrial Processes (Mt CO2/yr)"],x_hist=histData[mainReg,,"Emi|CO2|Fossil Fuels and Industry (Mt CO2/yr)"],
                         ylab='Emi|CO2|Energy and Industrial Processes [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|Energy and Industrial Processes (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"Emi|CO2|Fossil Fuels and Industry (Mt CO2/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CO2|Energy and Industrial Processes [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\subsubsection{Energy and Industrial Processes - Gross}")
  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Gross|Energy and Industrial Processes (Mt CO2/yr)"],x_hist=histData[mainReg,,"Emi|CO2|Fossil Fuels and Industry (Mt CO2/yr)"],
                         ylab='Emi|CO2|Gross|Energy and Industrial Processes [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|Gross|Energy and Industrial Processes (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"Emi|CO2|Fossil Fuels and Industry (Mt CO2/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CO2|Gross|Energy and Industrial Processes [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\subsubsection{Energy}")
  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Energy (Mt CO2/yr)"],x_hist=histData[mainReg,,"Emi|CO2|Energy (Mt CO2/yr)"],
                         ylab='Emi|CO2|Energy  [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|Energy (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"Emi|CO2|Energy (Mt CO2/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CO2|Energy  [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\subsubsection{Energy Supply}")
  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Energy|Supply (Mt CO2/yr)"],x_hist=histData[mainReg,,"Emi|CO2|Fossil Fuels and Industry|Energy Supply (Mt CO2/yr)"],
                         ylab='Emi|CO2|Energy|Supply [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|Energy|Supply (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"Emi|CO2|Fossil Fuels and Industry|Energy Supply (Mt CO2/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CO2|Energy|Supply [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\subsubsection{Electricity}")
  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Energy|Supply|Electricity w/ couple prod (Mt CO2/yr)"],x_hist=histData[mainReg,,"Emi|CO2|Energy|Supply|Electricity (Mt CO2/yr)"],
                         ylab='Emi|CO2|Energy|Supply|Electricity [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|Energy|Supply|Electricity w/ couple prod (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"Emi|CO2|Energy|Supply|Electricity (Mt CO2/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CO2|Energy|Supply|Electricity [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\subsubsection{Buildings}")
  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Energy|Demand|Buildings (Mt CO2/yr)"],x_hist=histData[mainReg,,"Emi|CO2|Buildings|Direct (Mt CO2/yr)"],
                         ylab='Emi|CO2|Energy|Demand|Buildings [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|Energy|Demand|Buildings (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"Emi|CO2|Buildings|Direct (Mt CO2/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CO2|Energy|Demand|Buildings [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\subsubsection{Industry}")
  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Energy|Demand|Industry (Mt CO2/yr)"],x_hist=histData[mainReg,,"Emi|CO2|Industry|Direct (Mt CO2/yr)"],
                         ylab='Emi|CO2|Energy|Demand|Industry [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|Energy|Demand|Industry (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"Emi|CO2|Industry|Direct (Mt CO2/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CO2|Energy|Demand|Industry [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  
  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Gross|Energy|Demand|Industry (Mt CO2/yr)"],x_hist=histData[mainReg,,"Emi|CO2|Industry|Direct (Mt CO2/yr)"],
                         ylab='Emi|CO2|Gross|Energy|Demand|Industry [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|Gross|Energy|Demand|Industry (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"Emi|CO2|Industry|Direct (Mt CO2/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CO2|Gross|Energy|Demand|Industry [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\subsubsection{Transport}")
  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Energy|Demand|Transport (Mt CO2/yr)"],x_hist=histData[mainReg,,"Emi|CO2|Transport|Demand (Mt CO2/yr)"],
                         ylab='Emi|CO2|Energy|Demand|Transport [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|Energy|Demand|Transport (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"Emi|CO2|Transport|Demand (Mt CO2/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CO2|Energy|Demand|Transport [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  

  swlatex(sw,"\\subsubsection{Process Emissions}")
  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Industrial Processes (Mt CO2/yr)"],x_hist=histData[mainReg,,"Emi|CO2|FFaI|Industry|Process (Mt CO2/yr)"],
                         ylab='Emi|CO2|Industrial Processes [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|Industrial Processes (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"Emi|CO2|FFaI|Industry|Process (Mt CO2/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CO2|Industrial Processes [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\subsubsection{Land-Use Change (Mt CO2/yr)}")
  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Land-Use Change (Mt CO2/yr)"],x_hist=histData[mainReg,,"Emi|CO2|Land Use (Mt CO2/yr)"],
                         ylab='Emi|CO2|Land-Use Change [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|Land-Use Change (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"Emi|CO2|Land Use (Mt CO2/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CO2|Land-Use Change [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\subsubsection{non-BECCS CDR (Mt CO2/yr)}")
  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|non-BECCS CDR (Mt CO2/yr)"],
                         ylab='Emi|CO2|non-BECCS CDR [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|non-BECCS CDR (Mt CO2/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|CO2|non-BECCS CDR [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  

  ## ---- CDR ----
  swlatex(sw,"\\subsubsection{CDR}")
  
  # gross and negative CO2
  
  tot <-"Emi|CO2 (Mt CO2/yr)"
  items <- c("Emi|CO2|Gross|Energy and Industrial Processes (Mt CO2/yr)",
             "Emi|CO2|Land-Use Change (Mt CO2/yr)",
             "Emi|CO2|CDR|BECCS (Mt CO2/yr)",
             "Emi|CO2|CDR|DACCS (Mt CO2/yr)",
             "Emi|CO2|CDR|EW (Mt CO2/yr)")
  
  var <- data[,,intersect(items,getNames(data,dim=3))]
  
  
  p <- mipArea(var[mainReg,,],scales="free_y", total = F)
  p <- p + theme(legend.position="bottom") + 
    scale_y_continuous("Emi|CO2 (Mt CO2/yr)") +
    geom_line(data=as.quitte(data[mainReg,,tot]), 
              mapping=aes(period, value),
              size=1.3)
  swfigure(sw,print,p,sw_option="height=3.5,width=7")
  
  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")
  
  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],total=data[,,tot][mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")
  
  # negative CO2 (CDR)
  tot <-"Emi|CO2|CDR (Mt CO2/yr)"
  items <- c(
             "Emi|CO2|CDR|Land-Use Change (Mt CO2/yr)",
             "Emi|CO2|CDR|BECCS (Mt CO2/yr)",
             "Emi|CO2|CDR|DACCS (Mt CO2/yr)",
             "Emi|CO2|CDR|EW (Mt CO2/yr)")
  
  var <- data[,,intersect(items,getNames(data,dim=3))]
  
  
  p <- mipArea(var[mainReg,,],scales="free_y", total = F)
  p <- p + theme(legend.position="bottom") + 
    scale_y_continuous("Emi|CO2 (Mt CO2/yr)") +
    geom_line(data=as.quitte(data[mainReg,,tot]), 
              mapping=aes(period, value),
              size=1.3)
  swfigure(sw,print,p,sw_option="height=3.5,width=7")
  
  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")
  
  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],total=data[,,tot][mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")
  
  # line plots
  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|CDR|BECCS (Mt CO2/yr)"],x_hist=NULL,
                         ylab='Emi|CO2|CDR|BECCS [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|CDR|BECCS (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Emi|CO2|CDR|BECCS [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|CDR|DACCS (Mt CO2/yr)"],x_hist=NULL,
                         ylab='Emi|CO2|CDR|DACCS [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|CDR|DACCS (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Emi|CO2|CDR|DACCS [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|CDR|EW (Mt CO2/yr)"],x_hist=NULL,
                         ylab='Emi|CO2|CDR|EW [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|CDR|EW (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Emi|CO2|CDR|EW [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  
  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|CDR|Land-Use Change (Mt CO2/yr)"],x_hist=NULL,
                         ylab='Emi|CO2|CDR|Land-Use Change [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|CDR|Land-Use Change (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Emi|CO2|CDR|Land-Use Change [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")



  ## ---- Cumulated Emissions ----

  swlatex(sw,"\\subsubsection{Cumulated Emissions}")

  toplot <- data[,,"Emi|CO2|Cumulated (Mt CO2)"]

  toplot2010 <- setYears(toplot[,2010,], NULL)
  toplot <- toplot - toplot2010

  p <- mipLineHistorical(toplot[mainReg,,"Emi|CO2|Cumulated (Mt CO2)"],x_hist=NULL,
                         ylab='Emi|CO2|Cumulated [Mt CO2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")

  p <- mipLineHistorical(toplot[mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Emi|CO2|Cumulated [Mt CO2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Cumulated|CDR|Land-Use Change (Mt CO2)"],x_hist=NULL,
                         ylab='Emi|CO2|Cumulated|CDR|Land-Use Change [Mt CO2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|Cumulated|CDR|Land-Use Change (Mt CO2)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Emi|CO2|Cumulated|CDR|Land-Use Change [Mt CO2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Cumulated|CDR|BECCS (Mt CO2)"],x_hist=NULL,
                         ylab='Emi|CO2|Cumulated|CDR|BECCS [Mt CO2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|CO2|Cumulated|CDR|BECCS (Mt CO2)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Emi|CO2|Cumulated|CDR|BECCS [Mt CO2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  if("Emi|CO2|CDR|DACCS (Mt CO2/yr)" %in% magclass::getNames(data,dim=3)) {
    p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Cumulated|CDR|DACCS (Mt CO2)"],x_hist=NULL,
                           ylab='Emi|CO2|Cumulated|CDR|DACCS [Mt CO2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Emi|CO2|Cumulated|CDR|DACCS (Mt CO2)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Emi|CO2|Cumulated|CDR|DACCS [Mt CO2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
  }

  if("Emi|CO2|CDR|EW (Mt CO2/yr)" %in% magclass::getNames(data,dim=3)) {
    p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Cumulated|CDR|EW (Mt CO2)"],x_hist=NULL,
                           ylab='Emi|CO2|Cumulated|CDR|EW [Mt CO2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Emi|CO2|Cumulated|CDR|EW (Mt CO2)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Emi|CO2|Cumulated|CDR|EW [Mt CO2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
  }

 

  ## ---- Emissions CDR by sector cumulated

  tot <-"Emi|CO2|Cumulated (Mt CO2)"
  items <- c("Emi|CO2|Cumulated|Gross|Energy and Industrial Processes (Mt CO2)",
             "Emi|CO2|Cumulated|Land-Use Change (Mt CO2)",
             "Emi|CO2|Cumulated|CDR|BECCS (Mt CO2)",
             "Emi|CO2|Cumulated|CDR|DACCS (Mt CO2)",
             "Emi|CO2|Cumulated|CDR|EW (Mt CO2)")
  
  var <- data[,,intersect(items,getNames(data,dim=3))]
  
  
  p <- mipArea(var[mainReg,,],scales="free_y", total = F)
  p <- p + theme(legend.position="bottom") + 
    scale_y_continuous("Emi|CO2|Cumulated (Mt CO2/yr)") +
    geom_line(data=as.quitte(data[mainReg,,tot]), 
              mapping=aes(period, value),
              size=1.3)
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],total=data[,,tot][mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")
  
  if(mainReg %in% c("EUR","EU27")){
    
    ## Market Emissions ----
    
    swlatex(sw,"\\subsection{Market Emissions}")
    
    swlatex(sw,"\\subsubsection{GHG ETS Emissions}")
    
    
    tot <-"Emi|GHG|ETS (Mt CO2eq/yr)"
    items <- c(
      "Emi|GHG|ETS|Energy Supply (Mt CO2eq/yr)",
      "Emi|GHG|ETS|Industry (Mt CO2eq/yr)",
      "Emi|GHG|ETS|Transport (Mt CO2eq/yr)",
      "Emi|GHG|ETS|Extraction (Mt CO2eq/yr)",
      "Emi|GHG|ETS|Waste (Mt CO2eq/yr)",
      "Emi|GHG|ETS|non-BECCS CDR (Mt CO2eq/yr)")
    
    
    var <- data[,,intersect(items,getNames(data,dim=3))]
    
    p <- mipArea(var[mainReg,,],scales="free_y", total = F)
    p <- p + theme(legend.position="bottom") + 
      scale_y_continuous("Emi|GHG|ETS (Mt CO2eq/yr)") +
      geom_line(data=as.quitte(data[mainReg,,tot]), 
                mapping=aes(period, value),
                size=1.3)
    swfigure(sw,print,p,sw_option="height=3.5,width=7")
    
    p <- mipBarYearData(var[mainReg,y_bar,])
    p <- p + theme(legend.position="none")
    swfigure(sw,print,p,sw_option="height=4.5,width=7")
    
    p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE]) +
      guides(fill=guide_legend(ncol=3))
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    swlatex(sw,"\\onecolumn")
    p <- mipArea(var[mainReg,,,invert=TRUE],total=data[,,tot][mainReg,,,invert=TRUE],scales="free_y")
    swfigure(sw,print,p,sw_option="height=8,width=16")
    swlatex(sw,"\\twocolumn")
    
    swlatex(sw,"\\subsubsection{GHG ESR Emissions}")
    
    tot <-"Emi|GHG|ESR (Mt CO2eq/yr)"
    items <- c(
      "Emi|GHG|ESR|Buildings (Mt CO2eq/yr)",
      "Emi|GHG|ESR|Industry (Mt CO2eq/yr)",
      "Emi|GHG|ESR|Transport (Mt CO2eq/yr)",
      "Emi|GHG|ESR|Agriculture (Mt CO2eq/yr)",
      "Emi|GHG|ESR|Waste (Mt CO2eq/yr)")
    
    
    var <- data[,,intersect(items,getNames(data,dim=3))]
    
    p <- mipArea(var[mainReg,,],scales="free_y", total = F)
    p <- p + theme(legend.position="bottom") + 
      scale_y_continuous("Emi|GHG|ESR (Mt CO2eq/yr)") +
      geom_line(data=as.quitte(data[mainReg,,tot]), 
                mapping=aes(period, value),
                size=1.3)
    swfigure(sw,print,p,sw_option="height=3.5,width=7")
    
    p <- mipBarYearData(var[mainReg,y_bar,])
    p <- p + theme(legend.position="none")
    swfigure(sw,print,p,sw_option="height=4.5,width=7")
    
    p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE]) +
      guides(fill=guide_legend(ncol=3))
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    swlatex(sw,"\\onecolumn")
    p <- mipArea(var[mainReg,,,invert=TRUE],total=data[,,tot][mainReg,,,invert=TRUE],scales="free_y")
    swfigure(sw,print,p,sw_option="height=8,width=16")
    swlatex(sw,"\\twocolumn")
    
    swlatex(sw,"\\subsubsection{GHG Other Emissions - Outisde ETS and ESR}")
    
    tot <-"Emi|GHG|Outside ETS and ESR (Mt CO2eq/yr)"
    items <- c(
      "Emi|GHG|Outside ETS and ESR|Transport (Mt CO2eq/yr)",
      "Emi|GHG|Outside ETS and ESR|Land-Use Change (Mt CO2eq/yr)",
      "Emi|GHG|Outside ETS and ESR|F-Gases (Mt CO2eq/yr)")
    
    
    var <- data[,,intersect(items,getNames(data,dim=3))]
    
    p <- mipArea(var[mainReg,,],scales="free_y", total = F)
    p <- p + theme(legend.position="bottom") + 
      scale_y_continuous("Emi|GHG|Outside ETS and ESR (Mt CO2eq/yr)") +
      geom_line(data=as.quitte(data[mainReg,,tot]), 
                mapping=aes(period, value),
                size=1.3)
    swfigure(sw,print,p,sw_option="height=3.5,width=7")
    
    p <- mipBarYearData(var[mainReg,y_bar,])
    p <- p + theme(legend.position="none")
    swfigure(sw,print,p,sw_option="height=4.5,width=7")
    
    p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE]) +
      guides(fill=guide_legend(ncol=3))
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    swlatex(sw,"\\onecolumn")
    p <- mipArea(var[mainReg,,,invert=TRUE],total=data[,,tot][mainReg,,,invert=TRUE],scales="free_y")
    swfigure(sw,print,p,sw_option="height=8,width=16")
    swlatex(sw,"\\twocolumn")
    
    
    
    
    swlatex(sw,"\\subsubsection{Market Emissions across GHGs}")
    
    p <- mipLineHistorical(data[mainReg,,"Emi|CO2|ETS (Mt CO2/yr)"],x_hist=NULL,
                           ylab='Emi|CO2|ETS [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")  
    
    p <- mipLineHistorical(data[,,"Emi|CO2|ETS (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Emi|CO2|ETS [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    
    p <- mipLineHistorical(data[mainReg,,"Emi|CO2|ESR (Mt CO2/yr)"],x_hist=NULL,
                           ylab='Emi|CO2|ESR [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")  
    
    p <- mipLineHistorical(data[,,"Emi|CO2|ESR (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Emi|CO2|ESR [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    
    p <- mipLineHistorical(data[mainReg,,"Emi|CO2|Outside ETS and ESR (Mt CO2/yr)"],x_hist=NULL,
                           ylab='Emi|CO2|Other - Outside ETS and ESR [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")  
    
    p <- mipLineHistorical(data[,,"Emi|CO2|Outside ETS and ESR (Mt CO2/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Emi|CO2|Other - Outside ETS and ESR [Mt CO2/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    
    swlatex(sw,"\\subsubsection{Market CH4}")
    
    p <- mipLineHistorical(data[mainReg,,"Emi|CH4|ETS (Mt CH4/yr)"],x_hist=NULL,
                           ylab='Emi|CH4|ETS [Mt CH4/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")  
    
    p <- mipLineHistorical(data[,,"Emi|CH4|ETS (Mt CH4/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Emi|CH4|ETS [Mt CH4/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    
    p <- mipLineHistorical(data[mainReg,,"Emi|CH4|ESR (Mt CH4/yr)"],x_hist=NULL,
                           ylab='Emi|CH4|ESR [Mt CH4/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")  
    
    p <- mipLineHistorical(data[,,"Emi|CH4|ESR (Mt CH4/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Emi|CH4|ESR [Mt CH4/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    
    p <- mipLineHistorical(data[mainReg,,"Emi|CH4|Outside ETS and ESR (Mt CH4/yr)"],x_hist=NULL,
                           ylab='Emi|CH4|Other - Outside ETS and ESR [Mt CH4/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")  
    
    p <- mipLineHistorical(data[,,"Emi|CH4|Outside ETS and ESR (Mt CH4/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Emi|CH4|Other - Outside ETS and ESR [Mt CH4/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    
    swlatex(sw,"\\subsubsection{Market N2O}")
    
    p <- mipLineHistorical(data[mainReg,,"Emi|N2O|ETS (kt N2O/yr)"],x_hist=NULL,
                           ylab='Emi|N2O|ETS [kt N2O/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")  
    
    p <- mipLineHistorical(data[,,"Emi|N2O|ETS (kt N2O/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Emi|N2O|ETS [kt N2O/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    
    p <- mipLineHistorical(data[mainReg,,"Emi|N2O|ESR (kt N2O/yr)"],x_hist=NULL,
                           ylab='Emi|N2O|ESR [kt N2O/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")  
    
    p <- mipLineHistorical(data[,,"Emi|N2O|ESR (kt N2O/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Emi|N2O|ESR [kt N2O/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    
    p <- mipLineHistorical(data[mainReg,,"Emi|N2O|Outside ETS and ESR (kt N2O/yr)"],x_hist=NULL,
                           ylab='Emi|N2O|Other - Outside ETS and ESR [kt N2O/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")  
    
    p <- mipLineHistorical(data[,,"Emi|N2O|Outside ETS and ESR (kt N2O/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                           ylab='Emi|N2O|Other - Outside ETS and ESR [kt N2O/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
  }
    
    
    
    
    
    
  ## ---- Emissions CH4 ----

  swlatex(sw,"\\subsection{CH4}")

  tot <-"Emi|CH4 (Mt CH4/yr)"
  items <- c(
    "Emi|CH4|Extraction (Mt CH4/yr)",
    "Emi|CH4|Agriculture (Mt CH4/yr)",
    "Emi|CH4|Land-Use Change (Mt CH4/yr)",
    "Emi|CH4|Waste (Mt CH4/yr)",
    "Emi|CH4|Energy Supply (Mt CH4/yr)")
  
  
  var <- data[,,intersect(items,getNames(data,dim=3))]
  
  p <- mipArea(var[mainReg,,],scales="free_y", total = F)
  p <- p + theme(legend.position="bottom") + 
    scale_y_continuous("Emi|CH4 (Mt CH4/yr)") +
    geom_line(data=as.quitte(data[mainReg,,tot]), 
              mapping=aes(period, value),
              size=1.3)
  swfigure(sw,print,p,sw_option="height=3.5,width=7")
  
  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")
  
  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE]) +
    guides(fill=guide_legend(ncol=3))
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],total=data[,,tot][mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")
  
 

  ## ---- Emissions N2O ----

  swlatex(sw,"\\subsection{N2O}")

  tot <-"Emi|N2O (kt N2O/yr)"
  items <- c(
    "Emi|N2O|Agriculture (kt N2O/yr)",
    "Emi|N2O|Land-Use Change (kt N2O/yr)",
    "Emi|N2O|Waste (kt N2O/yr)",
    "Emi|N2O|Transport (kt N2O/yr)",
    "Emi|N2O|Industry (kt N2O/yr)",
    "Emi|N2O|Energy Supply (kt N2O/yr)")
  
  
  var <- data[,,intersect(items,getNames(data,dim=3))]
  
  p <- mipArea(var[mainReg,,],scales="free_y", total = F)
  p <- p + theme(legend.position="bottom") + 
    scale_y_continuous("Emi|N2O (kt N2O/yr)") +
    geom_line(data=as.quitte(data[mainReg,,tot]), 
              mapping=aes(period, value),
              size=1.3)
  swfigure(sw,print,p,sw_option="height=3.5,width=7")
  
  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")
  
  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE]) +
    guides(fill=guide_legend(ncol=3))
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],total=data[,,tot][mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")
  
  
  
  ## ---- Emissions F-Gases ----
  swlatex(sw,"\\subsection{F-Gases}")

  p <- mipLineHistorical(data[mainReg,,"Emi|GHG|F-Gases (Mt CO2eq/yr)"],
                         ylab='Emi|GHG|F-Gases [Mt CO2-eq./yr]',
                         scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Emi|GHG|F-Gases (Mt CO2eq/yr)"][mainReg,,,invert=TRUE],
                         ylab='Emi|GHG|F-Gases [Mt CO2-eq./yr]',
                         scales="free_y",plot.priority=c("x_hist","x","x_proj"),
                         facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
 
  ## ---- ++++ E N E R G Y ++++ ----

  swlatex(sw,"\\section{Energy}")

  ## ---- Investments Electricity ----

  swlatex(sw,"\\subsection{Investments Electricity}")

  items <- c ("Energy Investments|Elec|Coal|w/ CCS (billion US$2005/yr)",
              "Energy Investments|Elec|Coal|w/o CCS (billion US$2005/yr)",
              "Energy Investments|Elec|Gas|w/ CCS (billion US$2005/yr)",
              "Energy Investments|Elec|Gas|w/o CCS (billion US$2005/yr)",
              "Energy Investments|Elec|Oil (billion US$2005/yr)",
              "Energy Investments|Elec|Biomass|w/ CCS (billion US$2005/yr)",
              "Energy Investments|Elec|Biomass|w/o CCS (billion US$2005/yr)",
              "Energy Investments|Elec|Nuclear (billion US$2005/yr)",
              "Energy Investments|Elec|Solar (billion US$2005/yr)",
              "Energy Investments|Elec|Wind (billion US$2005/yr)",
              "Energy Investments|Elec|Hydro (billion US$2005/yr)",
              "Energy Investments|Elec|Geothermal (billion US$2005/yr)",
              "Energy Investments|Elec|Hydrogen (billion US$2005/yr)",
              "Energy Investments|Elec|Grid (billion US$2005/yr)",
              "Energy Investments|Elec|Storage (billion US$2005/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- Capacities Electricity ----

  swlatex(sw,"\\subsection{Capacities Electricity}")

  items <- c ("Cap|Electricity|Coal|w/ CCS (GW)",
              "Cap|Electricity|Coal|w/o CCS (GW)",
              "Cap|Electricity|Gas|w/ CCS (GW)",
              "Cap|Electricity|Gas|w/o CCS (GW)",
              "Cap|Electricity|Oil|w/o CCS (GW)",
              "Cap|Electricity|Biomass (GW)",
              "Cap|Electricity|Nuclear (GW)",
              "Cap|Electricity|Hydro (GW)",
              "Cap|Electricity|Geothermal (GW)",
              "Cap|Electricity|Hydrogen (GW)",
              "Cap|Electricity|Storage|Battery (GW)",
              "Cap|Electricity|Solar (GW)")
  
  if ("Cap|Electricity|Wind|Offshore (GW)" %in% magclass::getNames(data, dim = 3)) {
    items <- append(items, c( "Cap|Electricity|Wind|Onshore (GW)",
                              "Cap|Electricity|Wind|Offshore (GW)"))
  } else {
    items <- append(items, c( "Cap|Electricity|Wind (GW)"))
  }
  
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")
  
  
  # ---- Capacities electricity line plots ----
  swlatex(sw,"\\subsection{Capacities electricity line plots}")
  
  swlatex(sw,"\\subsubsection{Nuclear}")
  
  var <- "Cap|Electricity|Nuclear (GW)"
  p <- mipLineHistorical(data[mainReg,,var],x_hist=histData[mainReg,,var],
                         ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,var][mainReg,,,invert=TRUE],x_hist=histData[,,var][mainReg,,,invert=TRUE],
                         ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\subsubsection{Hydro}")
  var <- "Cap|Electricity|Hydro (GW)"
  p <- mipLineHistorical(data[mainReg,,var],x_hist=histData[mainReg,,var],
                         ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,var][mainReg,,,invert=TRUE],x_hist=histData[,,var][mainReg,,,invert=TRUE],
                         ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\subsubsection{Wind}")
  var <- "Cap|Electricity|Wind (GW)"
  p <- mipLineHistorical(data[mainReg,,var],x_hist=histData[mainReg,,var],
                         ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,var][mainReg,,,invert=TRUE],x_hist=histData[,,var][mainReg,,,invert=TRUE],
                         ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  if ("Cap|Electricity|Wind|Offshore (GW)" %in% magclass::getNames(data, dim = 3)) {
    swlatex(sw,"\\subsubsection{Wind Onshore}")
    
    var <- "Cap|Electricity|Wind|Onshore (GW)"
    
    p <-mipLineHistorical(data[mainReg,,var],x_hist=histData[mainReg,,var],
                          ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,var][mainReg,,,invert=TRUE],x_hist=histData[,,var][mainReg,,,invert=TRUE],
                           ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

    swlatex(sw,"\\subsubsection{Wind Offshore}")
    var <- "Cap|Electricity|Wind|Offshore (GW)"
    p <-mipLineHistorical(data[mainReg,,var],
                          ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,var][mainReg,,,invert=TRUE],
                           ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

  }
  
  swlatex(sw,"\\subsubsection{Solar}")
  var <- "Cap|Electricity|Solar (GW)"
  p <- mipLineHistorical(data[mainReg,,var],x_hist=histData[mainReg,,var],
                         ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,var][mainReg,,,invert=TRUE],x_hist=histData[,,var][mainReg,,,invert=TRUE],
                         ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\subsubsection{Coal}")
  var <- "Cap|Electricity|Coal (GW)"
  p <- mipLineHistorical(data[mainReg,,var],x_hist=histData[mainReg,,var],
                         ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,var][mainReg,,,invert=TRUE],x_hist=histData[,,var][mainReg,,,invert=TRUE],
                         ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\subsubsection{Gas}")
  var <- "Cap|Electricity|Gas (GW)"
  p <- mipLineHistorical(data[mainReg,,var],x_hist=histData[mainReg,,var],
                         ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,var][mainReg,,,invert=TRUE],x_hist=histData[,,var][mainReg,,,invert=TRUE],
                         ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\subsubsection{Oil}")
  var <- "Cap|Electricity|Oil|w/o CCS (GW)"
  p <- mipLineHistorical(data[mainReg,,var],x_hist=histData[mainReg,,var],
                         ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,var][mainReg,,,invert=TRUE],x_hist=histData[,,var][mainReg,,,invert=TRUE],
                         ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\subsubsection{Biomass}")
  var <- "Cap|Electricity|Biomass (GW)"
  p <- mipLineHistorical(data[mainReg,,var],x_hist=histData[mainReg,,var],
                         ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,var][mainReg,,,invert=TRUE],x_hist=histData[,,var][mainReg,,,invert=TRUE],
                         ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\subsubsection{Hydrogen}")
  var <- "Cap|Electricity|Hydrogen (GW)"
  p <- mipLineHistorical(data[mainReg,,var],x_hist=histData[mainReg,,var],
                         ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,var][mainReg,,,invert=TRUE],x_hist=histData[,,var][mainReg,,,invert=TRUE],
                         ylab=var,scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  

  ## ---- PE Mix ----

  ## ---- PE Mix Coal ----

  swlatex(sw,"\\subsubsection{PE|Coal}")

  items<- c ("PE|Coal|Gases (EJ/yr)",
             "PE|Coal|Liquids (EJ/yr)",
             "PE|Coal|Solids (EJ/yr)",
             "PE|Coal|Heat (EJ/yr)",
             "PE|Coal|Electricity (EJ/yr)",
             "PE|Coal|Hydrogen (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- PE Mix Gas ----

  swlatex(sw,"\\subsubsection{PE|Gas}")

  items<- c ("PE|Gas|Gases (EJ/yr)",
             "PE|Gas|Heat (EJ/yr)",
             "PE|Gas|Electricity|w/ CCS (EJ/yr)",
             "PE|Gas|Electricity|w/o CCS (EJ/yr)",
             "PE|Gas|Liquids|w/ CCS (EJ/yr)",
             "PE|Gas|Liquids|w/o CCS (EJ/yr)",
             "PE|Gas|Hydrogen|w/ CCS (EJ/yr)",
             "PE|Gas|Hydrogen|w/o CCS (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- PE Mix Biomass ----

  swlatex(sw,"\\subsubsection{PE|Biomass}")

  items<- c ("PE|Biomass|Solids (EJ/yr)",
             "PE|Biomass|Heat (EJ/yr)",
             "PE|Biomass|Liquids|w/ CCS (EJ/yr)",
             "PE|Biomass|Liquids|w/o CCS (EJ/yr)",
             "PE|Biomass|Gases (EJ/yr)",
             "PE|Biomass|Electricity|w/ CCS (EJ/yr)",
             "PE|Biomass|Electricity|w/o CCS (EJ/yr)",
             "PE|Biomass|Hydrogen|w/ CCS (EJ/yr)",
             "PE|Biomass|Hydrogen|w/o CCS (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- PE Line ----

  swlatex(sw,"\\subsection{Primary Energy line plots}")

  swlatex(sw,"\\subsubsection{PE|Coal}")
  p <- mipLineHistorical(data[mainReg,,"PE|Coal (EJ/yr)"],x_hist=histData[mainReg,,"PE|Coal (EJ/yr)"],
                         ylab='PE|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"PE|Coal (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"PE|Coal (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='PE|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsubsection{PE|Oil}")
  p <- mipLineHistorical(data[mainReg,,"PE|Oil (EJ/yr)"],x_hist=histData[mainReg,,"PE|Oil (EJ/yr)"],
                         ylab='PE|Oil [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"PE|Oil (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"PE|Oil (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='PE|Oil [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsubsection{PE|Gas}")
  p <- mipLineHistorical(data[mainReg,,"PE|Gas (EJ/yr)"],x_hist=histData[mainReg,,"PE|Gas (EJ/yr)"],
                         ylab='PE|Gas [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"PE|Gas (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"PE|Gas (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='PE|Gas [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsubsection{PE|Biomass}")
  p <- mipLineHistorical(data[mainReg,,"PE|Biomass (EJ/yr)"],x_hist=histData[mainReg,,"PE|Biomass (EJ/yr)"],
                         ylab='PE|Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"PE|Biomass (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"PE|Biomass (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='PE|Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsubsection{Primary Energy Production|Biomass|Energy Crops}")
  p <- mipLineHistorical(data[mainReg,,"Primary Energy Production|Biomass|Energy Crops (EJ/yr)"],x_hist=NULL,
                         ylab='Primary Energy Production|Biomass|Energy Crops [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Primary Energy Production|Biomass|Energy Crops (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Primary Energy Production|Biomass|Energy Crops [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\subsubsection{PE|Coal|Extraction}")
  p <- mipLineHistorical(data[mainReg,,"Res|Extraction|Coal (EJ/yr)"],x_hist=NULL,
                         ylab='Res|Extraction|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Res|Extraction|Coal (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Res|Extraction|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsubsection{PE|Oil|Extraction}")
  p <- mipLineHistorical(data[mainReg,,"Res|Extraction|Oil (EJ/yr)"],x_hist=NULL,
                         ylab='Res|Extraction|Oil [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Res|Extraction|Oil (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Res|Extraction|Oil [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\subsubsection{PE|Gas|Extraction}")
  p <- mipLineHistorical(data[mainReg,,"Res|Extraction|Gas (EJ/yr)"],x_hist=NULL,
                         ylab='Res|Extraction|GAs [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"Res|Extraction|Gas (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='Res|Extraction|Gas [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  ## ---- SE Mix ----

  swlatex(sw,"\\subsection{Secondary Energy Mixes}")

  ## ---- SE Mix Liquids ----

  swlatex(sw,"\\subsubsection{SE|Liquids}")

  items<- c ("SE|Liquids|Oil (EJ/yr)",
             "SE|Liquids|Biomass|w/ CCS (EJ/yr)",
             "SE|Liquids|Biomass|w/o CCS (EJ/yr)",
             "SE|Liquids|Coal|w/ CCS (EJ/yr)",
             "SE|Liquids|Coal|w/o CCS (EJ/yr)",
             "SE|Liquids|Gas|w/ CCS (EJ/yr)",
             "SE|Liquids|Gas|w/o CCS (EJ/yr)",
             "SE|Liquids|Hydrogen (EJ/yr)",
             "SE|Liquids|Hydrogen|Net Imports (EJ/yr)")
  
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- SE Mix Gases ----

  swlatex(sw,"\\subsubsection{SE|Gases}")

  items<- c ("SE|Gases|Natural Gas (EJ/yr)",
             "SE|Gases|Biomass|w/ CCS (EJ/yr)",
             "SE|Gases|Biomass|w/o CCS (EJ/yr)",
             "SE|Gases|Coal|w/ CCS (EJ/yr)",
             "SE|Gases|Coal|w/o CCS (EJ/yr)",
             "SE|Gases|Hydrogen (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- SE Mix Hydrogen ----

  swlatex(sw,"\\subsubsection{SE|Hydrogen}")

  items<- c ("SE|Hydrogen|Biomass|w/ CCS (EJ/yr)",
             "SE|Hydrogen|Biomass|w/o CCS (EJ/yr)",
             "SE|Hydrogen|Coal|w/ CCS (EJ/yr)",
             "SE|Hydrogen|Coal|w/o CCS (EJ/yr)",
             "SE|Hydrogen|Gas|w/ CCS (EJ/yr)",
             "SE|Hydrogen|Gas|w/o CCS (EJ/yr)",
             "SE|Hydrogen|Electricity|VRE Storage Electrolysis (EJ/yr)",
             "SE|Hydrogen|Electricity|Standard Electrolysis (EJ/yr)",
             "SE|Hydrogen|Net Imports (EJ/yr)")

  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")
  
  ## ---- SE Hydrogen Usage ----
  
  swlatex(sw,"\\subsubsection{SE|Hydrogen - Usage}")
  
  items<- c ("FE|Industry|Hydrogen (EJ/yr)",
             "FE|Buildings|Hydrogen (EJ/yr)",
             "FE|Transport|Hydrogen (EJ/yr)",
             "FE|CDR|Hydrogen (EJ/yr)",
             "SE|Hydrogen|used for electricity|normal turbines (EJ/yr)",
             "SE|Hydrogen|used for electricity|forced VRE turbines (EJ/yr)",
             "SE|Hydrogen|used for synthetic fuels|liquids (EJ/yr)",
             "SE|Hydrogen|used for synthetic fuels|gases (EJ/yr)")



  var <- data[,,intersect(items,getNames(data,dim=3))]
  
  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")
  
  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")
  
  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")
  
  swlatex(sw,"\\subsubsection{SE|Electricity - Usage}")
  
  items<- c ("SE|Electricity|used for H2|for synthetic fuels|gases (EJ/yr)",
             "SE|Electricity|used for H2|for synthetic fuels|liquids (EJ/yr)",
             "SE|Electricity|used for H2|direct FE H2 (EJ/yr)",
             "SE|Electricity|used for H2|VRE Storage (EJ/yr)",
             "SE|Electricity|used in Buildings (EJ/yr)",
             "SE|Electricity|used in Industry (EJ/yr)",
             "SE|Electricity|used in Transport (EJ/yr)",
             "SE|Electricity|used for CDR (EJ/yr)",
             "SE|Electricity|used for own consumption of energy system (EJ/yr)")
  
  
  
  
  var <- data[,,intersect(items,getNames(data,dim=3))]
  
  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")
  
  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")
  
  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")
  
  
  
  
  ## ---- SE Mix Solids ----

  swlatex(sw,"\\subsubsection{SE|Solids}")

  items<- c ("SE|Solids|Biomass (EJ/yr)",
             "SE|Solids|Traditional Biomass (EJ/yr)",
             "SE|Solids|Coal (EJ/yr)")
  var <- data[,,intersect(items,getNames(data,dim=3))]

  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")
  
  
  
  ## ---- SE Mix Heat ----
  
  swlatex(sw,"\\subsubsection{SE|Heat}")
  
  items<- c ("SE|Heat|Biomass (EJ/yr)",
             "SE|Heat|Coal (EJ/yr)",
             "SE|Heat|Gas (EJ/yr)",
             "SE|Heat|Geothermal (EJ/yr)")
  
  var <- data[,,intersect(items,getNames(data,dim=3))]
  
  p <- mipArea(var[mainReg,,],scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")
  
  p <- mipBarYearData(var[mainReg,y_bar,])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")
  
  p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE])
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\onecolumn")
  p <- mipArea(var[mainReg,,,invert=TRUE],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- SE Line ----

  swlatex(sw,"\\subsection{Secondary Energy line plots}")

  ## ---- SE Line Gases ----

  swlatex(sw,"\\subsubsection{SE|Gases}")

  p <- mipLineHistorical(data[mainReg,,"SE|Gases|Natural Gas (EJ/yr)"],x_hist=histData[mainReg,,"SE|Gases|Gas (EJ/yr)"],
                         ylab='SE|Gases|(Natural)Gas [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Gases|Natural Gas (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"SE|Gases|Gas (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Gases|(Natural)Gas [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Gases|Coal (EJ/yr)"],x_hist=histData[mainReg,,"SE|Gases|Coal (EJ/yr)"],
                         ylab='SE|Gases|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Gases|Coal (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"SE|Gases|Coal (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Gases|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Gases|Biomass (EJ/yr)"],x_hist=histData[mainReg,,"SE|Gases|Biomass (EJ/yr)"],
                         ylab='SE|Gases|Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Gases|Biomass (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"SE|Gases|Biomass (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Gases|Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- SE Line Electricity ----

  swlatex(sw,"\\subsubsection{SE|Electricity}")

  p <- mipLineHistorical(data[mainReg,,"SE|Electricity|Gas (EJ/yr)"],x_hist=histData[mainReg,,"SE|Electricity|Gas (EJ/yr)"],
                         ylab='SE|Electricity|Gas [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Electricity|Gas (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"SE|Electricity|Gas (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Electricity|Gas [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Electricity|Coal (EJ/yr)"],x_hist=histData[mainReg,,"SE|Electricity|Coal (EJ/yr)"],
                         ylab='SE|Electricity|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Electricity|Coal (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"SE|Electricity|Coal (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Electricity|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Electricity|Oil (EJ/yr)"],x_hist=histData[mainReg,,"SE|Electricity|Oil (EJ/yr)"],
                         ylab='SE|Electricity|Oil [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Electricity|Oil (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"SE|Electricity|Oil (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Electricity|Oil [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Electricity|Hydro (EJ/yr)"],x_hist=histData[mainReg,,"SE|Electricity|Hydro (EJ/yr)"],
                         ylab='SE|Electricity|Hydro [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Electricity|Hydro (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"SE|Electricity|Hydro (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Electricity|Hydro [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Electricity|Wind (EJ/yr)"],x_hist=histData[mainReg,,"SE|Electricity|Wind (EJ/yr)"],
                         ylab='SE|Electricity|Wind [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Electricity|Wind (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"SE|Electricity|Wind (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Electricity|Wind [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Electricity|Nuclear (EJ/yr)"],x_hist=histData[mainReg,,"SE|Electricity|Nuclear (EJ/yr)"],
                         ylab='SE|Electricity|Nuclear [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Electricity|Nuclear (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"SE|Electricity|Nuclear (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Electricity|Nuclear [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Electricity|Solar (EJ/yr)"],x_hist=histData[mainReg,,"SE|Electricity|Solar (EJ/yr)"],
                         ylab='SE|Electricity|Solar [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Electricity|Solar (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"SE|Electricity|Solar (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Electricity|Solar [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Electricity|Biomass (EJ/yr)"],x_hist=histData[mainReg,,"SE|Electricity|Biomass (EJ/yr)"],
                         ylab='SE|Electricity|Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Electricity|Biomass (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"SE|Electricity|Biomass (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Electricity|Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- SE Line Solids ----

  swlatex(sw,"\\subsubsection{SE|Solids}")

  p <- mipLineHistorical(data[mainReg,,"SE|Solids|Coal (EJ/yr)"],x_hist=histData[mainReg,,"SE|Solids|Coal (EJ/yr)"],
                         ylab='SE|Solids|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Solids|Coal (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"SE|Solids|Coal (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Solids|Coal [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Solids|Biomass (EJ/yr)"],x_hist=histData[mainReg,,"SE|Solids|Biomass (EJ/yr)"],
                         ylab='SE|Solids|Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Solids|Biomass (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"SE|Solids|Biomass (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Solids|Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"SE|Solids|Traditional Biomass (EJ/yr)"],x_hist=histData[mainReg,,"SE|Solids|Traditional Biomass (EJ/yr)"],
                         ylab='SE|Solids|Traditional Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"SE|Solids|Traditional Biomass (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"SE|Solids|Traditional Biomass (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='SE|Solids|Traditional Biomass [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ----  FE Line ----

  swlatex(sw,"\\subsection{Final Energy line plot}")

  ## ----  FE Line Total ----

  swlatex(sw,"\\subsubsection{Total}")

  p <- mipLineHistorical(data[mainReg,,"FE (EJ/yr)"],x_hist=histData[mainReg,,"FE (EJ/yr)"],
                         ylab='FE [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"FE (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- FE Line by Carrier ----

  swlatex(sw,"\\subsubsection{FE|Electricity}")

  p <- mipLineHistorical(data[mainReg,,"FE|Electricity (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Electricity (EJ/yr)"],
                         ylab='FE|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Electricity (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Electricity (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsubsection{FE|Gases}")

  p <- mipLineHistorical(data[mainReg,,"FE|Gases (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Gases (EJ/yr)"],
                         ylab='FE|Gases [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Gases (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Gases (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE|Gases [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsubsection{FE|Heat}")

  p <- mipLineHistorical(data[mainReg,,"FE|Heat (EJ/yr)"],x_hist=NULL,
                         ylab='FE|Heat [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Heat (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='FE|Heat [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsubsection{FE|Solids}")

  p <- mipLineHistorical(data[mainReg,,"FE|Solids (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Solids (EJ/yr)"],
                         ylab='FE|Solids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Solids (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Solids (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE|Solids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsubsection{FE|Liquids}")

  p <- mipLineHistorical(data[mainReg,,"FE|Liquids (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Liquids (EJ/yr)"],
                         ylab='FE|Liquids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Liquids (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Liquids (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE|Liquids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsubsection{FE|Hydrogen}")

  p <- mipLineHistorical(data[mainReg,,"FE|Hydrogen (EJ/yr)"],x_hist=NULL,
                         ylab='FE|Hydrogen [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Hydrogen (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='FE|Hydrogen [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- FE Line Buildings ----

  if("FE|Buildings (EJ/yr)" %in% magclass::getNames(data,dim=3)){
    swlatex(sw,"\\subsection{Buildings Final Energy}")
    p <- mipLineHistorical(data[mainReg,,"FE|Buildings (EJ/yr)"],x_hist=histData[mainReg,,"FE|Buildings (EJ/yr)"],
                           ylab='FE|Buildings [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Buildings (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"FE|Buildings (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Buildings [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    swlatex(sw,"\\subsubsection{per carrier}")
    
    p <- mipLineHistorical(data[mainReg,,"FE|Buildings|Electricity (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Buildings|Electricity (EJ/yr)"],
                           ylab='FE|Buildings|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Buildings|Electricity (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Buildings|Electricity (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Buildings|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(data[mainReg,,"FE|Buildings|Gases (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Buildings|Gases (EJ/yr)"],
                           ylab='FE|Buildings|Gases [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Buildings|Gases (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Buildings|Gases (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Buildings|Gases [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(data[mainReg,,"FE|Buildings|Heat (EJ/yr)"],x_hist=histData[mainReg,,"FE|Buildings|Heat (EJ/yr)"],
                           ylab='FE|Buildings|Heat [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Buildings|Heat (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"FE|Buildings|Heat (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Buildings|Heat [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(data[mainReg,,"FE|Buildings|Solids (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Buildings|Solids (EJ/yr)"],
                           ylab='FE|Buildings|Solids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Buildings|Solids (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Buildings|Solids (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Buildings|Solids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(data[mainReg,,"FE|Buildings|Liquids (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Buildings|Liquids (EJ/yr)"],
                           ylab='FE|Buildings|Liquids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Buildings|Liquids (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Buildings|Liquids (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Buildings|Liquids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    p <- mipLineHistorical(data[mainReg,,"FE|Buildings|Hydrogen (EJ/yr)"],,
                           ylab='FE|Buildings|Hydrogen [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Buildings|Hydrogen (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Buildings|Hydrogen [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

  }

  ## ---- FE Line Industry ----

  if("FE|Industry (EJ/yr)" %in% magclass::getNames(data,dim=3)){
    swlatex(sw,"\\subsection{Industry Final Energy}")

    p <- mipLineHistorical(data[mainReg,,"FE|Industry (EJ/yr)"],x_hist=histData[mainReg,,"FE|Industry (EJ/yr)"],
                           ylab='FE|Industry [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Industry (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"FE|Industry (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Industry [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    swlatex(sw,"\\subsubsection{per carrier}")
    
    p <- mipLineHistorical(data[mainReg,,"FE|Industry|Electricity (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Industry|Electricity (EJ/yr)"],
                           ylab='FE|Industry|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Industry|Electricity (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Industry|Electricity (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Industry|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(data[mainReg,,"FE|Industry|Gases (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Industry|Gases (EJ/yr)"],
                           ylab='FE|Industry|Gases [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Industry|Gases (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Industry|Gases (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Industry|Gases [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(data[mainReg,,"FE|Industry|Heat (EJ/yr)"],x_hist=histData[mainReg,,"FE|Industry|Heat (EJ/yr)"],
                           ylab='FE|Industry|Heat [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Industry|Heat (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"FE|Industry|Heat (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Industry|Heat [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(data[mainReg,,"FE|Industry|Solids (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Industry|Solids (EJ/yr)"],
                           ylab='FE|Industry|Solids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Industry|Solids (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Industry|Solids (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Industry|Solids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

    p <- mipLineHistorical(data[mainReg,,"FE|Industry|Liquids (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Industry|Liquids (EJ/yr)"],
                           ylab='FE|Industry|Liquids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Industry|Liquids (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Industry|Liquids (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Industry|Liquids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    p <- mipLineHistorical(data[mainReg,,"FE|Industry|Hydrogen (EJ/yr)"],x_hist=hist_edge[mainReg,,"FE|Industry|Hydrogen (EJ/yr)"],
                           ylab='FE|Industry|Hydrogen [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Industry|Hydrogen (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_edge[,,"FE|Industry|Hydrogen (EJ/yr)"][mainReg,,,invert=TRUE],
                           ylab='FE|Industry|Hydrogen [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")

  }
  
  # Industry subsectors
  
  items <- c("FE|Industry|Steel (EJ/yr)",
             "FE|Industry|Steel|Primary (EJ/yr)",
             "FE|Industry|Steel|Secondary (EJ/yr)",
             "FE|Industry|Cement (EJ/yr)",
             "FE|Industry|Chemicals (EJ/yr)",
             "FE|Industry|other (EJ/yr)"
  )
  
  if(all(c(items) %in% magclass::getNames(data,dim=3))){
    swlatex(sw, "\\subsubsection{per sector}")
    
    p <- mipLineHistorical(data[mainReg,,"FE|Industry|Steel (EJ/yr)"],x_hist=histData_NA[mainReg,,"FE|Industry|Steel (EJ/yr)"],
                           ylab='FE|Industry|Steel [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Industry|Steel (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData_NA[,,"FE|Industry|Steel (EJ/yr)"][c(mainReg, "GLO"),,,invert=TRUE],
                           ylab='FE|Industry|Steel [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    p <- mipLineHistorical(data[mainReg,,"FE|Industry|Steel|Primary (EJ/yr)"],x_hist=histData_NA[mainReg,,"FE|Industry|Steel|Primary (EJ/yr)"],
                           ylab='FE|Industry|Steel|Primary [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Industry|Steel|Primary (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData_NA[,,"FE|Industry|Steel|Primary (EJ/yr)"][c(mainReg, "GLO"),,,invert=TRUE],
                           ylab='FE|Industry|Steel|Primary [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    p <- mipLineHistorical(data[mainReg,,"FE|Industry|Steel|Secondary (EJ/yr)"],x_hist=histData_NA[mainReg,,"FE|Industry|Steel|Secondary (EJ/yr)"],
                           ylab='FE|Industry|Steel|Secondary [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Industry|Steel|Secondary (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData_NA[,,"FE|Industry|Steel|Secondary (EJ/yr)"][c(mainReg, "GLO"),,,invert=TRUE],
                           ylab='FE|Industry|Steel|Secondary [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    p <- mipLineHistorical(data[mainReg,,"FE|Industry|Cement (EJ/yr)"],x_hist=histData_NA[mainReg,,"FE|Industry|Cement (EJ/yr)"],
                           ylab='FE|Industry|Cement [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Industry|Cement (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData_NA[,,"FE|Industry|Cement (EJ/yr)"][c(mainReg, "GLO"),,,invert=TRUE],
                           ylab='FE|Industry|Cement [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    p <- mipLineHistorical(data[mainReg,,"FE|Industry|Chemicals (EJ/yr)"],x_hist=histData_NA[mainReg,,"FE|Industry|Chemicals (EJ/yr)"],
                           ylab='FE|Industry|Chemicals [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Industry|Chemicals (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData_NA[,,"FE|Industry|Chemicals (EJ/yr)"][c(mainReg, "GLO"),,,invert=TRUE],
                           ylab='FE|Industry|Chemicals [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    p <- mipLineHistorical(data[mainReg,,"FE|Industry|other (EJ/yr)"],x_hist=histData_NA[mainReg,,"FE|Industry|other (EJ/yr)"],
                           ylab='FE|Industry|other [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"FE|Industry|other (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData_NA[,,"FE|Industry|other (EJ/yr)"][c(mainReg, "GLO"),,,invert=TRUE],
                           ylab='FE|Industry|other [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
  }

  ## ---- FE Line Transport ----

  swlatex(sw,"\\subsection{Transport Final Energy}")
  p <- mipLineHistorical(data[mainReg,,"FE|Transport (EJ/yr)"],x_hist=histData[mainReg,,"FE|Transport w/ Bunkers (EJ/yr)"],
                         ylab='FE|Transport [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Transport (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"FE|Transport w/ Bunkers (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE|Transport [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"FE|Transport|w/o Bunkers (EJ/yr)"],x_hist=histData[mainReg,,"FE|Transport (EJ/yr)"],
                         ylab='FE|Transport|w/o Bunkers [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Transport|w/o Bunkers (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"FE|Transport (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE|Transport|w/o Bunkers [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  # Transport per type
  
  items<- c(
    "FE|Transport|LDV|Liquids (EJ/yr)",
    "FE|Transport|LDV|Gases (EJ/yr)",
    "FE|Transport|LDV|Electricity (EJ/yr)",
    "FE|Transport|LDV|Hydrogen (EJ/yr)",
    "FE|Transport|non-LDV|w/o Bunkers|Liquids (EJ/yr)",
    "FE|Transport|non-LDV|w/o Bunkers|Gases (EJ/yr)",
    "FE|Transport|non-LDV|w/o Bunkers|Electricity (EJ/yr)",
    "FE|Transport|non-LDV|w/o Bunkers|Hydrogen (EJ/yr)",
    "FE|Transport|non-LDV|Bunkers|Liquids (EJ/yr)",
    "FE|Transport|non-LDV|Bunkers|Gases (EJ/yr)",
    "FE|Transport|non-LDV|Bunkers|Electricity (EJ/yr)",
    "FE|Transport|non-LDV|Bunkers|Hydrogen (EJ/yr)"
  )

  if(all(c(items) %in% getNames(data,dim=3))){
    
    swlatex(sw,"\\subsubsection{Transport per type}")
    
    tot <-"FE|Transport (EJ/yr)"
    
    var <- data[,,intersect(items,getNames(data,dim=3))]
    
    p <- mipArea(var[mainReg,,],total=data[mainReg,,tot],scales="free_y")
    p <- p + theme(legend.position="none")
    swfigure(sw,print,p,sw_option="height=3.5,width=7")
    
    p <- mipBarYearData(var[mainReg,y_bar,])
    p <- p + theme(legend.position="none")
    swfigure(sw,print,p,sw_option="height=4.5,width=7")
    
    p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE]) +
      guides(fill=guide_legend(ncol=3))
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    swlatex(sw,"\\onecolumn")
    p <- mipArea(var[mainReg,,,invert=TRUE],total=data[,,tot][mainReg,,,invert=TRUE],scales="free_y")
    swfigure(sw,print,p,sw_option="height=8,width=16")
    swlatex(sw,"\\twocolumn")
    
  }

  swlatex(sw,"\\subsubsection{per carrier with Bunkers}")
  
  hist_bunkers <- setNames(histData[,,"Eurostat.FE|Transport w/ Bunkers (EJ/yr)"] - histData[,,"Eurostat.FE|Transport (EJ/yr)"],"historical.Eurostat.FE|Transport|Bunkers (EJ/yr)")
  hist_bunkers <- mbind(histData, hist_bunkers)
  
  p <- mipLineHistorical(data[mainReg,,"FE|Transport|Bunkers (EJ/yr)"],x_hist=hist_bunkers[mainReg,,"FE|Transport|Bunkers (EJ/yr)"],
                         ylab='FE|Transport|Bunkers [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Transport|Bunkers (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=hist_bunkers[,,"FE|Transport|Bunkers (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE|Transport|Bunkers [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"FE|Transport|Liquids (EJ/yr)"],x_hist=histData[mainReg,,"FE|Transport|Liquids w/ Bunkers (EJ/yr)"],
                         ylab='FE|Transport|Liquids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Transport|Liquids (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"FE|Transport|Liquids w/ Bunkers (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE|Transport|Liquids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"FE|Transport|Gases (EJ/yr)"],x_hist=histData[mainReg,,"FE|Transport|Gases (EJ/yr)"],
                         ylab='FE|Transport|Gases [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Transport|Gases (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"FE|Transport|Gases (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE|Transport|Gases [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"FE|Transport|Electricity (EJ/yr)"],x_hist=histData[mainReg,,"FE|Transportation|Electricity (EJ/yr)"],
                         ylab='FE|Transport|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Transport|Electricity (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"FE|Transportation|Electricity (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE|Transport|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"FE|Transport|Hydrogen (EJ/yr)"],x_hist=NULL,
                         ylab='FE|Transport|Hydrogen [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Transport|Hydrogen (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='FE|Transport|Hydrogen [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\subsubsection{per carrier Without Bunkers}")
  
  p <- mipLineHistorical(data[mainReg,,"FE|Transport|w/o Bunkers|Liquids (EJ/yr)"],x_hist=histData[mainReg,,"FE|Transport|Liquids (EJ/yr)"],
                         ylab='FE|Transport|Liquids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Transport|w/o Bunkers|Liquids (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"FE|Transport|Liquids (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE|Transport|Liquids [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"FE|Transport|w/o Bunkers|Gases (EJ/yr)"],x_hist=histData[mainReg,,"FE|Transport|Gases (EJ/yr)"],
                         ylab='FE|Transport|Gases [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Transport|w/o Bunkers|Gases (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"FE|Transport|Gases (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE|Transport|Gases [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"FE|Transport|w/o Bunkers|Electricity (EJ/yr)"],x_hist=histData[mainReg,,"FE|Transportation|Electricity (EJ/yr)"],
                         ylab='FE|Transport|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Transport|w/o Bunkers|Electricity (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=histData[,,"FE|Transportation|Electricity (EJ/yr)"][mainReg,,,invert=TRUE],
                         ylab='FE|Transport|Electricity [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  p <- mipLineHistorical(data[mainReg,,"FE|Transport|w/o Bunkers|Hydrogen (EJ/yr)"],x_hist=NULL,
                         ylab='FE|Transport|Hydrogen [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(data[,,"FE|Transport|w/o Bunkers|Hydrogen (EJ/yr)"][mainReg,,,invert=TRUE],x_hist=NULL,
                         ylab='FE|Transport|Hydrogen [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")


  ## ---- ++++ ENERGY SERVICES ++++ ----

  swlatex(sw,"\\twocolumn")
  
  swlatex(sw,"\\section{Energy Services and Products}")
  
  ## Buildings
  items<- c(
    "FE|Buildings|non-Heating|Electricity|Conventional (EJ/yr)",
    "FE|Buildings|Heating|Electricity|Resistance (EJ/yr)",
    "FE|Buildings|Heating|Electricity|Heat pumps (EJ/yr)",
    "FE|Buildings|Heating|District Heating (EJ/yr)",
    "FE|Buildings|Heating|Solids (EJ/yr)",
    "FE|Buildings|Heating|Liquids (EJ/yr)",
    "FE|Buildings|Heating|Gases (EJ/yr)",
    "FE|Buildings|Heating|Hydrogen (EJ/yr)"
  )
  
  if(all(c(items) %in% getNames(data,dim=3))){
    
    swlatex(sw,"\\subsection{Buildings}")
    
    tot <-"FE|Buildings (EJ/yr)"
    
    var <- data[,,intersect(items,getNames(data,dim=3))]
    
    p <- mipArea(var[mainReg,,],total=data[mainReg,,tot],scales="free_y")
    p <- p + theme(legend.position="none")
    swfigure(sw,print,p,sw_option="height=3.5,width=7")
    
    p <- mipBarYearData(var[mainReg,y_bar,])
    p <- p + theme(legend.position="none")
    swfigure(sw,print,p,sw_option="height=4.5,width=7")
    
    p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE]) +
      guides(fill=guide_legend(ncol=3))
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    swlatex(sw,"\\onecolumn")
    p <- mipArea(var[mainReg,,,invert=TRUE],total=data[,,tot][mainReg,,,invert=TRUE],scales="free_y")
    swfigure(sw,print,p,sw_option="height=8,width=16")
    swlatex(sw,"\\twocolumn")
  
  }
  
  ## Transport
  
  items<- c(
    "ES|Transport|Pass (bn pkm/yr)",
    "ES|Transport|Pass|Road|LDV (bn pkm/yr)",
    "ES|Transport|Pass|non-LDV (bn pkm/yr)",
    "ES|Transport|Freight (bn tkm/yr)",
    "Est LDV Stock (million vehicles)",
    "Est EV LDV Stock (million vehicles)",
    "Est H2 LDV Stock (million vehicles)",
    "Est ICE LDV Stock (million vehicles)",
    "Est LDV Sales (million vehicles)",
    "Est EV LDV Sales (million vehicles)",
    "Est H2 LDV Sales (million vehicles)",
    "Est ICE LDV Sales (million vehicles)"
  )
  
    
  if(all(c(items) %in% getNames(data,dim=3))){
    
    swlatex(sw,"\\subsection{Transport}")
  
    swlatex(sw,"\\onecolumn")
  
    ## ---- ES passenger transport per capita (time domain, line graph)----
  
    swlatex(sw,"\\subsubsection{Energy Services for Passenger Transport (per Capita, year)}")
     
    items<- c(
      "ES|Transport|Pass (bn pkm/yr)",
      "ES|Transport|Pass|Road|LDV (bn pkm/yr)",
      "ES|Transport|Pass|non-LDV (bn pkm/yr)"
    )
    
    p <- lineplots_perCap(data, items, 1e3, "Mobility Demand per Cap. (km/yr)",
                          global = T, per_gdp = F)
    swfigure(sw,print,p,sw_option="height=9,width=16")
    
    p <- lineplots_perCap(data, items, 1e3, "Mobility Demand per Cap. (km/yr)",
                          global = F, per_gdp = F)
    swfigure(sw,print,p,sw_option="height=9,width=16")
    
    
    ## ---- ES per capita for transport (GDP domain)----
    
    swlatex(sw,"\\subsubsection{Energy Services for Transport (per Capita, GDP)}")
    
    p <- lineplots_perCap(data, items, 1e3, "Mobility Demand per Cap. (km/yr)",
                          global = T, per_gdp = T)
    swfigure(sw,print,p,sw_option="height=9,width=16")
    
    p <- lineplots_perCap(data, items, 1e3, "Mobility Demand per Cap. (km/yr)",
                          global = F, per_gdp = T)
    swfigure(sw,print,p,sw_option="height=9,width=16")
   
    ## ---- ES freight transport per capita (time domain, line graph)----
    
    swlatex(sw,"\\subsubsection{Energy Services for Freight Transport (per Capita, year)}")
    
    p <- lineplots_perCap(data, "ES|Transport|Freight (bn tkm/yr)", 1e3, "Freight Demand per Cap. (tkm/yr)",
                          global = T, per_gdp = F)
    
    swfigure(sw,print,p,sw_option="height=9,width=16")
    
    p <- lineplots_perCap(data, "ES|Transport|Freight (bn tkm/yr)", 1e3, "Freight Demand per Cap. (tkm/yr)",
                          global = F, per_gdp = F)
    
    swfigure(sw,print,p,sw_option="height=9,width=16")

    ## ---- ES per capita for transport (GDP domain)----
    
    
    p <- lineplots_perCap(data, "ES|Transport|Freight (bn tkm/yr)", 1e3, "Freight Demand per Cap. (tkm/yr)",
                          global = T, per_gdp = T)
    swfigure(sw,print,p,sw_option="height=9,width=16")
    
    p <- lineplots_perCap(data, "ES|Transport|Freight (bn tkm/yr)", 1e3, "Freight Demand per Cap. (tkm/yr)",
                          global = F, per_gdp = T)
    
    swfigure(sw,print,p,sw_option="height=9,width=16")
      
    swlatex(sw,"\\twocolumn")
  
    # Vehicles Stock
    
    swlatex(sw,"\\subsubsection{LDV Vehicles Stock}")
    
    tot <-"Est LDV Stock (million vehicles)"
    
    items <- c("Est EV LDV Stock (million vehicles)",
               "Est H2 LDV Stock (million vehicles)",
               "Est ICE LDV Stock (million vehicles)")
    var <- data[,,intersect(items,getNames(data,dim=3))]
    
    p <- mipArea(var[mainReg,,],total=data[mainReg,,tot],scales="free_y")
    p <- p + theme(legend.position="none")
    swfigure(sw,print,p,sw_option="height=3.5,width=7")
    
    p <- mipBarYearData(var[mainReg,y_bar,])
    p <- p + theme(legend.position="none")
    swfigure(sw,print,p,sw_option="height=4.5,width=7")
    
    p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE]) +
      guides(fill=guide_legend(ncol=3))
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    swlatex(sw,"\\onecolumn")
    p <- mipArea(var[mainReg,,,invert=TRUE],total=data[,,tot][mainReg,,,invert=TRUE],scales="free_y")
    swfigure(sw,print,p,sw_option="height=8,width=16")
    swlatex(sw,"\\twocolumn")

    # Sales
    
    swlatex(sw,"\\subsubsection{LDV Vehicles Sales}")
    
    tot <-    "Est LDV Sales (million vehicles)"
    
    items <- c("Est EV LDV Sales (million vehicles)",
               "Est H2 LDV Sales (million vehicles)",
               "Est ICE LDV Sales (million vehicles)")
    var <- data[,,intersect(items,getNames(data,dim=3))]
    
    p <- mipArea(var[mainReg,,],total=data[mainReg,,tot],scales="free_y")
    p <- p + theme(legend.position="none")
    swfigure(sw,print,p,sw_option="height=3.5,width=7")
    
    p <- mipBarYearData(var[mainReg,y_bar,])
    p <- p + theme(legend.position="none")
    swfigure(sw,print,p,sw_option="height=4.5,width=7")
    
    p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE]) +
      guides(fill=guide_legend(ncol=3))
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    swlatex(sw,"\\onecolumn")
    p <- mipArea(var[mainReg,,,invert=TRUE],total=data[,,tot][mainReg,,,invert=TRUE],scales="free_y")
    swfigure(sw,print,p,sw_option="height=8,width=16")
    swlatex(sw,"\\twocolumn")    
    
  }

  ## ---- non-LDV ----
  
  items<- c(
    "Services and Products|Transport|non-LDV|Stock|uedit (NA)",
    "Services and Products|Transport|non-LDV|Stock|apCarDiT (NA)",
    "Services and Products|Transport|non-LDV|Stock|apcarDiEffT (NA)",
    "Services and Products|Transport|non-LDV|Stock|apcarDiEffH2T (NA)",
    "Services and Products|Transport|non-LDV|Sales|uedit (NA)",
    "Services and Products|Transport|non-LDV|Sales|apCarDiT (NA)",
    "Services and Products|Transport|non-LDV|Sales|apcarDiEffT (NA)",
    "Services and Products|Transport|non-LDV|Sales|apcarDiEffH2T (NA)"
  )
  
  if(all(c(items) %in% getNames(data,dim=3))){
    
    # Freight Vehicles Stock
    
    swlatex(sw,"\\subsubsection{non-LDV Vehicles Stock}")
    
    tot <-"Services and Products|Transport|non-LDV|Stock|uedit (NA)"
    
    items <- c("Services and Products|Transport|non-LDV|Stock|apCarDiT (NA)",
               "Services and Products|Transport|non-LDV|Stock|apcarDiEffT (NA)",
               "Services and Products|Transport|non-LDV|Stock|apcarDiEffH2T (NA)")
    var <- data[,,intersect(items,getNames(data,dim=3))]
    
    p <- mipArea(var[mainReg,,],total=data[mainReg,,tot],scales="free_y")
    p <- p + theme(legend.position="none")
    swfigure(sw,print,p,sw_option="height=3.5,width=7")
    
    p <- mipBarYearData(var[mainReg,y_bar,])
    p <- p + theme(legend.position="none")
    swfigure(sw,print,p,sw_option="height=4.5,width=7")
    
    p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE]) +
      guides(fill=guide_legend(ncol=3))
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    swlatex(sw,"\\onecolumn")
    p <- mipArea(var[mainReg,,,invert=TRUE],total=data[,,tot][mainReg,,,invert=TRUE],scales="free_y")
    swfigure(sw,print,p,sw_option="height=8,width=16")
    swlatex(sw,"\\twocolumn")
    
    # HDV Sales
    
    swlatex(sw,"\\subsubsection{non-LDV Vehicles Sales}")
    
    tot <-    "Services and Products|Transport|non-LDV|Sales|uedit (NA)"
    
    items <- c("Services and Products|Transport|non-LDV|Sales|apCarDiT (NA)",
               "Services and Products|Transport|non-LDV|Sales|apcarDiEffT (NA)",
               "Services and Products|Transport|non-LDV|Sales|apcarDiEffH2T (NA)")
    var <- data[,,intersect(items,getNames(data,dim=3))]
    
    p <- mipArea(var[mainReg,,],total=data[mainReg,,tot],scales="free_y")
    p <- p + theme(legend.position="none")
    swfigure(sw,print,p,sw_option="height=3.5,width=7")
    
    p <- mipBarYearData(var[mainReg,y_bar,])
    p <- p + theme(legend.position="none")
    swfigure(sw,print,p,sw_option="height=4.5,width=7")
    
    p <- mipBarYearData(var[,y_bar,][mainReg,,,invert=TRUE]) +
      guides(fill=guide_legend(ncol=3))
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    swlatex(sw,"\\onecolumn")
    p <- mipArea(var[mainReg,,,invert=TRUE],total=data[,,tot][mainReg,,,invert=TRUE],scales="free_y")
    swfigure(sw,print,p,sw_option="height=8,width=16")
    swlatex(sw,"\\twocolumn")    
    
  }
  
  ## Industry Production
  
  items <- c(
    "Production|Industry|Steel (Mt/yr)",
    "Production|Industry|Steel|Primary (Mt/yr)",
    "Production|Industry|Steel|Secondary (Mt/yr)",
    "Production|Industry|Cement (Mt/yr)",
    "Value Added|Industry|Chemicals (billion US$2005/yr)"
  )
  
  if(all(c(items) %in% magclass::getNames(data,dim=3))){
    swlatex(sw, "\\subsection{Industry Production}")
    swlatex(sw, "\\subsubsection{per sector}")
    
    p <- mipLineHistorical(data[mainReg,,"Production|Industry|Steel (Mt/yr)"],x_hist=histData_NA[mainReg,,"Production|Industry|Steel (Mt/yr)"],
                           ylab='Production|Industry|Steel [Mt/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Production|Industry|Steel (Mt/yr)"][mainReg,,,invert=TRUE],x_hist=histData_NA[,,"Production|Industry|Steel (Mt/yr)"][c(mainReg, "GLO"),,,invert=TRUE],
                           ylab='Production|Industry|Steel [Mt/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    p <- mipLineHistorical(data[mainReg,,"Production|Industry|Steel|Primary (Mt/yr)"],x_hist=histData_NA[mainReg,,"Production|Industry|Steel|Primary (Mt/yr)"],
                           ylab='Production|Industry|Steel|Primary [Mt/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Production|Industry|Steel|Primary (Mt/yr)"][mainReg,,,invert=TRUE],x_hist=histData_NA[,,"Production|Industry|Steel|Primary (Mt/yr)"][c(mainReg, "GLO"),,,invert=TRUE],
                           ylab='Production|Industry|Steel|Primary [Mt/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    p <- mipLineHistorical(data[mainReg,,"Production|Industry|Steel|Secondary (Mt/yr)"],x_hist=histData_NA[mainReg,,"Production|Industry|Steel|Secondary (Mt/yr)"],
                           ylab='Production|Industry|Steel|Secondary [Mt/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Production|Industry|Steel|Secondary (Mt/yr)"][mainReg,,,invert=TRUE],x_hist=histData_NA[,,"Production|Industry|Steel|Secondary (Mt/yr)"][c(mainReg, "GLO"),,,invert=TRUE],
                           ylab='Production|Industry|Steel|Secondary [Mt/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    p <- mipLineHistorical(data[mainReg,,"Production|Industry|Cement (Mt/yr)"],x_hist=histData_NA[mainReg,,"Production|Industry|Cement (Mt/yr)"],
                           ylab='Production|Industry|Cement [Mt/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Production|Industry|Cement (Mt/yr)"][mainReg,,,invert=TRUE],x_hist=histData_NA[,,"Production|Industry|Cement (Mt/yr)"][c(mainReg, "GLO"),,,invert=TRUE],
                           ylab='Production|Industry|Cement [Mt/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
    
    p <- mipLineHistorical(data[mainReg,,"Value Added|Industry|Chemicals (billion US$2005/yr)"],x_hist=histData_NA[mainReg,,"Value Added|Industry|Chemicals (billion US$2005/yr)"],
                           ylab="Value Added|Industry|Chemicals (billion US$2005/yr)",scales="free_y",plot.priority=c("x_hist","x","x_proj"))
    swfigure(sw,print,p,sw_option="height=8,width=8")
    p <- mipLineHistorical(data[,,"Value Added|Industry|Chemicals (billion US$2005/yr)"][mainReg,,,invert=TRUE],x_hist=histData_NA[,,"Value Added|Industry|Chemicals (billion US$2005/yr)"][c(mainReg, "GLO"),,,invert=TRUE],
                           ylab="Value Added|Industry|Chemicals (billion US$2005/yr)",scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
    swfigure(sw,print,p,sw_option="height=9,width=8")
  }
  
  
  ## ---- ++++ C L I M A T E ++++ ----

  if(all(c("Forcing (W/m2)","Temperature|Global Mean [K]") %in% getNames(data,dim=3))){
    
    swlatex(sw,"\\section{Climate}")
  
    swfigure(sw,mipLineHistorical,data[,,"Forcing (W/m2)"],x_hist=NULL,
             ylab='Forcing [W/m2]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")
  
    swfigure(sw,mipLineHistorical,data[,,"Temperature|Global Mean (K)"],x_hist=NULL,
             ylab='Temperature|Global Mean [K]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),sw_option="height=10,width=9")
  }
  
  ## Close output-pdf
  swclose(sw)
}
