# TODO: document/comment functions in this file



calacuateRatio <- function(data, numerators, denominator, newUnit="1", conversionFactor=1) {
  data %>% 
    filter(variable == denominator) %>% 
    rename(denom_value = value) %>% 
    select(model, scenario, region, period, denom_value) ->
    denom
  data %>% 
    filter(variable %in% numerators) %>% 
    left_join(denom) %>% 
    mutate(
      value = value / denom_value * conversionFactor,
      unit = newUnit)
}


showAreaAndBarPlots <- function(data, items, tot=NULL, fill=FALSE) {
  # This function uses the 'global' variables: mainReg, yearsBarPlot.
  
  if (fill) {
    if (is.null(tot)) 
      stop("fill=TRUE without explicit tot variable is not implemented yet")
    data %>% 
      calacuateRatio(items, tot) ->
      data
    tot <- NULL
  }
  
  data %>% 
    filter(variable %in% items, scenario != "historical") ->
    d
  if (NROW(d) == 0) {
    warning("Nothing to plot:", paste(items, collapse=","))
    return(invisible(NULL))
  }
  
  # order variables by mean value
  d %>% 
    mutate(variable = fct_reorder(variable, value, mean, na.rm=TRUE)) ->
    d
  
  # Create plots.
  
  d %>% 
    filter(region == mainReg) %>% 
    mipArea(scales="free_y", total = is.null(tot)) + 
    theme(legend.position="none") ->
    p1
  if (!is.null(tot)) {
    p1 <- p1 +
      geom_line(
        data = data %>% 
          filter(region == mainReg, variable == tot, scenario != "historical"), 
        mapping = aes(period, value),
        size=1.3)
  }
  
  d %>% 
    filter(region == mainReg, period %in% yearsBarPlot) %>% 
    mipBarYearData() +
    theme(legend.position="none") ->
    p2
  
  d %>% 
    filter(region != mainReg, period %in% yearsBarPlot) %>% 
    mipBarYearData() +
    guides(fill=guide_legend(reverse=TRUE, ncol=3)) ->
    p3
  
  d %>% 
    filter(region != mainReg) %>% 
    mipArea(scales="free_y", total = is.null(tot)) +
    guides(fill=guide_legend(reverse=TRUE)) ->
    p4
  
  # Show plots.
  grid.arrange(p1, p2, p3, layout_matrix = rbind(c(1, 3), c(2, 3)))
  plot(p4)
  
  return(invisible(NULL))
}


showLinePlots <- function(data, filterVars=NULL, scales="free_y") {
  # This function uses the 'global' variables: mainReg
  
  if (!is.null(filterVars)) {
    d <- filter(data, variable %in% filterVars)
    label <- paste0(filterVars, " [", paste0(unique(d$unit), collapse=","), "]")
  } else {
    d <- data
    label <- paste0(
      paste0(unique(d$variable), collapse=","), 
      " [", paste0(unique(d$unit), collapse=","), "]")
  }
  d %>% 
    filter(region == mainReg, scenario != "historical") ->
    dMainScen
  d %>% 
    filter(region == mainReg, scenario == "historical") ->
    dMainHist
  d %>% 
    filter(region != mainReg, scenario != "historical") ->
    dRegiScen
  d %>% 
    filter(region != mainReg, scenario == "historical") ->
    dRegiHist
  
  if (NROW(dMainScen) == 0 && NROW(dRegiScen) == 0) {
    warning("Nothing to plot:", paste(filterVars, collapse=","))
    return(invisible(NULL))
  }
  if (NROW(dMainScen) == 0) {
    p1 <- ggplot() + theme_minimal()
  } else {
   dMainScen %>% 
      mipLineHistorical(
        x_hist = d %>% filter(region == mainReg, scenario == "historical"),
        ylab = label,
        scales = scales,
        plot.priority = c("x_hist","x","x_proj")) ->
      p1
  }
  if (NROW(dRegiScen) == 0) {
    p2 <- ggplot() + theme_minimal()
  } else {
    dRegiScen %>% 
      mipLineHistorical(
        x_hist = d %>% filter(region != mainReg, scenario == "historical"),
        ylab = NULL,
        scales = scales,
        plot.priority = c("x_hist","x","x_proj"),
        facet.ncol = 3) ->
      p2
  }
 
  grid.arrange(p1, p2, nrow=1)
  
  return(invisible(NULL))
}


showLinePlotsWithTarget <- function(data, filterVars) {
  filterVars %>% 
    paste0("|target|") %>% 
    str_replace_all(fixed("|"), fixed("\\|")) %>% 
    paste0(collapse="|") ->
    target_pattern
  
  data %>% 
    filter(str_detect(variable, target_pattern)) ->
    dTar
  regionsWithTarget <- unique(dTar$region)
  data %>% 
    filter(variable %in% filterVars, region %in% regionsWithTarget) ->
    d
  if (NROW(d) == 0) {
    warning("Nothing to plot:", paste(filterVars, collapse=","))
    return(invisible(NULL))
  }
  
  label <- paste0(filterVars, " [", paste0(unique(d$unit), collapse=","), "]")
  d %>% 
    filter(scenario != "historical") %>% 
    mipLineHistorical(
      x_hist = d %>% filter(scenario == "historical"),
      ylab = label,
      scales = "free_y",
      plot.priority = c("x_hist","x","x_proj"),
      facet.ncol = 3) + 
    geom_hline(data = dTar, aes(yintercept=value), linetype=2, color = "coral") +
    geom_vline(data = dTar, aes(xintercept=period), linetype=2, color = "coral") +
    geom_text(data = dTar, aes(x=max(d$period) - (max(d$period) - min(d$period)) / 4, y=value, label = paste(variable, period))) ->
    p
  print(p)
  
  return(invisible(NULL))
}


showMultiLinePlots <- function(data, items) {
  # This function uses the 'global' variables: mainReg, regions.
  
  data %>% 
    filter(variable %in% items) ->
    d
  
  label <- paste0("[", paste0(unique(d$unit), collapse=","), "]")
  
  d %>% 
    filter(region == mainReg, scenario != "historical") %>% 
    ggplot(aes(period, value)) + 
    geom_line(aes(linetype=scenario)) +
    geom_point(
      data = d %>% filter(region == mainReg, scenario == "historical"), 
      aes(shape = model)) +
    theme_minimal() +
    ylab(label) ->
    p1
  
  d %>% 
    filter(region != mainReg, scenario != "historical") %>% 
    ggplot(aes(period, value, color=region)) + 
    geom_line(aes(linetype=scenario)) +
    geom_point(
      data = d %>% filter(region != mainReg, scenario == "historical"), 
      aes(shape = model)) +
    theme_minimal() +
    scale_color_manual(values = plotstyle(regions)) + 
    ylab(label) ->
    p2
  
  if (length(unique(d$variable)) > 1) {
    p1 <- p1 + facet_wrap(vars(variable), scales="free_y")
    p2 <- p2 + facet_wrap(vars(variable), scales="free_y")
  }
  
  print(p1)
  print(p2)
  
  return(invisible(NULL))
}


showMultiLinePlotsByGDP <- function(data, items) {
  # This function uses the 'global' variables: mainReg.
  
  data %>% 
    filter(variable %in% items) ->
    d
  
  label <- paste0("[", paste0(unique(d$unit), collapse=","), "]")
  
  d %>% 
    filter(region == mainReg, scenario != "historical") %>% 
    ggplot(aes(gdp, value)) + 
    geom_line(aes(linetype=scenario)) +
    geom_point(
      data = d %>% filter(region == mainReg, scenario == "historical"), 
      aes(shape = model)) +
    theme_minimal() +
    ylab(label) + xlab("GDP PPP pCap (kUS$2005)") ->
    p1
  
  d %>% 
    filter(region != mainReg, scenario != "historical") %>% 
    ggplot(aes(gdp, value, color=region)) + 
    geom_line(aes(linetype=scenario)) +
    geom_point(
      data = d %>% filter(region != mainReg, scenario == "historical"), 
      aes(shape = model)) +
    theme_minimal() +
    ylab(label) + xlab("GDP PPP pCap (kUS$2005)") ->
    p2
  
  if (length(unique(d$variable)) > 1) {
    p1 <- p1 + facet_wrap(vars(variable), scales="free_y")
    p2 <- p2 + facet_wrap(vars(variable), scales="free_y")
  }
  
  print(p1)
  print(p2)
  
  return(invisible(NULL))
}

