


#SDG|SDG07|Intensity|GDP|UE
addIntensityUE <- function(output){
  
 
  output <- mbind(
    output,
    setNames(
      ((( output[,,"UE|per capita|Transport (GJ/cap/yr)"] + output[,,"UE|per capita|Industry (GJ/cap/yr)"]) / 1e3 *output[,,"Population (million)"]) +output[,,"UE|Buildings (EJ/yr)"] * 1e12) / (output[,,"GDP|MER (billion US$2005/yr)"] * 1e9),
      "Intensity|GDP|UE (MJ/US$2005's (EJ/yr))"))
 
  return(output)  
  }