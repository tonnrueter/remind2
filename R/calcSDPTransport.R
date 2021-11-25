## calculating the UE from the FEs manually is not required anymore
  ## once the UE variable in REMIND is reporting real UE -- AD 17-09-20

addUEperCapForTransport <- function(output){
  

   
  
      output <- mbind(
        output,
        setNames(
          1e3 * (output[,,"FE|Transport|Pass|+|Liquids (EJ/yr)"] * 0.23
                 + output[,,"FE|Transport|Pass|+|Electricity (EJ/yr)"] * 0.64
                 + output[,,"FE|Transport|Pass|+|Hydrogen (EJ/yr)"] * 0.36)
          / output[,,"Population (million)"],
          "UE|per capita|Transport|Pass (GJ/cap/yr)"),
        setNames(
          1e3 * (output[,,"FE|Transport|+|Liquids (EJ/yr)"] * 0.23
                 + output[,,"FE|Transport|+|Electricity (EJ/yr)"] * 0.64
                 + output[,,"FE|Transport|+|Hydrogen (EJ/yr)"] * 0.36)
          / output[,,"Population (million)"],
          "UE|per capita|Transport (GJ/cap/yr)"),
        setNames(
          (output[,,"FE|Transport|+|Liquids (EJ/yr)"]
           + output[,,"FE|Transport|+|Electricity (EJ/yr)"]
           + output[,,"FE|Transport|+|Hydrogen (EJ/yr)"])
          / (output[,,"FE|Transport|+|Liquids (EJ/yr)"] * 0.23
             + output[,,"FE|Transport|+|Electricity (EJ/yr)"] * 0.64
             + output[,,"FE|Transport|+|Hydrogen (EJ/yr)"] * 0.36),
          "Intensity|Final Energy|Useful Energy|Transport (-)"),
        setNames(
          1e2*(output[,,"FE|Transport|+|Electricity (EJ/yr)"] * 0.64
               + output[,,"FE|Transport|+|Hydrogen (EJ/yr)"] * 0.36)
          / (output[,,"FE|Transport|+|Liquids (EJ/yr)"] * 0.23
             + output[,,"FE|Transport|+|Electricity (EJ/yr)"] * 0.64
             + output[,,"FE|Transport|+|Hydrogen (EJ/yr)"] * 0.36),
          "UE|Electricity and Hydrogen|Share|Transport (%)"),
        setNames(
          1e2*(output[,,"FE|Transport|Pass|+|Electricity (EJ/yr)"] * 0.64
               + output[,,"FE|Transport|Pass|+|Hydrogen (EJ/yr)"] * 0.36)
          / (output[,,"FE|Transport|Pass|+|Liquids (EJ/yr)"] * 0.23
             + output[,,"FE|Transport|Pass|+|Electricity (EJ/yr)"] * 0.64
             + output[,,"FE|Transport|Pass|+|Hydrogen (EJ/yr)"] * 0.36),
          "UE|Electricity and Hydrogen|Share|Transport|Pass (%)"),
        setNames(
          1e3*(output[,,"FE|Transport|+|Electricity (EJ/yr)"]
               + output[,,"FE|Transport|+|Liquids (EJ/yr)"]
               + output[,,"FE|Transport|+|Hydrogen (EJ/yr)"])
          / output[,,"GDP|MER (billion US$2005/yr)"],
          "Intensity|GDP|Final Energy|Transport (MJ/US$2005)"),
        setNames(
          1e3 * (output[,,"FE|Transport|Pass|+|Liquids (EJ/yr)"] * 0.23
                 + output[,,"FE|Transport|Pass|+|Electricity (EJ/yr)"] * 0.64
                 + output[,,"FE|Transport|Pass|+|Hydrogen (EJ/yr)"] * 0.36)
          / output[,,"GDP|MER (billion US$2005/yr)"],
          "Intensity|GDP|Useful Energy|Transport (MJ/US$2005)"))
      
   
    return(output) 
    
    
  }