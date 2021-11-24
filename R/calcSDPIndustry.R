


# adds SDG|SDG09|FE|Industry|Electricity/FE|Industry in % as
# FE|Industry|Electricity / FE|Industry * 100


# adds SDG|SDG07|Useful Energy|per capita|Industry in GJ/yr as sum of
# FE|Industry|<energy carrier> times FE-to-UE factor over Population

addIndustry_UE_from_FE <- function(output) {
    
    output <- mbind(
        output,
        setNames(
            output[,,"FE|Industry|+|Solids (EJ/yr)"] / 1.95902175,
            "UE|Industry|Solids (EJ/yr)"),
        setNames(
            output[,,"FE|Industry|+|Liquids (EJ/yr)"] / 2.484320995,
            "UE|Industry|Liquids (EJ/yr)"),
        setNames(
            output[,,"FE|Industry|+|Gases (EJ/yr)"] / 1.931690542,
            "UE|Industry|Gases (EJ/yr)"),
        setNames(
            output[,,"FE|Industry|+|Hydrogen (EJ/yr)"] / 1.931690542,
            "UE|Industry|Hydrogen (EJ/yr)"),
        setNames(
            output[,,"FE|Industry|+|Heat (EJ/yr)"] / 1.572467703,
            "UE|Industry|Heat (EJ/yr)"),
        setNames(
            output[,,"FE|Industry|+|Electricity (EJ/yr)"] / 1.824715164,
            "UE|Industry|Electricity (EJ/yr)"))
    
    
    output <- mbind(
        output,
    setNames(
        output[,,"UE|Industry|Solids (EJ/yr)"] 
        + output[,,"UE|Industry|Liquids (EJ/yr)"] 
        + output[,,"UE|Industry|Gases (EJ/yr)"] 
        + output[,,"UE|Industry|Hydrogen (EJ/yr)"] 
        + output[,,"UE|Industry|Heat (EJ/yr)"] 
        + output[,,"UE|Industry|Electricity (EJ/yr)"] ,
        "UE|Industry (EJ/yr)"))
    
    output <- mbind(
        output,
    setNames(
        output[,,"UE|Industry (EJ/yr)"] / output[,,"Population (million)"] * 1e3,
        "UE|per capita|Industry (GJ/cap/yr)"))
    
    
    return(output) 
}