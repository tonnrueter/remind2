library(gdx)
library(piamInterfaces)


magiccVars <- c(
  "Concentration|CH4 (ppb)",
  "Concentration|CO2 (ppm)",
  "Concentration|N2O (ppb)",
  "Forcing (W/m2)",
  "Forcing|Aerosol (W/m2)",
  "Forcing|Aerosol|BC (W/m2)",
  "Forcing|Aerosol|Cloud Indirect (W/m2)",
  "Forcing|Aerosol|OC (W/m2)",
  "Forcing|Aerosol|Other (W/m2)",
  "Forcing|Aerosol|Sulfate Direct (W/m2)",
  "Forcing|Albedo Change and Mineral Dust (W/m2)",
  "Forcing|CH4 (W/m2)",
  "Forcing|CO2 (W/m2)",
  "Forcing|F-Gases (W/m2)",
  "Forcing|Kyoto Gases (W/m2)",
  "Forcing|Montreal Gases (W/m2)",
  "Forcing|N2O (W/m2)",
  "Forcing|Other (W/m2)",
  "Forcing|Tropospheric Ozone (W/m2)",
  "Temperature|Global Mean (K)"
)

test_that("Test if REMIND reporting produces mandatory variables for NGFS reporting", {
  # for now, we enforce this test locally to ensure that remind2 reportings do
  # not accidentally mess up the reporting
  skip_on_ci()

  gdxPath <- file.path(tempdir(), "fulldata.gdx")
  utils::download.file("https://rse.pik-potsdam.de/data/example/remind2_test-NGFS_fulldata.gdx",
    gdxPath,
    mode = "wb", quiet = TRUE
  )

  mif <- convGDX2MIF(gdxPath, gdx_ref = gdxPath)

  computedVariables <- getItems(mif, dim = 3.3)

  computedVariables <- gsub("\\(\\)", "(unitless)", computedVariables)

  templateVariables <- unique(
    piamInterfaces::getREMINDTemplateVariables("AR6"),
    piamInterfaces::getREMINDTemplateVariables("AR6_NGFS")
  )


  missingVariables <- setdiff(templateVariables, computedVariables)

  # MAGICC variables in template are not created here and therefore not considered
  missingVariables <- setdiff(missingVariables, magiccVars)

  if (length(missingVariables) > 0) {
    warning(
      "The following variables are expected in the piamInterfaces package,
          but cannot be found in the reporting generated: ",
      paste(missingVariables, collapse = ",\n ")
    )
  }
  expect_true(length(missingVariables) == 0)
  unlink(tempdir(), recursive = TRUE)
  tempdir(TRUE)
})
