library(gdx)
library(piamInterfaces)

test_that("Test if REMIND reporting produces mandatory variables for NGFS reporting", {
  # for now, we enforce this test locally to ensure that remind2 reportings do
  # not accidentally mess up the reporting
  skip_on_ci()

  gdxPath <- file.path(tempdir(), "fulldata.gdx")
  utils::download.file("https://rse.pik-potsdam.de/data/example/remind2_test-NGFS_fulldata_oneRegi.gdx",
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
