#gdx <- "~/dev/playground/calibration_report/fulldata_test.gdx"

# source: /p/tmp/schreyer/Modeling/remind/ariadne/output/Base_IndUpd_2022-05-23_14.16.15
gdx <- "~/dev/playground/calibration_report/fulldataBase_IndUpd_2022-05-23_14.16.15.gdx"

outputDir = getwd()
outputFile = "reportCalibration.pdf"

reportCalibration <- function(gdx, outputDir = getwd(), outputFile = "reportCalibration.pdf") {


  yamlParams <- c(list(gdx = normalizePath(gdx, mustWork = TRUE)))


  rmarkdown::render(
#    system.file("markdown/reportCalibration/rc_main.Rmd", package = "remind2"),
    "~/dev/pik-piam/remind2/inst/markdown/reportCalibration/rc_main.Rmd",
    output_dir = outputDir,
    output_file = outputFile,
    output_format = "pdf_document",
    params = yamlParams
  )
}
