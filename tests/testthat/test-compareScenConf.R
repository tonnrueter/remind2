test_that("Check compareScenConf", {
  writeLines("cfg <- list()\ncfg$gms$start <- 5", "default.cfg")
  writeLines("cfg <- list()\ncfg$gms$start <- 3", "default2.cfg")
  file1 <- "test1.csv"
  file2 <- "test2.csv"
  file3 <- "test3.csv"
  csv1df <- data.frame("title" = c("default", "SSP2-Base", "SSP5-Base"),
                       "start" = c(1, 0, 0),
                       "value" = c(1, 2, 3),
                       "description" = c("A", "B", "C"),
                       row.names = "title")
  write.csv2(csv1df, file1)
  csv2df <- data.frame("title" = c("default", "SSP2-Base", "SSP5-Base"),
                       "start" = c(1, 1, 0),
                       "value" = c(1, 2, 3),
                       "description" = c("D", "E", "F"),
                       row.names = "title")
  write.csv2(csv2df, file2)
  csv3df <- data.frame("title" = c("default", "SSP2-Base", "SSP6-Base"),
                       "startnew" = c(1, 1, 0),
                       "value" = c(1, 2, 3),
                       row.names = "title")
  write.csv2(csv3df, file3)
  # normal case: should go through without a warning
  test1 <- compareScenConf(fileList = c(file1, file2), printit = FALSE)
  expect_true(is.null(test1$allwarnings))
  expect_true(! any(grepl("No configfile", test1$out)))
  expect_true(sum(grepl("description: was changed", test1$out)) == 3)
  # check renamed column start -> startnew
  test2 <- compareScenConf(fileList = c(file1, file3), renamedColumns = c(start = "startnew"),
                           renamedRows = c("SSP5-Base" = "SSP6-Base"), printit = FALSE)
  expect_true(is.null(test2$allwarnings))
  expect_true(any(grepl("Renamed columns:.*start -> startnew", test2$out)))
  expect_true(any(grepl("Renamed rows:.*SSP5-Base -> SSP6-Base", test2$out)))
  # check renamed column and row where this didn't happen
  test3 <- suppressWarnings(compareScenConf(fileList = c(file1, file2), renamedColumns = c(start = "startnew"),
                                            renamedRows = c("SSP5-Base" = "SSP6-Base"), printit = FALSE,
                                            configfile = "default2.cfg"))
  expect_true(all(c("oldColAlsoIn2", "newColNotIn2", "oldRowAlsoIn2", "newRowNotIn2") %in% names(test3$allwarnings)) &
              ! any(c("oldColNotIn1", "newColAlsoIn1", "newRowNotIn1", "newRowAlsoIn1") %in% names(test3$allwarnings)))
  expect_true(all(c("SSP6-Base was deleted.", "SSP5-Base was added.") %in% test3$out))
  expect_true(any(grepl("Columns deleted:.*startnew", test3$out)))
  expect_true(any(grepl("Columns added:.*start", test3$out)))
  expect_true(! any(grepl("No configfile", test3$out)))
  # check renamed column and row where old files doesn't contain them
  test4 <- suppressWarnings(compareScenConf(fileList = c(file1, file2), renamedColumns = c(startwrong = "start"),
                                            renamedRows = c("SSP9-Base" = "SSP5-Base"), printit = FALSE,
                                            configfile = "nonexisting.cfg"))
  expect_true(all(c("oldColNotIn1", "newColAlsoIn1", "newRowNotIn1", "newRowAlsoIn1") %in% names(test4$allwarnings)) &
              ! any(c("oldColAlsoIn2", "newColNotIn2", "oldRowAlsoIn2", "newRowNotIn2") %in% names(test4$allwarnings)))
  expect_true(any(grepl("No configfile nonexisting.cfg found in ", test4$out)))
  unlink(c(file1, file2, file3, "default.cfg", "default2.cfg"))
})
