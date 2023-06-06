test_that("Check compareScenConf", {
  filemain <- file.path(tempdir(), "main.gms")
  writeLines(c('Parameter start "start" / 5 /;', 'start = 5;',
               'Parameter notstart "nooo" / 3 /;', 'notstart = 7;'
              ), filemain)
  dir.create(file.path(tempdir(), "config"))
  dir.create(file.path(tempdir(), "otherfolder", "config"), recursive = TRUE)
  fileconfig <- file.path(tempdir(), "config", "default.cfg")
  writeLines('cfg <- list()', fileconfig)
  file1 <- file.path(tempdir(), "config", "test1.csv")
  file1otherfolder <- file.path(tempdir(), "otherfolder", "config", "test1.csv")
  file2 <- file.path(tempdir(), "config", "test2.csv")
  file3 <- file.path(tempdir(), "config", "test3.csv")
  csv1df <- data.frame("title" = c("default", "SSP2-Base", "SSP5-Base"),
                       "start" = c(1, 0, 0),
                       "value" = c(1, 2, 3),
                       "description" = c("A", "B", "C"),
                       row.names = "title")
  write.csv2(csv1df, file1)
  write.csv2(csv1df, file1otherfolder)
  csv2df <- data.frame("title" = c("default", "SSP2-Base", "SSP5-Base"),
                       "start" = c(1, 1, 0),
                       "value" = c(1, 2, 3),
                       "description" = c("D", "E", "F"),
                       row.names = "title")
  write.csv2(csv2df, file2)
  csv3df <- data.frame("title" = c("default", "SSP2-Base", "SSP6-Base", "SSP7-Base"),
                       "startnew" = c(1, 1, 0, 2),
                       "value" = c(1, 2, 3, 4),
                       "newswitch" = c(1, 3, NA, NA),
                       row.names = "title")
  write.csv2(csv3df, file3)
  # find file1 automatically if file1otherfolder is given
  test0 <- compareScenConf(file1otherfolder, remindPath = tempdir(), printit = FALSE, expanddata = FALSE)
  expect_true(is.null(test0$allwarnings))
  # normal case: should go through without a warning
  test1 <- compareScenConf(fileList = c(file1, file2), printit = FALSE, expanddata = FALSE)
  expect_true(is.null(test1$allwarnings))
  expect_true(sum(grepl("description: was changed", test1$out)) == 3)
  # check renamed column start -> startnew
  test2 <- compareScenConf(fileList = c(file1, file3), renamedCols = c(start = "startnew"),
                           renamedRows = c("SSP5-Base" = "SSP6-Base", "SSP5-Base" = "SSP7-Base"), printit = FALSE,
                           expanddata = FALSE)
  expect_true(is.null(test2$allwarnings))
  expect_true(any(grepl("Renamed columns:.*start -> startnew", test2$out)))
  expect_true(any(grepl("Renamed rows:.*SSP5-Base -> SSP6-Base", test2$out)))
  expect_true(any(grepl("Renamed rows:.*SSP5-Base -> SSP7-Base", test2$out)))
  expect_true(any(grepl("Columns added:.*newswitch", test2$out)))
  expect_true(any(grepl("newswitch: NA -> 1", test2$out)))
  expect_false(any(grepl("newswitch: NA -> NA", test2$out)))
  # check renamed column and row where this didn't happen
  test3 <- suppressWarnings(compareScenConf(fileList = c(file1, file2), renamedCols = c(start = "startnew"),
                                            renamedRows = c("SSP5-Base" = "SSP6-Base"), printit = FALSE,
                                            remindPath = tempdir(), expanddata = FALSE))
  expect_true(all(c("oldColAlsoIn2", "newColNotIn2", "newRowNotIn2") %in% names(test3$allwarnings)) &&
              ! any(c("oldColNotIn1", "newColAlsoIn1", "newRowNotIn1", "newRowAlsoIn1") %in% names(test3$allwarnings)))
  expect_true(any(grepl("Columns deleted:.*startnew", test3$out)))
  expect_true(any(grepl("Columns added:.*start", test3$out)))
  # check renamed column and row where old files doesn't contain them
  test4 <- suppressWarnings(compareScenConf(fileList = c(file1, file2), renamedCols = c(startwrong = "start"),
                                            renamedRows = c("SSP9-Base" = "SSP5-Base"), printit = FALSE,
                                            remindPath = NULL, expanddata = FALSE))
  expect_true(all(c("oldColNotIn1", "newColAlsoIn1", "newRowNotIn1", "newRowAlsoIn1") %in% names(test4$allwarnings)) &&
              ! any(c("oldColAlsoIn2", "newColNotIn2", "newRowNotIn2") %in% names(test4$allwarnings)))
  unlink(c(file1, file2, file3, fileconfig, filemain))
})
