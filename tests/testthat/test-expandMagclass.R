test_that("expandMagclass works", {
  region <- c("AAA", "BBB", "CCC")
  t <- paste0("y", c(2000, 2005, 2010))
  name <- c("foo", "bar", "bazz")

  A <- new.magpie(
    cells_and_regions = region, years = t, names = name, fill = 1,
    sets = c("region", "t", "name")
  )

  # spatial ----

  ## x smaller than ref ----
  B <- expandMagclass(A[region[-2], , ], A)
  expect_true(all(region %in% getRegions(B)))

  ## x larger than ref ----
  expect_equal(expandMagclass(A, A[region[-2], , ]), A)

  # temporal ----

  ## x smaller than ref ----
  B <- expandMagclass(A[, t[-2], ], A)
  expect_true(all(t %in% getYears(B)))

  ## x larger than ref ----
  expect_equal(expandMagclass(x = A, ref = A[, t[-2], ]), A)

  # names ----

  ## x smaller than ref ----
  B <- expandMagclass(A[,,name[-2]], A)
  expect_true(all(name %in% getNames(B)))

  ## x larger than ref ----
  expect_equal(expandMagclass(A, A[,,name[-2]]), A)

})
