test_that("matchDim works", {
  region <- c("AAA", "BBB", "CCC")
  t <- paste0("y", c(2000, 2005, 2010))
  name <- c("foo", "bar", "bazz")

  A <- new.magpie(
    cells_and_regions = region, years = t, names = name, fill = 1,
    sets = c("region", "t", "name")
  )

  # spatial ----
  ## x smaller then ref ----
  expect_equal(matchDim(A[region[-2],,], A),
               `mselect<-`(A, region = region[2], value = 0))

  ## x larger then ref ----
  expect_equal(matchDim(A, A[region[-2],,]),
               A[region[-2],,])

  # temporal ----
  ## x smaller then ref ----
  expect_equal(matchDim(A[,t[-2],], A),
               `mselect<-`(A, t = t[2], value = 0))

  ## x larger then ref ----
  expect_equal(matchDim(x = A, ref = A[,t[-2],]),
               A[,t[-2],])

  # names ----
  ## x smaller then ref ----
  expect_equal(matchDim(A[,,name[-2]], A),
               `mselect<-`(A, name = name[2], value = 0))

  ## x larger then ref ----
  expect_equal(matchDim(A, A[,,name[-2]]),
               A[,,name[-2]])

})
