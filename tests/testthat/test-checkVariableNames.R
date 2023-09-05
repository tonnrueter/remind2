test_that("checkVariableNames works", {
  fine <- c("PE (EJ)", "PE (NA/yr)", "FE|What (yr)", "FE|Electricity (EJ/yr)")
  for (v in fine) {
    expect_no_warning(checkVariableNames(v))
  }
  wrong <- c("PE", "PE| (EJ)", "PE||Elec (EJ)", "PE||Elec", "PE  (EJ)", "PE (NA)",
             "NA|PE (EJ)", "PE|NA|What (EJ)", " PE (EJ)", "PE (EJ) ")
  for (v in wrong) {
    w <- capture_warnings(checkVariableNames(v))
    expect_true(length(w) > 0)
  }
})
