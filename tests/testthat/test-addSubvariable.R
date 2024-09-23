test_that(".addSubvariable works", {
  int2ext <- c("Price|Final Energy (US$2017/GJ)" = "FE (EJ)")
  int2extafter <- c("Price|Final Energy (US$2017/GJ)" = "FE (EJ)",
                    "Price|Final Energy|Rawdata (US$2017/GJ)" = "FE (EJ)")
  expect_equal(int2extafter,
               .addSubvariable(int2ext, "|Rawdata"))
})
