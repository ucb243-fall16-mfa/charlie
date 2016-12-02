testthat::context("multiple factor analysis sumeigen function")

data(wines)
wines = wines[,2:54]
sets = list(1:6,7:12,13:18,19:23,24:29,30:34,35:38,39:44,45:49,50:53)
obj = mfa(wines, sets, ncomps = 2, center = TRUE, scale = TRUE)
result =sumeigen(obj)

testthat::test_that("the function returns a table",{
  testthat::expect_is(result,"table")
})

testthat::test_that("The returned table has correct dimension",{
  testthat::expect_equal(nrow(result),5)
  testthat::expect_equal(ncol(result),2)
})