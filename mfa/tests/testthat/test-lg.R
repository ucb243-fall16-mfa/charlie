testthat::context("Test functions LG and LG_table")

data(wines)
wines = wines[,2:54]
sets = list(1:6,7:12,13:18,19:23,24:29,30:34,35:38,39:44,45:49,50:53)

testthat::test_that("RV returns a numeric value", {
  table1 = wines[,1:6]
  table2 = wines[,7:12]
  lg = LG(table1,table2)
  testthat::expect_is(lg, "numeric")
})

lgs = LG_table(wines, sets)
testthat::test_that("RV_table returns a symmetric matrix with correct dimension", {
  testthat::expect_is(lgs, "matrix")
  testthat::expect_equal(nrow(lgs), 10)
  testthat::expect_equal(ncol(lgs), 10)
  testthat::expect_true(isSymmetric(lgs))
})
