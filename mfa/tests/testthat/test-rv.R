testthat::context("rv coefficients for two tables and a large dataset")

data = read.csv("wines.csv")
data = data[,2:54]
sets = list(1:6,7:12,13:18,19:23,24:29,30:34,35:38,39:44,45:49,50:53)

testthat::test_that("RV returns a valid coefficient", {
  table1 = data[,1:6]
  table2 = data[,7:12]
  rv = RV(table1,table2)
  testthat::expect_true(rv>=0&rv<=1)
})

testthat::test_that("RV_table returns a valid matrix of RV coefficients", {
  rv = RV_table(data, sets)
  testthat::expect_is(rv, "matrix")
  testthat::expect_identical(rv, t(rv))
  testthat::expect_true(all(rv >= 0 & rv <=1))
})
