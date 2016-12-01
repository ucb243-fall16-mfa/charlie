testthat::context("Test three contribution functions of observations, variables, and tables")

data(wines)
wines = wines[,2:54]
sets = list(1:6,7:12,13:18,19:23,24:29,30:34,35:38,39:44,45:49,50:53)
obj = mfa(wines, sets, ncomps = 2, center = TRUE, scale = TRUE)

testthat::test_that("contribution functions return a matrix", {
  ctr1 = ctr_observations(obj)
  ctr2 = ctr_variables(obj)
  ctr3 = ctr_tables(obj)
  testthat::expect_is(ctr1, "matrix")
  testthat::expect_is(ctr2, "matrix")
  testthat::expect_is(ctr3, "matrix")
})

testthat::test_that("Sum of each column of contribution of observation is 1",{
  ctr1 = ctr_observations(obj)
  expect_true(all(abs(apply(ctr1, 2, sum)-1) < 1e-10))
})

testthat::test_that("Sum of each column of contribution of variables is 1",{
  ctr2 = ctr_variables(obj)
  expect_true(all(abs(apply(ctr2, 2, sum)-1) < 1e-10))
})

testthat::test_that("Sum of each column of contribution of tables is 1",{
  ctr3 = ctr_tables(obj)
  expect_true(all(abs(apply(ctr3, 2, sum)-1) < 1e-10))
})
