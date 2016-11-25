testthat::context("multiple factor analysis main function")

data = read.csv("wines.csv")
data = data[,2:54]
sets = list(1:6,7:12,13:18,19:23,24:29,30:34,35:38,39:44,45:49,50:53)

testthat::test_that("mfa creates a mfa object", {
  result1 = mfa(data, sets, ncomps = 2, center = TRUE, scale = TRUE)
  testthat::expect_is(result1, "mfa")
  result2 = mfa(data, sets, ncomps = NULL, center = TRUE, scale = TRUE)
  testthat::expect_is(result2, "mfa")
})