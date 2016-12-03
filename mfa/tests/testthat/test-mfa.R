testthat::context("multiple factor analysis main function")

data(wines)
wines = wines[,2:54]
sets = list(1:6,7:12,13:18,19:23,24:29,30:34,35:38,39:44,45:49,50:53)
testthat::test_that("mfa creates a mfa object", {
  result1 = mfa(wines, sets, ncomps = 2, center = TRUE, scale = TRUE)
  testthat::expect_is(result1, "mfa")
  result2 = mfa(wines, sets, ncomps = NULL, center = TRUE, scale = TRUE)
  testthat::expect_is(result2, "mfa")
})

sets1 = list(c("V1","V2","V3","V4","V5","V6"), c("V1.1","V2.1","V3.1","V4.1","V7","V8"),
            c("V1.2","V2.2","V3.2","V4.2","V9","V10"),c("V1.3","V2.3","V3.3","V4.3","V8.1"),
            c("V1.4","V2.4","V3.4","V4.4","V11","V12"),c("V1.5","V2.5","V3.5","V4.5","V13"),
            c("V1.6","V2.6","V3.6","V4.6"),c("V1.7","V2.7","V3.7","V4.7","V14","V5.1"),
            c("V1.8","V2.8","V3.8","V4.8","V15"),c("V1.9","V2.9","V3.9","V4.9"))
testthat::test_that("mfa accepts a list of character vectors", {
  result1 = mfa(wines, sets1, ncomps = 2, center = TRUE, scale = TRUE)
  testthat::expect_is(result1, "mfa")
  result2 = mfa(wines, sets1, ncomps = NULL, center = TRUE, scale = TRUE)
  testthat::expect_is(result2, "mfa")
})


