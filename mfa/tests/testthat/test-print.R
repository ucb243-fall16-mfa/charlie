testthat::context("multiple factor analysis print function")

data(wines)
wines = wines[,2:54]
sets = list(1:6,7:12,13:18,19:23,24:29,30:34,35:38,39:44,45:49,50:53)

testthat::test_that("the length of compromise plot is correct",{
  result1 = mfa(wines, sets, ncomps = 2, center = TRUE, scale = TRUE)
  testthat::expect_length(result1@cfs[,1],12)
  testthat::expect_length(result1@cfs[,2],12)
})

testthat::test_that("the length of pfs plot is correct",{
  result1 = mfa(wines, sets, ncomps = 2, center = TRUE, scale = TRUE)
  testthat::expect_length(result1@pfs[[1]][,1],12)
  testthat::expect_length(result1@pfs[[1]][,2],12)
})

testthat::test_that("the length of loadings plot is correct",{
  result1 = mfa(wines, sets, ncomps = 2, center = TRUE, scale = TRUE)
  testthat::expect_length(result1@loadings[result1@sets[[1]],][,1],6)
  testthat::expect_length(result1@loadings[result1@sets[[1]],][,1],6)
})


