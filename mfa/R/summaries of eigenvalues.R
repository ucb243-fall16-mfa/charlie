#' summaries of eigenvalues
#'
#' returns a table with the singular values, the eigenvalues, cumulative, percentage of intertia, cumulative percentage of inertia, for all the extracted components.
#'
#' @return a table
#'
#'
sumeigen <- function(Mfa){
  s = Mfa@singularValue
  e = Mfa@eigenvalues
  n = length(s)
  c1 = cumsum(e)
  i = round(e/c1[n]*100)
  c2 = cumsum(i)
  sum <- rbind( s, e, c1, i, c2)
  colnames(sum) <- 1:n
  rownames(sum) <- c("Singular value(δ)", "Eigenvalue(λ = δ^2)", "Cumulative", "%Inertia(τ)", "Cumulative")
  sum = as.table(sum)
  sum
}

