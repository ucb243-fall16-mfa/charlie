#' summaries of eigenvalues
#'
#' returns a table with the singular values, the eigenvalues, cumulative, percentage of intertia, cumulative percentage of inertia, for all the extracted components.
#'
#' s: the vector of singular values
#' e: the vector of eigenvalues
#' n: the length of s
#' i: the percentage of inertia
#' c1: cumulative sums of eigenvalues
#' C2: cumulative sums of the percentage of inertia
#' sum: matrix of Eigenvalues and Percentage of Explained Inertia of the MFA
#' @return a table
#' @rdname sumeigen
#' @export
sumeigen <- function(Mfa){
  s = Mfa@singularValue
  e = Mfa@eigenvalues
  n = length(s)
  c1 = cumsum(e)
  i = round(e/Mfa@sumEigenValues*100)
  c2 = cumsum(i)
  sum <- rbind(s, e, c1, i, c2)
  colnames(sum) <- 1:n
  rownames(sum) <- c("Singular value(δ)", "Eigenvalue(λ = δ^2)", "Cumulative", "%Inertia(τ)", "Cumulative")
  sum = as.table(sum)
  sum
}

