#' summaries of eigenvalues
#'
#' returns a table with the singular values, the eigenvalues, cumulative, percentage of intertia, cumulative percentage of inertia, for all the extracted components.
#'
#' @param Mfa a `mfa` class object
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
  tau='\u03c4'
  delta='\u03b4'
  lambda='\u03bb'
  rownames(sum) <- c(paste("Singular value(",delta,")",sep=""), paste("Eigenvalue(",lambda, "=", delta, "^2)",sep=""), "Cumulative", paste("%Inertia(",tau,")",sep=""), "Cumulative")
  sum = as.table(sum)
  sum
}

