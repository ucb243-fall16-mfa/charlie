#' LG
#'
#' To evaluate the similarities between two tables
#' 
#' @param table1 a data frame or matrix
#' @param table2 a data frame or matrix
#' 
#' @return Lg coefficient
#' @examples
#' t1 = matrix(rnorm(100),nrow=10)
#' t2 = matrix(rnorm(100),nrow=10)
#' LG(t1, t2)
#' @export

LG = function(table1, table2){
  table1 = as.matrix(table1)
  table2 = as.matrix(table2)
  a1 = 1/(svd(table1)$d[1])^2
  a2 = 1/(svd(table2)$d[1])^2
  lg = traceM((table1%*%t(table1))%*%(table2%*%t(table2)))*a1*a2
  lg
}

traceM = function(M){
  sum(diag(M))
}
