#' RV
#'
#' To evaluate the similarities between two tables
#' 
#' @param table1 a data frame or matrix
#' @param table2 a data frame or matrix
#' 
#' @return RV coefficient
#' @examples
#' t1 = matrix(rnorm(100),nrow=10)
#' t2 = matrix(rnorm(100),nrow=10)
#' RV(t1, t2)
#' @export

RV = function(table1, table2){
  table1 = as.matrix(table1)
  table2 = as.matrix(table2)
  top = traceM((table1%*%t(table1))%*%(table2%*%t(table2)))
  bottom = traceM((table1%*%t(table1))%*%(table1%*%t(table1))) * traceM((table2%*%t(table2))%*%(table2%*%t(table2)))
  rv = top/sqrt(bottom)
  rv
}

traceM = function(M){
  sum(diag(M))
}
