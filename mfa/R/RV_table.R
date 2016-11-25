#' RV_table
#'
#' To evaluate the similarities between each pair of tables among all tables in the dataset
#' 
#' @include RV.R
#' @param dataset a data frame or matrix
#' @param sets list of vectors indicating the sets of variables (i.e. the blocks).
#' 
#' @return a matrix
#' 
#' @export

RV_table = function(dataset, sets){
  tables = lapply(sets, function(x){dataset[,x]})
  sapply(tables, function(t){sapply(tables, function(x){RV(t,x)})})
}
