#' RV_table
#'
#' To evaluate the similarities between each pair of tables among all tables in the dataset
#' 
#' @include RV.R
#' @param dataset a data frame or matrix
#' @param sets list of vectors indicating the sets of variables (i.e. the blocks).
#' 
#' @return a matrix
#' @examples
#' data(wines)
#' wines = wines[,2:54]
#' sets = list(1:6,7:12,13:18,19:23,24:29,30:34,35:38,39:44,45:49,50:53)
#' RV_table(wines, sets)
#' @export

RV_table = function(dataset, sets){
  tables = lapply(sets, function(x){dataset[,x]})
  sapply(tables, function(t){sapply(tables, function(x){RV(t,x)})})
}
