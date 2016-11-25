#' ctr_tables
#'
#' ctr_tables
#' @include classes.R ctr_variables.R
#' @param object a `mfa` object
#' 
#' @return a matrix with contribution of a table to a dimension
#' @export

ctr_tables = function(object){
  if (class(object)!="mfa"){
    stop("Invalid class of object")
  }
  ctr_var = ctr_variables(object)
  t(sapply(object@sets, function(x){apply(ctr_var[x,],2,sum)}))
}
