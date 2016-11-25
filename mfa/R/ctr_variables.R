#' ctr_variables
#'
#' ctr_variables
#' @include classes.R
#' @param object a `mfa` object
#' 
#' @return a matrix with contribution of a variable to a dimension
#' @export

ctr_variables = function(object){
  if (class(object)!="mfa"){
    stop("Invalid class of object")
  }
  a = matrix(object@a_weights,nrow=length(object@a_weights))
  apply(object@loadings, 2, function(x){a*x^2})
}