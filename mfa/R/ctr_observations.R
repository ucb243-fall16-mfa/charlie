#' ctr_observation
#'
#' ctr_observation
#' @include classes.R
#' @param object a `mfa` object
#' 
#' @return a matrix with contribution of an observation to a dimension
#' @export

ctr_observations = function(object){
  if (class(object)!="mfa"){
    stop("Invalid class of object")
  }
  apply(object@cfs, 2, function(x){x^2/sum(x^2)})
}
