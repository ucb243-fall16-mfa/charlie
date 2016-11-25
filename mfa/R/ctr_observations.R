#' ctr_observations
#'
#' ctr_observations
#' @include classes.R
#' @param object a `mfa` object
#' 
#' @return a matrix with contribution of an observation to a dimension
#' @rdname ctr_observations
#' @export

setGeneric("ctr_observations",
           function(object) standardGeneric("ctr_observations") )

#' @describeIn ctr_observations contribution of an observation for mfa
setMethod("ctr_observations", 
          signature = "mfa",
          function(object){
            apply(object@cfs, 2, function(x){x^2/sum(x^2)})
          })
