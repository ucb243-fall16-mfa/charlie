#' ctr_observations
#'
#' ctr_observations
#' @include classes.R
#' @param object a `mfa` object
#' 
#' @return a matrix with contribution of an observation to a dimension
#' @examples
#' data(wines)
#' wines = wines[,2:54]
#' sets = list(1:6,7:12,13:18,19:23,24:29,30:34,35:38,39:44,45:49,50:53)
#' obj = mfa(wines, sets, ncomps = 2, center = TRUE, scale = TRUE)
#' ctr_observations(obj)
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
