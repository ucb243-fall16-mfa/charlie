#' ctr_variables
#'
#' ctr_variables
#' @include classes.R
#' @param object a `mfa` object
#' 
#' @return a matrix with contribution of a variable to a dimension
#' @examples
#' data(wines)
#' wines = wines[,2:54]
#' sets = list(1:6,7:12,13:18,19:23,24:29,30:34,35:38,39:44,45:49,50:53)
#' obj = mfa(wines, sets, ncomps = 2, center = TRUE, scale = TRUE)
#' ctr_variables(obj)
#' @rdname ctr_variables
#' @export

setGeneric("ctr_variables",
            function(object) standardGeneric("ctr_variables") )

#' @describeIn ctr_variables contribution of a variable for mfa
setMethod("ctr_variables", 
          signature = "mfa", 
          function(object){
            a = matrix(object@a_weights,nrow=length(object@a_weights))
            apply(object@loadings, 2, function(x){a*x^2})
})
