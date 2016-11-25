#' ctr_variables
#'
#' ctr_variables
#' @include classes.R
#' @param object a `mfa` object
#' 
#' @return a matrix with contribution of a variable to a dimension
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
