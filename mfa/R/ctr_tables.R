#' ctr_tables
#'
#' ctr_tables
#' @include classes.R ctr_variables.R
#' @param object a `mfa` object
#' 
#' @return a matrix with contribution of a table to a dimension
#' @rdname ctr_tables
#' @export

setGeneric("ctr_tables",
           function(object) standardGeneric("ctr_tables") )

#' @describeIn ctr_tables contribution of a table for mfa
setMethod("ctr_tables",
          signature = "mfa",
          function(object){
            ctr_var = ctr_variables(object)
            t(sapply(object@sets, function(x){apply(ctr_var[x,],2,sum)}))
          })
