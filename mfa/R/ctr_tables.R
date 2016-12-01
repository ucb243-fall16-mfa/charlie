#' ctr_tables
#'
#' ctr_tables
#' @include classes.R ctr_variables.R
#' @param object a `mfa` object
#' 
#' @return a matrix with contribution of a table to a dimension
#' @examples
#' data(wines)
#' wines = wines[,2:54]
#' sets = list(1:6,7:12,13:18,19:23,24:29,30:34,35:38,39:44,45:49,50:53)
#' obj = mfa(wines, sets, ncomps = 2, center = TRUE, scale = TRUE)
#' ctr_tables(obj)
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
