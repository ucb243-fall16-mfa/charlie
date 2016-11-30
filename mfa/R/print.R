#' print
#'
#' print
#' @include classes.R
#' @param object a `mfa` object
#' 
#' @return A visualization table
#' @rdname print
#' @export

setMethod(
  "print",
  signature = "mfa",
  function(x,compromise = TRUE,pfs = TRUE){
    cat("Number of objects: ")
    print(nrow(x@cfs))
    cat("Number of tables: ")
    print(length(x@sets))
    cat("Number of components: ")
    print(ifelse(is.null(x@ncomps),length(x@sets),x@ncomps))
    if(compromise == TRUE){
      compromise_plot(x)
    }
    if(pfs == TRUE){
      pfs_table(x)
    }
  }
)

#' compromise_plot
#'
#' compromise_plot
#' @include classes.R
#' @param object a `mfa` object
#' 
#' @return A plot of the compromises
#' @rdname compromise_plot
#' @export

setGeneric(
  "compromise_plot",
  function(x) standardGeneric("compromise_plot")
)

#' @describeIn compromise_plot plot of the tables
setMethod(
  "compromise_plot",
  signature = "mfa",
  function(x){
    # simple scatter-plot
    plot(x@cfs[,1],x@cfs[,2],type = "n",
         xlab = "first component", ylab = "second component")
    # plot points
    points(x@cfs[,1],x@cfs[,2], pch = 19, col = "blue")
    # plot text
    text(x@cfs[,1],x@cfs[,2],
         pos = 4, col = "gray50")
    # graphic title
    title("Compromise of tables")
  }
)

#' pfs_table
#'
#' pfs_table
#' @include classes.R
#' @param object a `mfa` object
#' 
#' @return A plot of the compromises
#' @rdname pfs_table
#' @export

setGeneric(
  "pfs_table",
  function(x) standardGeneric("pfs_table")
)
setMethod(
  "pfs_table",
  signature = "mfa",
  function(x){
   m <- x@pfs[[1]]
   i = 2
   while(i<=length(x@pfs)){
     m <- cbind(m,x@pfs[[i]])
     i = i + 1
   }
   return(m)
  }
)

