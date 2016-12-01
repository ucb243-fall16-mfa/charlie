#' print
#'
#' print
#' @include classes.R
#' @param x a `mfa` object
#' @param compromise boolean; output a plot of the first two components of compromise factor score if TRUE
#' @param pfs boolean; output a plot of the first two components of partial factor score for table with specified `tablenumber`if TRUE
#' @param loadings boolean; output a plot of the first two components of loadings for table with specified `tablenumber` if TRUE
#' @param tablenumber table number, used for partial factor score or loadings plot
#' 
#' @return A visualization table; also plots if any of parameters `compromise`, `pfs`,
#' `loadings` are set to be TRUE
#' 
#' @importFrom graphics plot points text title
#' @rdname print
#' @export

setMethod(
  "print",
  signature = "mfa",
  function(x, compromise = TRUE,pfs = TRUE, loadings = TRUE, tablenumber = 1){
    cat("Number of observations: ")
    print(nrow(x@cfs))
    cat("Number of tables: ")
    print(length(x@sets))
    cat("Number of factors: ")
    print(ifelse(is.null(x@ncomps),length(x@sets),x@ncomps))
    if(compromise == TRUE){
      compromise_plot(x)
    }
    if(pfs == TRUE){
      pfs_plot(x, tablenumber)
    }
    if(loadings == TRUE){
      loadings_plot(x, tablenumber)
    }
  }
)

#' compromise_plot
#'
#' compromise_plot
#' @include classes.R
#' @param x a `mfa` object
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
    text(x@cfs[,1],x@cfs[,2], labels = rownames(x@data),
         pos = 4, col = "gray50")
    # graphic title
    title("Observations projected onto the compromise")
  }
)

#' pfs_plot
#'
#' pfs_plot
#' @include classes.R
#' @param x a `mfa` object
#' @param tablenumber The number of the table to display partial 
#'        factor scores for.
#' 
#' @return A plot of the partial factor scores for a table.
#' @rdname pfs_plot
#' @export

setGeneric(
  "pfs_plot",
  function(x,tablenumber) standardGeneric("pfs_plot")
)

#' @describeIn pfs_plot plot of the partial factor scores for a table
setMethod(
  "pfs_plot",
  signature = "mfa",
  function(x,tablenumber){
    i = tablenumber
    # simple scatter-plot
    plot(x@pfs[[i]][,1],x@pfs[[i]][,2],type = "n",
         xlab = "first component", ylab = "second component")
    # plot points for pfs
    points(x@pfs[[i]][,1],x@pfs[[i]][,2], pch = 19, col = "blue")
    # plot text for pfs
    text(x@pfs[[i]][,1],x@pfs[[i]][,2], labels = rownames(x@data),
         pos = 4, col = "gray50")
    # graphic title
    title(paste("Partial Factor Scores for table",i))
    
  }
)

#' loadings_plot
#'
#' loadings_plot
#' @include classes.R
#' @param x a `mfa` object
#' @param tablenumber the table in the dataset that you wish to display 
#'        loadings for
#' 
#' @return A plot of the variable loadings
#' @rdname loadings_plot
#' @export

setGeneric(
  "loadings_plot",
  function(x,tablenumber) standardGeneric("loadings_plot")
)
#' @describeIn loadings_plot a plot of the variable loadings
setMethod(
  "loadings_plot",
  signature = "mfa",
  function(x,tablenumber){
    i = tablenumber
    # simple scatter-plot
    plot(x@loadings[x@sets[[i]],][,1], x@loadings[x@sets[[i]],][,2],type = "n",
         xlab = "first component", ylab = "second component")
    # plot points for loadings
    points(x@loadings[x@sets[[i]],][,1], x@loadings[x@sets[[i]],][,2], pch = 17, col = "red")
    # plot text for loadings
    text(x@loadings[x@sets[[i]],][,1], x@loadings[x@sets[[i]],][,2],labels = colnames(x@data[x@sets[[i]]]),
         pos = 4, col = "black")
    # graphic title
    title(paste("Variable Loadings for table",i))
  }
)
