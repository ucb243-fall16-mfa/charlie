
setMethod(
  "print",
  signature = "mfa",
  function(x,data,compromise = TRUE,pfs = TRUE){
    cat("Number of objects: ")
    print(nrow(x@cfs))
    cat("Number of tables: ")
    print(length(x@sets))
    cat("Number of components: ")
    print(ifelse(is.null(x@ncomps),length(x@sets),x@ncomps))
    if(compromise == TRUE){
      compromise_plot(x,data)
    }
    if(pfs == TRUE){
      pfs_table(x,data)
    }
  }
)


setGeneric(
  "compromise_plot",
  function(x,data) standardGeneric("compromise_plot")
)
setMethod(
  "compromise_plot",
  signature = "mfa",
  function(x,data){
    # simple scatter-plot
    plot(x@cfs[,1],x@cfs[,2],type = "n",
         xlab = "first component", ylab = "second component")
    # plot points
    points(x@cfs[,1],x@cfs[,2], pch = 19, col = "blue")
    # plot text
    text(x@cfs[,1],x@cfs[,2], labels = rownames(data),
         pos = 4, col = "gray50")
    # graphic title
    title("Compromise of tables")
  }
)


setGeneric(
  "pfs_table",
  function(x,data) standardGeneric("pfs_table")
)
setMethod(
  "pfs_table",
  signature = "mfa",
  function(x,data){
   m <- x@pfs[[1]]
   i = 2
   while(i<=length(x@pfs)){
     m <- cbind(m,x@pfs[[i]])
     i = i + 1
   }
   return(m)
  }
)

print(m,data)
