
setMethod(
  "print",
  signature = "mfa",
  function(x,data,compromise = TRUE,pfs.loadings = TRUE){
    cat("Number of objects: ")
    print(nrow(x@cfs))
    cat("Number of tables: ")
    print(length(x@sets))
    cat("Number of components: ")
    print(ifelse(is.null(x@ncomps),length(x@sets),x@ncomps))
    if(compromise == TRUE){
      compromise_plot(x,data)
    }
    if(pfs.loadings == TRUE){
      pfs.loadings_plot(x,data)
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
  "pfs.loadings_plot",
  function(x,data) standardGeneric("pfs.loadings_plot")
)
setMethod(
  "pfs.loadings_plot",
  signature = "mfa",
  function(x,data){
    i = 1
    while(i <= length(x@pfs)){
      # simple scatter-plot
      plot(x@pfs[[i]][,1],x@pfs[[i]][,2],type = "n",
           xlab = "first component", ylab = "second component")
      # plot points for pfs
      points(x@pfs[[i]][,1],x@pfs[[i]][,2], pch = 19, col = "blue")
      # plot text for pfs
      text(x@pfs[[i]][,1],x@pfs[[i]][,2], labels = rownames(data),
           pos = 4, col = "gray50")
      # plot points for loadings
      points(x@loadings[x@sets[[1]],][,1], x@loadings[x@sets[[1]],][,2], pch = 17, col = "red")
      # plot text for loadings
      text(x@loadings[x@sets[[1]],][,1], x@loadings[x@sets[[1]],][,2],labels = colnames(data[x@sets[[i]]]),
           pos = 4, col = "black")
      # graphic title
      title(paste("Partial Factor Scores of table",i))
      i = i + 1
    }

  }
)
