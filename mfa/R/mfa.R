#' mfa
#'
#' Multiple Factor Analysis
#' 
#' @include classes.R
#' @param data data set (matrix or data frame).
#' @param sets list of vectors indicating the sets of variables (i.e. the blocks).
#' @param ncomps integer indicating how many number of components (i.e. factors) are to be extracted.
#' @param center either a logical value or a numeric vector of length equal to the number of active
#' variables in the analysis
#' @param scale either a logical value or a numeric vector of length equal to the number of active
#' variables in the analysis
#' 
#' @return 
#' The returned value is an object of class "mfa" with the following elements: \cr
#' \code{eigenvalues} a vector containing eigenvalues for GSVD \cr
#' \code{cfs} a matrix of common factor scores \cr
#' \code{pfs} a list of partial factor scores for each table \cr
#' \code{loadings} a matrix of loadings \cr
#' \code{a_weights} alpha weight for each table \cr
#' 
#' @importFrom methods new
#' @export

# main function
mfa = function(data, sets, ncomps = NULL, center = TRUE, scale = TRUE){
  # main function
  sets = convert_sets(data, sets)
  data1 = preprocess(data,center,scale)
  res = gsvd_mfa(data1,sets,ncomps)
  create_mfa(res, sets, center, scale, ncomps)
}

# helper functions for mfa
convert_sets = function(data, sets){
  #convert the names of active variables to column indices
  lapply(sets, function(x){
    if(class(x)=="character"){
      which(colnames(data)%in%x)
    }else{
      x
    }
  })
}

preprocess = function(data, center=TRUE, scale=TRUE){
  # preprocessing: center + normalize
  d = scale(as.matrix(data), center = center, scale = scale)
  apply(d, 2, function(x){sign(x) * sqrt(x^2/sum(x^2))})
}

split_tables = function(data, sets, bycol=TRUE){
  # split tables
  if(bycol){
    l = lapply(sets, function(x){data[,x]})
  } else{
    l = lapply(sets, function(x){data[x,]})
  }
}

alpha_weight = function(tables){
  # collect the values of the alpha weights of all tables into a weight vector
  sapply(tables, function(x){1/(svd(x)$d[1])^2})
}

weight_matrix = function(tables, alphas){
  # create weight matrix according to alpha weights returned from pca for each table
  nc = sapply(tables, ncol)
  diag(rep(alphas, times=nc))
}

partial_factor_score = function(tables, set, a, Q, ncomps=NULL){
  K = length(tables)
  Qs = split_tables(Q, set, bycol=FALSE)
  pfs = list()
  for (i in 1:K){
    if (is.null(ncomps)){
      pfs[[i]] = K * a[i] * tables[[i]] %*% Qs[[i]]
    }else{
      pfs[[i]] = K * a[i] * tables[[i]] %*% Qs[[i]][,1:ncomps]
    }
  }
  return(pfs)
}

gsvd_mfa = function(data, sets, ncomps){
  # GSVD for mfa
  tables = split_tables(data,sets)
  a = alpha_weight(tables)
  A = weight_matrix(tables, a)
  M = diag(rep(1/nrow(data), nrow(data))) #diagonal mass matrix
  W = as.matrix(data)
  W2 = sqrt(M)%*%W%*%sqrt(A)
  GSVD = svd(W2)
  M2 = diag(1/sqrt(diag(M)))
  A2 = diag(1/sqrt(diag(A)))
  P = M2%*%GSVD$u
  Q = A2%*%GSVD$v
  d = GSVD$d
  eigenvalues = d^2
  cfs = P%*%diag(d)
  pfs = partial_factor_score(tables, sets, a, Q, ncomps)
  return(list(d=d,eigenvalues=eigenvalues,cfs=cfs,pfs=pfs,Q=Q,A=A))
}

create_mfa = function(res, sets, center, scale, ncomps){
  # create mfa class object
  if (is.null(ncomps)){
    new(Class = "mfa",
        ncomps = length(res$eigenvalues),
        sets = sets,
        center = center,
        scale = scale,
        singularValue = res$d,
        eigenvalues = res$eigenvalues,
        cfs = res$cfs,
        pfs = res$pfs,
        loadings = res$Q,
        a_weights = diag(res$A))
  }else{
    new(Class = "mfa",
        ncomps = ncomps,
        sets = sets,
        center = center,
        scale = scale,
        singularValue = res$d,
        eigenvalues = res$eigenvalues[1:ncomps],
        cfs = res$cfs[,1:ncomps],
        pfs = res$pfs,
        loadings = res$Q[,1:ncomps],
        a_weights = diag(res$A))
  }
}
