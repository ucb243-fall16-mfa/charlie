#' mfa
#'
#' mfa
#' @include classes.R
#' @param data data set (matrix or data frame).
#' @param sets list of vectors indicating the sets of variables (i.e. the blocks).
#' @param ncomps integer indicating how many number of components (i.e. factors) are to be extracted.
#' @param center either a logical value or a numeric vector of length equal to the number of active
#' variables in the analysis
#' @param scale either a logical value or a numeric vector of length equal to the number of active
#' variables in the analysis
#' 
#' @return an object of class "mfa"
#' @importFrom methods new
#' @export

mfa = function(data, sets, ncomps = NULL, center = TRUE, scale = TRUE){
  # main function
  data1 = preprocess(data,center,scale)
  tables = split_tables(data1,sets)
  a = alpha_weight(tables)
  A = weight_matrix(tables, a)
  M = diag(rep(1/nrow(data1), nrow(data1))) #diagonal mass matrix
  W = as.matrix(data1)
  W2 = sqrt(M)%*%W%*%sqrt(A)
  GSVD = svd(W2)
  M2 = diag(1/sqrt(diag(M)))
  A2 = diag(1/sqrt(diag(A)))
  P = M2%*%GSVD$u
  Q = A2%*%GSVD$v
  d = GSVD$d
  eigenvalues = d
  cfs = P%*%diag(d)
  pfs = partial_factor_score(tables, sets, a, Q, ncomps)
  if (is.null(ncomps)){
    new(Class = "mfa",
        ncomps = length(d),
        center = center,
        scale = scale,
        eigenvalues = eigenvalues,
        cfs = cfs,
        pfs = pfs,
        loadings = Q)
  }else{
    new(Class = "mfa",
        ncomps = ncomps,
        center = center,
        scale = scale,
        eigenvalues = eigenvalues[1:ncomps],
        cfs = cfs[,1:ncomps],
        pfs = pfs,
        loadings = Q[,1:ncomps])
  }
  
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

