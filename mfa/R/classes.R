# create a S4 class for mfa

setClass("mfa",
    slots = list(
      data = "data.frame",
      ncomps = "numeric",
      sets = "list",
      center = "logical",
      scale = "logical",
      singularValue = "numeric",
      eigenvalues = "numeric",
      sumEigenValues = "numeric",
      cfs = "matrix",
      pfs = "list",
      loadings = "matrix",
      a_weights = "numeric")
)


