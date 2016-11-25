# create a S4 class for mfa

setClass("mfa",
    slots = list(
      ncomps = "numeric",
      sets = "list",
      center = "logical",
      scale = "logical",
      eigenvalues = "numeric",
      cfs = "matrix",
      pfs = "list",
      loadings = "matrix",
      a_weights = "numeric")
)


