# create a S4 class for mfa
setClass(
  Class = "mfa",
  representation = representation(
    ncomps = "numeric",
    center = "logical",
    scale = "logical",
    eigenvalues = "numeric",
    cfs = "matrix",
    pfs = "list",
    loadings = "matrix"
  ),
  prototype = prototype(
    center = TRUE,
    scale = TRUE
  )
)


