# create a S4 class for mfa

setClass(
  Class = "mfa",
  representation = representation(
    ncomps = "numeric",
    sets = "list",
    center = "logical",
    scale = "logical",
    eigenvalues = "numeric",
    cfs = "matrix",
    pfs = "list",
    loadings = "matrix",
    a_weights = "numeric"
  ),
  prototype = prototype(
    center = TRUE,
    scale = TRUE
  )
)


