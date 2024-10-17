get_ast <- function(inp) {
  if (!is.call(inp)) {
    return(inp)
  }

  inp <- as.list(inp)

  # check if is function
  fct <- inp[[1]]

  allowed_fcts <- c(
    "-", "+", "*", "/",
    "log", "log10", "sqrt", "exp", "^",
    "sin", "cos", "tan", "tanh", "sinh", "cosh", "acos", "asin", "atan",
    "is.numeric", "is.character", "is.logical", "is.factor", "is.integer",
    "as.numeric", "as.character", "as.logical", "as.factor", "as.integer",
    ">", "<", "<=", ">=", "==", "!=",
    "abs", "ceiling", "floor", "trunc", "round",
    "grep", "substr", "sub", "paste", "paste0",
    "strsplit", "tolower", "toupper",
    "dnorm", "pnorm", "qnorm", "rnorm", "dbinom",
    "pbinom", "qbinom", "rbinom", "dpois",
    "ppois", "rpois", "dunif", "punif", "qunif", "runif",
    "mean", "sd", "median", "quantile", "range",
    "sum", "diff", "min", "max", "scale",
    "c", "vector", "length", "matrix", "~"
  )

  check <- deparse(fct)

  if ((check %in% allowed_fcts) == FALSE) {
    return("Error")
  }

  lapply(inp, get_ast)
}
