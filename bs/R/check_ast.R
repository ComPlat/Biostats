allowed_fcts <- function() {
  c(
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
    "c", "vector", "length", "matrix", "~",
    "get_rows", "get_cols", "get_elem",
    "as.char", "as.int", "as.real", "as.fact"
  )
}

check_ast <- function(inp, allowed_variables) {
  if (!is.call(inp)) {
    return(inp)
  }
  inp <- as.list(inp)
  # check fct
  fct <- inp[[1]]
  check <- deparse(fct)
  if ((check %in% allowed_fcts()) == FALSE) {
    stop(paste("Found unallowed function: ", check))
  }
  # check variables
  lapply(inp, function(x) {
    if (!is.list(x) && is.symbol(x) && !(deparse(x) %in% allowed_fcts())) {
      if (!(deparse(x) %in% allowed_variables)) {
        stop(paste0("Found unknown variable:", deparse(x)))
      }
    }
  })
  lapply(inp, function(x) {
    check_ast(x, allowed_variables)
  })
}
