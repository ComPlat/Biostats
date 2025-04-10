split_formula <- function(formula) {
  f <- as.character(formula)
  list(
    response = str2lang(f[2]),
    right_site = str2lang(f[3])
  )
}

vars_rhs <- function(rhs) {
 all.vars(rhs)
}

determine_types <- function(vars, df) {
  vapply(vars, function(var) {
    classes <- class(df[, var])
    paste(classes, collapse = "")
  }, character(1))
}

create_new_numeric <- function(var, df, n = 100L) {
  data <- df[, var]
  data <- data[!is.na(data)]
  seq(min(data), max(data), length.out = n)
}

create_new_non_numeric <- function(var, df) {
  unique(df[, var])
}

create_new_data <- function(formula, df, vars, n = 100L) {
  types <- determine_types(vars, df)
  data <- Map(function(var, type) {
    if (type == "numeric" || type == "integer") {
      return(create_new_numeric(var, df, n))
    }
    create_new_non_numeric(var, df)
  }, vars, types)
  expand.grid(data)
}
