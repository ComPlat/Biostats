check_formula_dose_response <- function(DataModelState) {
  formula <- DataModelState$formula
  if (is.null(formula)) return("No model defined.")
  if (!inherits(formula, "LinearFormula")) return("Only linear models are supported.")

  f <- formula@formula
  response <- all.vars(f[[2]])
  predictor <- all.vars(f[[3]])

  if (length(response) != 1 || length(predictor) != 1) {
    return("The formula must have one predictor and one response.")
  }

  df <- DataModelState$df
  if (!(response %in% names(df))) {
    return(sprintf("The response variable '%s' is not a column of the active dataset", response))
  }
  if (!(predictor %in% names(df))) {
    return(sprintf("The predictor variable '%s' is not a column of the active dataset", predictor))
  }
  if (!is.numeric(df[[response]])) {
    return(sprintf("The response variable '%s' must be numeric.", response))
  }

  if (!is.numeric(df[[predictor]])) {
    return(sprintf("The predictor variable '%s' must be numeric.", predictor))
  }

  return(NULL)
}

