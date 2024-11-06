get_formula <- function(model) {
  call <- model$call |> as.character()
  e <- try(
    {
      as.formula(call[2])
    },
    silent = TRUE
  )
  if (inherits(e, "try-error")) {
    return(get(call[2]) |> knitr::kable())
  } else {
    return(knitr::kable(call[2]))
  }
}

show <- function(model) {
  knitr::kable(broom::tidy(model))
}
