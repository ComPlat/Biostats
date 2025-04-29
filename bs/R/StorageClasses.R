setClass("LinearFormula",
  slots = c(
    formula = "formula"
  )
)

setClass("GeneralisedLinearFormula",
  slots = c(
    formula = "formula",
    family = "character",
    link_fct = "character"
  )
)

setClass("doseResponse",
  slots = c(
    df = "data.frame",
    p = "ANY",
    outlier_info = "character"
  )
)

setClass("plot",
  slots = c(
    p = "ANY",
    width = "numeric",
    height = "numeric",
    resolution = "numeric"
  )
)

setClass("diagnosticPlot",
  slots = c(
    p = "character"
  )
)

setClass("summaryModel",
  slots = c(
    p = "plot",
    summary = "data.frame",
    information_criterions = "data.frame"
  )
)
