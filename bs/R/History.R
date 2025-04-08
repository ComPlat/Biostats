history_to_table <- function(history) {
  hist <- lapply(history, function(step) {
    result_name <- ifelse(is.null(step[["Result name"]]), "", step[["Result name"]])
    df <- data.frame(Step = step$type, Result = result_name)
    step <- step[!names(step) %in% c("type", "Result name")]
    names_step <- names(step)
    df$details <- paste(names_step, step, sep = ": ", collapse = "; ")
    df
  })
  hist <- do.call(rbind, hist) |> as.data.frame()
  list(hist) # NOTE: required to add it to result list otherwise the dataframe is split
}

# jsonlite::read_json("./test_data/History.json")
#
# types <- c(
#   "Correlation",
#   "Visualisation",
#
#   "ApplyFilter",
#   "RemoveFilter",
#
#   "CreateIntermediateVariable",
#   "RemoveIntermediateVariable",
#   "CreateNewColumn",
#
#   "CreateFormula"
#
#   "ShapiroOnData",
#   "shapiroOnResiduals",
#   "LeveneTest",
#   "DiagnosticPlots",
#
#   "DoseResponse",
#
#   "TTest",
#   "ANOVA",
#   "Kruskal-Wallis Test",
#   "Tukey HSD",
#   "Kruskal Wallis post hoc test",
#   "Least significant difference test",
#   "Scheffe post hoc test",
#   "REGW post hoc test",
#
#   "RemoveResult"
# )
#
# Data <- R6::R6Class(
#   "Data",
#   public = list(
#     df = NULL,
#     intermediate_vars = list(),
#     plots = list(),
#     test_results = list(),
#     formula = NULL,
#     counter = 1,
#     initialize = function(df) {
#       self$df <- df
#     },
#
#   )
# )
#
# CreateIntermediateVariable <- R6::R6Class(
#   "CreateIntermediateVariable",
#   public = list(
#     operation = NULL,
#     name = NULL,
#     initialize = function(operation, name) {
#       self$operation <- operation
#       self$name <- name
#     },
#     eval = function(data) {
#       res <- NULL
#       e <- try({
#         # Can code be used
#         op <- str2lang(self$operation)
#         # Allowed code
#         vars <- c("df", names(data$df))
#         if (length(data$intermediate_vars) >= 1) {
#           vars <- c(vars, names(data$intermediate_vars))
#           check_ast(op, vars)
#         }
#         # Eval
#         eval_env <- new.env()
#         list2env(data$intermediate_vars, envir = eval_env)
#         list2env(data$df, envir = eval_env) # NOTE: this adds each column as own variable
#         check_length_code(self$operation)
#         res <- eval(parse(text = self$operation), envir = eval_env)
#         check_type_res(res)
#         res
#       })
#       if (!inherits(e, "try-error")) {
#         data$counter <- data$counter + 1
#         new_name <- make.names(paste0(self$name, data$counter))
#         data$intermediate_vars[[new_name]] <- res
#       }
#     }
#   )
# )
