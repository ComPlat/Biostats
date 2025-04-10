test_data_dir <- system.file("test_data", package = "bs")
files <- list.files(test_data_dir, pattern = "\\.json$", full.names = TRUE)

load_and_eval_history <- function(file, df) {
  json <- jsonlite::read_json(file)
  bs:::eval_history(json, df)
}

tinytest::expect_error(
  load_and_eval_history(files[1], CO2),
  info = "Invalid text BlaBla"
)
tinytest::expect_error(
  load_and_eval_history(files[2], CO2),
  info = "Invalid type (RocketLaunch)"
)

dose_response <- read.csv(paste0(test_data_dir, "/DoseResponse.csv"))
l <- jsonlite::read_json(files[3])
result_state <- bs:::backend_result_state$new()
data_model_state <- bs:::backend_data_model_state$new(dose_response)
data_wrangling_state <- bs:::backend_data_wrangling_state$new(data_model_state)
for (i in seq_along(l)) {
  inner_e <- try({
    bs:::eval_entry(l[[i]], data_model_state, data_wrangling_state, result_state)
  })
  if (inherits(inner_e, "try-error")) {
    print(i)
    err <- conditionMessage(attr(inner_e, "condition"))
    stop(err)
  }
}

