test_data_dir <- system.file("test_data", package = "bs")
files <- list.files(test_data_dir, pattern = "\\.json$", full.names = TRUE)

load_and_eval_history <- function(file, df) {
  json <- readLines(file, n = -1)
  bs:::eval_history(json, df, TRUE)
}

tinytest::expect_null(
  load_and_eval_history(files[1], CO2),
  info = "Invalid text BlaBla"
)
tinytest::expect_null(
  load_and_eval_history(files[2], CO2),
  info = "Invalid type (RocketLaunch)"
)

dose_response <- read.csv(paste0(test_data_dir, "/DoseResponse.csv"))
result <- load_and_eval_history(files[3], dose_response)
result <- result$ResultsState$all_data
tinytest::expect_true(
  inherits(result[[1]], "plot"),
  info = "Plot of model"
)
tinytest::expect_equal(
  broom::tidy(lm(abs ~ conc, data = dose_response)),
  result[[2]],
  info = "Result summary"
)
tinytest::expect_true(
  inherits(result[[3]], "doseResponse"),
  info = "Dose response result"
)
tinytest::expect_true(
  inherits(result[[4]], "doseResponse"),
  info = "Dose response result"
)
tinytest::expect_true(
  inherits(result[[5]], "doseResponse"),
  info = "Dose response result"
)
tinytest::expect_equal(
  result[[5]]@outlier_info, "S1: 10, 15",
  info = "Dose response result"
)

CO2 <- read.csv(paste0(test_data_dir, "/CO2.csv"))
result <- load_and_eval_history(files[4], CO2)
result <- result$ResultsState$all_data
tinytest::expect_true(
  length(result) == 2,
  info = "Apply filter"
)
tinytest::expect_true(
  inherits(result[[1]], "plot"),
  info = "Apply filter"
)
tinytest::expect_true(
  inherits(result[[2]], "plot"),
  info = "Apply filter"
)


CO2 <- read.csv(paste0(test_data_dir, "/CO2.csv"))
result <- load_and_eval_history(files[5], CO2)
result <- result$ResultsState$all_data
tinytest::expect_true(
  length(result) == 4,
  info = "Formula editor"
)
tinytest::expect_true(
  inherits(result[[1]], "plot"),
  info = "Formula plot 1"
)
tinytest::expect_true(
  identical(
    broom::tidy(lm(uptake ~ conc, data = CO2)), result[[2]]
  ),
  "Summary model 1"
)
tinytest::expect_true(
  inherits(result[[3]], "plot"),
  info = "Formula plot 2"
)
tinytest::expect_true(
  identical(
    broom::tidy(lm(uptake ~ conc*Type, data = CO2)), result[[4]]
  ),
  "Summary model 2"
)


CO2 <- read.csv(paste0(test_data_dir, "/CO2.csv"))
result <- load_and_eval_history(files[6], CO2)
result <- result$ResultsState$all_data
tinytest::expect_equal(
  result[[1]], data.frame(max_conc = max(CO2$conc)),
  info = "max_conc <- Max(conc)"
)
tinytest::expect_equal(
  result[[2]],
  {
    CO2$conc_norm <-  CO2$conc / max(CO2$conc)
    CO2
  },
  info = "CO2$conc_norm <-  CO2$conc / max(CO2$conc)"
)
tinytest::expect_true(
  inherits(result[[3]], "plot"),
  info = "plot uptake against conc_norm"
)
tinytest::expect_true(
  inherits(result[[4]], "plot"),
  info = "plot uptake against Treatment"
)
tinytest::expect_true(
  inherits(result[[5]], "plot"),
  info = "Model creation uptake ~ conc"
)
tinytest::expect_true(
  identical(
    broom::tidy(lm(uptake ~ conc, data = CO2)), result[[6]]
  ),
  "Summary model 1"
)
tinytest::expect_true(
  inherits(result[[7]], "data.frame"),
  info = "Shapiro on data"
)
tinytest::expect_equal(
  result[[8]],
  {
    fit <- lm(uptake ~ conc, data = CO2)
    r <- resid(fit)
    res <- broom::tidy(shapiro.test(r))
    res$`Residuals normal distributed` <- res$p.value > 0.05
    res
  },
  info = "CO2$conc_norm <-  CO2$conc / max(CO2$conc)"
)
tinytest::expect_true(
  inherits(result[[9]], "plot"),
  info = "Model creation uptake ~ Treatment"
)
tinytest::expect_true(
  identical(
    broom::tidy(lm(uptake ~ Treatment, data = CO2)), result[[10]]
  ),
  "Summary model 2"
)
tinytest::expect_equal(
  result[[11]],
  {
    res <- broom::tidy(
      car::leveneTest(uptake ~ Treatment, data = CO2, center = "mean")
    )
    res$`Variance homogenity` <- res$p.value > 0.05
    res
  },
  info = "Levene test on model 2"
)
tinytest::expect_true(
  inherits(result[[12]], "plot"),
  info = "Diagnostic plots"
)
tinytest::expect_true(
  inherits(result[[13]], "plot"),
  info = "Model creation uptake ~ conc_norm"
)
CO2$conc_norm <-  CO2$conc / max(CO2$conc)
tinytest::expect_true(
  identical(
    broom::tidy(lm(uptake ~ conc_norm, data = CO2))
    , result[[14]]
  ),
  "Summary model 3"
)
tinytest::expect_equal(
  result[[15]],
  {
    res <- broom::tidy(
      cor.test(
        CO2$uptake, CO2$conc_norm,
        method = "pearson",
        alternative = "two.sided",
        conf.level = 0.95
      )
    )
    res
  },
  info = "pearson"
)
tinytest::expect_equal(
  result[[16]],
  {
    res <- broom::tidy(
      cor.test(
        CO2$uptake, CO2$conc_norm,
        method = "spearman",
        alternative = "two.sided",
        conf.level = 0.95
      )
    )
    res
  },
  info = "spearman"
)
tinytest::expect_equal(
  result[[17]],
  {
    res <- broom::tidy(
      cor.test(
        CO2$uptake, CO2$conc_norm,
        method = "kendall",
        alternative = "two.sided",
        conf.level = 0.95
      )
    )
    res
  },
  info = "kendall"
)
tinytest::expect_true(
  inherits(result[[18]], "plot"),
  info = "Model creation uptake ~ Type"
)
tinytest::expect_true(
  identical(
    broom::tidy(lm(uptake ~ Type, data = CO2))
    , result[[19]]
  ),
  "Summary model 4"
)
tinytest::expect_equal(
  broom::tidy(t.test(uptake ~ Type,
    data = CO2, conf.level = 0.95,
    alternative = "two.sided", var.equal = TRUE
  )),
  result[[20]],
  info = "Ttest"
)
tinytest::expect_true(
  inherits(result[[21]], "plot"),
  info = "Model creation uptake ~ conc"
)
tinytest::expect_true(
  identical(
    broom::tidy(lm(uptake ~ conc, data = CO2))
    , result[[22]]
  ),
  "Summary model 5"
)
tinytest::expect_equal(
  {
    fit <- broom::tidy(aov(uptake ~ conc, data = CO2))
    fit <- cbind(fit, row.names(fit))
    names(fit)[ncol(fit)] <- paste0("conc", collapse = ".")
    fit
  },
  result[[23]],
  info = "ANOVA"
)
tinytest::expect_equal(
  {
    fit <- broom::tidy(kruskal.test(uptake ~ conc, data = CO2))
    fit <- cbind(fit, row.names(fit))
    names(fit)[ncol(fit)] <- paste0("conc", collapse = ".")
    fit
  },
  result[[24]],
  info = "Kruskal Wallis test"
)
tinytest::expect_equal(
  {
    aov_res <- aov(uptake ~ conc, data = CO2)
    bal <- "Balanced"
    if (bal == "Balanced") {
      bal <- TRUE
    } else {
      bal <- FALSE
    }
    fit <- agricolae::HSD.test(aov_res,
      trt = "conc",
      alpha = 0.05, group = TRUE, unbalanced = bal
    )$groups
    fit <- cbind(fit, row.names(fit))
    names(fit)[ncol(fit)] <- paste0("conc", collapse = ".")
    fit
  },
  result[[25]],
  info = "TukeyHSD"
)
tinytest::expect_equal(
  {
    fit <- with(CO2, agricolae::kruskal(CO2[, "uptake"], CO2[, "conc"]),
      alpha = 0.05, p.adj = "Holm", group = TRUE
    )$groups
    names(fit)[1] <- "uptake"
    fit <- cbind(fit, row.names(fit))
    names(fit)[ncol(fit)] <- paste0("conc", collapse = ".")
    fit
  },
  result[[26]],
  info = "Kruskal Wallis PostHoc test"
)
tinytest::expect_equal(
  {
    aov_res <- aov(uptake ~ conc, data = CO2)
    fit <- agricolae::LSD.test(aov_res,
      trt = "conc",
      alpha = 0.05, p.adj = "holm", group = TRUE
    )$groups
    fit <- cbind(fit, row.names(fit))
    names(fit)[ncol(fit)] <- paste0("conc", collapse = ".")
    fit
  },
  result[[27]],
  info = "LSD"
)
tinytest::expect_equal(
  {
    aov_res <- aov(uptake ~ conc, data = CO2)
    fit <- agricolae::scheffe.test(aov_res,
      trt = "conc",
      alpha = 0.05, group = TRUE
    )$groups
    fit <- cbind(fit, row.names(fit))
    names(fit)[ncol(fit)] <- paste0("conc", collapse = ".")
    fit
  },
  result[[28]],
  info = "scheffe"
)
tinytest::expect_equal(
  {
    aov_res <- aov(uptake ~ conc, data = CO2)
    fit <- agricolae::REGW.test(aov_res,
      trt = "conc",
      alpha = 0.05, group = TRUE
    )$groups
    fit <- cbind(fit, row.names(fit))
    names(fit)[ncol(fit)] <- paste0("conc", collapse = ".")
    fit
  },
  result[[29]],
  info = "REGW"
)
