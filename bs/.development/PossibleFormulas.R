# NOTE: This file showcases possible formulas used in R
show <- function(model) {
  call <- model$call |> as.character()
  e <- try(
    {
      as.formula(call[2])
    },
    silent = TRUE
  )
  if (inherits(e, "try-error")) {
    get(call[2]) |> print()
  } else {
    print(call[2])
  }
  print(broom::tidy(model), n = 100)
}
# linear models
# - `~` (basic response ~ predictor setup)
# - `+` (adding main effects)
# - `*` (main effects and interactions)
# - `:` (interaction only)
# - `/` (nested effects with both levels)
# - `%in%` (nested effects without including the main level)
# - `-` (removing terms)
# - `^` (specifying interaction orders)
# - `I()` (arithmetic operations within the formula)

# Basic formula with ~
data <- data.frame(
  height = c(150, 160, 170, 180, 190),
  weight = c(50, 60, 70, 80, 90)
)

# Simple linear model
model <- lm(weight ~ height, data = data)
show(model)

# Adding multiple predictors with +
# Add size and bedrooms as main effects
# Interactions are ignored
data <- data.frame(
  price = c(200, 250, 300, 350, 400),
  size = c(1000, 1500, 2000, 2500, 3000),
  bedrooms = c(2, 3, 3, 4, 4)
)

# Linear model with multiple predictors
model <- lm(price ~ size + bedrooms, data = data)
show(model)

# Model with main effects and interaction using *
# fertilizer * water expands to fertilizer + water + fertilizer:water.
# This includes the main effects of fertilizer and water as well as their interaction.
data <- data.frame(
  yield = c(30, 45, 50, 55, 35, 40, 48, 53),
  fertilizer = factor(rep(c("High", "Low"), each = 4)),
  water = factor(rep(c("High", "Low"), times = 4))
)

# Model with main effects and interaction
model <- lm(yield ~ fertilizer * water, data = data)
show(model)

# Model with interaction only using :
data <- data.frame(
  score = c(70, 80, 75, 85, 90, 95),
  gender = factor(rep(c("Male", "Female"), each = 3)),
  study_program = factor(c("STEM", "Arts", "STEM", "Arts", "STEM", "Arts"))
)

# Model with interaction only
model <- lm(score ~ gender:study_program, data = data)
show(model)

# Sample data
data <- data.frame(
  school = rep(c("School1", "School2"), each = 6),
  class = rep(c("ClassA", "ClassB", "ClassC"), 4),
  test_score = c(75, 78, 80, 82, 76, 79, 70, 72, 74, 73, 71, 72)
)
# Model with nested factors
# school represents the primary grouping factor
# class is nested within school, modeling different classes within each school
# The formula school / class allows us to see
# the effects of both school and class within each school on test_score
model <- lm(test_score ~ school / class, data = data)
show(model)

# Sample data
data <- data.frame(
  hospital = rep(c("HospitalA", "HospitalB"), each = 6),
  ward = rep(c("Ward1", "Ward2", "Ward3"), 4),
  recovery_time = c(10, 12, 9, 11, 10, 13, 8, 7, 9, 8, 9, 10)
)

# Model using nested factor with %in%
# ward %in% hospital tests whether ward is nested within hospital.
# Unlike /, it does not include hospital by itself in the model,
# focusing only on ward within each hospital.
model <- lm(recovery_time ~ ward %in% hospital, data = data)
show(model)

# Sample data
data <- data.frame(
  price = c(300000, 350000, 250000, 400000, 450000, 320000),
  sqft = c(2000, 2500, 1800, 3000, 2800, 2200),
  bedrooms = c(3, 4, 3, 5, 4, 3),
  neighborhood = factor(c("A", "A", "B", "B", "C", "C")),
  age = c(10, 15, 5, 20, 30, 25)
)

# Model without 'age'
model <- lm(price ~ sqft + age + bedrooms + neighborhood - age, data = data)
show(model)

# Sample data
data <- data.frame(
  yield = c(30, 45, 50, 55, 35, 40, 48, 53, 60, 65, 70, 75),
  fertilizer = factor(rep(c("High", "Low"), each = 6)),
  water = factor(rep(c("High", "Low"), times = 6)),
  sunlight = factor(c(
    "High", "Low", "High", "Low", "High",
    "Low", "High", "Low", "High", "Low", "High", "Low"
  ))
)

# Model including all main effects and two-way interactions
# (fertilizer + water + sunlight)^2 expands to include main
# effects (fertilizer, water, and sunlight) and all possible
# two-way interactions (fertilizer:water, fertilizer:sunlight, water:sunlight),
# but excludes the three-way interaction (fertilizer:water:sunlight).
model <- lm(yield ~ (fertilizer + water + sunlight)^2, data = data)
show(model)

# Sample data
data <- data.frame(
  sales = c(100, 80, 60, 40, 30, 20),
  price = c(10, 20, 30, 40, 50, 60)
)

# Model with price and price squared
# everything wrapped in I is handled as
# arithmetic operation
model <- lm(sales ~ price + I(price^2), data = data)
show(model)

# Sample data for repeated measures
data <- data.frame(
  response = c(5, 6, 7, 5, 6, 8, 9, 7, 6, 5),
  treatment = factor(rep(c("A", "B"), each = 5)),
  subject = factor(rep(1:5, times = 2))
)
# Linear model with error structure
# Error for repeated measurement anova
# does not work for lm
model <- aov(response ~ treatment + Error(subject/treatment), data = data)
broom::tidy(model)



