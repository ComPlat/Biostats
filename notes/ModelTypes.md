# Model Types Kanbas ToDo board

- [ ] Seperate parameteric and non-parametric tests in two different tabs
  - [ ] Non Parameteric tests:
    * [ ] Kruskal Wallis test
    * [ ] Kruskal Wallis PostHoc test
  - [ ] Parameteric tests: t-test, aov etc.
- [ ] Add dropdown in Formula Editor to chose the model type
- [ ] Implement the R6 class Model
  - [ ] Store: type, formula, and additional information
    * Supported Types:
      * [ ] Correlation: chose response and one predictor both numeric
      * [ ] DoseResponse: chose response and one predictor both numeric & names of substances
      * [ ] Linear: current formula choices
      * [ ] GeneralizedLinear: current formula choices & Link function
      * [ ] LinearMixed: extended formula
      * [ ] NonParameteric: chose response and one predictor. Response has to be numeric.
      * [ ] Optimization: current formula choices with additional unknown parameter
- [ ] Replace everywhere the formula with an instance of class Model
- [ ] Conditional show the appropriate Assumptions tab
  - [ ] For Correlation, DoseResponse and Optimization no Assumptions are available
  - [ ] For NonParameteric no Assumptions are available ???
  - [ ] Linear: Current Assumptions
  - [ ] GeneralizedLinear: Only partial support
    * Residual plots are possible but depend on link and family.
    * Diagnostic plots require care.
    * Recommend summarised output and dispersion/residual analysis.
  - [ ] LinearMixed: Use residuals and random effects diagnostics
    * Plots of residuals per group
    * Check for overfitting
    * Optionally support packages like `DHARMa` (you could skip external deps at first)
  - [ ] NonParameteric: do not show Assumptions tab.
        Used when Assumptions for parameteric tests are violated.
- [ ] Conditional show the appropriate ParametericTest tab
  - [ ] For Correlation, DoseResponse and Optimization no Tests are available.
  - [ ] Linear: Current Assumptions
  - [ ] GeneralLinear: See theory
  - [ ] LinearMixed: See theory
- [ ] Add regression tab
  - [x] Backend for fitting a formula to data ./development/Refression.R
  - [ ] Implement engine class
  - [ ] Update replay history
  - [ ] Write UI
- [ ] When a user tries to run something with a wrong model type, the formula modal window
      should be opened and a notification should pop up which tell the user which model type
      is required.
