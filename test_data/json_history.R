json_string <- '
[
  {
    "type": "CreateIntermediateVariable",
    "operation": "Max(conc)",
    "name": "max_conc"
  },
  {
    "type": "CreateNewColumn",
    "operation": " conc / max_conc",
    "column name": "norm_conc"
  },
  {
    "type": "Visualisation",
    "x": "norm_conc",
    "y": "uptake",
    "Plot-type": "box",
    "X axis label": "x label",
    "Y axis label": "y label",
    "Type of x": "factor",
    "Colour variable": "",
    "Legend title for colour": "Title colour",
    "Colour theme": "Accent",
    "Fill variable": "",
    "Legend title for fill": "Title fill",
    "Fill theme": "BuGn",
    "Split in subplots": "none",
    "Split by": "",
    "How to scale y in subplots": "free",
    "X-Range": [0.0475, 1.25],
    "Y-Range": [7.315, 47.775],
    "Width": 10,
    "Height": 10,
    "Resolution": 300,
    "Result name": "3 Visualization Boxplot"
  },
  {
    "type": "CreateFormula",
    "formula": "uptake ~ Treatment"
  },
  {
    "type": "ShapiroOnData",
    "formula": "uptake ~ Treatment",
    "Result name": "ShapiroDataNr4"
  },
  {
    "type": "ShapiroOnResiduals",
    "formula": "uptake ~ Treatment",
    "Result name": "ShaprioResidualsNr5"
  },
  {
    "type": "LeveneTest",
    "formula": "uptake ~ Treatment",
    "Data center": "mean",
    "Result name": "LeveneTestNr6"
  },
  {
    "type": "DiagnosticPlots",
    "formula": "uptake ~ Treatment",
    "Result name": "DiagnosticPlotNr7"
  },
  {
    "type": "CreateFormula",
    "formula": "uptake ~ norm_conc"
  },
  {
    "type": "Correlation",
    "formula": "uptake ~ norm_conc",
    "Correlation method": "pearson",
    "Alternative hypothesis": "two.sided",
    "Confidence level of the interval": 0.95,
    "Result name": "8 Correlation Pearson"
  },
  {
    "type": "Correlation",
    "formula": "uptake ~ norm_conc",
    "Correlation method": "spearman",
    "Alternative hypothesis": "two.sided",
    "Confidence level of the interval": 0.95,
    "Result name": "9 Correlation Spearman"
  },
  {
    "type": "Correlation",
    "formula": "uptake ~ norm_conc",
    "Correlation method": "kendall",
    "Alternative hypothesis": "two.sided",
    "Confidence level of the interval": 0.95,
    "Result name": "10 Correlation Kendall"
  },
  {
    "type": "CreateFormula",
    "formula": "uptake ~ Type"
  },
  {
    "type": "TTest",
    "formula": "uptake ~ Type",
    "Confidence level of the interval": 0.95,
    "alternative hypothesis": "two.sided",
    "The two variances are": "eq",
    "Result name": "TTestNr11"
  },
  {
    "type": "CreateFormula",
    "formula": "uptake ~ conc"
  },
  {
    "type": "ANOVA",
    "formula": "uptake ~ conc",
    "Result name": "Test_aovNr12"
  },
  {
    "type": "Kruskal-Wallis Test",
    "formula": "uptake ~ conc",
    "Result name": "Test_kruskalNr13"
  },
  {
    "type": "Tukey HSD",
    "formula": "uptake ~ conc",
    "Balanced design": false,
    "P-value": 0.05,
    "Result name": "Test_HSDNr14"
  },
  {
    "type": "Kruskal Wallis post hoc test",
    "formula": "uptake ~ conc",
    "Adjusted p value method": "holm",
    "P-value": 0.05,
    "Result name": "Test_kruskalTestNr15"
  },
  {
    "type": "Least significant difference test",
    "formula": "uptake ~ conc",
    "Adjusted p value method": "holm",
    "P-value": 0.05,
    "Result name": "Test_LSDNr16"
  },
  {
    "type": "Scheffe post hoc test",
    "formula": "uptake ~ conc",
    "P-value": 0.05,
    "Result name": "Test_scheffeNr17"
  },
  {
    "type": "REGW post hoc test",
    "formula": "uptake ~ conc",
    "P-value": 0.05,
    "Result name": "Test_REGWNr18"
  }
]
'
