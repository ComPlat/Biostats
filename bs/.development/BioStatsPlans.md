**BioStats**

Biostats is a web application offering many statistical methods (visualization with ggplot2, t-test, aov, correlations, etc.). For more information see https://github.com/ComPlat/Biostats.   
Within this document, the plans for future versions of BioStats are described.

**Current State**

BioStats is divided into two R packages.

- COMELN V0.1  
- bs 1.1

COMELN is responsible for downloading and uploading files from/to Chemotion ELN. Currently, no further improvements or new features are required for COMELN. The app itself is defined in the R package bs. 

**Features**

BioStats support a wide range of methods which are summarised in the tables below. 

| Data Wrangling | Visualization | Assumption | Correlation | Tests | Dose-response analysis |
| :---- | :---- | :---- | :---- | :---- | :---- |
| Modify columns of the loaded data table and offer the possibility to create intermediate variables. Within this section, a wide range of operations (arithmetic-, Statistics-, cast-functions) are supported. | The data can be visualized using boxplots, scatterplots, and lineplots.  | Within this section the assumptions of the linear model can be tested using the Shapiro- and Levene-test. Furthermore, diagnostic plots can be employed. | Correlations between two variables can be investigated using either Pearson, Spearman, or Kendall methods. | Comparison of two groups via t-test | Conducting a dose-response analysis.  |
|  |  |  |  | Comparison of more than two groups using either ANOVA or the Kruskal-Wallis test |  |
|  |  |  |  | Determine which groups differ from each other using PostHoc tests (Tukey HSD, Kruskal-Wallis, etc.) |  |

| Subset data.frame | Formula editor |
| :---- | :---- |
| It is possible to apply a filter so that only a subset of the data can be analyzed. | A formula describes how the columns of the dataset are mapped in the statistical model. Currently, only linear models are provided.  |

**Planned features**

