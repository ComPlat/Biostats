library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)

jsCode <- '
shinyjs.closewindow = function() { 
  //top.window.close();
}'

ui <- dashboardPage(

  dashboardHeader(title = "Biostats"),
  dashboardSidebar(
    useShinyjs(),
    extendShinyjs(text = jsCode, functions = c("geturl", "closewindow")),
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Plots", tabName = "plot", icon = icon("table")),
      menuItem("Correlations", tabName = "corr", icon = icon("table")),
      menuItem("Assumptions", tabName = "ass", icon = icon("table")),
      menuSubItem("Statistical tests", tabName = "tests",  icon = icon("table")),
      menuSubItem("LC50", tabName = "lc50",  icon = icon("table")),
      menuSubItem("Documentation", tabName = "documentation",  icon = icon("table"))
    )
  ),

    dashboardBody(
      tabItems(
        # data & transformation
        tabItem(
          tabName = "data",
          DT::DTOutput("dat1"),
          h3("Types of columns"),

          box(
            h5(strong("Modify a variable")),
            textInput("op", "Operations", value = "var1 + 10"),
            textInput("new_mod", "Name of new variable", value = "new"),
            actionButton("mod","Modify"),
            verbatimTextOutput("mod_error"),
            width = 12
          )
        ),


        # plotting
        tabItem(tabName = "plot",
          fluidRow(
            box(
              h5(strong("Plot variables")),
              textInput("y", "Y variable", value = "y"),
              textInput("x", "X variable", value = "x"),
              radioButtons("xtype", "Type of x",
                           choices = c(factor = "factor",
                                       numeric = "numeric"),
                           selected = "numeric"),

              textInput("xaxis_text", "X axis label", value = "x label"),
              textInput("yaxis_text", "Y axis label", value = "y label"),

              textInput("fill", "Fill variable", value = "fill variable"),
              textInput("legendtitle_fill", "Legend title for fill", value = "Title fill"),
              textInput("col", "Colour variable", value = "colour variable"),
              textInput("legendtitle_col", "Legend title for colour", value = "Title colour"),
              actionButton("boxplot","Boxplot"),
              actionButton("dotplot","Dotplot"),
              actionButton("lineplot","Lineplot"),
              selectInput("themes", "Choose a colour theme",
                          c("BuPu" = "BuPu",
                            "RdYIBu" = "RdYIBu",
                            "Paired" = "Paired",
                            "PuOr" = "PuOr",
                            "Spectral" = "Spectral",
                            "Pastel1" = "Pastel1",
                            "hue" = "hue",
                            "grey" = "grey"), selectize = FALSE ),
              width = 12
            ),
            box(
              actionButton("plot_save", "Add output to result-file"),
              actionButton("plot_upload", "Save and exit"),
              width = 12
            ),
            box(
              checkboxGroupInput("TableSaved2", "Saved results to file", NULL),
              width = 12
            ),
            box(
              plotOutput("plot_res"),
              width = 12
            )
          )
        ),



        # calculate correlations
        tabItem(tabName = "corr",
                fluidRow(
                  box(textInput("dep3", "Variable Nr.1", value = "var1")  ),
                  box(textInput("indep3", "Variable Nr.2", value = "var2") ),
                  box(
                    actionButton("pear", "Pearson correlation"),
                    actionButton("spear", "Spearman correlation"),
                    actionButton("kendall", "Kendall correlation"),
                    sliderInput("conflevel0", "Confidence level of the interval",
                                min = 0, max = 1, value = 0.95),
                    selectInput("alt0", "Alternative hypothesis",
                                c("Two sided" = "two.sided",
                                  "Less" = "less",
                                  "Greater" = "greater")),
                    h4(strong("Results of test:")),
                    tableOutput("cor_result"),
                    verbatimTextOutput("cor_error"),
                    width = 12
                  ),
                box(
                  actionButton("corr_save", "Add output to result-file"),
                  actionButton("corr_upload", "Save and exit"),
                  width = 12
                ),
                box(
                  checkboxGroupInput("TableSaved", "Saved results to file", NULL),
                  width = 12
                )
                )
        ),

        # assumptions
        tabItem(tabName = "ass",
                fluidRow(
                  box(textInput("dep4", "dependent variable", value = "dep_var")  ),
                  box(textInput("indep4", "independent variable(s)", value = "indep_var1 * indep_var2") ),
                  box(
                    actionButton("leve", "Levene test",
                                 style = "background-color: #2e6da4"),
                    actionButton("shap", "Shapiro test"),

                    tags$style("#center {background-color:#2e6da4;}"),
                    selectInput("center", "Data center of each group: mean or median",
                                c("Mean" = "mean",
                                  "Median" = "median"), selectize = FALSE),

                    h4(strong("Results of test:")),
                    tableOutput("as_result"),
                    verbatimTextOutput("as_error"),
                    width = 12
                  ),
                  box(
                    actionButton("ass_save", "Add output to result-file"),
                    actionButton("ass_upload", "Save and exit"),
                    width = 12
                  ),
                  box(
                    checkboxGroupInput("TableSaved3", "Saved results to file", NULL),
                    width = 12
                  )
                )
       ),

      # statistical tests
      tabItem(tabName = "tests",
              fluidRow(
                box(textInput("dep6", "dependent variable", value = "dep_var")  ),
                box(textInput("indep6", "independent variable(s)", value = "indep_var1 * indep_var2") ),
                column(6,
                  box(
                    actionButton("ttest", "t test"),
                    sliderInput("conflevel", "Confidence level of the interval",
                                min = 0, max = 1, value = 0.95),
                    selectInput("alt", "Alternative hypothesis",
                                c("Two sided" = "two.sided",
                                  "Less" = "less",
                                  "Greater" = "greater")),
                    selectInput("paired", "Paired or unpaired t-test",
                                c("Unpaired" = "up",
                                  "Paired" = "p")),
                    selectInput("vareq", "Are the two variances treated as equal or not?",
                                c("Equal" = "eq",
                                  "Not equal" = "noeq")),
                    width = 12
                  )
                ),
                column(6,
                  box(actionButton("aov", "Anova"),
                      actionButton("kw", "Kruskal Wallis"),
                      width = 12
                  ),
                  box(
                    tags$style("#padj {background-color:#2e6da4;}"),
                    actionButton("tuk", "Tukey HSD test"),
                    actionButton("kwposthoc", "Kruskal Wallis test & multiple comparison of treatments",
                                 style = "background-color: #2e6da4"),
                    actionButton("lsd", "Least significant difference (LSD) test",
                                 style = "background-color: #2e6da4"),
                    actionButton("scheffe", "Scheffe test"),
                    actionButton("regw", "REGW test"),
                    sliderInput("pval", "P-value",
                                min = 0, max = 0.15, value = 0.05),
                    selectInput("bvsub", "Design",
                                c("Balanced" = "ba",
                                  "Unbalanced" = "ub") ),
                    selectInput("padj", "Adjusted p method",
                                c("Holm" = "holm",
                                  "Hommel" = "hommel",
                                  "Hochberg" = "hochberg",
                                  "Bonferroni" = "bonferroni",
                                  "BH" = "BH",
                                  "BY" = "BY",
                                  "fdr" = "fdr"), selectize = FALSE ),
                    "fdr" = "fdr", width = 12)
              ),



              box(h4(strong("Results of tests:")),
                  tableOutput("tests_result"),
                  verbatimTextOutput("tests_error"),
                  width = 12
              ),


              box(
                actionButton("tests_save", "Add output to result-file"),
                actionButton("tests_upload", "Save and exit"),
                width = 12
              ),
              box(
                checkboxGroupInput("TableSaved4", "Saved results to file", NULL),
                width = 12
              )


            )
      ),


      # LC50
      tabItem(tabName = "lc50",
              fluidRow(
                box(textInput("abs", "Absorbance", value = "abs")  ),
                box(textInput("names", "names of compounds", value = "names") ),
                box(textInput("conc", "concentrations", value = "conc") ),
                box(
                  actionButton("lc50", "Calculate LC50"),
                  h4(strong("Results of test:")),
                  tableOutput("lc50_result"),
                  verbatimTextOutput("lc50_error"),
                  width = 12
                ),
                box(
                  actionButton("lc50_save", "Add output to result-file"),
                  actionButton("lc50_upload", "Save and exit"),
                  width = 12
                ),
                box(
                  checkboxGroupInput("TableSaved5", "Saved results to file", NULL),
                  width = 12
                )
              )
      ),



      # Documentation
      # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      tabItem(tabName = "documentation",
              box(
            
            markdown("
            <div style='text-align: justify'>

            ## Raw Data
              In the *Data* tab the file from the ELN is depicted. In case the download didn't work or the file couldn't be uploaded into R (the programming language used for all tests) a message is printed.
            </div>"),
            tableOutput("example_csv"),
            
            markdown("
            <div style='text-align: justify'>

            ## Data transformation
              In case data transformation is required it can be conducted using the forms below the datatable.
              In the first text field the operations which should be conducted have to be specified. The allowed functions are listed in the table below.
              To do this the names of the columns can be used as variables.
              After specifiyng the operation a new name for a column has to be chosen in which the result of the calculation is stored.
              For more information about the allowed functions check the documentation (https://stat.ethz.ch/R-manual/R-devel/library/stats/html/00Index.html).
              After pressing **Modify** the operations are conducted on each row independently. Below you can see the allowed functions.
            </div>"),
            tableOutput("allowed_functions"),

            markdown("
              <div style='text-align: justify'>

              # Plots
              The tab *Plots* enables the user to visualize data. Therefore, a x and y variable have to be specified. Additionally, it is possible among other paramters to add a colour variable.
              This variable is used to group the data. Moreover, three different plots: Boxplots, Dotplots and Lineplots can be generated.
              An example can be seen below. Here the variables are defined as the following: **x = Plant**, **y = uptake** and **colour variable = Treatment**.
              </div>
              "),

            plotOutput("example_plot"),

            markdown("
              <div style='text-align: justify'>

              ## Statistical tests
              The different statistical tests can be found in the tab *Correlations*, *Assumptions* and *Statistical tests*.
              Within the tab **Correlations** it is possible to check the association between paired samples.
              Three different correlation tests **Pearson**, **Spearman** and **Kendall** can be used.
              Furthermore, the **Confidence level of the interval** can be specified in a range from 0 to 1. The start value is set to 0.95.
              Beyond that the **Alternative hypothesis** can be chosen between **Two sided**, **Less** and **Greater**. The default is set to **Two sided**.
              For instance (see below), one could calculate the correlation between **uptake** and **conc** of the table above.  To do this one has to write **uptake** in the first text field and **conc** in the second one.
              After pressing **Pearson correlation** the result is shown below. The first value **estimate** is the correlation coefficient.
              The second value **statistic** is the t-test statistic value. Next the **p-value** indicates whether it is significant or not.
              The value **parameter** defines the degrees of freedom. Finally, the last two values are the lower and upper boundary of the 95% confidence interval of the correlation coefficient.
              </div>"),

            tableOutput("example_cortest"),

            markdown("
              <div style='text-align: justify'>

              ## Defining formulas
              For each statistical test except the **shapiro test** the dependent and independent variable(s) have to follow the R syntax for **formulas**.
              Therefore, in the next paragraph the syntax for the tests is explained.
              If only one variable is used as independent variable then it is enough to state the name of the column in the according text field.
              The different terms of the model are seperated using the **+** sign. The terms themselves consist of variable and factor names separated by **:**.
              This is interpreted as the interaction of all variables and factors within the specific term.
              If one wants to examine interactions (factor crossing) one can write **a\\*b**. Alternatively one can write **a + b + a:b**.
              Furthermore, the operator **^** indicates crossing to the specified degree. For instance, **(a + b)^2** is the same as **(a + b)*(a + b)**.
              Moreover, the operator **%in%** indicates that the term at the left are nested within those of the right. For example, **a + b %in% a** is the same as **a + a:b**.
              The **/** operator is a shorthand. Thus **a / b** is the same as **a + b %in% a**.
              Finally, the operator **-** removes the specified term from the model. For instance, **(a + b + c)^2 - a:b** is the same as **a + b + c + b:c + a:c**.
              For more information check the documentation (https://stat.ethz.ch/R-manual/R-devel/library/stats/html/00Index.html).
              ## Check assumptions
              It is possible to check assumptions for tests such as the anova. The variance homogenity can be checked with the **Levene test**.
              Therefore one has to specify the dependent variable and the independent variable(s).
              For instance (see below) the **Levene test** is conducted for the dependent variable **uptake** and the independent variable **Plant**.
              As you can see the p-value is above 0.05 thus one would assume equal variances.
              </div>"),

            tableOutput("example_levene"),

            markdown("
              <div style='text-align: justify'>

              Moreover, the **Shapiro test** can be used to test whether the data is normal distributed.
              In contrast to the other tests it is required to combine the independent variables in a comma separated way.
              For instance conducting the shapiro test for the dependent variable **uptake** one has to write the variable **'uptake'** in the corresponding text field.
              Assuming that later the interaction between **Plant** and **Type** one has to type **Plant, Type** into the text field describing the independent variables.
              The results are depicted below. As you can see the p-value is smaller then 0.05 for the first group meaning that the data is not normally distributed.
              In contrast the group **Mississippi.MC2** is normally distributed as the p-value (0.15) is larger then 0.05.
              </div>"),

            tableOutput("example_shapiro"),

            markdown("
              <div style='text-align: justify'>

              ## Compare 2 groups
              To compare two groups with each other a **t-test** can be used. Which follows the usual syntax for dependent and independent variable (see above).
              Several parameters can be set for the test. First of all the **Confidence level of the interval** can be set. The default is 0.95.
              Moreover, the **Alternative hypothesis** can be changed. The default is **Two sided**. Alternatively one can chose **Less** and **Greater**.
              Furthermore, it is required to decide between an unpaired and paired t-test. The default is set to unpaired.
              The last option which is available is the question whether the variances are equal or not. The default is set to equal.
              Below you see the result for the dependent variable **uptake** and the independent variable **Type**. **Parameter** indicates are the number of degrees aof freedom.
              The p-value is belwo 0.05 thus there is a significant difference between the two groups. For more information see http://www.sthda.com/english/wiki/paired-samples-t-test-in-r.
              </div>
              "),

            tableOutput("example_ttest"),

            markdown("
              <div style='text-align: justify'>

              ## Compare more than 2 groups
              If you want to compare more than two groups you have to state the dependent and independt variable in the same way as for the Levene or t-test.
              The Anova can be used if the assumptions for normaility and variance homogentiy are met.
              In case the assumptions are not met even after data transformation one can conduct the Kruskal Wallis test.
              However, in this case only one independent variable is allowed. In case someone want to compare more groups one can combine them in the *data* tab. \
              After Anova or Kruskal Wallis test PostHoc tests can be conducted.
              The Tukey HSD, LSD-test, Scheffe-test and the REGW-test calculate an Anova in advance to the PostHOC test.
              In contrast the other PostHoc tests (Kurskal Wallis test & multiple comparison treatment) performs independently.
              Moreover, can the p-value be set as threshold for significance. The default is set to 0.05.
              Furthermore, are different methods for adjusting the p-value in case of using the **Kruskal Wallis test & multiple comparison of treatments** and **LSD-test**.
              The default of the correction is set to **Holm**.
              For example an anova is conducted with the dependent variable **uptake** and the independent term: **Plant** the results are shown below.
              As one can see there is a significant difference for **Treatment** and **Type**. However, the interaction is not significant.
              Afterwards, a TukeyHSD is conducted. Which explicitly shows which groups differ from each offer. The information for **Treatment**, **Type** and are shown as they are included in the **Plant** variable.
              </div>
              "),

            tableOutput("example_aov"),
            tableOutput("example_tukey"),

            width = 12
              ),

            height = 12
      )




    )

  )
)
