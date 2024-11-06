library(shinytest2)
source("MainApp.R")
Sys.setenv(RUN_MODE = "BROWSER") # SERVER
app <- shinyApp(ui, server)
app <- AppDriver$new(app)

app$upload_file(
  file = "./test_data/CO2.csv"
)
app$get_screenshot()

#                            "OP-acos"                         
#                             "OP-as.char"                      
#  [37] "OP-as.fact"                       "OP-as.int"                       
#  [39] "OP-as.real"                       "OP-asin"                         
#  [41] "OP-atan"                          "OP-bracket_close"                
#  [43] "OP-bracket_open"                  "OP-c"                            
#  [45] "OP-ceil"                          "OP-colnames_Plant_0"             
#  [47] "OP-colnames_Treatment_0"          "OP-colnames_Type_0"              
#  [49] "OP-colnames_X_0"                  "OP-colnames_conc_0"              
#  [51] "OP-colnames_df_0"                 "OP-colnames_uptake_0"            
#  [53] "OP-comma"                         "OP-cos"                          
#  [55] "OP-cosh"                          "OP-dbinom"                       
#  [57] "OP-div"                           "OP-dnorm"                        
#  [59] "OP-dpois"                         "OP-dunif"                        
#  [61] "OP-editable_code"                 "OP-eq"                           
#  [63] "OP-exp"                           "OP-exponent"                     
#  [65] "OP-floor"                         "OP-get_cols"                     
#  [67] "OP-get_rows"                      "OP-grep"                         
#  [69] "OP-iv"                            "OP-larger"                       
#  [71] "OP-larger_eq"                     "OP-log"                          
#  [73] "OP-log10"                         "OP-max"                          
#  [75]                         "OP-median"                       
#                             "OP-mul"                          
#  [79] "OP-nc"                            "OP-not_eq"                       
#  [81] "OP-paste"                         "OP-paste0"                       
#  [83] "OP-pbinom"                        "OP-pnorm"                        
#  [85] "OP-ppois"                         "OP-punif"                        
#  [87] "OP-qbinom"                        "OP-qnorm"                        
#  [89] "OP-qunif"                         "OP-rbinom"                       
#  [91] "OP-rnorm"                         "OP-round"                        
#  [93] "OP-rpois"                         "OP-run_op"                       
#  [95] "OP-run_op_intermediate"            OP-sub
#  [97] "OP-sd"                            "OP-sin"                          
#  [99] "OP-sinh"                          "OP-smaller"                      
# [101] "OP-smaller_eq"                    "OP-sqrt"                         
# [103] "OP-strsplit"                      "OP-sub"                          
# [105] "OP-substr"                        "OP-sum"                          
# [107] "OP-tan"                           "OP-tanh"                         
# [109] "OP-tolower"                       "OP-toupper"                      
# [111] "OP-trunc"                         


app$set_inputs(`conditionedPanels` = "DataWrangling")
app$get_screenshot()
# app$get_values()$input |> names()

app$click("OP-abs")
app$click("OP-colnames_conc_0")
app$click("OP-bracket_close")
app$click("OP-add")
app$click("OP-runif")
app$click("OP-colnames_conc_0")
app$click("OP-bracket_close")
app$set_inputs(`OP-iv` = "test1")
app$click("OP-run_op_intermediate")
Sys.sleep(3)

app$click("OP-sub")
app$click("OP-min")
app$click("OP-colnames_conc_0")
app$click("OP-bracket_close")
app$click("OP-sub")
app$click("OP-mean")
app$click("OP-colnames_conc_0")
app$click("OP-bracket_close")
app$set_inputs(`OP-iv` = "test2")
app$click("OP-run_op_intermediate")


app$get_screenshot()

