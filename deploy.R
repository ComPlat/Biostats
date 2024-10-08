setwd("/home/konrad/Documents/Biostats")
shinylive::export("./BiostatsGithubPage", "docs", verbose = TRUE)
httpuv::runStaticServer("docs")

setwd("/home/konrad/Documents/Biostats/BiostatsGithubPage")
source("app.R")
shinyApp(ui, server)
