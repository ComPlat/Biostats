shinylive::export("./bs/R/", "docs")
setwd("/home/konrad/Documents/Biostats/docs")
httpuv::runStaticServer(".")

# setwd("/home/konrad/Documents/Biostats/BiostatsGithubPage")
# source("app.R")
# shinyApp(ui, server)
