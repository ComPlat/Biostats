setwd("/home/konrad/Documents/GitHub/shinychem")
shinylive::export("./BiostatsGithubPage", "docs", verbose = TRUE)
httpuv::runStaticServer("docs")

setwd("/home/konrad/Documents/GitHub/shinychem/BiostatsGithubPage")
source("/home/konrad/Documents/GitHub/shinychem/BiostatsGithubPage/app.R")
shinyApp(ui, server)