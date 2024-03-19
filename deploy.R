setwd("/home/konrad/Documents/GitHub/shinychem")
shinylive::export("./BiostatsGithubPage", "docs", verbose = TRUE)
httpuv::runStaticServer("docs")

