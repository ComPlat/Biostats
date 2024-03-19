setwd("/home/konrad/Documents/GitHub/shinychem/BiostatsGithubPage/App")
shinylive::export(".", "docs", verbose = TRUE)
httpuv::runStaticServer("docs")

