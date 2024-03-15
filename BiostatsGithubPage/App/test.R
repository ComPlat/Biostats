setwd("/home/konrad/Documents/GitHub/shinychem/BiostatsGithubPage/App")
shinylive::export(".", "site", verbose = FALSE)
httpuv::runStaticServer("site")
