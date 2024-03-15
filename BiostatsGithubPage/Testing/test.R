setwd("/home/konrad/Documents/GitHub/shinychem/BiostatsGithubPage/Testing")
shinylive::export(".", "site", verbose = FALSE)
httpuv::runStaticServer("site")