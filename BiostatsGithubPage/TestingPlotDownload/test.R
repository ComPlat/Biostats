setwd("/home/konrad/Documents/GitHub/shinychem/BiostatsGithubPage/TestingPlotDownload")
shinylive::export(".", "site", verbose = FALSE)
httpuv::runStaticServer("site")