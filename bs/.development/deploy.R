setwd("/home/konrad/Documents/Biostats")

shinylive::export("./bs/inst/serverless_app/", "docs")
httpuv::runStaticServer("docs/")
