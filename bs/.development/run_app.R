# Run in browser

# setwd("/home/konrad/Documents/Biostats")
# shinylive::export("./App", "docs", verbose = TRUE)
# httpuv::runStaticServer("docs")

Sys.setenv(RUN_MODE = "BROWSER")
library(bs)
run_app()
