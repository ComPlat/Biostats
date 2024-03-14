shinylive::export(".", "site", verbose = FALSE)
httpuv::runStaticServer("site")
