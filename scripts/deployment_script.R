shinylive::export(appdir = "bowel_brawl", destdir = "docs")

httpuv::runStaticServer("docs/", port=8008)