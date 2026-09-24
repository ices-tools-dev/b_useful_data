filename <- "ICES_ecoregions.zip"
# download and unzip
download(paste0("http://gis.ices.dk/shapefiles/", filename))
unzip(filename)
# delete zip file
unlink(filename)