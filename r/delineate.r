library(watershed)
library(terra)
library(raster)

dem = rast("../ybbs_rn/data/dhm_at_lamb_10m_2018_epsg3035.tif")
ext = c(4670000, 4710000, 2810000, 2860000)
dem = crop(dem, ext)
outlet = c(4704445, 2847453)
kamp = delineate(raster(dem), threshold = 3e6, outlet = outlet, reach_len = 500)

writeRaster(kamp, file="output/kamp.grd", overwrite = TRUE, gdal=c("COMPRESS=DEFLATE"))

