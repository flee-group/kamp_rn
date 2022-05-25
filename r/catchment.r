library(raster)
library(watershed)

kamp = stack("output/kamp.grd")
Tp = pixel_topology(kamp)

pts = as.data.frame(rasterToPoints(kamp$stream))
pts$ca = NA
nr = nrow(pts)
reaches = unique(pts$stream)
for(r in reaches) {
	i = which(pts$stream == r)
	pts$ca[i] = catchment(kamp, type="points", y = as.matrix(pts[i, 1:2]), area = TRUE, Tp = Tp)
	dn = sum(!is.na(pts$ca))
	cat(paste0(Sys.time(), "  ", dn, "/", nr, " (", round(100 * dn/nr, 0), "%)", "\r"))
}
ca = rasterFromXYZ(pts[, c('x', 'y', 'ca')])
writeRaster(ca, "output/catchment.tif", overwrite = TRUE, gdal=c("COMPRESS=DEFLATE"))
