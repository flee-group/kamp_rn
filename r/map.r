library(terra)
library(sf)
dem = rast("../ybbs_rn/data/dhm_at_lamb_10m_2018_epsg3035.tif")
riv = st_read("output/kamp.gpkg")
kamp = rast("output/kamp.grd")
dem = crop(dem, kamp)

# pts = data.frame(name = c("dam", "ds_sample", "source"), 
# 	x = c(17.762885, 17.962372, 18.552181), y = c(43.655467, 43.651676, 43.283714))
# pts = st_transform(st_as_sf(pts, coords = c('x', 'y'), crs = 4326), crs = 3035)
# pts = rbind(pts, st_as_sf(data.frame(name = "outlet", x = 4945013, y = 2308310), 
# 	coords = c('x', 'y'), crs = 3035))
sl = terrain(dem, unit = "radians")
as = terrain(dem, "aspect", unit = "radians")
hs = shade(sl, as)

ptcol = "#ffcc66"
rivcol = "#0099ff"
sf::sf_use_s2(FALSE)

bbox = st_as_sf(as.data.frame(matrix(ext(hs), ncol=2)), coords = c(1,2), crs = 3035)
bbox = st_transform(bbox, 4326)
bbox = st_coordinates(bbox)
lims = ext(as.vector(bbox + c(-1, 1, -1, 1)*7))

ne_base = rast("data/NE1_HR_LC_SR_W/NE1_HR_LC_SR_W.tif")
ne_base = crop(ne_base, lims)
ne_countries = st_read("data/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
ne_countries = st_crop(ne_countries, lims)


png(width=2000, height = 2000, file = "output/kamp.png")
## catch a weird R-internal error that pops up sometimes
par(mar = c(5,5,5,5), oma = c(5,5,5,5))
tryCatch(
	plot(hs, col=grey(0:100/100), alpha = 1, legend = FALSE, axes = FALSE),
	error = function(e) print(e))
axis(1, at = 4675000 + c(0, 1, 2, 5)*2000, labels = c("0 km", "1", "2", "5"), lwd=5, line = 1, cex.axis=2)
plot(dem, add = TRUE, alpha = 0.6, col = terrain.colors(100), axes = FALSE, legend=TRUE, 
	type = "continuous", plg=list(cex = 2))
mtext("Elevation(m)", outer=TRUE, cex = 2, adj=1)
plot(kamp$catchment, add = TRUE, alpha = 0.4, col = "#c3e4e3", axes = FALSE, legend = FALSE)
plot(st_geometry(riv), col=rivcol, lwd=riv$order, add = TRUE)
# plot(st_geometry(pts), col=ptcol, pch=16, add = TRUE, cex=2)
# text(st_coordinates(pts)[,1], st_coordinates(pts)[,2], pts$name, pos=4, col = ptcol, cex=1.5)

# map.scale(4675000, 2820000)

par(fig = c(0.75, 0.98, 0.04, 0.3), new = T, mar = c(1,1,1,1), bty='o')
plotRGB(ne_base)
plot(st_geometry(ne_countries), add = TRUE)
rect(bbox[1,1], bbox[1,2], bbox[2,1], bbox[2,2], border='red', col = 'NA', lwd=4)
dev.off()


