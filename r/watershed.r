library(WatershedTools)
library(raster)
library(sf)

dem = raster("../ybbs_rn/data/dhm_at_lamb_10m_2018_epsg3035.tif")
stream = stack("output/kamp.grd")
catch_area = raster("output/catchment.tif")
vv = st_read("output/kamp.gpkg")
dem = crop(dem, catch_area)
stream = crop(stream, catch_area)
ol = stream$id
ol = stack(ol)
names(ol) = "pixel_id"

k_ws = Watershed(stream$stream, stream$drainage, dem, stream$accum, catch_area, 
	otherLayers = ol)

# distance to outlet for all pixels
out_dist = wsDistance(k_ws, outlets(k_ws)$id) / -1000
k_ws$data$dist = out_dist
saveRDS(k_ws, "output/kamp_ws.rds")


