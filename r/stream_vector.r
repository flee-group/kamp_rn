library(sf)
library(watershed)
library(raster)
library(data.table)

dem = raster("../ybbs_rn/data/dhm_at_lamb_10m_2018_epsg3035.tif")
stream = stack("output/kamp.grd")
corine = st_read("data/kamp_lc.gpkg")
geo = st_read("data/kamp_geo.gpkg")
geo = st_transform(geo, st_crs(corine))

Tp = pixel_topology(stream)
kamp_rn = vectorise_stream(stream, Tp)
kamp_rn$slope = river_slope(kamp_rn, dem)

Tr = reach_topology(stream, Tp)
kamp_rn$order = strahler(Tr)

# png("~/Desktop/kamp.png", width = 1000, height = 1000)
# plot(st_geometry(kamp_rn), lwd = kamp_rn$order*0.5, col = 'blue')
# dev.off()

kamp_lc = w_intersect(kamp_rn, areas = corine, buff = 100,
            area_id = "code_18", drainage = stream$drainage)

kamp_geo = w_intersect(kamp_rn, areas = geo, buff = 100,
            area_id = "xx", drainage = stream$drainage)

kamp_lc[, layer := "land_cover"]
kamp_geo[, layer := "geology"]

kamp_stats = rbind(kamp_lc, kamp_geo)

st_write(kamp_rn, "output/kamp.gpkg", append = FALSE)
saveRDS(kamp_stats, "output/kamp_lc_geo.rds")

