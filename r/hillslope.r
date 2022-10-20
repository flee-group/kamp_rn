library(sf)
library(watershed)
library(terra)
library(WatershedTools)
library(ggplot2)
library(data.table)


kamp_rn = st_read("output/kamp.gpkg")
rn_buff = st_buffer(kamp_rn, dist = 100)
rn_buff_d = st_union(rn_buff)
# ggplot() + geom_sf(data = rn_buff, fill = "#3366aa33") + 
        # geom_sf(data = kamp_rn, colour = "#2244bb")

stream = rast("output/kamp.grd")
dem = rast("../ybbs_rn/data/dhm_at_lamb_10m_2018_epsg3035.tif")
dem = crop(dem, stream)

ws = readRDS("output/kamp_ws.rds")

slope_rad = terrain(dem, unit = "radians")
slope = rast(c(slope_rad = slope_rad, slope_prop = tan(slope_rad)))


## get the outlets of each reach (note that outlets funciton is broken)
rch_out = sapply(kamp_rn$reach_id, \(i) {
	ii = which(ws$data$reachID == i)
	mat = ws$adjacency[ii, ii, drop = FALSE]
	ii[which(Matrix::colSums(mat) == 0)]
})
sf_outlets = st_as_sf(ws$data[rch_out,])

# ggplot() + geom_sf(data = rn_buff, fill = "#3366aa33") + 
#         geom_sf(data = kamp_rn, colour = "#2244bb") + 
#         geom_sf(data = sf_outlets, size = 0.5, col = "#660033")

## catchments

# get average, sd slope for each reach in buffer area
rch_slope = data.table(terra::extract(slope, vect(rn_buff)))
rch_slope = rch_slope[, .(slope_rad = mean(slope_rad), slope_rad_sd = sd(slope_rad), 
	slope_prop = mean(slope_prop), slope_prop_sd = sd(slope_prop)), keyby = .(reach_id = ID)]
colnames(rch_slope) = c("reach_id", "slope_radians_buffer", "slope_radians_buffer_sd", 
		"slope_proportion_buffer", "slope_buffer_proportion_sd")
rch_slope$slope_radians_catchment = numeric()
rch_slope$slope_radians_catchment_sd = numeric()
rch_slope$slope_proportion_catchment = numeric()
rch_slope$slope_proportion_catchment_sd = numeric()
rch_slope$slope_radians_upstream = numeric()
rch_slope$slope_radians_upstream_sd = numeric()
rch_slope$slope_proportion_upstream = numeric()
rch_slope$slope_proportion_upstream_sd = numeric()

i = 1
for(rch in kamp_rn$reach_id) {
	ca = rast(catchment(stream, "points", st_coordinates(sf_outlets)[rch,], area = FALSE))
	ca = resample(ca, slope)
	ca_v = as.polygons(ca)
	ca_v = st_as_sf(ca_v)
	ca_v = st_transform(ca_v, crs = crs(rn_buff_d))
	
	# catchment statistics
	means_c = zonal(slope, ca, na.rm = TRUE)
	sds_c = zonal(slope, ca, fun = sd, na.rm = TRUE)

	# upstream statistics
	up_polygon = st_intersection(rn_buff_d, ca_v)
	slope_up = data.table(terra::extract(slope, vect(up_polygon)))
	slope_up = slope_up[, .(sl_r_u = mean(slope_rad, na.rm = TRUE), sl_r_u_sd = sd(slope_rad, na.rm = TRUE), 
		sl_p_u = mean(slope_prop, na.rm = TRUE), sl_p_u_sd = sd(slope_prop, na.rm = TRUE))]

	## reach buffer
	rch_slope[reach_id == rch, c("slope_radians_catchment", "slope_radians_catchment_sd", 
			"slope_proportion_catchment", "slope_proportion_catchment_sd", 
			"slope_radians_upstream", "slope_radians_upstream_sd", "slope_proportion_upstream", 
			"slope_proportion_upstream_sd") := .(means_c$slope_rad, sds_c$slope_rad, 
			means_c$slope_prop, sds_c$slope_prop, slope_up$sl_r_u, slope_up$sl_r_u_sd,
			slope_up$sl_p_u, slope_up$sl_p_u_sd)]
	cat(i, " of ", nrow(kamp_rn), "    \r")
	i = i + 1
}
saveRDS(rch_slope, "output/reach_slope_kamp_data_table.rds")

rch_slope_tall = melt(rch_slope, id = "reach_id")
rch_slope_tall[, units := .(ifelse(grepl("radians", variable), "radians", "proportion"))]
rch_slope_tall[, statistic := .(ifelse(grepl("sd$", variable), "stdev", "mean"))]
rch_slope_tall[, location := .(ifelse(grepl("buffer", variable), "buffer", NA))]
rch_slope_tall[grep("upstream", variable), location := "upstream_buffer"]
rch_slope_tall[grep("catchment", variable), location := "catchment"]
rch_slope_tall$variable <- NULL
colnames(rch_slope_tall)[2] = "slope"

saveRDS(rch_slope_tall, "output/reach_slope_kamp_tall.rds")
