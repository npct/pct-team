# Aim: find average gradient in administrative zones anywhere in the world

# Requirements:
# devtools::install_github ('osmdatar/osmdatar')
library(osmdatar)
# devtools::install_github("hrbrmstr/overpass")
library(overpass)
# devtools::install_github("edzer/sfr")
library(sf)
library(tmap)
library(raster)

# load administrative data
source("set-up.R")
download.file("http://download.cbs.nl/regionale-kaarten/2011-buurtkaart-shape-versie-3.zip", "2011-buurtkaart-shape-versie-3.zip")
unzip("2011-buurtkaart-shape-versie-3.zip")
# file.copy("shape 2011 versie 3.0/buurt_2011_v3.prj", "/tmp/oppervlakte cbs buurten.prj", overwrite = T)
zones = shapefile("shape 2011 versie 3.0/buurt_2011_v3.shp")
# zones = shapefile("/tmp/oppervlakte cbs buurten.shp") # 11574
zones = readRDS("../pct-bigdata/lsoa_zones_ew.Rds")
proj4string(zones)
z = spTransform(zones, CRSobj = CRS("+init=epsg:4326"))
# zones = zones[1:20,] # subset zones (if needed)
b = bb(z)

# download.file("http://download.geofabrik.de/europe/netherlands-latest-free.shp.zip", destfile = "/tmp/netherlands-latest-free.shp.zip")
# unzip("/tmp/netherlands-latest-free.shp.zip", exdir = "/tmp/")
# res = sf::st_read(dsn = "/tmp/", "gis.osm_roads_free_1") # 1.2 gb!
# summary(res$fclass)
# head(res)
# cyclable = c("bridleway", "cycleway", "living_street", "residential", "secondary", "tertiary")
# res_cyclable = res[res$fclass %in% cyclable,]
# # sf::st_write(res_cyclable, dsn = "input-data/", layer = "res_cyclable")
# res_mini = res_cyclable[sample(nrow(res_cyclable), size = nrow(res_cyclable) / 20),]
# plot(res_mini)

plot(z, col = "red")
plot(r, add = T)

rsample_points = spsample(x = r, n = 10000, type = "random")
points(rsample_points, col = "blue")

# get hilliness data
alt90 = getData(name = "alt", country = "NLD", download = TRUE)
# alt90 = getData(name = "alt", country = "GBR", download = TRUE)
plot(alt90)

grad90 = raster::terrain(x = alt90, opt = "slope", unit = "tangent", neighbors = 4)
summary(grad90)
zones_p = gCentroid(zones, byid = T)
zones_nl_slope = raster::extract(x = grad90, y = z, fun = mean)
zones@data = cbind(zones@data, slope_percentage = zones_nl_slope * 100)
zones$slope_percentage[is.na(zones$slope_percentage)] = 0
tmap_mode()
tm_shape(zones) +
  tm_fill("slope_percentage")

summary(zones@data)
write.csv(zones@data[c("BU_CODE", "slope_percentage")], file = "../pct-bigdata/nl-gradients-average.csv")
mapview::mapview(zones[zones$slope_percentage > 1,])
