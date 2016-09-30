# Aim: find average gradient in administrative zones anywhere in the world

# Requirements:
# devtools::install_github ('osmdatar/osmdatar')
library(osmdatar)
# devtools::install_github("edzer/sfr")
library(sf)

# load administrative data
source("set-up.R")
download.file("http://download.cbs.nl/regionale-kaarten/2011-buurtkaart-shape-versie-3.zip", "2011-buurtkaart-shape-versie-3.zip")
unzip("2011-buurtkaart-shape-versie-3.zip")
file.copy("shape 2011 versie 3.0/buurt_2011_v3.prj", "/tmp/oppervlakte cbs buurten.prj", overwrite = T)
zones = shapefile("shape 2011 versie 3.0/buurt_2011_v3.shp")
# zones = shapefile("/tmp/oppervlakte cbs buurten.shp") # 11574
proj4string(zones)
# subset zones (if needed)
z = zones[1:20,]
plot(z) # see what we've got
z = spTransform(z, CRSobj = CRS("+init=epsg:4326"))
b = bb(z)
# download road network
system.time({
  r = get_lines(bbox = b, key = "highway") # slow
})

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
