# Aim: update centroids used in the PCT

# load centroids
source("set-up.R")
centsa = readOGR("../pct-bigdata/cents.geojson", "OGRGeoJSON")

# identify problematic centroids
p = readxl::read_excel("/tmp/160301_MovePoints_v2.xlsx")
head(p)
head(p$msoa11cd)
p$msoa11cd = gsub(pattern = " ", replacement = "", x = p$msoa11cd)
p = rename(p, geo_code = msoa11cd)
head(cents$geo_code)
sel = centsa$geo_code %in% p$geo_code
sum(sel)
# where are they?
r = read_osm(bb(centsa))
tm_shape(r) +
  tm_raster() +
  tm_shape(centsa[sel,]) +
  tm_dots(size = 0.3)

# plot just one:
eg_old = centsa[which(sel)[1],]
r2 = read_osm(bb(centsa[which(sel)[1]:(which(sel)[1] - 1),], ext = 2))
tm_shape(r2) +
  tm_raster() +
  tm_shape(eg_old) +
  tm_dots(size = 0.3)

# change the coordinates
cfrom = coordinates(centsa[sel,])
cto = inner_join(centsa@data, p[c("geo_code", "new_longitude", "new_latitiude")])
plot(coordinates(centsa[sel,])[,1], cto$new_longitude)
cor(coordinates(centsa[sel,])[,1], cto$new_longitude) # good fit!
plot(coordinates(centsa[sel,])[,2], cto$new_latitiude)
cor(coordinates(centsa[sel,])[,2], cto$new_latitiude) # good fit

coordinates(centsa[sel,]) <- cto[c("new_longitude", "new_latitiude")] # fail
centsa@coords[sel,1] <- cto$new_longitude
centsa@coords[sel,2] <- cto$new_latitiude

centsa_new = centsa

# plot updated example:
# r2 = read_osm(bb(centsa[which(sel)[1]:(which(sel)[1] - 1),], ext = 2))
tm_shape(r2) +
  tm_raster() +
  tm_shape(centsa[which(sel)[1],]) +
  tm_dots(size = 0.3, col = "red") +
  tm_shape(eg_old) +
  tm_dots(size = 0.3)

library(leaflet)
centsa = readOGR("../pct-bigdata/cents.geojson", "OGRGeoJSON")
leaflet() %>% addTiles() %>%
  addCircles(data = centsa[sel,], col = "red") %>%
  addCircles(data = centsa_new[sel,]) # they've all changed!

geojson_write(centsa_new, file = "../pct-bigdata/cents.geojson")
writeOGR(centsa_new, "../pct-bigdata/cents", "OGRGeoJSON", driver = "GeoJSON")
# knitr::spin("analysis/update-cents.R", format = "pdf")