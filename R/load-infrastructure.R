# Aim: load the hs2 data
hs2codes = readxl::read_excel("/tmp/HS2 Cycleway Codes.xlsx")
saveRDS(hs2codes, "input-data/hc.Rds")

hs2route = tmap::read_shape("/tmp/HS2_Cycleway_Ph2_01022016.shp")
library(mapview)
mapview(hs2route)
names(hs2route)
saveRDS(hs2route, "input-data/hrc.Rds")

u = "http://www.thehs2.com/phase1/kmls/b/all.kml"
download.file(u, "/tmp/all1.kml")
library(rgdal)
ogrListLayers("/tmp/all1.kml")
hs21 = readOGR("/tmp/all1.kml", layer = "HS2 phase 1: post consultation route")
mapview(hs21)
saveRDS(hs21, "input-data/hs21.Rds")
geojsonio::geojson_write(hs21, file = "input-data/hs21.geojson")

u = "http://www.thehs2.com/phase2/kmls/b/all.kml"
download.file(u, "/tmp/all2.kml")
library(rgdal)
ogrListLayers("/tmp/all2.kml")
hs22 = readOGR("/tmp/all2.kml", layer = "HS2 phase 2: consultation route")
mapview(hs22)
saveRDS(hs22, "input-data/hs22.Rds")
geojsonio::geojson_write(hs22, file = "input-data/hs22.geojson")
