# update broken lines
source("set-up.R")
rf = readRDS("../pct-bigdata/rf.Rds")
rf$nv = n_vertices(rf)
rf_redo = rf[rf$nv < 3,]
plot(rf_redo)
l = readRDS("../pct-bigdata/l.Rds")
l_redo = l[rf$nv < 3,]
plot(l_redo)
names(l_redo)
destnames = c(l$Area.of.residence, l$Area.of.workplace)
top_20 = sort(table(destnames), decreasing=TRUE)[1:20]

# get centroids
centsa = geojson_read("../pct-bigdata/cents-scenarios.geojson", what = "sp")
cents_check = centsa[centsa$geo_code %in% names(top_20),]
plot(cents_check[1,])
lines(l_redo[])
library(mapview)
mapview(l_redo) +
plot(cents_check)


