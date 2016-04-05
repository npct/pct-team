# update broken lines
source("set-up.R")
rf = readRDS("../pct-bigdata/rf.Rds")
rf$nv = n_vertices(rf)
rf_redo = rf[rf$nv < 3,]
plot(rf_redo)
l = readRDS("../pct-bigdata/pct_lines_oneway_shapes.Rds")
l_redo = l[rf$nv < 3,]
plot(l_redo)
names(l_redo)
destnames = c(l$Area.of.residence, l$Area.of.workplace)
top_20 = sort(table(destnames), decreasing=TRUE)[1:20]
geojson_write(l_redo, file = "/tmp/l_redo.geojson") # to plot in qgis

lredo_code = readxl::read_excel("/tmp/160331_ExtraODpairs.xlsx", sheet = 2)
head(lredo_code)
head(l@data[1:4])
head(l$id)
lredo_code$id = paste0(lredo_code$home_msoa, lredo_code$work_msoa)
lsel = l$id %in% lredo_code$id
l_redo = l[lsel,]
# get centroids
centsa = geojson_read("../pct-bigdata/cents-scenarios.geojson", what = "sp")
l2do = od2line(l_redo@data, centsa)
plot(l_redo)
plot(l2do, col = "red")
# check the l2do lines are different
library(leaflet)
leaflet() %>% addTiles() %>% addPolylines(data = l2do) %>%
  addPolylines(data = l_redo, col = "red")
rf2 = line2route(l2do, route_fun = "route_cyclestreet", plan = "fastest")
rq2 = line2route(l2do, route_fun = "route_cyclestreet", plan = "quietest")
leaflet() %>% addTiles() %>% addPolylines(data = l2do, col = "grey") %>%
  addPolylines(data = rf2, col = "aqua") %>%
  addPolylines(data = rq2, col = "purple") %>%
  addPolylines(data = l_redo, col = "red")
nrow(rf2)
head(rf2)
rf2@data = cbind(l2do@data[1:2], rf2@data)
rq2@data = cbind(l2do@data[1:2], rq2@data)
write_csv(rf2@data, "/tmp/rf2.csv")
write_csv(rq2@data, "/tmp/rq2.csv")
write_csv(l2do@data, "/tmp/l2.csv")
geojson_write(rf2, file = "/tmp/rf2.geojson")
geojson_write(rq2, file = "/tmp/rq2.geojson")

# plot(cents_check[1,])
# lines(l_redo[])
# library(mapview)
# mapview(l_redo) +
# plot(cents_check)
#
#
