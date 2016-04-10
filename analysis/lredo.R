# update broken lines
source("set-up.R")
library(leaflet)

# load data
centsa = geojson_read("../pct-bigdata/cents-scenarios.geojson", what = "sp")
rf = readRDS("../pct-bigdata/rf.Rds")
rq = readRDS("../pct-bigdata/rq.Rds")
rf$nv = n_vertices(rf)
l = readRDS("../pct-bigdata/pct_lines_oneway_shapes.Rds")
nrow(l)
nrow(rf)

# make l and rf identical
head(l$id)
head(rf$id)
# update rf id
nchar(rf$id[1:2])
id1 = stringr::str_sub(rf$id, 1, 9)
head(id1)
id2 = stringr::str_sub(rf$id, 10, 18)
rf$id = paste(id1, id2)

rf = rf[rf$id %in% l$id,] # subset
nrow(rf) # the right length
summary(rf$id == l$id) # they're the same ids
plot(rf[nrow(rf),])
plot(l[nrow(l),], add = T) # yes they're the same!

sel = rf$nv < 3
l_redo = l[sel,]
plot(l_redo)
plot(rf[sel,], add = T, col = "red")
names(l_redo)
destnames = c(l_redo$Area.of.residence, l_redo$Area.of.workplace)

sum(sel)
s = which(sel)
message(paste0(length(s), " 'quiet' lines are straight"))
# pdf = sapply(rf[sel,]@lines, function(x) x@Lines[[1]]@coords)
m = leaflet() %>% addTiles() %>% addPolylines(data = rf[s,], col = "red") %>%
  addCircles(data = centsa)

for(i in s){
  # mapview(rf[i,])
  # if(i == 362) next()
  # plot(rf[i,])
  p = line2points(rf[i,])
  # nl = route_cyclestreet(p[1,], p[2,], plan = "fastest")
  pdists1 = spDistsN1(centsa, pt = p[1,])
  pdists2 = spDistsN1(centsa, pt = p[2,])
  sel_mindist1 = which.min(pdists1)
  sel_mindist2 = which.min(pdists2)
  # pl = spDistsN1(pts = p, pt = centsa[sel_mindist,])
  tryCatch({
    # if(which.min(pl) == 2)
      nl = route_cyclestreet(centsa[sel_mindist1,], centsa[sel_mindist2,], plan = "fastest")
  # else
        # nl = route_cyclestreet(p[2,], centsa[sel_mindist,], plan = "fastest")
      plot(nl)
      plot(rf[i,], add = T)
      rf@lines[[i]] <- Lines(nl@lines[[1]]@Lines, row.names(rf[i,]))
      rf@data[i,][1:ncol(nl)] = nl@data
  }, error = function(e){warning(paste0("Fail for line number ", i))})

  # plot(rf[i,], add = T)
  # mapview(rf[i,])
  message(i)
}
mapview::mapview(rf[s,])
centsa_old = geojson_read("../pct-bigdata/cents-scenarios.geojson", what = "sp")
leaflet() %>% addTiles() %>% addCircles(data = centsa_old[mc,]) %>%
  addCircles(data = centsa[mc,], col = "green")
geojson_write(centsa_old[mc,], file = "data-sources/failing-points.geojson")

new_nverts = n_vertices(rf[s,])
sum(new_nverts < 3) # halved the number of 'bad' lines

# Move the centroids that fail more than once using nearest in osrm
top_n = sort(table(destnames), decreasing=TRUE)
top_n = top_n[1:8]
c_update = names(top_n)
# nearest2spdf(lat = centsa@coords[1,2],  lng = centsa@coords[1,1],
#              osrmurl = "http://router.project-osrm.org/") # not working...
mc = which(centsa$geo_code %in% c_update)
length(mc) # number of points that need updating
for(i in mc){
  print(which(i == mc))
  tryCatch({
  ci = centsa[i,]
  coordinates(ci)
  new_cent = nearest_cyclestreets(lat = ci@coords[2], lng = ci@coords[1])
  # new_cent = nearest_google(lat = ci@coords[2], lng = ci@coords[1], google_api = gapi)
  coordinates(new_cents)
  centsa@coords[i,] = new_cent@coords
  }
  , error = function(e){warning(paste0("Fail for line number ", i))})
}

# now iterate, until
sum(n_vertices(rf) < 3) == 0
# save
saveRDS(rf, "../pct-bigdata/rf.Rds")

nverts = n_vertices(rq)
sel = nverts < 3
sum(sel)
s = which(sel)
plot(rq[sel,]) # lots of quiet routes
message(paste0(length(s), " 'quiet' lines are straight in "))

for(i in s){
  # mapview(rq[i,])
  # if(i == 362) next()
  # plot(rq[i,])
  p = line2points(rq[i,])
  # nl = route_cyclestreet(p[1,], p[2,], plan = "fastest")
  pdists1 = spDistsN1(centsa, pt = p[1,])
  pdists2 = spDistsN1(centsa, pt = p[2,])
  sel_mindist1 = which.min(pdists1)
  sel_mindist2 = which.min(pdists2)
  # pl = spDistsN1(pts = p, pt = centsa[sel_mindist,])
  tryCatch({
    # if(which.min(pl) == 2)
    nl = route_cyclestreet(centsa[sel_mindist1,], centsa[sel_mindist2,], plan = "fastest")
    # else
    # nl = route_cyclestreet(p[2,], centsa[sel_mindist,], plan = "fastest")
    plot(nl)
    plot(rq[i,], add = T)
    rq@lines[[i]] <- Lines(nl@lines[[1]]@Lines, row.names(rq[i,]))
    rq@data[i,][1:ncol(nl)] = nl@data
  }, error = function(e){warning(paste0("Fail for line number ", i))})

  # plot(rq[i,], add = T)
  # mapview(rq[i,])
  message(i)
}



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
