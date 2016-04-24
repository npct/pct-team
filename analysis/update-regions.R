# Aim: update regions

source("set-up.R")
regions = geojson_read("../pct-bigdata/regions.geojson", what = "sp")

# Regions 1: london

lnd_files = list.files(path = "../pct-bigdata/london/", pattern = "shp", full.names = T)
lnd_regions = NULL

for(i in 1:length(lnd_files)){
  if(is.null(lnd_regions))
    lnd_regions = shapefile(lnd_files[i]) else{
      lnd_tmp = shapefile(lnd_files[i])
      lnd_regions = rbind(lnd_regions, lnd_tmp, makeUniqueIDs = TRUE)
    }
}

plot(lnd_regions)
head(lnd_regions)
head(regions)

lnd_regions@data = data.frame(
  Region = c("london-central", "london-east", "london-north", "london-south", "london-west"),
  url = c("http://geo8.webarch.net/london-central", "http://geo8.webarch.net/london-east",
          "http://geo8.webarch.net/london-north", "http://geo8.webarch.net/london-south",
          "http://geo8.webarch.net/london-west"),
  url_txt = c("<a href=\"http://geo8.webarch.net/london-central\" target =\"_top\">london-central</a>  <br> 2% cycling to work",
              "<a href=\"http://geo8.webarch.net/london-east\" target =\"_top\">london-east</a>  <br> 2% cycling to work",
              "<a href=\"http://geo8.webarch.net/london-north\" target =\"_top\">london-north</a>  <br> 2% cycling to work",
              "<a href=\"http://geo8.webarch.net/london-south\" target =\"_top\">london-south</a>  <br> 2% cycling to work",
              "<a href=\"http://geo8.webarch.net/london-west\" target =\"_top\">london-west</a>  <br> 2% cycling to work"
  ),
  pcycle = NA,
  Upper = c("London-central", "London-east", "London-north", "London-south",
            "London-west")
  )

lnd_regions = spTransform(lnd_regions, proj4string(regions))
library(maptools)
lnd_regions = spChFIDs(lnd_regions, as.character(45:49))
row.names(lnd_regions) = getSpPPolygonsIDSlots(lnd_regions)


plot(regions)
regions = rbind(regions, lnd_regions, makeUniqueIDs = T)
plot(regions)

geojson_write(regions, file = "../pct-bigdata/regions-london.geojson")

# generate report
# knitr::spin("analysis/update-regions.R")

# 2 update warwickshire
las = readOGR(dsn = "../pct-bigdata/cuas-mf.geojson", layer = "OGRGeoJSON")
cov = las[grepl(pattern = "oventry", las$CTYUA12NM), ]
cov = gBuffer(cov, byid = T, width = 0.007)
plot(cov)
library(rmapshaper)
cov_simple = ms_simplify(input = cov, keep = 0.1)
plot(cov_simple, add = T)
plot(regions, add = T)

cov_cent = gCentroid(cov)
plot(cov_cent, add = T)
sel = grep(pattern = "west-mid", regions$Region)
wm = regions[cov_cent,]
wm_new = gDifference(wm, cov_simple)
spChFIDs(wm_new) = row.names(regions@data[sel,])
wm_new = SpatialPolygonsDataFrame(Sr = wm_new, data = regions@data[sel,])

plot(wm_new, col = "red", add = T)

# double check regions are right
plot(regions[sel,])
plot(wm_new, add = T, col = "red")
regions@polygons[[sel]] = wm_new@polygons[[1]]
plot(cov_simple)
plot(regions[sel,], add =T)
plot(regions, add = T, col = "grey")
s = which(regions$Region == "warwickshire")
plot(regions[s,], col = "red", add  = T)
war_new = rgeos::gUnion(regions[s,], cov_simple)
plot(war_new, add = T, col = "blue")
plot(war_new)
plot(cov, add = T)
regions@polygons[[s]] = war_new@polygons[[1]]
plot(regions)
plot(regions[s,], add = T, col = "red")
plot(cov, col = "blue", add = T)

geojson_write(regions, file = "../pct-bigdata/regions.geojson")
