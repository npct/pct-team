source("set-up.R")

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

regions = geojson_read("../pct-bigdata/regions.geojson", what = "sp")

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
