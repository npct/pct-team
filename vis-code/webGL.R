# Aim: create demo of webGL route network layer

# devtools::install_github('bhaskarvk/leaflet.extras')
# devtools::install_github('rstudio/leaflet')
library(leaflet.extras)
source("../pct-load/set-up.R")
rnet = readRDS("../pct-data/west-yorkshire/rnet.Rds")
rf = readRDS("../pct-data/west-yorkshire/rf.Rds")
l = readRDS("../pct-data/west-yorkshire/l.Rds")
rf@data = inner_join(rf@data, l@data[c("id", "all")])
sel_high = rf$all > 100
plot(rf[sel_high,])
rf = rf[sel_high,]
p = spsample(x = rf, n = 5e4, type = "regular", weight = rf$all)

# with new stplanr function:
# install sfr branch of stplanr and sf
devtools::install_github(repo = "robinlovelace/stplanr", ref = "sfr-dep")
devtools::install_github("edzer/sfr")
library(sf)
sf::st_line_sample
system.time(
  {p = stplanr::line_sample(l = rf, n = 1e5, weights = l$bicycle)}
)
plot(p)
library(dplyr)
library(leaflet.extras)
leaflet() %>%
  addTiles() %>%
  addWebGLHeatmap(lng = p@coords[,1], lat = p@coords[,2], size = 10, units = "px", alphaRange = 0.0001)

plot(p)
leaflet() %>%
  addTiles() %>%
  addWebGLHeatmap(lng = p@coords[,1], lat = p@coords[,2], size = 10, units = "px", alphaRange = 0.0001)

# Test weight argument
rf_minmax = rf[c(which.min(rf$all), which.max(rf$all)),]
plot(rf_minmax)
p = spsample(x = rf_minmax, n = 10e6, type = "regular", weight = rf_minmax$all)
plot(p)
leaflet() %>%
  addTiles() %>%
  addWebGLHeatmap(lng = p@coords[,1], lat = p@coords[,2], size = 10, units = "px")
