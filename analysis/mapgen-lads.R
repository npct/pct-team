source("../pct-load/set-up.R")

# devtools::install_github("berndbischl/BBmisc")
library(BBmisc)

# V2: builds all operational zones
library(rgeos)
library(maptools)
library(shiny)
library(htmlwidgets)

# load national results at zone level
if(!exists("ukmsoas")) # MSOA zones
  ukmsoas <- readRDS("../pct-bigdata/ukmsoas-scenarios.Rds")
if(!exists("centsa")) # Population-weighted centroids
  centsa <- readOGR("../pct-bigdata/cents-scenarios.geojson", layer = "OGRGeoJSON")
centsa$geo_code <- as.character(centsa$geo_code)
regions_large = geojson_read("../pct-bigdata/regions.geojson", what = "sp")
dplyr::select(regions_large@data, Region, pcycle) %>%
  arrange(pcycle)

# uncomment this to get LA data
regions = shapefile("../pct-bigdata/infuse_dist_lyr_2011_clippedmapshaped_0.5%.shp")
proj4string(regions)
regions = spTransform(regions, CRS("+proj=longlat"))
plot(regions)
regions$Region = regions$geo_label

i = 1
regions$pcycle = NA
regions$Region_cap = gsub(pattern = "-", replacement = " ", x = regions$Region)
# devtools::install_github("berndbischl/BBmisc")
regions$Region_cap = capitalizeStrings(regions$Region_cap, all.words = T)
regions$Region_cap = gsub(pattern = "And", replacement = "and", x = regions$Region_cap)
regions$Region_cap = gsub(pattern = "Of", replacement = "of", x = regions$Region_cap)

proj4string(regions)=CRS("+init=epsg:4326 +proj=longlat")
proj4string(centsa)=CRS("+init=epsg:4326 +proj=longlat")
regions = regions[grep(pattern = "E", regions$geo_code),]
i = 1
for(i in 1:length(regions)){
  print(i)
  region_shape = regions[i,]
  region_name = region_shape$Region

  cents <- centsa[region_shape,]
  zones <- ukmsoas[ukmsoas@data$geo_code %in% cents$geo_code, ]
  plot(zones)

  regions$pcycle[i] <- round(100 * sum(zones$bicycle) / sum(zones$all), 3)
  regions$govtarget_slc[i] <- round(100 * sum(zones$govtarget_slc) / sum(zones$all), 3)
  regions$gendereq_slc[i] <- round(100 * sum(zones$gendereq_slc) / sum(zones$all), 3)
  regions$dutch_slc[i] <- round(100 * sum(zones$dutch_slc) / sum(zones$all), 3)
  regions$ebike_slc[i] <- round(100 * sum(zones$ebike_slc) / sum(zones$all), 3)

  regions$url[i] <- paste0("./", regions$Region[i])
  regions$url_text[i] <- as.character(a(regions$Region_cap[i], href = regions$url[i]))
  regions$url_text[i] <- gsub('">', '" target ="_top">', regions$url_text[i])
}
names(regions)
geojson_write(regions, file = "../pct-bigdata/lads-scenarios.geojson", overwrite = T)
saveRDS(regions, "../pct-bigdata/lads-scenarios.Rds")
popup_census <- paste0(regions$geo_label, "</br>", round(regions$pcycle, 1), "% in 2011 Census<br>")

popup_govt_target <- paste0(regions$geo_label, "</br>",round(regions$govtarget_slc, 1), "% in Government Target<br>")

popup_gender_eq <- paste0(regions$geo_label, "</br>", round(regions$gendereq_slc, 1), "% in Gender Equality<br>")

popup_dutch <- paste0(regions$geo_label, "</br>", round(regions$dutch_slc, 1), "% in Go Dutch<br>")

popup_ebikes <- paste0(regions$geo_label, "</br>", round(regions$ebike_slc, 1), "% in Ebikes<br>")

library(leaflet)
source("../pct-shiny/pct-shiny-funs.R")
# qpal <- colorBin("RdYln", regions$pcycle, bins = c(0, 3, 6, 12, 20, 40), pretty = TRUE)
qpal <- colorBin("RdYlBu", regions$pcycle, bins = zone_fill_breaks * 100, pretty = TRUE)

m <- leaflet() %>%
  addPolygons(data = regions, popup = popup_census, weight = 1,
              fillColor = ~qpal(regions$pcycle), fillOpacity = 0.5, color = "black", group = "2011 Census") %>%
  addPolygons(data = regions, popup = popup_govt_target, weight = 1,
              fillColor = ~qpal(regions$govtarget_slc), fillOpacity = 0.5, color = "black", group = "Government Target") %>%
  addPolygons(data = regions, popup = popup_gender_eq, weight = 1,
              fillColor = ~qpal(regions$gendereq_slc), fillOpacity = 0.5, color = "black", group = "Gender Equality") %>%
  addPolygons(data = regions, popup = popup_dutch, weight = 1,
              fillColor = ~qpal(regions$dutch_slc), fillOpacity = 0.5, color = "black", group = "Go Dutch") %>%
  addPolygons(data = regions, popup = popup_ebikes, weight = 1,
              fillColor = ~qpal(regions$ebike_slc), fillOpacity = 0.5, color = "black", group = "Ebikes") %>%
  addLegend(pal = qpal, position = c("topleft"), values = regions$pcycle, title = "% Cycling\nto work", opacity = 0.5) %>%
  addLayersControl(
    position = c("topleft"),
    baseGroups = c("2011 Census", "Government Target", "Gender Equality", "Go Dutch", "Ebikes"),
    options = layersControlOptions(collapsed = F)
  ) %>%
  addTiles('http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png', attribution = '<a target="_blank" href ="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors')

m
old = setwd("regions_www/")
saveWidget(m, file = "new_test_map.html")
setwd(old)
