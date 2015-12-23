# Aim: set-up the regions names

regions <- geojson_read("../pct-bigdata/regions.geojson", what = "sp")
regions$Region_upper <- gsub("-", " ", regions$Region)
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
regions$Region_upper <- sapply(regions$Region_upper, simpleCap)
regions$url <- paste0("http://geo8.webarch.net/", regions$Region)

library(shiny)
a(regions$Region_upper[1], href = regions$url[1])

las <- readRDS("../pct-bigdata/las.Rds")
summary(las$pcycle)
bbox(las)
las <- spTransform(las, CRS("+init=epsg:4326"))
lasp <- gCentroid(las, byid = T)
lasp <- SpatialPointsDataFrame(lasp, data = las@data)
proj4string(lasp) <- proj4string(regions)

regions$url_text <- regions$pcycle <- NA

# regions <- spTransform(regions, CRS("+init=epsg:27700"))
# regions_simple <- gSimplify(regions, tol = 1000)
# regions <- SpatialPolygonsDataFrame(regions_simple, regions@data)

for(i in 1:nrow(regions)){
  regions$url_text[i] <- as.character(a(regions$Region_upper[i], href = regions$url[i]))
  regions$url_text[i] <- gsub('">', '" target ="_top">', regions$url_text[i])

  # Now: pcycle
  lastmp <- lasp[regions[i,],]
  regions$pcycle[i] <- mean(lastmp$pcycle)

  regions$url_text[i] <-
    HTML(regions$url_text[i], " <br>",
         paste0(round(regions$pcycle[i]), "% cycling to work"
                                     ))
}

# simplify
# shapefile(regions, file = "regions_new")
# mapshape("regions_new.shp", percent = 9)
# regions <- shapefile("regions_newmapshaped_9%.shp")

qpal <- colorBin("RdYlGn", regions$pcycle, bins = c(0, 3, 5, 10), pretty = TRUE)

library(leaflet)
m <- leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = regions, popup = regions$url_txt, weight = 1,
              fillColor = ~qpal(regions$pcycle), fillOpacity = 0.5, color = "black") %>%
  addLegend(pal = qpal, values = regions$pcycle, opacity = 1, title = "% Cycling")

htmlwidgets::saveWidget(m, file = "../pct-shiny/map.html")
