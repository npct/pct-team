#Travel to Public Transport

library(rgeos)
library(rgdal)
library(leaflet)
library(stplanr)

#Read in stops
stops <- readOGR(dsn="../pct/input-data",layer = "lews_uckfiled_stations", encoding = "ESRI Shapefile")
stops@data <- stops@data[,c(2,3,1)]
stops_proj <- spTransform(stops, CRS("+init=epsg:4326"))

#Project Cents
cents_proj <- spTransform(cents_lc, CRS("+init=epsg:27700"))

#Find Nearest stop to each MSOA
#Get distances
d <- gDistance(cents_proj, stops, byid=T)
colnames(d) <- cents_proj$geo_code
rownames(d) <- stops$Name
stat_near <- data.frame(mosa = cents_proj$geo_code, station = NA, dist = NA)

#Loop through and find nearest station
for (j in 1:nrow(stat_near)){
  sel <- as.data.frame(d[,as.character(stat_near$mosa[j])])
  colnames(sel) <- "Distance"
  sel$station <- rownames(sel)
  sel <- as.data.frame(sel[order(sel$Distance),])
  stat_near$station[j] <- sel$station[1]
  stat_near$dist[j] <- sel$Distance[1]
}

#Remove lines not nearby
l_sub <- l_lc[l_lc$msoa1 %in% cents_lc$geo_code,]
l_sub <- l_sub[l_sub$msoa2 %in% cents_lc$geo_code,]
stat_near <- stat_near[stat_near$dist < 3000,]
l_sub <- l_sub[l_sub$msoa1 %in% stat_near$mosa,]
l_sub <- l_sub[l_sub$msoa2 %in% stat_near$mosa,]

#Get lines that have different Oriding and Destination Stations
l_sub$Ostat <- NA
l_sub$Dstat <- NA
for(k in 1:nrow(l_sub)){
  l_sub$Ostat[k] <- stat_near$station[stat_near$mosa == l_sub$msoa1[k]]
  l_sub$Dstat[k] <- stat_near$station[stat_near$mosa == l_sub$msoa2[k]]
}
nrow(l_sub)
l_sub <- l_sub[l_sub$Ostat != l_sub$Dstat,]
nrow(l_sub)

#Make lines and routes to stations
l2stat <- od2line(flow = stat_near, zones = cents_lc, destinations = stops_proj)
l2stat$id <- paste0(l2stat$mosa," ",l2stat$station)
##########################################################
#r2stat <- line2route(l2stat,route_fun = "route_cyclestreet", l_id = "id")
#saveRDS(r2stat,"../pct/input-data/routes2station.Rds")

#Or Load in data
r2stat <- readRDS("../pct/input-data/routes2station.Rds")
############################################################

#Remove lines where cycling via station would be futher than cycling direct

#Get direct routes
r_direct <- line2route(l_sub,route_fun = "route_cyclestreet", l_id = "id")
l_sub$dist_dir <- r_direct$length[base::match(r_direct$id, l_sub$id)]
l_sub$dist_train <- r2stat$length[base::match(paste0(l_sub$msoa1," ",l_sub$Ostat),r2stat$id)] + r2stat$length[base::match(paste0(l_sub$msoa2," ",l_sub$Dstat),r2stat$id)]
l_short <- l_sub[(l_sub$dist_train * 1.1) < l_sub$dist_dir,]


#Calcualte the number of cylist on the route
l2stat$bicycle <- 0
for (l in 1:nrow(l2stat)){
  sel <- l_short@data[l_short$msoa1 == l2stat$mosa[l] | l_short$msoa2 == l2stat$mosa[l] ,]
  l2stat$bicycle[l] <- sum(sel$bicycle)
}
l2stat <- l2stat[l2stat$bicycle > 0, ]
r2stat <- r2stat[r2stat$id %in% l2stat$id]
r2stat$bicycle <- l2stat$bicycle[match(l2stat$id, r2stat$id)]

station_icon <- makeIcon(
  iconUrl = "../pct/input-data/city-raiway-station-icon.png",
  iconWidth = 20, iconHeight = 20
)


leaflet() %>%
  addProviderTiles("Stamen.Toner") %>%
  addMarkers(data = cents_lc) %>%
  addMarkers(data = stops_proj, icon = station_icon) %>%
  #addPolylines(data = l_sub, color = "red") %>%
  #addPolylines(data = l2stat[l2stat$bicycle > 0, ], color = "blue") %>%
  #addPolylines(data = l_short, color = "green")
  addPolylines(data = r2stat, color = "green", weight = (r2stat$bicycle)/2)
  #addPolylines(data = r_direct[r_direct$], color = "red")
  #addPolygons(data = buff)

