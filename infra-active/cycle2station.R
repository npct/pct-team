#Travel to Public Transport

library(rgeos)
library(rgdal)
library(leaflet)
library(stplanr)

lewes_uckfield = readRDS("input-data/lewes_uckfield.Rds")
load("input-data/lewes_uckfield_objects.Rdata")
cents = geojsonio::geojson_read("../pct-bigdata/cents-scenarios.geojson", what = "sp")
cents = cents[(cents$geo_code %in% l_lc$msoa1)|(cents$geo_code %in% l_lc$msoa2),]
cents <- spTransform(cents, CRS("+init=epsg:4326"))
cents_lc <- cents

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

#Get lines that can't use the old network
l_sub$Ogrp <- NA
l_sub$Dgrp <- NA
for(m in 1:nrow(l_sub)){
  l_sub$Ogrp[m] <- as.character(stops$Type[stops$Name == l_sub$Ostat[m]])
  l_sub$Dgrp[m] <- as.character(stops$Type[stops$Name == l_sub$Dstat[m]])
}

l_sub <- l_sub[l_sub$Ogrp != l_sub$Dgrp,]


#Make lines and routes to stations
l2stat <- od2line(flow = stat_near, zones = cents_lc, destinations = stops_proj)
l2stat$id <- paste0(l2stat$mosa," ",l2stat$station)
saveRDS(l2stat, "input-data/l2stat.Rds")
##########################################################
r2stat <- line2route(l2stat,route_fun = "route_cyclestreet", l_id = "id")
saveRDS(r2stat,"../pct/input-data/routes2station.Rds")

#Or Load in data
#r2stat <- readRDS("../pct/input-data/routes2station.Rds")
############################################################

#Remove lines where cycling via station would be futher than cycling direct

#Get direct routes
r_direct <- line2route(l_sub,route_fun = "route_cyclestreet", l_id = "id")
saveRDS(r_direct,"../pct/input-data/routesdirect.Rds")
#Or Load in data
#r_direct <- readRDS("../pct/input-data/routesdirect.Rds")
l_sub$dist_dir <- r_direct$length[base::match(r_direct$id, l_sub$id)]
l_sub$dist_train <- r2stat$length[base::match(paste0(l_sub$msoa1," ",l_sub$Ostat),r2stat$id)] + r2stat$length[base::match(paste0(l_sub$msoa2," ",l_sub$Dstat),r2stat$id)]
l_short <- l_sub[(l_sub$dist_train * 1.1) < l_sub$dist_dir,]
l_short_list <- data.frame(msoa1 = l_short$msoa1, msoa2 = l_short$msoa2, Ostat = l_short$Ostat, Dstat = l_short$Dstat)
l_short_list$Oid <- paste0(l_short_list$msoa1," ",l_short_list$Ostat)
l_short_list$Did <- paste0(l_short_list$msoa2," ",l_short_list$Dstat)
list <- c(l_short_list$Oid,l_short_list$Did)

r_short <- r2stat[r2stat$id %in% list,]

#Calcualte the number of cylist on the route
l2stat$bicycle <- 0
for (l in 1:nrow(l2stat)){
  sel <- l_short@data[l_short$msoa1 == l2stat$mosa[l] | l_short$msoa2 == l2stat$mosa[l] ,]
  l2stat$bicycle[l] <- sum(sel$bicycle)
}
l2stat <- l2stat[l2stat$bicycle > 0, ]
r2stat <- r2stat[r2stat$id %in% l2stat$id,]
r2stat$bicycle <- l2stat$bicycle[match(l2stat$id, r2stat$id)]

station_icon <- makeIcon(
  iconUrl = "../pct/input-data/city-raiway-station-icon.png",
  iconWidth = 20, iconHeight = 20
)


leaflet() %>%
  #addProviderTiles("OpenStreetMap") %>%
  #addMarkers(data = cents_lc) %>%
  addMarkers(data = stops_proj, icon = station_icon) %>%
  #addPolylines(data = l_sub, color = "red") %>%
  #addPolylines(data = l_lc, color = "green") %>%
  #addPolylines(data = l2stat[l2stat$bicycle > 0, ], color = "blue") %>%
  addPolylines(data = l_short, color = "green") %>%
  #addPolylines(data = r2stat, color = "blue") %>%
  addPolylines(data = r_short, color = "red") %>%
  addProviderTiles("OpenStreetMap")
  #addPolygons(data = buff)


saveRDS(l_short, "input-data/l_short.Rds")
saveRDS(stops, "input-data/stops.Rds")

# estimate cycling potential to the stations
summary(l_short$dist) # Those that are shorter cycled to train stations
summary(l_short$all)

# which desire lines (l_short) touch which cycle routes (r_short)
i = 1
l_short$cycle_dist = NA # distance cycled to populate
for(i in 1:length(l_short)){

  sel_home = r_short$id %in% paste(l_short$msoa1[i], l_short$Ostat[i])
  r_short_home = r_short[sel_home,]

  sel_work = r_short$id %in% paste(l_short$msoa2[i], l_short$Dstat[i])
  r_short_work = r_short[sel_work,]

  l_short$cycle_dist[i] = sum(r_short_home$length + r_short_work$length)

}
l_short$cycle_dist = l_short$cycle_dist / 1000
summary(l_short$cycle_dist)
sum(l_short$bicycle)

# scenarios - see https://github.com/Robinlovelace/pct-menai/blob/master/vignettes/menai-bridge-cycle.Rmd
logit_pcycle = -3.894 + (-0.5872 * l_short$cycle_dist) +
  (1.832 * sqrt(l_short$cycle_dist) ) + (0.007956 * l_short$cycle_dist^2)
l_short$govtarget =
  boot::inv.logit(logit_pcycle) + l_short$`Percent cycling` / 100
l_short$`Government Target` = l_short$govtarget * l_short$all
logit_pcycle_dutch = logit_pcycle + 2.499 -0.07384 * l_short$cycle_dist
l_short$godutch = boot::inv.logit(logit_pcycle_dutch)
l_short$`Go Dutch` = l_short$godutch * l_short$all

# save results
res = readRDS("input-data/res.Rds")
res_stns = res[0,]
res_stns[1,] = c(sum(l_short$all),
                 sum(l_short$bicycle),
                 sum(l_short$bicycle) / sum(l_short$all) * 100,
                 mean(l_short$dist)
                 )
res_stns[2,] = c(sum(l_short$all),
                 sum(l_short$`Government Target`),
                 sum(l_short$`Government Target`) / sum(l_short$all) * 100,
                 mean(l_short$cycle_dist)
)
res_stns[3,] = c(sum(l_short$all),
                 sum(l_short$`Go Dutch`),
                 sum(l_short$`Go Dutch`) / sum(l_short$all) * 100,
                 mean(l_short$cycle_dist)
)
addition_cyclists = sum(l_short$govtarget * l_short$all)

saveRDS(res_stns, "input-data/res_stns.Rds")

# out-takes:
tmap_mode("view")
qtm(l_short[1,]) +
  qtm(r_short)
l_short_buff = buff_geo(l_short, width = 20)
qtm(l_short_buff)
qtm(l_short[1,]) +
  qtm(r_short[l_short_buff[1,],])
summary(l)



qtm(l_short[i,])
cent_o = cents[cents$geo_code == l_short$msoa1[i],]
r_o = r_short[buff_geo(cent_o, width = 20),]
qtm(l_short[i,]) +
  qtm(r_short) +
  qtm(buff_geo(cent_o, width = 20))