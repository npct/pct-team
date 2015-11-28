source("set-up.R")
# Aim: chop lines and beginning and end

# load in some lines
rf <- readRDS("pct-data/leeds/rf.Rds")

epsg_trans <- function(sp_obj, epsg_code = "4326"){
  epsg_code <- paste0("+init=epsg:", epsg_code)
  spTransform(x = sp_obj, CRSobj = CRS(epsg_code))
}

rf <- epsg_trans(rf, epsg_code = "27700")

plot(rf)
cp <- gCentroid(rf)

q <- (bbox(rf)[1,2] - bbox(rf)[1,1]) / 10

cp <- gBuffer(cp, width = q)
plot(cp, add = T, col = "red")
sel <- gContains(cp, rf, byid = T)
summary(sel)
rf <- rf[as.vector(sel),]
nrow(rf) / nrow(sel)
plot(rf, add = T, col = "green")

cents <- readRDS("pct-data/leeds/c.Rds")
cents <- epsg_trans(cents, "27700")
cents <- cents[cp,]

library(leaflet)
cents <- gBuffer(cents, width = 200)
plot(cents, add = T)

rf <- epsg_trans(rf)
cents <- epsg_trans(cents)

leaflet() %>% addPolylines(data = routes, col = "red") %>%
  addPolylines(data = out, col = "blue") %>%
  addPolygons(data = cents)

# Output ready for a stackexchange question
geojson_write(cents, file = "~/repos/Creating-maps-in-R/data/leeds-cents.geojson")
geojson_write(rf, file = "~/repos/Creating-maps-in-R/data/leeds-lines.geojson")

gurl <- "https://github.com/Robinlovelace/Creating-maps-in-R/raw/master/data/leeds-cents.geojson"
cents <- topojson_read(gurl)
gurl <- "https://github.com/Robinlovelace/Creating-maps-in-R/raw/master/data/leeds-lines.geojson"
routes <- topojson_read(gurl)

out <- gDifference(routes, cents)
plot(routes, col = "red", lwd = 3)
plot(cents, add = T)
plot(out, col = "blue", add = T)







# with 1 line

l1 <- geojson_read("~/repos/Creating-maps-in-R/data/line1.geojson", what = "sp")
gurl <- "https://github.com/Robinlovelace/Creating-maps-in-R/raw/master/data/line1.geojson"
l1 <- topojson_read(gurl)
l2 <- gDifference(l1, cents)
plot(cents)
plot(l2, add = T)


