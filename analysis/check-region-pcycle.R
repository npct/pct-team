# Aim: check regional % cycling estimates

# las data: from https://census.edina.ac.uk/easy_download_data.html?data=infuse_dist_lyr_2011
las = tmap::read_shape("../pct-bigdata/infuse_dist_lyr_2011_clippedmapshaped_0.5%.shp")
names(las)
head(las$geo_label)

# travel to work data: from http://www.nomisweb.co.uk/census/2011/dc7101ewla
df = readxl::read_excel("/tmp/nomis_2016_04_28_224840.xlsx")
dfnames = df[7,]
dfnames[5]
df = df[-c(1:7),]
names(df) = dfnames
df[-1] = apply(df[-1], 2, as.numeric)

summary(las$geo_label %in% df$`2011 census merged local authority district`)
las$geo_label[!las$geo_label %in% df$`2011 census merged local authority district`]
names(df)[1] = "geo_label"
las@data = dplyr::left_join(las@data, df)
library(tmap)
qtm(las, "Bicycle")
las$pcycle = las$Bicycle / las$`All categories: Method of travel to work (2001 specification)` * 100
qtm(las, "pcycle")
las = sp::spTransform(las, CRSobj = sp::CRS("+init=epsg:4326"))
saveRDS(las, "../pct-bigdata/las-geo-mode.Rds")

library(leaflet)
qpal <- colorBin("RdYlGn", las$pcycle, bins = c(0, 3, 5, 40), pretty = TRUE)
m <- leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = las, weight = 1, popup = las$geo_label,
              fillColor = ~qpal(las$pcycle), fillOpacity = 0.5, color = "black") %>%
  addLegend(pal = qpal, values = las$pcycle, title = "% Cycling\nto work", opacity = 0.5)
m
