source("set-up.R")
library(osmplotr)
las <- geojson_read("../pct-bigdata/las-pcycle.geojson", what = "sp")

head(las)

# get and read excel file on ttw
url = "http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/rel/census/2011-census-analysis/method-of-travel-to-work-in-england-and-wales/rft-table-ct0015ew.xls"
download.file(url, "rft-table-ct0015ew.xls")

ttw = readxl::read_excel("rft-table-ct0015ew.xls", 4, skip = 13)
ttw_names = readxl::read_excel("rft-table-ct0015ew.xls", 4, skip = 9)
ttw_names = as.character(ttw_names[1,])
names(ttw) = ttw_names
rm(ttw_names)
ttw = ttw[
  !vapply(names(ttw), FUN = is.na, FUN.VALUE = T)
]
ttw = rename(ttw, CODE = `Area code`)

las_df_left = left_join(las@data, ttw)
# las_df_inner = inner_join(las@data, ttw)
nrow(las_df_left)
# nrow(las_df_inner)
nrow(las)
las_df_left$Active = las_df_left$Bicycle +
  las_df_left$`On foot`


las@data = las_df_left

?read_osm

cent1 = geocode_OSM("Solihull")
cent1_points = SpatialPoints(matrix(cent1$coords, nrow = 1))
proj4string(cent1_points) = CRS("+init=epsg:4326")
cent1_buff = buff_geo(cent1_points, 1000)
bb1 = bb(cent1_buff)
ur1 = read_osm(bb1, type = "bing")
cway1 = extract_osm_objects(key = "highway", value = "cycleway", bbox = bb1)
byes1 = extract_osm_objects(key = "bicycles", value = "yes", bbox = bb1)
plot(byes1)
qtm(ur1)
m1 = qtm(ur1) +
  tm_shape(cway1) +
  tm_lines() +
  tm_scale_bar()
save_tmap(m1, "/tmp/solihull.png", width = 682, height = 684)

cent2 = geocode_OSM("Cambridge")
cent2_points = SpatialPoints(matrix(cent2$coords, nrow = 1))
proj4string(cent2_points) = CRS("+init=epsg:4326")
cent2_buff = buff_geo(cent2_points, 1000)
bb2 = bb(cent2_buff)
ur2 = read_osm(bb2, type = "bing")
cway2 = extract_osm_objects(key = "highway", value = "cycleway", bbox = bb2)
byes2 = extract_osm_objects(key = "bicycles", value = "yes", bbox = bb2)
plot(byes2)
qtm(ur2)
m2 = qtm(ur2) +
  tm_shape(cway2) +
  tm_lines() +
  tm_scale_bar()
save_tmap(m2, "/tmp/cambridge.png", width = 682, height = 684)

library(png)
r1 = readPNG("/tmp/cambridge.png")
r2 = readPNG("/tmp/solihull.png")

layout(matrix(1:2, ncol = 2))

plot()