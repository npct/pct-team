# Aim: create simple regional geojson file to test batch routing with CycleStreets.net
cents = geojsonio::geojson_read("../pct-bigdata/cents.geojson", what = "sp")
cents$region = gsub(pattern = " ...", replacement = "", cents$MSOA11NM)
grep(pattern = "[0-9]", cents$region)
head(cents$region)
cents@data = dplyr::select(cents@data, id = MSOA11NM, region)
head(cents@data)
geojsonio::geojson_write(cents, file = "/tmp/centroids-for-cyclestreets.geojson")
