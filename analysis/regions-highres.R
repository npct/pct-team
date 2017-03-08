# Aim: update regions to be high res

z = readRDS("../pct-bigdata/ukmsoas-scenarios.Rds")
cents = geojsonio::geojson_read("../pct-bigdata/cents-scenarios.geojson", what = "sp")
r = geojsonio::geojson_read("../pct-shiny/regions_www/regions.geojson", what = "sp")
cr = over(cents, r)
class(cr)
summary(z$Region) # 400 missing!
sel = which(is.na(cr$Region))
plot(z[sel,])

# for loop way
rdirs = paste0("../pct-data/", r$Region)
zfiles = paste0(rdirs, "/z.Rds")
for(i in 1:length(zfiles)) {
  ztemp = readRDS(zfiles[i])
  plot(ztemp)
  ztemp_buff = stplanr::buff_geo(ztemp, width = 20)
  plot(ztemp_buff)
  r@polygons[[i]] = ztemp_buff@polygons[[1]]
  plot(r[i,])
}
row.names(r) = as.character(1:length(r))
mapview::mapview(r)
saveRDS(r, "/tmp/regions-highres2.Rds")
zip(zipfile = "/tmp/regions-highres2.zip", files = "/tmp/regions-highres2.Rds")

# find, add missing zones
missing_ids = c(
  "E02004287",
  "E02004288",
  "E02004289"
)
summary(z$geo_code %in% missing_ids) # they are in there
z_dorset = readRDS("../pct-data/dorset/z.Rds")
summary(z_dorset$geo_code %in% missing_ids) # they are absent

# fix centroids
summary(cents$geo_code %in% missing_ids) # they are in there
c_dorset = readRDS("../pct-data/dorset/c.Rds")
summary(c_dorset$geo_code %in% missing_ids) # they are absent
c_add = cents[cents$geo_code %in% missing_ids,]
ncol(c_add)
ncol(c_dorset)
names(c_dorset)[!names(c_dorset) %in% names(c_add)]
names(c_dorset) # the two ones appended...
c_add@data = cbind(c_add@data, c_dorset@data[1:3, c(85, 86)])
c_dorset_new = maptools::spRbind(c_dorset, c_add)
saveRDS(c_dorset_new, "../pct-data/dorset/c.Rds")

# fix zones
summary(z$geo_code %in% missing_ids) # they are in there
z_dorset = readRDS("../pct-data/dorset/z.Rds")
summary(z_dorset$geo_code %in% missing_ids) # they are absent
z_add = z[z$geo_code %in% missing_ids,]
ncol(z_add)
ncol(z_dorset)
plot(z_add)
z_add = spChFIDs(z_add, row.names(z_add))
z_dorset = spChFIDs(z_dorset, row.names(z_dorset))
z_dorset_new = maptools::spRbind(z_dorset, z_add)
saveRDS(z_dorset_new, "../pct-data/dorset/z.Rds")

# geographic ways
library(rgeos)
zr = gUnaryUnion(z, z$Region) # fails
plot(zr)

library(sf)
zs = st_as_sfc(z)

zsr = st_union()