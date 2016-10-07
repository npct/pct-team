# Load and save lsoa data

# see http://webarchive.nationalarchives.gov.uk/20160105160709/http:/www.ons.gov.uk/ons/guide-method/geography/products/census/spatial/centroids/index.html
library(sp)
lsoa_centroids_ew = raster::shapefile("/tmp/LSOA_2011_EW_PWC.shp")
plot(lsoa_centroids_ew)
saveRDS(lsoa_centroids_ew, "../pct-bigdata/lsoa_centroids_ew.Rds")

oa_centroids_ew = raster::shapefile("/tmp/OA_2011_EW_PWC.shp")
saveRDS(oa_centroids_ew, "../pct-bigdata/oa_centroids_ew.Rds")

# lsoa boundaries - from http://geoportal.statistics.gov.uk/datasets?q=LSOA
lsoa_zones_ew = raster::shapefile("/tmp/Lower_Super_Output_Areas_December_2001_Full_Extent_Boundaries_in_England_and_Wales.shp")
names(lsoa_zones_ew)
object.size(lsoa_zones_ew) / 1000000
lsoa_zones_full = readRDS("../pct-bigdata/lsoa_zones_ew.Rds")
names(lsoa_zones_full)
plot(lsoa_zones_ew[5000,])
plot(lsoa_zones_full[5000,])
lsoa_zones_ew@data = lsoa_zones_full@data
saveRDS(lsoa_zones_ew, "../pct-bigdata/lsoa_zones_ew_simple.Rds")

library(rmapshaper)
lsoa_zones_ew_simplified = ms_simplify(lsoa_zones_ew, keep = 0.01)
