# Load local authorities and districts
if(!exists("geo_level")) geo_level <- "regional"
# if you use a custom geometry, regions should already be saved from buildmaster.R

if(!exists("regions")){
  if (geo_level == "regional")
    regions <-
  readOGR(file.path(pct_bigdata, "regions.geojson"), layer = "OGRGeoJSON")
  else {
    regions <- readOGR(dsn = file.path(pct_bigdata, "cuas-mf.geojson"), layer = "OGRGeoJSON")
    regions$Region <- regions$CTYUA12NM
  }
}
region <- 'greater-manchester'  #we change region JUST & ONYL to get the shape
region_shape <- region_orig <- # create region shape (and add buffer in m)
  regions[grep(pattern = region, x = regions$Region, ignore.case = T),]


region <- 'greater-manchester-NC'  #back to GM


# Only transform if needed
if(params$buff_dist > 0){
  region_shape <- spTransform(region_shape, CRS("+init=epsg:27700"))
  region_shape <- gBuffer(region_shape, width = params$buff_dist * 1000)
  if(!exists("centsa")) # Population-weighted centroids
    centsa <- readOGR(file.path(pct_bigdata, "cents-scenarios.geojson"), "OGRGeoJSON")
  region_shape <- spTransform(region_shape, proj4string(centsa))
}
if(!exists("las"))
  las <- readOGR(dsn = file.path(pct_bigdata, "las-pcycle.geojson"), layer = "OGRGeoJSON")
if(!exists("las_cents"))
  las_cents <- SpatialPoints(coordinates(las))
