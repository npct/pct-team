# Aim: load and extract intrazonal flows
source("set-up.R")
centsa = readOGR("../pct-bigdata/cents.geojson", "OGRGeoJSON")
head(centsa@data)
centroids_new = read_csv("~/Dropbox/PCT/160229_AreaLines/pct_centroids.csv")
centroids_new = rename(centroids_new, geo_code = msoa1)
centsa@data = left_join(centsa@data, centroids_new)

head(centsa@data)

# save
geojson_write(centsa, file = "../pct-bigdata/cents-scenarios.geojson")

#
# # Old attempts: with pct_lines
# pct_lines = readRDS("../pct-bigdata/pct_lines.Rds")
# pct_lines = read_csv("../pct-bigdata/pct_lines.csv")
#
# summary(pct_lines$)
#
# head(pct_lines)
#
# summary(pct_lines$msoa1 == pct_lines$msoa2)
# summary(pct_lines$msoa2 == "other")
#
# head(pct_lines[pct_lines$msoa2 == "other",])
#
