
rm(list=ls())
library(stplanr)
Sys.setenv(CYCLESTREET = "e149b266f9ec77c2")   #old key:db6ab7dcd748408c

gm.od <- readRDS('./Output/gm.od.rds')
sum(gm.od$Area.of.residence== gm.od$Area.of.workplace)
gm.od1 <-cbind(gm.od,CycleGM=0)
rm(gm.od)
gm.od1 = gm.od1[, c(1:8) ]
names(gm.od1)[c(1,2)]= c('msoa1', 'msoa2')

#  NO NEED to ELIMINATE CENTROIDS before onewayid
gm.od1= stplanr::onewayid(gm.od1, attrib= c(3:8))


pathGM <- '../../pct-data/greater-manchester/'  #before w/o: -NC
c <-readRDS(file.path(pathGM,'c.Rds'))   

gm.od1 = data.frame(gm.od1)
l <- stplanr::od2line(gm.od1,c)

#rf = line2route(l = l, route_fun = "route_cyclestreet", plan = "fastest")
rf = line2route(l=l, route_fun = route_cyclestreet, base_url = "http://pct.cyclestreets.net", plan = "fastest")
saveRDS(rf, '../../pct-bigdata/rf_gm.rds')

rq = line2route(l=l, route_fun = route_cyclestreet, base_url = "http://pct.cyclestreets.net", plan = "quietest")
saveRDS(rq, '../../pct-bigdata/rq_gm.rds')

ldist= cbind(l@data, rf@data[, c(1:11)])
ldist$length[is.na(ldist$length)]  = 0  #inner flows distances=0
saveRDS(ldist, './Output/gm.od1.Rds')  # flows w. fast route distances 


###################  DEPRECATED for FAST ROUTES DISTANCES
# rm(list=ls())
# gm.od <- readRDS('./Output/gm.od1.rds')
# sum(gm.od$Area.of.residence== gm.od$Area.of.workplace)
# gm.od1 <-cbind(gm.od,CycleGM=0)
# rm(gm.od)
# 
# gm.od1 = gm.od1[, c(1:8) ]
# 
# 
# ########### read NATIONAL CENTROIDS file, subset to G.M.
# cents = geojsonio::geojson_read("../../pct-bigdata/cents-scenarios.geojson", what = "sp")
# gmc = readRDS(file.path('../../pct-data/greater-manchester', 'c.rds'))    #c.rds
# cents = cents[cents$geo_code %in% gmc$geo_code, ]
# cents$geo_code = as.character(cents$geo_code)
# 
# plot(cents)
# flow = flow_cens = gm.od1
# 
# o <- flow_cens$Area.of.residence %in% cents$geo_code
# d <- flow_cens$Area.of.workplace %in% cents$geo_code
# 
# omatch = match(flow$Area.of.residence, cents$geo_code)
# dmatch = match(flow$Area.of.workplace, cents$geo_code)
# 
# cents_o = cents@coords[omatch,]
# cents_d = cents@coords[dmatch,]
# summary(is.na(cents_o)) # check how many origins don't match
# summary(is.na(cents_d))
# geodist = geosphere::distHaversine(p1 = cents_o, p2 = cents_d) / 1000 # assign euclidean distanct to lines (could be a function in stplanr)
# summary(is.na(geodist))
# 
# hist(geodist, breaks = 0:50)
# flow$dist = geodist     #this needs to be replaced by proper fast route distance
# flow = flow[!is.na(flow$dist),] 
# flow$dist1.25 = flow$dist * 1.25
# 
# saveRDS(flow, './Output/gm.od1.Rds')  # flows w. euclidean distances 
# 

