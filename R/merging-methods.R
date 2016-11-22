# Aim: prototype geographical joining function

setwd("/home/geoif/pct/pct-load")

# The CRS conversion to epsg:27700 took ages so I did it once and saved it to an RDS
#rf = readRDS("../pct-bigdata/rf_nat_Robin.Rds")
rf = readRDS("../pct-bigdata/rf_nat_Robin_27700.Rds")

l = readRDS("lines_oneway_shapes_updated_27700.Rds")

library(dplyr) # Loading this avoid that odd bug when trying to subset l[,]

# Take a random sample of the full data sets
# rf_test = rf[sample(1:10000),]
# l_test = l[sample(1:10000),]
floor(0.25*nrow(rf))
floor(0.25*nrow(l))
rf_test = rf[sample(1:floor(0.25*nrow(rf))),]
l_test = l[sample(1:floor(0.25*nrow(l))),]

rm(rf, l) # Remove full data sets from memory to give some headspace for computation


library(stplanr)

# Create data in projected CRS with units of m
#rf = routes_fast[5:1,] # wrong order
#l = flowlines[1:5,]

# Use this line if the RDS data isn't already converted to epsg:27700
#proj4string(rf) = CRS("+init=epsg:4326")

# Make sure the data is in epsg:27700
rf_test = spTransform(rf_test, CRS("+init=epsg:27700"))
l_test = spTransform(l_test, CRS("+init=epsg:27700"))

# The lines used to save the data sets converted to epsg:27700 once, for later loading
#saveRDS(rf, "../pct-bigdata/rf_nat_Robin_27700.Rds")
#saveRDS(l, "lines_oneway_shapes_updated_27700.Rds")

# Test mismatch
#plot(l); 
#plot(l[2,], lwd = 5)#, add = T)
# plot(rf[2,], add = T) # does not match

# Merge based on ods
l_od_coords = line2df(l_test)
rf_od_coords = line2df(rf_test)
sid1 = as.matrix(l_od_coords[-1])
sid2 = as.matrix(rf_od_coords[-1])

start_time = proc.time()

# install.packages("Hmisc")
tol = 200 # maximum tolerance for matches
m = Hmisc::find.matches(sid1, sid2, maxmatch = 1, #nrow(sid1),
                    tol = c(tol, tol, tol, tol))

end_time = proc.time()

end_time - start_time

rf_new = rf_test[m$matches,]
#l_new = l_test[1:length(l_test) %in% m$matches,]
l_new = l_test[m$matches != 0,]

# percentage of desire lines for which a match was found
nrow(l_new)/nrow(l_test)

# test 1
n = 1
plot(l_test[n,]); plot(rf_test[n,], add = T) # fail
plot(l_new[n,]); plot(rf_new[n,], add = T) # win!

# test with randomly chosen examples
n = sample(1:length(l_new), size=1)
n
plot(l_new[n,]); plot(rf_new[n,], add = T)



# # Old tests
#
# # round to nearest 10 m
# id1_cols = apply(l_od_coords, 2, round, -2)
# id2_cols = apply(rf_od_coords, 2, round, -2)
# points(id1_cols[,c("fx", "fy")])
# points(id1_cols[,c("tx", "ty")])
# points(id2_cols[,c("fx", "fy")])
# points(id2_cols[,c("tx", "ty")]) # they match
# id1 = apply(id1_cols, 1, paste, collapse = "-")
# id2 = apply(id2_cols, 1, paste, collapse = "-")
# (new_order = match(id2, id1))
# id1 %in% id2
