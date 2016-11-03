# Aim: find lowest number of layers of non-overlapping lines

rf = readRDS("../pct-data/isle-of-wight/rf.Rds")

# the over way
r = over(rf, rf, returnList = T)
nrow(rf)

class(r[[1]])
head(nrow(r[[1]]))
# ids1 = row.names(r[[1]])
ids1 = rf$id %in% r[[1]]$id

plot(rf)
plot(rf[1,], lwd = 5, add = T)
plot(rf[ids1,], add = T, col = "red")

r1_ = rf[rf[1,],]

plot(r1_)
plot(rf[ids1,])

identical(r1_, rf[ids1,])

# the rgeos way
library(rgeos)
r = gTouches(rf, rf, byid = T)
r[1:4, 1:3]
# which don't touch 1st line
r_not_touching = rf[!r[,1],]
r_touching = rf[r[,1],]
library(tmap)
tmap_mode("view")
qtm(r_touching) +
  qtm(rf[1,], add = T, col = "black")

r_touching = rf[rf[1,],]



