# Load flow-cyclestreeted

download.file("https://github.com/npct/pct-bigdata/releases/download/0.1/l.Rds", "l.Rds", mode = "wb")
l <- readRDS("l.Rds")
dim(l) # 281895 rows by 15 cols
names(l)
summary(l$All) # from 11 to 2000 people travelling on them
summary(l$dist) # up to 30 km in distance

# l data
ldat <- read.csv("d://tmp/l.csv")
dim(ldat) # also 29 colums
names(ldat)
sapply(ldat, class)
cor(l$dist, ldat$dist)
l@data <- ldat

ll <- readRDS("../pct-bigdata/l_less10.Rds")
dim(ll) # 281895 rows by 15 cols
names(ll)
summary(ll$All) # from 11 to 2000 people travelling on them
summary(ll$dist) # up to 30 km in distance

names_to_add <- names(ll)[!names(ll) %in% names(l)]

library(maptools)
row.names(ll)[1:10]
row.names(l)[1:10]
# row.names(l) <- paste0(row.names(l), "l")
# spChFIDs(l) <- row.names(l)
l <- SpatialLinesDataFrame(l, data = l@data, match.ID = T)
spChFIDs(ll) <- row.names(ll)

l3 <- rbind(l, ll)

plot(l3[1:10,])

names(l3)
object.size(l3) / 1000000

ldat <- l@data
saveRDS(l3, "../pct-data/l_all_cc.Rds")
