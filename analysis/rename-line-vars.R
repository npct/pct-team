# Renaming/updating data for pct-shiny

# get system time
start <- Sys.time()

library(dplyr)

# load original and new data
l <- readRDS("~/npct/pct-bigdata/pct_lines_oneway_shapes.Rds")
# install.packages("readstata13") # install package to read stata13 data
# l_new <- readstata13::read.dta13("~/Dropbox/PCT/160229_AreaLines/pct_lines_160229.dta")
l_new <- readstata13::read.dta13("/tmp/newdata/160401_AreaLines/pct_lines.dta")

# names
names(l_new)
names(l)

head(l_new$id)

# names in old data but not new
names(l)[!names(l) %in% names(l_new)]
names(l_new)[!names(l_new) %in% names(l)]

# which flows are included?
nrow(l)
nrow(l_new)

summary(l_new$all) # all lines
summary(l$All) # much higher average

# remove excess columns
l_new$Rail <- l_new$Train + l_new$light_rail
l_new$Train <- NULL

# check they are the same - nope!
# plot(l$All, l_new$All)
head(l$id)
head(l_new$id)
l_new$id <- paste(l_new$Area.of.residence, l_new$Area.of.workplace)
head(l_new$id)

newdat <- left_join(l@data["id"], l_new)

nrow(newdat)
nrow(l)

l@data <- newdat
names(newdat)
# save the new data:
proj4string(l)
l = spTransform(l, CRS("+init=epsg:27700"))
l$dist = rgeos::gLength(l, byid = T) / 1000
l = spTransform(l, CRS("+init=epsg:4326"))

saveRDS(l, "~/npct/pct-bigdata/pct_lines_oneway_shapes.Rds")
# knitr::spin("../pct/analysis/rename-line-vars.R", format = "Rtex")

# How long did it all take?
Sys.time() - start
