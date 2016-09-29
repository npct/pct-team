# Aim: create centroids file for PCT
#This scripts extracts/processes the inflows from pct_lines.dta
rm(list=ls())


# load centroids
source("../set-up.R")

#list of UK MSOA w.  : 7,201 X 84 (MSOA x scenarios values)
centsa = readOGR("../../pct-bigdata/cents-scenarios.geojson", "OGRGeoJSON")
head(centsa)

l_new <- read.csv("./Output/pct_lines_GM.csv", header=T, as.is = T)
l_new <- l_new[l_new$all!=0, ]
summary(sel <- l_new$msoa1 == l_new$msoa2)
l_cents = l_new[sel,]
rm(l_new)

#renaming cols. to match PCT's
l_cents <- dplyr::rename(l_cents,
                       geo_code = msoa1)

# remove excess columns
l_cents$msoa2 = NULL
l_cents$rail <- l_cents$train + l_cents$light_rail
l_cents$train <- NULL


l_cents = inner_join(l_cents, centsa@data[,1:4], by='geo_code')
l_cents = l_cents[,c(1, 82:84,2:81)]

names(l_cents)
head(l_cents[1:5])
head(centsa)

# merge new data into existing data
centsa$geo_code = as.character(centsa$geo_code)
head(sort(centsa$geo_code))
head(sort(l_cents$geo_code))
summary(centsa$geo_code %in% l_cents$geo_code)
names(centsa)

#aud: make centsa AS l_cents (but what about the spatial bit?)
#centsa@data=l_cents
head(centsa@data)
plot(centsa)

geojson_write(centsa)
file.copy("myfile.geojson", "../../pct-bigdata/cents-scenarios_GM.geojson", overwrite = T)
