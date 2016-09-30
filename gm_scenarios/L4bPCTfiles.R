#creates the centroids file used by ../pct/buildregion_GM.R  (not in geojson format).
rm(list=ls())

library(dplyr)
c = readRDS('V:/Group/GitHub/pct-data/greater-manchester/c.rds')
c.GM =read.csv('./Output/pct_lines_GM_Anna.csv', header=T, as.is = T)
c.GM =c.GM[c.GM$msoa1==c.GM$msoa2, ]
c.GM$msoa2=NULL

colnames(c.GM)[1] = c('geo_code')
 
c.GM = inner_join(c@data[, c(1,2,3,83,84)], c.GM,by='geo_code')
c@data = inner_join(c.GM, c@data[1], by='geo_code')
saveRDS(c, file = '../../pct-bigdata/cents-scenarios_GM.rds')

#####################

pct_areas = read.csv('./Output/pct_area_GM_Anna.csv', header = T, as.is = T)
z=readRDS('V:/Group/GitHub/pct-data/greater-manchester/z.rds')
colnames(pct_areas)[1] = 'geo_code'
pct_areas$geo_code = as.character(pct_areas$geo_code)
z@data = inner_join(z@data[c('geo_code', 'geo_label','avslope')],  pct_areas, by='geo_code')
plot(z)
saveRDS(z,'../../pct-bigdata/ukmsoas-scenarios_GM.rds')