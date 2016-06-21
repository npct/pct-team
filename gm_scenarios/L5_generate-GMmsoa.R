
library(dplyr)
library("readstata13")
library(rgdal)
library(raster)


gmmsoas <- read.csv(file.choose(), header = T, as.is = T)  #read l_

ukmsoas <- readRDS(file.choose())        # ukmsoas-scenarios.Rds from bigdata
ukmsoas <- ukmsoas@data

gmmsoas <- gmmsoas[gmmsoas$all !=0, ]


#renaming to Robin's format
gmmsoas <- dplyr::rename(.data = gmmsoas, 
                         
                           geo_code= home_msoa,
                           geo_label= home_msoa_name,
                           All=all,
                           Car =car_driver,
                           Bicycle = bicycle)


#add avslope to gmmsoa
gmmsoas <- inner_join(gmmsoas, ukmsoas[,c(1,83)], by=c('geo_code')  )  


#compose w. msoa geographical data to get a spatial DF
msoa   <- readOGR(file.choose(),"OGRGeoJSON")
