#new approach, uses average distances by O-D and not converted to MSOA distances
rm(list=ls())

#read wc flows
library(dplyr)

walkfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2_WC_MSOA.Rds'
wc <- readRDS(walkfile)  #reads L2_WC_MSOA.Rds
#wc=wc[,c("Origin","Destination","DemandOD","MSOAOrig","MSOADest")]

#add msoas distance, from gm.od3 flows
wc = left_join(wc, gm.od3[ ,c('dist','slope','msoa1', 'msoa2')], by=c("MSOAOrig"='msoa1',"MSOADest"='msoa2'     ))

#calc mean dist by O-D
wc.agg.od = aggregate(wc$dist,by=list(wc$Origin, wc$Destination), FUN=mean,na.rm=T)
names(wc.agg.od)=c('Origin','Destination', 'distmean')
hist(wc.agg.od$distmean)
summary(wc.agg.od$distmean)

#add distmean to wc flows
wc = inner_join(wc, wc.agg.od, by=c("Origin" = "Origin", "Destination" = "Destination"))
rm(wc.agg.od)

##################  STUFF TO PROCESS....
gm.od3$id=paste(gm.od3$msoa1, gm.od3$msoa2)
wc$id =paste(wc$MSOAOrig,wc$MSOADest)

sel = (gm.od3$FootGM != 0)

wc=inner_join(wc, wc.dist, by=c("Origin" = "Origin", "Destination" ="Destination"))
wc1 = aggregate(wc$distmean, by=list(wc$MSOAOrig, wc$MSOADest), FUN=mean,na.rm=T)
names(wc1)=c('msoa1','msoa2', 'distmean')

#lookup
sel = gm.od3$FootGM!=0  #rows with walking/cycling
gm.od3$dist[sel]

gm.od3 = inner_join(gm.od3, wc1[, c(1:3)], by=c("msoa1"="msoa1", "msoa2" = "msoa2" ))
gm.od3=gm.od3[,c(1,23,2:22)]

gm.od3 <- readRDS('./Output/gm.od2.rds')     #flows file w. fast route distances