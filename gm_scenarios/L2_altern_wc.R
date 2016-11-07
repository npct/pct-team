
rm(list=ls())
library("readstata13")
library(dplyr)

gm.od3 <- readRDS('./Output/gm.od1.rds')     #flows file w. fast route distances   
area_vdm = read.csv('C:/temp/Manchester_Traffic_data/2-L2_L3_level/Areas_VDM.csv',header=T, as.is=T)   
summary(gm.od3$dist)

###########################
walkfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2_WC_MSOA.Rds'
wc <- readRDS(walkfile)  #reads L2_WC_MSOA.Rds
wc = left_join(wc, gm.od3[ ,c('dist','slope','msoa1', 'msoa2')], by=c("MSOAOrig"='msoa1',"MSOADest"='msoa2'     ))

#calc mean dist by O-D
wc.agg.od = aggregate(wc$dist,by=list(wc$Origin, wc$Destination), FUN=mean,na.rm=T)
names(wc.agg.od)=c('Origin','Destination', 'distmean')
summary(wc.agg.od$distmean)

#add distmean to wc flows
wc = inner_join(wc, wc.agg.od, by=c("Origin" = "Origin", "Destination" = "Destination"))
rm(wc.agg.od)

#add VDM areas
wc = inner_join(wc, area_vdm[c("VDMZone", "AreaVDM")],  by=c("Origin" = "VDMZone") )
wc = inner_join(wc, area_vdm[c("VDMZone", "AreaVDM")],  by=c("Destination" = "VDMZone") )
colnames(wc)
wc = dplyr::rename(.data = wc, AreaVDMOrig = AreaVDM.x,
                              AreaVDMDest  = AreaVDM.y  )

wc$xDemand <- wc$DemandOD *  wc$AreaOrig / wc$AreaVDMOrig
wc$yDemand <-  wc$AreaDest  / wc$AreaVDMDest
wc$xyDemand <- wc$xDemand * wc$yDemand
sum(wc$xyDemand)


#read wu03 (Census)
wu03.gm = readRDS('./Output/wu03.gm.rds')      # Census flows GM

#add Census (~12 cols)
wc <-left_join(wc, wu03.gm, by=c('MSOAOrig'='msoa1','MSOADest'='msoa2'))
wc[is.na(wc)] = 0
rm(wu03.gm)

###adjust dist mean & run prediction on wc dataset
sel = (wc$Origin == wc$Destination)   #inner w/c flows
wc$distmean = wc$distmean/1000    #convert to Km
wc$distmean[sel] = 0.5 * wc$distmean[sel]    #convert to Km


sel10minus= (wc$Bicycle + wc$On.foot<=10) |(wc$Bicycle== 0)  | (wc$On.foot== 0)
sel10plus=  ! sel10minus

#delete OD flows w/o a distance
wc= wc[! is.na(wc$distmean),]  #typically all have distance
wc$CycleGM = 0

for (i in c(1, 2))   {
   
   # distmean ranges for prediction
   if (i==1) {sel = wc$xyDemand !=0 & sel10minus
   }   else { sel = wc$xyDemand !=0 & sel10plus }        #apply only to flows with 'potential' cyclists
   
   sel1 = sel & (wc$distmean>= 0) & (wc$distmean< 3)   ; sel1factor = 0.025
   sel2 = sel & (wc$distmean>= 3) & ( wc$distmean <  6)     ; sel2factor = 0.339
   sel3 = sel & (wc$distmean>= 6) &  (wc$distmean <  10)    ; sel3factor = 1.30
   sel4 = sel & (wc$distmean>= 10) & (wc$distmean <  15)  
   sel5 = sel & (wc$distmean>= 15)
   
   
   if (i==1)   {  #flows w. insufficient Census data
      
      #values as per Anna's table 20-Oct-2015 (replace w. Census MSOA-level cycling%)
      wc$CycleGM[sel1] = wc$xyDemand[sel1] *   0.0703 * 0.32   
      wc$CycleGM[sel2] = wc$xyDemand[sel2] *   0.298 * 0.85    
      wc$CycleGM[sel3] = wc$xyDemand[sel3] *   0.495 * 1.05
      wc$CycleGM[sel4] = wc$xyDemand[sel4] *   0.92            
      wc$CycleGM[sel5] = wc$xyDemand[sel5] *   1
      
      
   } else  {   #flows w. enough Census data
      
      wc$CycleGM[sel1] = 0.25 * wc$xyDemand[sel1] * wc$Bicycle[sel1]/(wc$Bicycle[sel1]+wc$On.foot[sel1])      # 0.025/(1+0.025)
      wc$CycleGM[sel2] = 0.339 * wc$xyDemand[sel2] * wc$Bicycle[sel2]/(wc$Bicycle[sel2]+ wc$On.foot[sel2])        # 0.339/ (1+ 0.339)   
      wc$CycleGM[sel3] = 1.3  * wc$xyDemand[sel3] * wc$Bicycle[sel3]/(wc$Bicycle[sel3] + wc$On.foot[sel3]) 
      wc$CycleGM[sel4] = 0.92  * wc$xyDemand[sel4] *  wc$Bicycle[sel4]/(wc$Bicycle[sel4] + wc$On.foot[sel4])       # 92% of total
      wc$CycleGM[sel5] = 1* wc$xyDemand[sel5] *   wc$Bicycle[sel5]/(wc$Bicycle[sel5] + wc$On.foot[sel5]  )                 # =1 => all people cycling
   }
   
}

#aggregation

wc <-aggregate(wc[, c('xyDemand','CycleGM')], by=list(wc$MSOAOrig,wc$MSOADest), FUN=sum,na.rm=T)
colnames(wc) <- c('MSOAOrig','MSOADest','FootGM','CycleGM')

#check & roundings
sum(wc$FootGM)  #checking demand is ~unchanged= 2.83
sum(wc$CycleGM)


wc$FootGM = wc$FootGM - wc$CycleGM
wc$FootGM[wc$FootGM<0 ]=0
wc[ , c("FootGM","CycleGM")] <- round(wc[ , c("FootGM","CycleGM")], 0)


wc <- cbind(wc,mode=1)
wc <- wc[wc$FootGM!=0 | wc$CycleGM!=0,]

saveRDS(wc,'./Intermediate/L3_wc_meandistance.rds')

#########
#weekly factors per mode
weekly_carDriver = 6.57
weekly_carPassenger = 7.83
weekly_walking = 6.77
weekly_cycling = 7.53
weekly_pt      = 6.35

gm.od3$CarDriver =    weekly_carDriver * gm.od3$CarDriver 
gm.od3$CarPassenger =     weekly_carPassenger * gm.od3$CarPassenger
gm.od3$BusGM   =  weekly_pt      * gm.od3$BusGM   
gm.od3$FootGM   = weekly_walking * gm.od3$FootGM
gm.od3$CycleGM  =   weekly_cycling * gm.od3$CycleGM

gm.od3[ , c(6:10)] = round(gm.od3[ , c(6:10)], 0)
gm.od3$AllGM = gm.od3$CarDriver + gm.od3$CarPassenger + gm.od3$BusGM + gm.od3$FootGM + gm.od3$CycleGM

########### PREPARE for SCENARIOS GENERATION
l <- gm.od3[,1:10]

#gm.od3.Rds contains the no. of cyclists per each GM flow (intraflows included?)
saveRDS(gm.od3, './Output/gm.od3.rds')     #gm.od3.Rds
rm(gm.od3)




