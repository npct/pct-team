
#v3 uses pro-rata method of OD traffic calculation (suggested by Anna)
rm(list=ls())

library("readstata13")
library(dplyr)
lkp_highways_gmsm <- read.csv('C:/temp/Manchester_Traffic_data/2-L2_L3_level/lkp_highways_GMSM.csv',header=T, as.is=T)
lkp_gmsm_msoa <- read.csv('C:/temp/Manchester_Traffic_data/2-L2_L3_level/lkp_GMSM_MSOA.csv',header=T, as.is=T)

# No areas per GMSM zones & per PT zone
hw_gm_nos <- read.csv('C:/temp/Manchester_Traffic_data/2-L2_L3_level/lkp_highways_GMSM_Nos.csv',header=T, as.is=T)
pt_gm_nos <- read.csv('C:/temp/Manchester_Traffic_data/2-L2_L3_level/lkp_pt_gmsm_Nos.csv',header=T, as.is=T)

setwd('//me-filer1/home$/au232/My Documents/1.CEDAR/3_Studies !!/28-DfT2.0/4-Manchester/1-Model OD data DFT2.0/modelODdata~')


#### PRE-CHECK: read unprocessed car OD traffic (morning/off-peak/afternoon rates already adjusted)
car0 <-read.csv('C:/temp/Manchester_Traffic_data/0-L0_level/0_CarOD.csv',header=T,as.is = T)
colnames(car0)
head(car0)

#total sum(car0$DemandN) for G.M. region: 3.8M (people)
#check commuter demand before processing: subset for commuters (UserClass==1)
car0comm <- subset(x = car0,UserClass==1)
sum(car0comm$DemandN)   #real commuters demand (rates have been adjusted)~ 600 K (people)
head(car0comm)
tail(car0comm)
rm(car0,car0comm)

################################################
#         NORMAL PROCESS: start with L1 car file, previous to geogr. conversion
################################################
car1 <-read.csv('C:/temp/Manchester_Traffic_data/1-Filter95/L1_Car_95.csv',header=T,as.is = T)
colnames(car1)
head(car1)
sum(car1$SumOfDemandN)       #demand ~ 3.64 M (95%)
rm(car1)                     #just checking 95% demand before DB processing

############# CAR TRAFFIC: L3 GENERATION FROM L2 (Car traffic processing)
carfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2_Car_MSOA.csv'
car <- read.csv(carfile,header=T, as.is=T, row.names = F)

nrow(car)
colnames(car) #  "Origin"      "Destination" "DemandOD"    "MSOAOrig"    "MSOADest"    
             #   "AreaOrig"    "AreaDest"    "xy"  (product Ax * Ay)

############# GLOBAL METHOD (*x *y, then divide by sum(x)* sum (y) )
car$xyDemand <- car$DemandOD * car$AreaOrig * car$AreaDest

#adding N.zones & sum of area for Orig-Dest
car <- inner_join(x=car, y=hw_gm_nos, by=c("Origin" = "X2013HighwayZone"))
car <- dplyr::rename(.data = car, 
                     Nmsoax=Nmsoa,
                     SumAreasx = SumAreas )

car <- inner_join(x=car, y=hw_gm_nos, by=c("Destination"= "X2013HighwayZone"))
car <- dplyr::rename(.data = car, 
                     Nmsoay=Nmsoa,
                     SumAreasy=SumAreas)

#demand before aggregation by MSOA Orig -> Dest pairs
car$Demand = car$xyDemand / (car$SumAreasx * car$SumAreasy )

#demand after aggregation by MSOA Orig -> Dest pairs
car <- aggregate(car$Demand, by=list(car$MSOAOrig,car$MSOADest), FUN=sum,na.rm=T)
colnames(car) <- c('MSOAOrig','MSOADest','DemandT')

#checking demand is ~unchanged (the same as at start)
sum(car$DemandT)   ##checking demand is ~unchanged: 3.604 M
car <- cbind(car,mode=3)
car$DemandT <- round(car$DemandT, 0)
car <- car[car$DemandT!=0,]

saveRDS(car,file.choose())                   #saved as:   L3_Car_Anna.Rds


############# WALKING:  L3 GENERATION FROM L2 (CAR & CYCLING TRAFFIC)
walkfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2_WC_MSOA.csv'
wc <- read.csv(walkfile,header=T, as.is=T)  #reads L2_WC_MSOA.csv 
colnames(wc)
head(wc)

#wc[which(wc$Origin==1 & wc$Destination==17),]     line for test
wc$xyDemand <- wc$DemandOD * wc$AreaOrig * wc$AreaDest  #steps 1-2: flow per dest MSOA (50*0.5*0.2)

wcaggx <- aggregate(wc$AreaOrig, by=list(wc$Origin,wc$Destination),FUN=sum,na.rm=T)
colnames(wcaggx) <- c('Origin','Destination','xSum')
wc <- inner_join(wc,wcaggx,by=c('Origin','Destination'))

wcaggy <- aggregate(wc$AreaDest, by=list(wc$Origin,wc$Destination),FUN=sum,na.rm=T)
colnames(wcaggy) <- c('Origin','Destination','ySum')
wc <- inner_join(wc,wcaggy,by=c('Origin','Destination'))
rm(wcaggx, wcaggy)

wc$DemandT <- wc$xyDemand / (wc$xSum * wc$ySum)
sum(wc$DemandT)  #checking nos. are stable

wc <-aggregate(wc$DemandT,by=list(wc$MSOAOrig,wc$MSOADest), FUN=sum,na.rm=T)
colnames(wc) <- c('MSOAOrig','MSOADest','DemandT')

wc <- cbind(wc,mode=0)
saveRDS(wc,file.choose())      #L3_wc.Rds
write.csv(wc,file.choose(),row.names = F)    #saved as L3_WC_MSOA.csv


############# L3 GENERATION FROM L2 (PUBLIC TRANSPORT TRAFFIC)
ptfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2_PT_MSOA.csv'
pt <- read.csv(ptfile,header=T, as.is=T)    #reads L2_PT_MSOA.csv 
colnames(pt)

##################GLOBAL method: allocate AreaOrig first, then AreaDest
pt$xyDemand <- pt$DemandOD * pt$AreaOrig * pt$AreaDest
pt$xy <-  pt$AreaOrig * pt$AreaDest

ptaggx <- aggregate(pt$AreaOrig, by=list(pt$Origin,pt$Destination),FUN=sum,na.rm=T)
colnames(ptaggx) <- c('Origin','Destination','xSum')
pt <- inner_join(pt,ptaggx,by=c('Origin','Destination'))

ptaggy <- aggregate(pt$AreaDest, by=list(pt$Origin,pt$Destination),FUN=sum,na.rm=T)
colnames(ptaggy) <- c('Origin','Destination','ySum')
pt <- inner_join(pt, ptaggy,by=c('Origin','Destination'))

rm(ptaggx, ptaggy)

#demand per original O-D pair
pt$DemandT <- pt$xyDemand / (pt$xSum * pt$ySum)
sum(pt$DemandT)  #checking demand is stable

#express demand per MSOA
pt <-aggregate(pt$DemandT,by=list(pt$MSOAOrig,pt$MSOADest), FUN=sum,na.rm=T)
colnames(pt) <- c('MSOAOrig','MSOADest','DemandT')
pt <- cbind(pt,mode=2)   #mode=2: 'Public transport
sum(pt$DemandT)  #should be same as before and the initial demand transformed

saveRDS(pt,file.choose())
write.csv(pt,file.choose(),row.names = F)  #saved as L3_PT_MSOA.csv


#######################  AGGREGATING TOTAL DEMAND  for Gr. MANCHESTER 
#
#
#######################  NEXT PHASE

rm(list=ls())   #clean previous vars
library(stplanr)

path <- '//me-filer1/home$/au232/My Documents/1.CEDAR/3_Studies !!/28-DfT2.0/4-Manchester/1-Model OD data DFT2.0/modelODdata~/7-Anna_CHECKS/'
wc <- readRDS(paste0(path,'L3_wc_prorrata.Rds'))
pt <- readRDS(paste0(path,'L3_pt_prorrata.Rds'))
car <- readRDS(paste0(path,'L3_car_prorrata.Rds'))

#reshape for rbind
colnames(wc) <- c("MSOAOrig","MSOADest","FootGM", "mode")
colnames(pt) <- c("MSOAOrig","MSOADest","BusGM", "mode")
colnames(car) <- c("MSOAOrig","MSOADest","CarGM", "mode")

#eliminates mode column
wc <-wc[,-4]
pt <-pt[,-4]
car <-car[,-4]

#create gm.od by joining car->pt->wc, then rounds flows  
gm.od <-left_join(car, pt, by=c('MSOAOrig', 'MSOADest'))
gm.od <-left_join(gm.od, wc, by=c('MSOAOrig', 'MSOADest'))
gm.od[,3:5] <- round(gm.od[,3:5],0)
gm.od[is.na(gm.od)] <-0

#eliminate flows where ALL are 0
allnull <- which(gm.od$CarGM==0 & gm.od$BusGM==0 & gm.od$FootGM==0)
gm.od <- gm.od[-allnull,]     #keep only results!=0
gm.od$AllGM <- gm.od$CarGM + gm.od$BusGM + gm.od$FootGM

#### gm.od: all trips (from Greater Manchester -> any UK point + from any UK point->Greater Manchester)
#### this means ~180K  flows (way more than Robin's file)


#centroids file for filtering G.M. MSOAs
pathGM <- 'V:/Group/GitHub/pct-data/greater-manchester/'
c <-readRDS(file.path(pathGM,'c.Rds'))
c.df <-c@data
colnames(c.df)

#filtering for MSOAOrig & MSOADest ONLY in G.M. &  >20
gm.od <-inner_join(gm.od, c.df[,1:2], by=c('MSOAOrig'='geo_code'))
gm.od <-inner_join(gm.od, c.df[,1:2], by=c('MSOADest'='geo_code'))
sum(gm.od$AllGM)   #inner G.M. demand=6.11M

gm.od <-gm.od[gm.od$AllGM>20,]
gm.od <-gm.od[, 1:6]
colnames(gm.od)<-c('Area.of.residence','Area.of.workplace','CarGM','BusGM','FootGM','AllGM')
gm.od <- gm.od[,c(1:2,6,3:5)]

#get ctw (derived from GM. travel survey) 
ctwfile <- '//me-filer1/home$/au232/My Documents/1.CEDAR/3_Studies !!/28-DfT2.0/4-Manchester/1-Model OD data DFT2.0/modelODdata~/3-Rds/gm.tsurvey.csv'
ctw <- read.csv(ctwfile,header=T, as.is =T)
ctw[is.na(ctw)] <- 0
ctw$AllTS <-rowSums(ctw[3:12])

#get l.RDs (Census flow file for G.Manchester) --alternative: full Census original file
l <- readRDS('V:/Group/GitHub/pct-data/greater-manchester/l.Rds')
l.df <- l@data


#link to l.df + link to ctw (Census+GM Travel survey added)
gm.od <-left_join(gm.od, l.df[,1:13],  
         by=c('Area.of.residence'='Area.of.residence','Area.of.workplace'='Area.of.workplace'))
gm.od <-left_join(gm.od, ctw, 
         by=c('Area.of.residence'='StartMSOA','Area.of.workplace'='EndMSOA'))
gm.od[is.na(gm.od)] <-0      #clean NAs

saveRDS(gm.od,file.choose())    #as gm.od.Rds=GM OD TRAFFIC+G.M. Census OD + GM T.Survey
rm(car,pt,wc,l,l.df,c,c.df,ctw, t1)


#make flows single - dated: this is done now with stplanr
######### MANUAL METHOD (NOT NEEDED):   combine gm.od flows OD-DO into one single flow
# gm.od0 <- gm.od[gm.od$MSOAOrig==gm.od$MSOADest,]
# gm.od1 <- inner_join(gm.od,gm.od, by=c('MSOAOrig'='MSOADest', 'MSOADest'='MSOAOrig'))
# gm.od1[,3:6] <-gm.od[,3:6]  + gm.od[,7:10]
# gm.od1[,c(3:6)] <-gm.od[,c(3:6)]  + gm.od[,c(7:10)]
# gm.od1[,c(3:6)] <-gm.od1[,c(3:6)]  + gm.od1[,c(7:10)]
# gm.od1 <-gm.od1[,c(3:6)]
# gm.od1 <- inner_join(gm.od,gm.od, by=c('MSOAOrig'='MSOADest', 'MSOADest'='MSOAOrig'))
# gm.od1[,c(3:6)] <-gm.od1[,c(3:6)]  + gm.od1[,c(7:10)]
# gm.od1 <-gm.od1[,c(1:6)]
# gm.od <- rbind(gm.od1,gm.od0)


# gm.od <- inner_join(gm.od, l, by=c('MSOAOrig'='Area.of.residence', 'MSOADest'='Area.of.workplace')) 
# gm.od <- gm.od[,1:18]
# saveRDS(gm.od,file.choose())                   #saved as:   gmODcompared.Rds

#########################

