
#v2.2 FINAL METHOD  ===== > uses TfGM proportional areas approach. See  docs & 
# the 2 databases for more reference 

rm(list=ls())
library("readstata13")
library(dplyr)

#key areas lookup files
# lkp_highways_gmsm <- read.csv('C:/temp/Manchester_Traffic_data/2-L2_L3_level/lkp_highways_GMSM.csv',header=T, as.is=T)
# lkp_pt_gmsm <- read.csv('C:/temp/Manchester_Traffic_data/2-L2_L3_level/lkp_pt_GMSM.csv',header=T, as.is=T)
# lkp_gmsm_msoa <- read.csv('C:/temp/Manchester_Traffic_data/2-L2_L3_level/lkp_GMSM_MSOA.csv',header=T, as.is=T)

area_hw = read.csv('C:/temp/Manchester_Traffic_data/2-L2_L3_level/Areas_Highways.csv',header=T, as.is=T)
area_pt = read.csv('C:/temp/Manchester_Traffic_data/2-L2_L3_level/Areas_PT.csv',header=T, as.is=T)
area_vdm = read.csv('C:/temp/Manchester_Traffic_data/2-L2_L3_level/Areas_VDM.csv',header=T, as.is=T)   


#### PRE-CHECK: read unprocessed car OD traffic (morning/off-peak/afternoon rates already adjusted)
car0 <-read.csv('C:/temp/Manchester_Traffic_data/0-L0_level/0_CarOD.csv',header=T,as.is = T)
colnames(car0)
head(car0)

#total sum(car0$DemandN) for G.M. region: 3.8M (people)
#check commuter demand before processing: subset for commuters (UserClass==1)
car0comm <- subset(x = car0,UserClass==1)
sel= (car0comm$TimeID==2)
sel1 = (car0comm$TimeID !=2)
sum(car0comm$DemandN[sel]) * 6 + sum(car0comm$DemandN[sel1])   
#commuters demand for whole day~ 1.82 M (people/trips)

head(car0comm)
tail(car0comm)
rm(car0,car0comm, sel, sel1)

################################################
#         OPTIONAL CHECK : start with L1 car file, previous to geogr. conversion
################################################
car1 <-read.csv('C:/temp/Manchester_Traffic_data/1-Filter95/L1_Car_95_v1.csv',header=T,as.is = T)
colnames(car1)
head(car1)
car1[ is.na(car1) ] = 0
sum(car1$DemandN)       #demand ~ 3.64 M (95%)
rm(car1)                #just checking 95% demand before DB processing

##################### NORMAL PROCESS #########################
#
############# CAR TRAFFIC: L3 GENERATION FROM L2 (Car traffic processing)
carfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2_Car_MSOA_v1.rds'
car <- readRDS(carfile)

nrow(car)
colnames(car)  # [1] "Origin"      "Destination" "MSOAOrig"    "AreaOrig"    "DDriv"       "DPass"       "MSOADest"   
               # [8] "AreaDest"


##add Area orig/dest column
car = inner_join(car, area_hw[c("X2013HighwayZone", "AreaHighway")],  by=c("Origin" = "X2013HighwayZone") )
car = inner_join(car, area_hw[c("X2013HighwayZone", "AreaHighway")],  by=c("Destination" = "X2013HighwayZone") )
colnames(car)

colnames(car)[c(9,10)]= c('AreaHighwayOrig','AreaHighwayDest')

############# GLOBAL METHOD (split demand proportional to area MSOA/area zone)

#method for driver figures
car$xDemand <- car$DDriv *  car$AreaOrig / car$AreaHighwayOrig  
car$yDemand <-  car$AreaDest  / car$AreaHighwayDest
car$xyDemand <- car$xDemand * car$yDemand

carDriver <-aggregate(car$xyDemand,by=list(car$MSOAOrig,car$MSOADest), FUN=sum,na.rm=T)
colnames(carDriver) <- c('MSOAOrig','MSOADest','DemandDriver')
sum(carDriver$DemandDriver)  #checking demand is unchanged: ~2.4 M
carDriver$DemandDriver <- round(carDriver$DemandDriver, 0)


#same for passenger
car$xDemand <- car$DPass *  car$AreaOrig / car$AreaHighwayOrig  
car$xyDemand <- car$xDemand * car$yDemand

carPass <-aggregate(car$xyDemand,by=list(car$MSOAOrig,car$MSOADest), FUN=sum,na.rm=T)
colnames(carPass) <- c('MSOAOrig','MSOADest','DemandPassenger')
sum(carPass$DemandPassenger)  #checking demand is unchanged ~1.36 M
carPass$DemandPassenger <- round(carPass$DemandPassenger, 0)

# car global demand
car <- cbind(carDriver,DemandPassenger=carPass$DemandPassenger, mode=3)


car <- car[car$DemandDriver+ car$DemandPassenger!=0,]

saveRDS(car,'./Intermediate/L3_car.rds')
rm(car)

############# WALKING:  L3 GENERATION FROM L2 (CAR & CYCLING TRAFFIC)
walkfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2_WC_MSOA.Rds'
wc <- readRDS(walkfile)  #reads L2_WC_MSOA.Rds


##add Area orig/dest column
wc = inner_join(wc, area_vdm[c("VDMZone", "AreaVDM")],  by=c("Origin" = "VDMZone") )
wc = inner_join(wc, area_vdm[c("VDMZone", "AreaVDM")],  by=c("Destination" = "VDMZone") )
colnames(wc)

colnames(wc)[c(8,9)]= c('AreaVDMOrig','AreaVDMDest')


##################GLOBAL method: allocate AreaOrig first, then AreaDest
wc$xDemand <- wc$DemandOD *  wc$AreaOrig / wc$AreaVDMOrig  
wc$yDemand <-  wc$AreaDest  / wc$AreaVDMDest
wc$xyDemand <- wc$xDemand * wc$yDemand

wc <-aggregate(wc$xyDemand,by=list(wc$MSOAOrig,wc$MSOADest), FUN=sum,na.rm=T)
colnames(wc) <- c('MSOAOrig','MSOADest','DemandT')
sum(wc$DemandT)  #checking demand is ~unchanged= 2.83
wc$DemandT <- round(wc$DemandT, 0)
wc <- cbind(wc,mode=1)
wc <- wc[wc$DemandT!=0,]

saveRDS(wc,'./Intermediate/L3_wc.rds')
rm(wc)

############# L3 GENERATION FROM L2 (PUBLIC TRANSPORT TRAFFIC)

ptfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2_PT_MSOA.Rds'
pt <- readRDS(ptfile)    #reads L2_PT_MSOA.Rds
colnames(pt)

##add PT orig/dest Area column
pt = inner_join(pt, area_pt[c("X2013PTZone", "AreaPT")],  by=c("Origin" = "X2013PTZone") )
pt = inner_join(pt, area_pt[c("X2013PTZone", "AreaPT")],  by=c("Destination" = "X2013PTZone") )
colnames(pt)

colnames(pt)[c(8,9)]= c('AreaPTOrig','AreaPTDest')


##################GLOBAL method: allocate AreaOrig first, then AreaDest
pt$xDemand <- pt$DemandOD *  pt$AreaOrig / pt$AreaPTOrig  
pt$yDemand <-  pt$AreaDest  / pt$AreaPTDest
pt$xyDemand <- pt$xDemand * pt$yDemand

pt <-aggregate(pt$xyDemand,by=list(pt$MSOAOrig,pt$MSOADest), FUN=sum, na.rm=T)
colnames(pt) <- c('MSOAOrig','MSOADest','DemandT')
sum(pt$DemandT)  #checking demand is unchanged at ~535K trips
pt$DemandT <- round(pt$DemandT, 0)
pt <- cbind(pt, mode=2)
pt <- pt[pt$DemandT!=0,]

saveRDS(pt,'./Intermediate/L3_pt.rds')

#######################  AGGREGATING TOTAL DEMAND  for Gr. MANCHESTER in file gm.od
#
#  
#######################  NEXT PHASE: combine  traffic modes & join to Census flows/centroids

rm(list=ls())   #clean previous vars

path <- './Intermediate/'
wc <- readRDS(paste0(path,'L3_wc.Rds'))
pt <- readRDS(paste0(path,'L3_pt.Rds'))
car <- readRDS(paste0(path,'L3_car.Rds'))

#reshape for rbind
colnames(wc) <- c("MSOAOrig","MSOADest","FootGM", "mode")
colnames(pt) <- c("MSOAOrig","MSOADest","BusGM", "mode")
colnames(car) <- c("MSOAOrig","MSOADest","CarDriver","CarPassenger", "mode")

#eliminates mode column
wc <-wc[,-4]
pt <-pt[,-4]
car <-car[,-5]

#create gm.od by joining car->pt->wc, then rounds flows  
gm.od <-left_join(car, pt, by=c('MSOAOrig', 'MSOADest'))
gm.od <-left_join(gm.od, wc, by=c('MSOAOrig', 'MSOADest'))
gm.od[,3:6] <- round(gm.od[,3:6],0)
gm.od[is.na(gm.od)] <-0

#eliminate flows where ALL are 0
# allnull <- which(gm.od$CarGM==0 & gm.od$BusGM==0 & gm.od$FootGM==0)
# gm.od <- gm.od[-allnull,]     #keep only results!=0
gm.od$AllGM <- gm.od$CarDriver + gm.od$CarPassenger + gm.od$BusGM + gm.od$FootGM

#### gm.od: all trips (from Greater Manchester -> any UK point + from any UK point->Greater Manchester)
#### this means >100K  flows (way more than Robin's file)


#centroids file for filtering G.M. MSOAs
pathGM <- '../../pct-data/greater-manchester/'
c <-readRDS(file.path(pathGM,'c.Rds'))
c.df <-c@data
colnames(c.df)

#filtering for MSOAOrig & MSOADest ONLY in G.M.
gm.od <-inner_join(gm.od, c.df[,1:2], by=c('MSOAOrig'='geo_code'))
gm.od <-inner_join(gm.od, c.df[,1:2], by=c('MSOADest'='geo_code'))
sum(gm.od$AllGM)   #inner G.M. demand=6.3 M
rm(c.df)

#keeping flows >20
#gm.od <-gm.od[gm.od$AllGM>20,]          --DEPRECATED
gm.od <-gm.od[, -c(8, 9)]
colnames(gm.od)<-c('Area.of.residence','Area.of.workplace','CarDriver','CarPassenger','BusGM','FootGM','AllGM')
gm.od <- gm.od[,c(1:2,7,3:6)]
saveRDS(gm.od, './Output/gm.od.rds')

#only execute if not run before
source('L3_addDistances.R')   #add distances to flows using stplanr (latest, from github)
rm(list=ls())

#get ctw (derived from GM. travel survey) 
# ctwfile <- './Input/gm.tsurvey.csv'
# ctw <- read.csv(ctwfile,header=T, as.is =T)
# ctw[is.na(ctw)] <- 0
# ctw$AllTS <-rowSums(ctw[3:12])

#get l.RDs (Census flow file for G.Manchester) 
#full Census original file from Anna 15-Oct-2016: 
# Good alternative: to KEEP the differences from GM model: l <- readRDS('../../pct-bigdata/l.Rds')
gm.od = readRDS('./Output/gm.od1.Rds')     #GM flows w. distances + Census values

# l <- readRDS('../../pct-data/greater-manchester/l.Rds')
# l <- l@data
l = readRDS('./Output/wu03.gm.rds')      # Census flows GM   

#join gm.od (gm layer) <> l (Census flows) to prepare prediction
gm.od <-left_join(gm.od, l[,c(1:14)], by=c('msoa1'='msoa1','msoa2'='msoa2'))

#Travel survey not used
# gm.od <-left_join(gm.od, ctw,
#          by=c('Area.of.residence'='StartMSOA','Area.of.workplace'='EndMSOA'))
gm.od[is.na(gm.od)] <-0      #clean NAs

saveRDS(gm.od, './Output/gm.od2.rds')    #as gm.od.Rds=GM OD TRAFFIC+G.M. Census OD


