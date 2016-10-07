

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
sum(car0comm$DemandN)   #real commuters demand (rates have been adjusted)~ 600 K (people)
head(car0comm)
tail(car0comm)
rm(car0,car0comm)


################################################
#         NORMAL PROCESS: start with L1 car file
################################################
car1 <-read.csv('C:/temp/Manchester_Traffic_data/1-Filter95/L1_Car_95_v1.csv',header=T,as.is = T)
colnames(car1)
head(car1)
sum(car1$DemandN)       #demand 0.95 ~ 1.435 M (95%)
rm(car1)                     #just checking 95% demand before DB processing

############# CAR TRAFFIC: L3 GENERATION FROM L2 (Car traffic processing)
carfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2_Car_MSOA_v1.rds'
#car <- read.csv(carfile,header=T, as.is=T)
car <- readRDS(carfile)
nrow(car)


##add Area orig/dest column
car = inner_join(car, area_hw[c("Highway", "AreaHighway")],  by=c("Origin" = "Highway") )
car = inner_join(car, area_hw[c("Highway", "AreaHighway")],  by=c("Destination" = "Highway") )
colnames(car)

colnames(car)[c(9,10)]= c('AreaHighwayOrig','AreaHighwayDest')

############# GLOBAL METHOD (split demand proportional to area MSOA/area zone)

#method for driver figures
car$xDemand <- car$DDriv *  car$AreaOrig / car$AreaHighwayOrig  
car$yDemand <-  car$AreaDest  / car$AreaHighwayDest
car$xyDemand <- car$xDemand * car$yDemand

carDriver <-aggregate(car$xyDemand,by=list(car$MSOAOrig,car$MSOADest), FUN=sum,na.rm=T)
colnames(carDriver) <- c('MSOAOrig','MSOADest','DemandDriver')
sum(carDriver$DemandDriver)  #checking demand is ~unchanged (the same as at start)
carDriver$DemandDriver <- round(carDriver$DemandDriver, 0)


#same for passenger
car$xDemand <- car$DPass *  car$AreaOrig / car$AreaHighwayOrig  
car$xyDemand <- car$xDemand * car$yDemand

carPass <-aggregate(car$xyDemand,by=list(car$MSOAOrig,car$MSOADest), FUN=sum,na.rm=T)
colnames(carPass) <- c('MSOAOrig','MSOADest','DemandPassenger')
sum(carPass$DemandPassenger)  #checking demand is ~unchanged (the same as at start)
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
wc = inner_join(wc, area_vdm[c("VDM", "AreaVDM")],  by=c("Origin" = "VDM") )
wc = inner_join(wc, area_vdm[c("VDM", "AreaVDM")],  by=c("Destination" = "VDM") )
colnames(wc)

colnames(wc)[c(8,9)]= c('AreaVDMOrig','AreaVDMDest')


##################GLOBAL method: allocate AreaOrig first, then AreaDest
wc$xDemand <- wc$DemandOD *  wc$AreaOrig / wc$AreaVDMOrig  
wc$yDemand <-  wc$AreaDest  / wc$AreaVDMDest
wc$xyDemand <- wc$xDemand * wc$yDemand

wc <-aggregate(wc$xyDemand,by=list(wc$MSOAOrig,wc$MSOADest), FUN=sum,na.rm=T)
colnames(wc) <- c('MSOAOrig','MSOADest','DemandT')
sum(wc$DemandT)  #checking demand is ~unchanged (the same as at start)
wc$DemandT <- round(wc$DemandT, 0)
wc <- cbind(wc,mode=1)
wc <- wc[wc$DemandT!=0,]

saveRDS(wc,'./Intermediate/L3_wc.rds')
rm(wc)

############# L3 GENERATION FROM L2 (PUBLIC TRANSPORT TRAFFIC)
# ptfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2_PT_MSOA.csv'

ptfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2_PT_MSOA.Rds'
pt <- readRDS(ptfile)    #reads L2_PT_MSOA.Rds
colnames(pt)

##add PT orig/dest Area column
pt = inner_join(pt, area_pt[c("PT", "AreaPT")],  by=c("Origin" = "PT") )
pt = inner_join(pt, area_pt[c("PT", "AreaPT")],  by=c("Destination" = "PT") )
colnames(pt)

colnames(pt)[c(8,9)]= c('AreaPTOrig','AreaPTDest')


##################GLOBAL method: allocate AreaOrig first, then AreaDest
pt$xDemand <- pt$DemandOD *  pt$AreaOrig / pt$AreaPTOrig  
pt$yDemand <-  pt$AreaDest  / pt$AreaPTDest
pt$xyDemand <- pt$xDemand * pt$yDemand

pt <-aggregate(pt$xyDemand,by=list(pt$MSOAOrig,pt$MSOADest), FUN=sum,na.rm=T)
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
#library(stplanr)

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
#### this means ~180K  flows (way more than Robin's file)


#centroids file for filtering G.M. MSOAs
pathGM <- 'V:/Group/GitHub/pct-data/greater-manchester/'
c <-readRDS(file.path(pathGM,'c.Rds'))
c.df <-c@data
colnames(c.df)

#filtering for MSOAOrig & MSOADest ONLY in G.M.
gm.od <-inner_join(gm.od, c.df[,1:2], by=c('MSOAOrig'='geo_code'))
gm.od <-inner_join(gm.od, c.df[,1:2], by=c('MSOADest'='geo_code'))
sum(gm.od$AllGM)   #inner G.M. demand=6.0121 M
rm(c.df)

#keeping flows >20
#gm.od <-gm.od[gm.od$AllGM>20,]          --DEPRECATED
gm.od <-gm.od[, -c(8, 9)]
colnames(gm.od)<-c('Area.of.residence','Area.of.workplace','CarDriver','CarPassenger','BusGM','FootGM','AllGM')
gm.od <- gm.od[,c(1:2,7,3:6)]

#get ctw (derived from GM. travel survey) 
ctwfile <- './Input/gm.tsurvey.csv'
ctw <- read.csv(ctwfile,header=T, as.is =T)
ctw[is.na(ctw)] <- 0
ctw$AllTS <-rowSums(ctw[3:12])

#get l.RDs (Census flow file for G.Manchester) 
#full Census original file from Anna 15-Sept-2016
l <- readRDS('V:/Group/GitHub/pct-data/greater-manchester/l.Rds')
l <- l@data
##alternative: l <- readRDS('./Intermediate/l.rds')

#join w l.df & ctw (Census+GM Travel survey added). NOT STRICTLY NEEDED.
# Good alternative: to KEEP the differences from GM model
gm.od <-left_join(gm.od, l[,1:16],  
         by=c('Area.of.residence'='msoa1','Area.of.workplace'='msoa2'))
gm.od <-left_join(gm.od, ctw, 
         by=c('Area.of.residence'='StartMSOA','Area.of.workplace'='EndMSOA'))
gm.od[is.na(gm.od)] <-0      #clean NAs

saveRDS(gm.od, './Output/gm.od.rds')    #as gm.od.Rds=GM OD TRAFFIC+G.M. Census OD + GM T.Survey
rm(car,pt,wc,l,c,ctw)


