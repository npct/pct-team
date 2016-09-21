
#v3 uses pro-rata method of OD traffic calculation (suggested by Anna)
rm(list=ls())

library("readstata13")
library(dplyr)
lkp_highways_gmsm <- read.csv('C:/temp/Manchester_Traffic_data/2-L2_L3_level/lkp_highways_GMSM.csv',header=T, as.is=T)
lkp_gmsm_msoa <- read.csv('C:/temp/Manchester_Traffic_data/2-L2_L3_level/lkp_GMSM_MSOA.csv',header=T, as.is=T)


# No areas per GMSM zones & per PT zone
hw_gm_nos <- read.csv('C:/temp/Manchester_Traffic_data/2-L2_L3_level/lkp_highways_GMSM_Nos.csv',header=T, as.is=T)
pt_gm_nos <- read.csv('C:/temp/Manchester_Traffic_data/2-L2_L3_level/lkp_pt_gmsm_Nos.csv',header=T, as.is=T)
wc_gm_nos  <- read.csv('C:/temp/Manchester_Traffic_data/2-L2_L3_level/lkp_vdm_gmsm_Nos.csv',header=T, as.is=T)

#setwd('//me-filer1/home$/au232/My Documents/1.CEDAR/3_Studies !!/28-DfT2.0/4-Manchester/1-Model OD data DFT2.0/modelODdata~')


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
#         OPTIONAL CHECK : start with L1 car file, previous to geogr. conversion
################################################
car1 <-read.csv('C:/temp/Manchester_Traffic_data/1-Filter95/L1_Car_95.csv',header=T,as.is = T)
colnames(car1)
head(car1)
sum(car1$SumOfDemandN)       #demand ~ 3.64 M (95%)
rm(car1)                     #just checking 95% demand before DB processing

##################### NORMAL PROCESS #########################
#
############# CAR TRAFFIC: L3 GENERATION FROM L2 (Car traffic processing)
carfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2_Car_MSOA.rds'
car <- readRDS(carfile)

nrow(car)
colnames(car) #  "Origin"      "Destination" "DemandOD"    "MSOAOrig"    "MSOADest"    
             #   "AreaOrig"    "AreaDest"    "xy"  (product Ax * Ay)

############# PRO-RRATA METHOD (*x *y, then divide by sum(x)* sum (y) )
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
car$DemandT <- round(car$DemandT, 0)
car <- cbind(car,mode=3)
car <- car[car$DemandT!=0,]

saveRDS(car,file.choose())                   #saved as:   L3_Car_Anna.Rds


############# WALKING:  L3 GENERATION FROM L2 (CAR & CYCLING TRAFFIC)
walkfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2_WC_MSOA.rds'
wc <- readRDS(walkfile)  
colnames(wc)
head(wc)

#wc[which(wc$Origin==1 & wc$Destination==17),]     line for test
wc$xyDemand <- wc$DemandOD * wc$AreaOrig * wc$AreaDest  #steps 1-2: flow per dest MSOA (50*0.5*0.2)


#adding N.zones & sum of area for Orig-Dest
wc <- inner_join(x=wc, y=wc_gm_nos, by=c("Origin" = "VDMZone"))
wc <- dplyr::rename(.data = wc, 
                     Nmsoax=Nmsoa,
                     SumAreasx = SumAreas )

wc <- inner_join(x=wc, y= wc_gm_nos, by=c("Destination"= "VDMZone"))
wc <- dplyr::rename(.data = wc, 
                     Nmsoay=Nmsoa,
                     SumAreasy=SumAreas)

#demand BEFORE aggregation by MSOA Orig -> Dest pairs
wc$Demand = wc$xyDemand / (wc$SumAreasx * wc$SumAreasy )

#demand AFTER aggregation by MSOA Orig -> Dest pairs
wc <- aggregate(wc$Demand, by=list(wc$MSOAOrig, wc$MSOADest), FUN=sum,na.rm=T)
colnames(wc) <- c('MSOAOrig','MSOADest','DemandT')

#checking demand is ~unchanged (the same as at start)
sum(wc$DemandT)   ##checking demand is unchanged: 2.65 M
wc$DemandT <- round(wc$DemandT, 0)
wc <- cbind(wc,mode=1)
wc <- wc[wc$DemandT!=0,]

saveRDS(wc,file.choose())                   #saved as:   L3_wc_Anna.Rds


############# L3 GENERATION FROM L2 (PUBLIC TRANSPORT TRAFFIC)
ptfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2_PT_MSOA.rds'
pt <- readRDS(ptfile)
colnames(pt)

##################GLOBAL method: allocate AreaOrig first, then AreaDest
pt$xyDemand <- pt$DemandOD * pt$AreaOrig * pt$AreaDest


#adding N.zones & sum of area for Orig-Dest
pt <- inner_join(x=pt, y=pt_gm_nos, by=c("Origin" = "X2013PTZone"))
pt <- dplyr::rename(.data = pt, 
                     Nmsoax=Nmsoa,
                     SumAreasx = SumAreas )

pt <- inner_join(x=pt, y=pt_gm_nos, by=c("Destination"= "X2013PTZone"))
pt <- dplyr::rename(.data = pt, 
                     Nmsoay=Nmsoa,
                     SumAreasy=SumAreas)

#demand before aggregation by MSOA Orig -> Dest pairs
pt$Demand = pt$xyDemand / (pt$SumAreasx * pt$SumAreasy )


#demand after aggregation by MSOA Orig -> Dest pairs
pt <- aggregate(pt$Demand, by=list(pt$MSOAOrig,pt$MSOADest), FUN=sum,na.rm=T)
colnames(pt) <- c('MSOAOrig','MSOADest','DemandT')

#checking demand is ~unchanged (the same as at start)
sum(pt$DemandT)   ##checking demand is unchanged:  ~535 K
pt$DemandT <- round(pt$DemandT, 0)
pt <- cbind(pt,mode=2)
pt <- pt[pt$DemandT!=0,]     #a bit of demand lost here: CHECK

saveRDS(pt,file.choose())                   #saved as:   L3_pt_Anna.Rds


#######################  AGGREGATING TOTAL DEMAND  for Gr. MANCHESTER 
#
#
#######################  NEXT PHASE

rm(list=ls())   #clean previous vars
library(stplanr)

path <- './Intermediate/'
wc <- readRDS(paste0(path,'L3_WC_Anna.Rds'))
pt <- readRDS(paste0(path,'L3_PT_Anna.Rds'))
car <- readRDS(paste0(path,'L3_Car_Anna.Rds'))

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
if (length(allnull!=0))  {gm.od <- gm.od[-allnull,] }     #keep only results!=0
gm.od$AllGM <- gm.od$CarGM + gm.od$BusGM + gm.od$FootGM

#### gm.od: all trips (from Greater Manchester -> any UK point + from any UK point->Greater Manchester)
#### this means ~170K  flows (way more than Robin's file)


#centroids file for filtering G.M. MSOAs
pathGM <- 'V:/Group/GitHub/pct-data/greater-manchester/'
c <-readRDS(file.path(pathGM,'c.Rds'))
c.df <-c@data
colnames(c.df)

#filtering for MSOAOrig & MSOADest ONLY in G.M. &  >20
gm.od <-inner_join(gm.od, c.df[,1:2], by=c('MSOAOrig'='geo_code'))
gm.od <-inner_join(gm.od, c.df[,1:2], by=c('MSOADest'='geo_code'))
sum(gm.od$AllGM)   #inner G.M. demand=6.02 M

gm.od <-gm.od[gm.od$AllGM>20,]
gm.od <-gm.od[, 1:6]
colnames(gm.od)<-c('Area.of.residence','Area.of.workplace','CarGM','BusGM','FootGM','AllGM')
gm.od <- gm.od[,c(1:2,6,3:5)]

#get ctw (derived from GM. travel survey) 
ctwfile <- './Input/gm.tsurvey.csv'
ctw <- read.csv(ctwfile,header=T, as.is =T)
ctw[is.na(ctw)] <- 0
ctw$AllTS <-rowSums(ctw[3:12])

#get l.RDs (Census flow file for G.Manchester) --alternative: full Census original file
l <- readRDS('./Intermediate/pct_lines.rds')
#alternative: l <- readRDS('V:/Group/GitHub/pct-data/greater-manchester/l.Rds')
#alternative: l.df <- l@data


gm.od <-left_join(gm.od, l[,1:13],  
                  by=c('Area.of.residence'='msoa1','Area.of.workplace'='msoa2'))
gm.od <-left_join(gm.od, ctw, 
                  by=c('Area.of.residence'='StartMSOA','Area.of.workplace'='EndMSOA'))
gm.od[is.na(gm.od)] <-0      #clean NAs

saveRDS(gm.od, './Output/gm.od.rds')    #as gm.od.Rds=GM OD TRAFFIC+G.M. Census OD + GM T.Survey
rm(car,pt,wc,l,c,ctw)


