#v2.1 uses a,b coefficients (most complex case of all)
rm(list=ls())

library("readstata13")
library(dplyr)
lkp_highways_gmsm <- read.csv('C:/temp/Manchester_Traffic_data/2-L2_L3_level/lkp_highways_GMSM.csv',header=T, as.is=T)
lkp_gmsm_msoa <- read.csv('C:/temp/Manchester_Traffic_data/2-L2_L3_level/lkp_GMSM_MSOA.csv',header=T, as.is=T)


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

#set the AreaOrig/AreaDest importance factors on final demand
a=0.8
b= 1-a

################################################
#         NORMAL PROCESS: start with L1 car file
################################################
car1 <-read.csv('C:/temp/Manchester_Traffic_data/1-Filter95/L1_Car_95.csv',header=T,as.is = T)
colnames(car1)
head(car1)
sum(car1$SumOfDemandN)       #demand 0.95 ~ 3.64 M (95%)
rm(car1)                     #just checking 95% demand before DB processing

############# CAR TRAFFIC: L3 GENERATION FROM L2 (Car traffic processing)
carfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2_Car_MSOA.rds'
#car <- read.csv(carfile,header=T, as.is=T)
car <- readRDS(carfile)
nrow(car)
colnames(car)

############# GLOBAL METHOD (a,b coefficients to distribute demand)
car$xyDemand <- car$DemandOD *  ( a*car$AreaOrig + b*car$AreaDest )  #demand from I -> II (numerator)
car$xDemand <-  a * car$AreaOrig
car$yDemand <-  b * car$AreaDest
car$xplusyDemand <- car$xDemand + car$yDemand

caragg <- aggregate(car$xplusyDemand, by=list(car$Origin,car$Destination),FUN=sum,na.rm=T)
colnames(caragg) <- c('Origin','Destination','xySum')
car <- inner_join(car, caragg, by=c('Origin','Destination'))
rm(caragg) 

car$DemandT <- car$xyDemand / car$xySum
sum(car$DemandT)  #checking nos. are right

car <-aggregate(car$DemandT,by=list(car$MSOAOrig,car$MSOADest), FUN=sum,na.rm=T)
colnames(car) <- c('MSOAOrig','MSOADest','DemandT')
sum(car$DemandT)  #checking demand is ~unchanged (the same as at start)
car <- cbind(car,mode=3)
car$DemandT <- round(car$DemandT, 0)
car <- car[car$DemandT!=0,]

saveRDS(car,file.choose())                   #saved as:   L3_Car.Rds

############# WALKING:  L3 GENERATION FROM L2 (CAR & CYCLING TRAFFIC)
#walkfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2_WC_MSOA.csv'
#wc <- read.csv(walkfile,header=T, as.is=T)  #reads L2_WC_MSOA.csv 
walkfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2_WC_MSOA.Rds'
wc <- readRDS(walkfile)  #reads L2_WC_MSOA.Rds
colnames(wc)
head(wc)

wc$xyDemand <- wc$DemandOD * (a * wc$AreaOrig + b * wc$AreaDest)  #steps 1-2: flow per dest MSOA (50*0.5*0.2)
wc$xDemand <-  a * wc$AreaOrig
wc$yDemand <-  b * wc$AreaDest

wc$xplusyDemand <- wc$xDemand + wc$yDemand

wcagg <- aggregate(wc$xplusyDemand, by=list(wc$Origin,wc$Destination),FUN=sum,na.rm=T)
colnames(wcagg) <- c('Origin','Destination','xySum')
wc <- inner_join(wc, wcagg, by=c('Origin','Destination'))
rm(wcagg) 

wc$DemandT <- wc$xyDemand / wc$xySum
sum(wc$DemandT)  #checking nos. are right

wc <-aggregate(wc$DemandT,by=list(wc$MSOAOrig,wc$MSOADest), FUN=sum,na.rm=T)
colnames(wc) <- c('MSOAOrig','MSOADest','DemandT')
sum(wc$DemandT)  #checking demand is ~unchanged (the same as at start)
wc <- cbind(wc,mode=1)
wc$DemandT <- round(wc$DemandT, 0)
wc <- wc[wc$DemandT!=0, ]

saveRDS(wc,file.choose())                   #saved as:   L3_wc.Rds

############# L3 GENERATION FROM L2 (PUBLIC TRANSPORT TRAFFIC)
# ptfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2_PT_MSOA.csv'
# pt <- read.csv(ptfile,header=T, as.is=T)    #reads L2_PT_MSOA.csv 
ptfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2_PT_MSOA.Rds'
pt <- readRDS(ptfile)    #reads L2_PT_MSOA.Rds
colnames(pt)

##################GLOBAL method: allocate AreaOrig first, then AreaDest
pt$xyDemand <- pt$DemandOD * (a * pt$AreaOrig + b * pt$AreaDest)  #steps 1-2: flow per dest MSOA (50*0.5*0.2)
pt$xDemand <-  a * pt$AreaOrig
pt$yDemand <-  b * pt$AreaDest

pt$xplusyDemand <- pt$xDemand + pt$yDemand

ptagg <- aggregate(pt$xplusyDemand, by=list(pt$Origin,pt$Destination),FUN=sum,na.rm=T)
colnames(ptagg) <- c('Origin','Destination','xySum')
pt <- inner_join(pt, ptagg, by=c('Origin','Destination'))
rm(ptagg) 

pt$DemandT <- pt$xyDemand / pt$xySum
sum(pt$DemandT)  #checking nos. are right

pt <-aggregate(pt$DemandT,by=list(pt$MSOAOrig,pt$MSOADest), FUN=sum,na.rm=T)
colnames(pt) <- c('MSOAOrig','MSOADest','DemandT')
sum(pt$DemandT)  #checking demand is ~unchanged (the same as at start)
pt <- cbind(pt,mode=2)
pt$DemandT <- round(pt$DemandT, 0)
pt <-pt[pt$DemandT !=0,]

saveRDS(pt,file.choose())                   #saved as:   L3_pt.Rds

#######################  AGGREGATING TOTAL DEMAND  for Gr. MANCHESTER 
#
#
#######################  NEXT PHASE

rm(list=ls())   #clean previous vars
library(stplanr)

path <- 'C:/Users/au232/Dropbox/PCT/2_WorkInProgress/Alvaro/Manchester_data/'
wc <- readRDS(paste0(path,'L3_WC.Rds'))
pt <- readRDS(paste0(path,'L3_PT.Rds'))
car <- readRDS(paste0(path,'L3_Car.Rds'))

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
# allnull <- which(gm.od$CarGM==0 & gm.od$BusGM==0 & gm.od$FootGM==0)
# gm.od <- gm.od[-allnull,]     #keep only results!=0
gm.od$AllGM <- gm.od$CarGM + gm.od$BusGM + gm.od$FootGM

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

#keeping flows >20
gm.od <-gm.od[gm.od$AllGM>20,]
gm.od <-gm.od[, -c(7,8)]
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


#join w l.df & ctw (Census+GM Travel survey added). NOT STRICTLY NEEDED.
# Good alternative: to KEEP the differences from GM model
gm.od <-left_join(gm.od, l.df[,1:13],  
         by=c('Area.of.residence'='msoa1','Area.of.workplace'='msoa2'))
gm.od <-left_join(gm.od, ctw, 
         by=c('Area.of.residence'='StartMSOA','Area.of.workplace'='EndMSOA'))
gm.od[is.na(gm.od)] <-0      #clean NAs

saveRDS(gm.od,file.choose())    #as gm.od.Rds=GM OD TRAFFIC+G.M. Census OD + GM T.Survey
rm(car,pt,wc,l,l.df,c,c.df,ctw)


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

