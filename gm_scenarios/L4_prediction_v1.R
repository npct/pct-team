library("readstata13")
library(dplyr)
# library(stplanr)   --install from github
library(rgdal)
library(rgeos)

########PREDICTION of no. of cyclists using 1) Census cyclist/pedestrian ratio + 2) distance 
rm(list=ls())
gm.od3 <- readRDS('./Output/gm.od2.rds')     #flows file w. fast route distances   
colnames(gm.od3)[which(names(gm.od3)=='length')] = 'dist'
gm.od3$dist = gm.od3$dist/1000    #convert to Km

sel10minus= (gm.od3$Bicycle + gm.od3$On.foot<=10) |(gm.od3$Bicycle== 0)  | (gm.od3$On.foot== 0)
sel10plus=  ! sel10minus

sel = gm.od3$Area.of.residence== gm.od3$Area.of.workplace
gm.od3$dist[sel]  = 0

#delete OD flows w/o a distance
gm.od3= gm.od3[! is.na(gm.od3$dist),]


for (i in c(1, 2))   {

# distance ranges for prediction
if (i==1) {sel = gm.od3$FootGM !=0 & sel10minus
  }   else { sel = gm.od3$FootGM !=0 & sel10plus }        #apply only to flows with 'potential' cyclists
   
sel1 = sel & (gm.od3$dist>= 0) & (gm.od3$dist< 3)   ; sel1factor = 0.025
sel2 = sel & (gm.od3$dist>= 3) & ( gm.od3$dist <  6)     ; sel2factor = 0.339
sel3 = sel & (gm.od3$dist>= 6) &  (gm.od3$dist <  10)    ; sel3factor = 1.30
sel4 = sel & (gm.od3$dist>= 10) & (gm.od3$dist <  15)  
sel5 = sel & (gm.od3$dist>= 15) & (gm.od3$dist <  30)
sel6 = sel &  (gm.od3$dist >=  30)

if (i==1)   {

gm.od3$CycleGM[sel1] = gm.od3$FootGM[sel1] *   0.10 * 0.25     # 0.025/(1+0.025)
gm.od3$CycleGM[sel2] = gm.od3$FootGM[sel2] *   0.423 * 0.80    # 0.339/ (1+ 0.339)   
gm.od3$CycleGM[sel3] = gm.od3$FootGM[sel3] *   0.998 * 1.295
gm.od3$CycleGM[sel4] = gm.od3$FootGM[sel4] *   0.92             # 92% of total
gm.od3$CycleGM[sel5] = gm.od3$FootGM[sel5] *   1
gm.od3$CycleGM[sel6] =  0   

} else  {

   gm.od3$CycleGM[sel1] = 0.25 * gm.od3$FootGM[sel1] * gm.od3$Bicycle[sel1]/(gm.od3$Bicycle[sel1]+gm.od3$On.foot[sel1])      # 0.025/(1+0.025)
   gm.od3$CycleGM[sel2] = 0.339 * gm.od3$FootGM[sel2] * gm.od3$Bicycle[sel2]/(gm.od3$Bicycle[sel2]+ gm.od3$On.foot[sel2])        # 0.339/ (1+ 0.339)   
   gm.od3$CycleGM[sel3] = (1.3)  * gm.od3$FootGM[sel3] * gm.od3$Bicycle[sel3]/(gm.od3$Bicycle[sel3] + gm.od3$On.foot[sel3]) 
   gm.od3$CycleGM[sel4] = 0.92  * gm.od3$FootGM[sel4] *  gm.od3$Bicycle[sel4]/(gm.od3$Bicycle[sel4] + gm.od3$On.foot[sel4])       # 92% of total
   gm.od3$CycleGM[sel5] = 1* gm.od3$FootGM[sel5] *   gm.od3$Bicycle[sel6]/(gm.od3$Bicycle[sel6] + gm.od3$On.foot[sel6]  )                 # =1 => all people cycling
   gm.od3$CycleGM[sel6] =  0   }


         }


sel = gm.od3$CycleGM > gm.od3$FootGM     #no. cyclist can't exceed cyclists+ pedestrians
gm.od3$CycleGM[sel] = gm.od3$FootGM[sel]

#deprecated: correlation model not used anymore
#gm.od3$CycleGM[!sel] = 0.032639 * gm.od3$all[!sel] - 0.083 * gm.od3$car_driver[!sel]-0.01*gm.od3$foot[!sel]
gm.od3$CycleGM[is.na(gm.od3$CycleGM)] = 0

#round 0 dec
gm.od3$CycleGM <- round(gm.od3$CycleGM, 0)
gm.od3$FootGM <- gm.od3$FootGM - gm.od3$CycleGM  #adjusts
sum(gm.od3$CycleGM)     #predicted total cyclists 400+ K

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

gm.od3[ , c(4:8)] = round(gm.od3[ , c(4:8)], 0)
gm.od3$AllGM = gm.od3$CarDriver + gm.od3$CarPassenger + gm.od3$BusGM + gm.od3$FootGM + gm.od3$CycleGM

########### PREPARE for SCENARIOS GENERATION
l <- gm.od3[,1:8]

#gm.od3.Rds contains the no. of cyclists per each GM flow (intraflows included?)
saveRDS(gm.od3, './Output/gm.od3.rds')     #gm.od3.Rds
rm(gm.od3)

#NO NEED: scenarios use them!! ----- eliminate intraflows (+put them in c.Rds)
# intraflows <- which(l$Area.of.residence==l$Area.of.workplace)
# l <- l[-intraflows,]

#rename-sort-add cols to match l.Rds in PCT
colnames(l)[3:8] <-c('all','car_driver','car_passenger', 'bus','foot','bicycle')

l <- cbind(l[,c(1:3)],  from_home=0, 
                        light_rail=0,
                        train=0,
                        bus=l$bus, 
                        taxi=0,  
                        motorbike=0,
                        car_driver=l$car_driver, 
                        car_passenger=l$car_passenger, 
                        bicycle=l$bicycle, 
                        foot=l$foot, 
                        other=0 )



#add & reorder to match Anna's flows source file (for scenarios generation)
namesl <-paste0('v',c(1:14))
colnames(l) <-namesl
save.dta13(l, './Output/l_scenariosGM.dta')   
write.csv(l,'./Output/l_scenariosGM.csv',row.names = F) 

## FOR REFERENCE next section: Columns meaning
#     home_msoa = v1
#     work_msoa  = v2
#     all = v3
#     from_home  = v4
#     light_rail  = v5
#     train  = v6
#     bus  = v7
#     taxi  = v8
#     motorbike  = v9
#     car_driver  = v10
#     car_passenger  = v11
#     bicycle  = v12
#     foot  = v13
#     other = v14

dropcols = grep(pattern = 'sel',x = ls())

dropcols = grep(pattern = 'weekly_',x = ls())


#this is needed to run scenarios (and is not in GM layer)
########## add msoa m/f ratios to msoa_t2w_sex_GM.dta (m/f ratios & m/f cyclist ratios)
td1 <-read.dta13('./Input/msoa_t2w_sex.dta')    # from ./Input/msoa_t2w_sex.dta  [2.31 M x 9]
td1 = td1[, c(1:9)]
td <- inner_join(td1,l, by=c('home_msoa'='v1','work_msoa'='v2')) #8476 flows not there 
rm(td1)
#global pop. cycling ratios (0.05 males, 0.02 females)
td <-cbind(td,maleperc=0, femaleperc=0, malecyc=0.05, femalecyc=0.02) 


#add m/f ratios of total population
# 0.99 = f/m ratio all trips vs Census for GM
td$femaleperc <- td$allcom_female/(td$allcom_male+td$allcom_female)
td$maleperc <- 1 - td$femaleperc

#add m/f cycling ratios to FLOWS 
# 0.93 = f/m cyclist ratio all trips vs Census for GM
#target <- which((td$bicycle_male!=0 | td$bicycle_female!=0)& (td$bicycle_male + td$bicycle_female>5))

td$femalecyc <- 0.99 * 0.93 * td$v12 * td$allcom_female  / ( td$allcom_female + td$allcom_female )
td$malecyc <- td$v12 - td$femalecyc

td$allcom_male <-round(td$v3 *td$maleperc,0)  #N_males=N.totalpop (td$v3) * %perc_male in pop.
td$allcom_female <-td$v3 - td$allcom_male  #N_females = N.totalpop - N_males

#Male cyclists = Estimated N_cyclists * %male cyclists
td$bicycle_male <- round(td$v12 * td$malecyc/(td$malecyc+td$femalecyc), 0)
td$bicycle_female <- td$v12 - td$bicycle_male

#delete NaN's
# td[!is.finite(td[target,24]),24]<-0
# td[!is.finite(td[target,25]),25]<-0

#sex ratio for GM MSOAs
td <-td[,1:9]
save.dta13(td, './Input/msoa_t2w_sex_GM.dta')

#### ----------------> run ANNA's SCENARIOS  (R, or Stata ) !!!
#
# produces 1: pct_lines_GM.dta
#         2: pct_areas_GM.dta ..........  (layer for Greater Manchester)
########################################################################





##############################################
#  AD-HOC procedure:
#  ONCE SCENARIOS ARE THERE,
# 'NORMALISATION' TAKES PLACE: the file is made as similar as
# possible to the existing PCT files (l.Rds, c.Rds, .......)

rm(list=ls())

#NORM. STEP 1:   read pct_lines file + get ready for next stage
pct <-read.dta13('./Output/pct_lines_GM.dta')   #pct_lines_GM.dta (generated from scenarios code)
pct = pct[, c(1:length(names(pct)))]

pct <-pct[pct$msoa2!='other', ]
#colnames(pct)[1:2] <-c('Area.of.residence','Area.of.workplace')
pct <- pct[pct$all!=0, ] 

pct <-cbind.data.frame(id=(paste(pct$msoa1,pct$msoa2, sep=' ')),pct)
pct$id <- as.character( pct$id)
cents = pct[pct$msoa1 == pct$msoa2, ]         #used to generate zones & centroids below
pct = pct[pct$msoa1!=pct$msoa2, ]


## RECOVER DISTANCE **for all FLOWS** FROM gm.od1 (probably not needed)
gm.od = readRDS('./Output/gm.od1.Rds')
gm.od <-cbind.data.frame(id=(paste(gm.od$msoa1, 
                                   gm.od$msoa2, sep=' ')), gm.od )
gm.od$id = as.character(gm.od$id)
pct = inner_join(pct, gm.od[,c(2,3,14)], by=c('msoa1'='msoa1', 'msoa2'='msoa2') )    

# ###ADD missing col. DISTANCE FROM flow_nat
# #keeping only same flows as PCT (optional)
# path <-'V:/Group/GitHub/pct-bigdata/'
# flow_nat <- readRDS(file.path(path,'pct_lines_oneway_shapes.Rds'))  
# pct <-inner_join(pct,flow_nat@data[,c(1,84)], by='id')    


#match PCT colnames (as most are uppercase)
# pct <- dplyr::rename(.data = pct, All=all,
#                                  Train=train,
#                                  Bus=bus,
#                                  Taxi=taxi,
#                                   Motorbike=motorbike,
#                                   Car_driver=car_driver,
#                                   Car_passenger=car_passenger,
#                                   Bicycle = bicycle,
#                                   Foot = foot,
#                                   Other = other)


#pct = pct[, -c(2,3) ] #delete Area.of.residence & Area.of.workplace

#read c.Rds
pathGM <- '../../pct-data/greater-manchester/'  #before w/o: -NC
c <-readRDS(file.path(pathGM,'c.Rds'))   

#replace DF + add cols from c:  geo_code | geo_label | percent_fem | avslope
c@data = inner_join(c@data[,c(1:3,84)], cents[,c(2,4:83)], by=c('geo_code'='msoa1') )
saveRDS(c, '../../pct-bigdata/cents-scenarios_GM.rds')

###### TRANSFORMATION required for PCT
#create Spatial Lines object (pct=DF, c=Spatial Polygons/Points DF).
pct = pct[,c(2,3,1,4:83)]
pct= stplanr::onewayid(pct, attrib= c(4:83))
pct = inner_join(pct,gm.od[, c(2:3, 14)], by=c('msoa1'='msoa1', 'msoa2'='msoa2') )



l <- od2line(pct,c)

#l@data=l@data[,c(2,3,1,4:83)]
saveRDS(l, '../../pct-bigdata/lines_oneway_shapes_updated_GM.Rds')


##### CAUTION:    No need to aggregate both directions: now l comes already grouped
#cols. 4-14 if you have col. "id", 3:13 otherwise
#l <- onewayid(l, attrib = c(3:13), id1 ='Area.of.residence', id2 = 'Area.of.workplace' )
#l <- onewayid(l, attrib = c(4:14), id1 ='msoa1', id2 = 'msoa2' )

saveRDS(l,'./Output/l.rds')    #save as l.rds in pathGM
write.csv(l, './Output/l.csv',row.names = F) 

#######  ========================

#NORM. STEP 3:   read pct_areas file -> produce z.Rds
#pct <-read.dta13(file.choose())        #pct_lines_GM.dta, the flows file
z = readRDS(file.path(pathGM,'z.Rds'))   
pctzones <-read.csv('./Output/pct_area_GM.csv',header = T, as.is = T)   
pctzones <- pctzones[pctzones$all!=0, ]

pctzones <- dplyr::rename(.data = pctzones,
                          geo_code = home_msoa,
                          geo_label=home_msoa_name)

#pctzones = inner_join(pctzones, c@data[,c(1,3)], by='geo_code')


#replace DF in z  + add  missing col. avslope (58)
z@data = inner_join(z@data[,c(1,58)], pctzones, by='geo_code') # z file FIRST: otherwise labelling issue!!

saveRDS(z, './Output/z.rds')    #copy z.rDS to pathGM -------->
saveRDS(z, '../../pct-bigdata/ukmsoas-scenarios_GM.rds')    #copy z.rDS to pathGM -------->

