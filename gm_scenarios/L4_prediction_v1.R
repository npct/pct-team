library("readstata13")
library(dplyr)
library(stplanr)
library(rgdal)
library(rgeos)


########PREDICTION of no. of cyclists using 1) Census cyclist/pedestrian ratio + distance 
rm(list=ls())
gm.od <- readRDS('./Output/gm.od.rds')  #gm.od <- readRDS('./Output/gm.od_Anna.rds')
gm.od3 <-cbind(gm.od,CycleGM=0)
rm(gm.od)
gm.od3 <-gm.od3[,c(1:6,33,7:32)]

#read cyclestreets to get OD distances
cs = read.dta13('./Input/cyclestreets_speedhilliness.dta')
cs=cs[, c(1:6)]


# add column distance 
gm.od3 = left_join ( gm.od3, cs[, c(1,2,5)], 
                     by=c('Area.of.residence'='home_msoa' , 'Area.of.workplace'='work_msoa' )    )

sel = gm.od3$Area.of.residence== gm.od3$Area.of.workplace
gm.od3$dist[sel]  = 0
rm(cs)

#delete OD flows w/o a distance
gm.od3= gm.od3[! is.na(gm.od3$dist),]

# distance ranges for prediction
sel = gm.od3$FootGM !=0   
sel1 = sel & (gm.od3$dist< 2.99) & (gm.od3$dist>= 0) ; sel1factor = 0.025
sel2 = sel  & (gm.od3$dist>= 3) & ( gm.od3$dist <  5.99)     ; sel2factor = 0.339
sel3 = sel & (gm.od3$dist>= 6) &  (gm.od3$dist <  9.99)    ; sel3factor = 1.30
sel4 = sel & (gm.od3$dist>= 10) & (gm.od3$dist <  14.99)  
sel5 = sel & (gm.od3$dist>= 15) & (gm.od3$dist <  30)
sel6 = sel &  (gm.od3$dist >  30)


gm.od3$CycleGM[sel1] = gm.od3$FootGM[sel1] *   0.02439024     # 0.025/(1+0.025)
gm.od3$CycleGM[sel2] = gm.od3$FootGM[sel2] *   0.253174       # 0.339/ (1+ 0.339)   
gm.od3$CycleGM[sel3] = gm.od3$FootGM[sel3] *   (1.295/2.295) 
gm.od3$CycleGM[sel4] = gm.od3$FootGM[sel4] *   (11.937/12.937)    # 92% of total
gm.od3$CycleGM[sel5] = gm.od3$FootGM[sel5] *   1
gm.od3$CycleGM[sel6] =  0

gm.od3$CycleGM[!sel] = 0.032639 * gm.od3$all[!sel] - 0.083 * gm.od3$car_driver[!sel]-0.01*gm.od3$foot[!sel]
gm.od3$CycleGM[is.na(gm.od3$CycleGM)] = 0

#checks no negative / unreasonable values
gm.od3$CycleGM[which(gm.od3$CycleGM<0)] <- 0  #cancels those wrongly predicted as negative

sel = gm.od3$CycleGM > gm.od3$FootGM
gm.od3$CycleGM[sel] <- gm.od3$FootGM[sel] 

sel =(gm.od3$CycleGM>= 0.5*gm.od3$FootGM & gm.od3$FootGM>20)
gm.od3$CycleGM[sel] <- round(gm.od3$CycleGM[sel]* 0.7,0)

#round 0 dec
gm.od3$CycleGM <- round(gm.od3$CycleGM, 0)
gm.od3$FootGM <- gm.od3$FootGM - gm.od3$CycleGM  #adjusts


########### PREPARE for SCENARIOS GENERATION
l <- gm.od3[,1:7]

#gm.od3.Rds contains the no. of cyclists per each GM flow (intraflows included?)
saveRDS(gm.od3, './Output/gm.od3.rds')     #gm.od3.Rds
rm(gm.od3)

#NO NEED: scenarios use them!! ----- eliminate intraflows (+put them in c.Rds)
# intraflows <- which(l$Area.of.residence==l$Area.of.workplace)
# l <- l[-intraflows,]


#rename-sort-add cols to match l.Rds in PCT
colnames(l)[3:7] <-c('all','car_driver','bus','foot','bicycle')
l <- cbind(l,light_rail=0,taxi=0,motorbike=0,car_passenger=0, other=0)
l <-l[,c(1:3,8,5,9,10,4,11,7,6,12)]


#add & reorder to match Anna's flows source file (for scenarios generation)
l <- cbind(l,from_home=0, train=0 )  #add train + from_home
l <- l[,c(1:3,13,4,14,5:12)]
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


#this is needed to run scenarios (and is not in GM layer)
########## add msoa m/f ratios to msoa_t2w_sex_GM.dta (m/f ratios & m/f cyclist ratios)
td1 <-read.dta13('./Input/msoa_t2w_sex.dta')    # from ./Input/msoa_t2w_sex.dta  [2.31 M x 9]
td1 = td1[, c(1:9)]
td <- inner_join(td1,l, by=c('home_msoa'='v1','work_msoa'='v2')) #1238 flows not there (prob. inflows)
rm(td1)
#global pop. cycling ratios (0.05 males, 0.02 females)
td <-cbind(td,maleperc=0, femaleperc=0, malecyc=0.05, femalecyc=0.02) 


#add m/f ratios of total population
# 0.99 = f/m ratio all trips vs Census for GM
td$femaleperc <-0.99 * td$allcom_female/(td$allcom_male+td$allcom_female)
td$maleperc <- 1 - td$femaleperc

#add m/f cycling ratios to FLOWS WITH enough cyclists (>5 people cycling)
# 0.93 = f/m cyclist ratio all trips vs Census for GM
target <- which((td$bicycle_male!=0 | td$bicycle_female!=0)& (td$bicycle_male + td$bicycle_female>5))
td$femalecyc[target] <- 0.93 * td$bicycle_female[target]/(td$bicycle_male[target]+ td$bicycle_female[target])
td$malecyc[target] <- 1- td$femalecyc[target]

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


#NORM. STEP 1:   read pct_lines file + get ready for next stage
#pct <-read.dta13(file.choose())        #pct_lines_GM.dta, the flows file, read from GM folder
pct <-read.csv('./Output/pct_lines_GM.csv',header = T, as.is = T)      #read <pct_lines_GM.csv>
pct <-pct[pct$msoa2!='other', ]
#colnames(pct)[1:2] <-c('Area.of.residence','Area.of.workplace')
pct <- pct[pct$all!=0, ] 

pct <-cbind.data.frame(id=(paste(pct$msoa1,pct$msoa2, sep=' ')),pct)
pct$id <- as.character( pct$id)
cents = pct[pct$msoa1 == pct$msoa2, ]         #used to generate zones & centroids below
pct = pct[pct$msoa1!=pct$msoa2, ]


###ADD missing col. DISTANCE FROM flow_nat
#keeping only same flows as PCT (optional)
path <-'V:/Group/GitHub/pct-bigdata/'
flow_nat <- readRDS(file.path(path,'pct_lines_oneway_shapes.Rds'))  
pct <-inner_join(pct,flow_nat@data[,c(1,84)], by='id')    


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

#pct$clc = 100 * (pct$bicycle / pct$all)   -now in buildregion.R


#pct = pct[, -c(2,3) ] #delete Area.of.residence & Area.of.workplace

#read c.Rds
pathGM <- '../../pct-data/greater-manchester/'  #before w/o: -NC
c <-readRDS(file.path(pathGM,'c.Rds'))   #c.Rds generated from L5_build_region_GM.R

#add cols:  geo_code | geo_label | avslope
c@data = inner_join(c@data[,c(1:3,84)], cents[,c(2,4:83)], by=c('geo_code'='msoa1') )
saveRDS(c, '../../pct-bigdata/cents-scenarios_GM.rds')

###### TRANSFORMATION required for PCT
#create Spatial Lines object (pct=DF, c=Spatial Polygons/Points DF).
l <- od2line(pct,c)
l@data=l@data[,c(2,3,1,4:84)]


##### CAUTION:    No need to aggregate both directions: now l comes already grouped
#cols. 4-14 if you have col. "id", 3:13 otherwise
#l <- onewayid(l, attrib = c(3:13), id1 ='Area.of.residence', id2 = 'Area.of.workplace' )
#l <- onewayid(l, attrib = c(4:14), id1 ='msoa1', id2 = 'msoa2' )

saveRDS(l,'./Output/l.rds')    #save as l.rds in pathGM
write.csv(l, './Output/l.csv',row.names = F) 

#######  ========================

#NORM. STEP 3:   read pct_areas file -> produce z.Rds
#pct <-read.dta13(file.choose())        #pct_lines_GM.dta, the flows file
pctzones <-read.csv('./Output/pct_area_GM.csv',header = T, as.is = T)   
pctzones <- pctzones[pctzones$all!=0, ]

pctzones <- dplyr::rename(.data = pctzones,
                          geo_code = home_msoa,
                          geo_label=home_msoa_name)

pctzones = inner_join(pctzones, c@data[,c(1,4)], by='geo_code')


#only col. missing is avslope
z <- od2line(pctzones, c)
saveRDS(z, './Output/z.rds')    #copy z.rDS to pathGM -------->
saveRDS(z, '../../pct-bigdata/ukmsoas-scenarios_GM.rds')    #copy z.rDS to pathGM -------->

