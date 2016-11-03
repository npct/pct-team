library("readstata13") 
library(dplyr)
library(stplanr)   #install from github
library(rgdal)
library(rgeos)

########PREDICTION of no. of cyclists using 1) Census cyclist/pedestrian ratio + 2) distance 
rm(list=ls())
gm.od3 <- readRDS('./Output/gm.od2.rds')     #flows file w. fast route distances   
gm.od3$dist = gm.od3$dist/1000    #convert to Km

sel10minus= (gm.od3$Bicycle + gm.od3$On.foot<=10) |(gm.od3$Bicycle== 0)  | (gm.od3$On.foot== 0)
sel10plus=  ! sel10minus

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
   sel5 = sel & (gm.od3$dist>= 15)
   
   
   if (i==1)   {  #flows w. insufficient Census data
      
      #values as per Anna's table 20-Oct-2015 (replace w. Census MSOA-level cycling%)
      gm.od3$CycleGM[sel1] = gm.od3$FootGM[sel1] *   0.0703 * 0.32   
      gm.od3$CycleGM[sel2] = gm.od3$FootGM[sel2] *   0.298 * 0.85    
      gm.od3$CycleGM[sel3] = gm.od3$FootGM[sel3] *   0.495 * 1.05
      gm.od3$CycleGM[sel4] = gm.od3$FootGM[sel4] *   0.92            
      gm.od3$CycleGM[sel5] = gm.od3$FootGM[sel5] *   1
      
      
   } else  {   #flows w. enough Census data
      
      gm.od3$CycleGM[sel1] = 0.25 * gm.od3$FootGM[sel1] * gm.od3$Bicycle[sel1]/(gm.od3$Bicycle[sel1]+gm.od3$On.foot[sel1])      # 0.025/(1+0.025)
      gm.od3$CycleGM[sel2] = 0.339 * gm.od3$FootGM[sel2] * gm.od3$Bicycle[sel2]/(gm.od3$Bicycle[sel2]+ gm.od3$On.foot[sel2])        # 0.339/ (1+ 0.339)   
      gm.od3$CycleGM[sel3] = 1.3  * gm.od3$FootGM[sel3] * gm.od3$Bicycle[sel3]/(gm.od3$Bicycle[sel3] + gm.od3$On.foot[sel3]) 
      gm.od3$CycleGM[sel4] = 0.92  * gm.od3$FootGM[sel4] *  gm.od3$Bicycle[sel4]/(gm.od3$Bicycle[sel4] + gm.od3$On.foot[sel4])       # 92% of total
      gm.od3$CycleGM[sel5] = 1* gm.od3$FootGM[sel5] *   gm.od3$Bicycle[sel5]/(gm.od3$Bicycle[sel5] + gm.od3$On.foot[sel5]  )                 # =1 => all people cycling
   }
   
}


#deprecated: correlation model not used anymore
#gm.od3$CycleGM[!sel] = 0.032639 * gm.od3$all[!sel] - 0.083 * gm.od3$car_driver[!sel]-0.01*gm.od3$foot[!sel]

#fix abnormally high flows
sel30 = (gm.od3$CycleGM/gm.od3$AllGM)>0.3
x= runif(sum(sel30),0.2,0.3)     #x = rnorm(sum(sel30), 0.25, sd =0.02 )
gm.od3$CycleGM[sel30] = x * gm.od3$AllGM[sel30]

#round 0 dec
gm.od3$CycleGM <- round(gm.od3$CycleGM, 0)
gm.od3$FootGM <- gm.od3$FootGM - gm.od3$CycleGM  #adjusts
gm.od3[is.na(gm.od3)] =  0
sum(gm.od3$CycleGM)     #predicted total cyclists ~300 K

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

#rename-sort-add cols to match l.Rds in PCT
l  = dplyr::rename(l,         all = AllGM,
                   car_driver = CarDriver,
                   car_passenger = CarPassenger,
                   bus = BusGM,
                   foot = FootGM,
                   bicycle = CycleGM   )


l <- cbind(l[,c(3:5)],  from_home=0, 
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
rm(list=ls()[dropcols])

dropcols = grep(pattern = 'weekly_',x = ls())
rm(list=ls()[dropcols])

#this is needed to run scenarios (and is not in GM layer)
########## add msoa m/f ratios to msoa_t2w_sex_GM.dta (m/f ratios & m/f cyclist ratios)
td1 <-read.dta13('./Input/msoa_t2w_sex.dta')    # from ./Input/msoa_t2w_sex.dta  [2.31 M x 9]
td1 = td1[, c(1:9)]
td <- inner_join(td1,l, by=c('home_msoa'='v1','work_msoa'='v2')) #6K flows not there 
rm(td1)
#global pop. cycling ratios (0.05 males, 0.02 females)
td <-cbind(td,maleperc=0, femaleperc=0, malecyc=0.04, femalecyc=0.02, f_m=0.3333) 

#add m/f ratios of total population
# 0.99 = f/m ratio all trips vs Census for GM
td$femaleperc <- 0.99 * td$allcom_female/(td$allcom_male+td$allcom_female)
td$maleperc <- 1 - td$femaleperc

#get GLOBAL males&females figures
td$allcom_male <-round(td$v3 * td$maleperc,0)  #N_males=N.totalpop (td$v3) * %perc_male in pop.
td$allcom_female <-td$v3 - td$allcom_male  #N_females = N.totalpop - N_males

#add m/f cycling ratios to FLOWS 
# 0.93 = f/m cyclist ratio all trips vs Census (for GM)
target <- which((td$bicycle_male!=0 & td$bicycle_female!=0)& (td$bicycle_male + td$bicycle_female> 10))
td$f_m[target] =  td$bicycle_female[target] / (td$bicycle_male[target]+ td$bicycle_female[target])

#calculate cyclists %
# td$femalecyc <-  (td$v12/td$v3) * td$f_m / 0.93 
# td$malecyc <- (1 - td$f_m)* (td$v12/td$v3)


#Male cyclists = N_cyclists * %male cyclists
td$bicycle_female <- round(td$v12 * td$f_m / 0.93 , 0)
td$bicycle_male <- td$v12 - td$bicycle_female

td[is.na(td)] = 0

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
pct <- pct[pct$all!=0, ] 

pct <-cbind.data.frame(id=(paste(pct$msoa1,pct$msoa2, sep=' ')),pct)
pct$id <- as.character( pct$id)
cents = pct[pct$msoa1 == pct$msoa2, ] #used to generate centroids (inner flows)
pct = pct[pct$msoa1!=pct$msoa2, ]     #used to generate outer flows


# ## RECOVER DISTANCE **for all FLOWS** FROM gm.od1
# gm.od1 = readRDS('./Output/gm.od1.Rds')
# gm.od1 <-cbind.data.frame(id=(paste(gm.od1$msoa1, gm.od1$msoa2, sep=' ')), gm.od1 )
# gm.od1$id = as.character(gm.od1$id)
# pct = inner_join(pct, gm.od1[,c(1:3)], by='id' )    

#read c.Rds
pathGM <- '../../pct-data/greater-manchester/'  #before w/o: -NC
c <-readRDS(file.path(pathGM,'c.Rds'))   

#replace DF + add cols from c:  geo_code | geo_label | percent_fem | avslope
c@data = inner_join(c@data[,c(1:3,84)], cents[,c(2,4:83)], by=c('geo_code'='msoa1') )
saveRDS(c, '../../pct-bigdata/cents-scenarios_GM.rds')

###### TRANSFORMATION required for PCT
#create Spatial Lines object (pct=DF, c=Spatial Polygons/Points DF).
pct = pct[,c(2,3,4:83)]
pct= stplanr::onewayid(pct, attrib= c(3:82))
pct=data.frame(pct)

l <- stplanr::od2line(pct,c)

#Add dist-slope from gm.od
l@data = inner_join(l@data,gm.od1[, c(1:5)], by=c('msoa1'='msoa1', 'msoa2'='msoa2') )
l$dist= l$dist/1000


saveRDS(l, '../../pct-bigdata/lines_oneway_shapes_updated_GM.Rds')
saveRDS(l,'./Output/l.rds')    #save as l.rds in pathGM


#######  ========================

#NORM. STEP 3:   read pct_areas file -> produce z.Rds
#pct <-read.dta13(file.choose())        #pct_lines_GM.dta, the flows file
z = readRDS(file.path(pathGM,'z.Rds'))   
pctzones <-read.dta13('./Output/pct_area_GM.dta')
pctzones = pctzones[, c(1:length(names(pctzones)))]

pctzones <- pctzones[pctzones$all!=0, ]

pctzones <- dplyr::rename(.data = pctzones,
                          geo_code = home_msoa,
                          geo_label=home_msoa_name)

#pctzones = inner_join(pctzones, c@data[,c(1,3)], by='geo_code')


#replace DF in z  + add  missing col. avslope (58)
z@data = inner_join(z@data[,c(1,58)], pctzones, by='geo_code') # z file FIRST: otherwise labelling issue!!

saveRDS(z, './Output/z.rds')    #copy z.rDS to pathGM -------->
saveRDS(z, '../../pct-bigdata/ukmsoas-scenarios_GM.rds')    #copy z.rDS to pathGM -------->

