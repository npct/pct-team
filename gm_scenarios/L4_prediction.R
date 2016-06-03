library("readstata13")
library(dplyr)
library(stplanr)

########PREDICTION of no. of cyclists
gm.od3 <-cbind(gm.od,CycleGM=0)
rm(gm.od)
gm.od3 <-gm.od3[,c(1:6,30,7:29)]


for (i in (1:nrow(gm.od3)) ) {


      #ratio from T.S.
      if (gm.od3$ctw[i]!=0) {gm.od3$CycleGM[i] = gm.od3$ctw[i] * gm.od3$FootGM[i] }

      #use Census
      else if (gm.od3$Bicycle.x[i]!=0)   {gm.od3$CycleGM[i] = (gm.od3$AllGM[i]/gm.od3$All[i]) * gm.od3$Bicycle.x[i]}

      #use Census popul. ratio
      else if (gm.od3$All[i]!=0) {gm.od3$CycleGM[i] = 0.032639 * gm.od3$All[i] - 0.083 * gm.od3$Car_driver[i]-0.01*gm.od3$Foot[i]}

      #use GM. travel survey ratio
      else if (gm.od3$Bicycle.y[i]!=0)   {gm.od3$CycleGM[i] = (gm.od3$AllGM[i]/gm.od3$AllTS[i]) * gm.od3$Bicycle.y[i]}

      #use global perc.
      else {gm.od3$CycleGM[i] = round(0.03 * gm.od3$FootGM[i],0)}

if (gm.od3$CycleGM[i] > gm.od3$FootGM[i]) {gm.od3$CycleGM[i] <- gm.od3$FootGM[i] }


                                 }     #end for


#checks
gm.od3$CycleGM[which(gm.od3$CycleGM<0)] <- 0  #cancels those wrongly predicted as negative
gm.od3$CycleGM[gm.od3$Cycle==gm.od3$FootGM] <- round(gm.od3$CycleGM[gm.od3$Cycle==gm.od3$FootGM]* 0.5,0)

#round 0 dec
gm.od3$CycleGM <- round(gm.od3$CycleGM, 0)
gm.od3$FootGM <- gm.od3$FootGM - gm.od3$CycleGM  #adjusts


#checks: bicycle never <0 + bicycle always<foot, change if not


########### PREPARE for SCENARIOS GENERATION
l <- gm.od3[,1:7]

#gm.od3.Rds contains the no. of cyclists per each GM flow (intraflows included)
saveRDS(gm.od3,file.choose())     #gm.od3.Rds
rm(gm.od3)

#NO NEED: scenarios use them!! ----- eliminate intraflows (+put them in c.Rds)
# intraflows <- which(l$Area.of.residence==l$Area.of.workplace)
# l <- l[-intraflows,]


#rename-sort-add cols to match l.Rds
colnames(l)[3:7] <-c('All','Car_driver','Bus','Foot','Bicycle')
l <- cbind(l,light_rail=0,Taxi=0,Motorbike=0,Car_passenger=0,Other=0)
l <-l[,c(1:3,8,5,9,10,4,11,7,6,12)]


#add & reorder to match Anna's flows source file (for scenarioS generation)
l <- cbind(l,from_home=0, train=0 )  #add train + from_home
l <- l[,c(1:3,13,4,14,5:12)]
namesl <-paste0('v',c(1:14))
colnames(l) <-namesl
save.dta13(l, file.choose())    #save as l_scenariosGM.dta
write.csv(l,file.choose(),row.names = F) #save as 'l_scenarios.csv'

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


########## add msoa male/female %perc to msoa_t2w_sex.dta (m/f ratios & m/f cyclist ratios)
td1 <-read.dta13(file.choose())    # <msoa_t2w_sex.dta>  [2.314 M x 9]

td <- inner_join(td1,l, by=c('home_msoa'='v1','work_msoa'='v2')) #1238 flows not there (prob. inflows)
rm(td1)
td <-cbind(td,maleperc=0, femaleperc=0, malecyc=0.05, femalecyc=0.02) 
#global pop. cycling ratios (0.05 males, 0.02 females)

#add m/f ratios of total population
td$maleperc <-td$allcom_male/(td$allcom_male+td$allcom_female)
td$femaleperc <-td$allcom_female/(td$allcom_male+td$allcom_female)


#add m/f cycling ratios to FLOWS WITH enough cyclists
target <- which((td$bicycle_male!=0 | td$bicycle_female!=0)& (td$bicycle_male + td$bicycle_female>5))
td$malecyc[target] <- td$bicycle_male[target]/(td$bicycle_male[target]+ td$bicycle_female[target])
td$femalecyc[target] <- td$bicycle_female[target]/(td$bicycle_male[target]+ td$bicycle_female[target])

td$allcom_male <-round(td$v3 *td$maleperc,0)  #N_males=N.totalpop (td$v3) * %perc_male in pop.
td$allcom_female <-td$v3 - td$allcom_male  #N_females

#Male cyclists = Estimated N_cyclists * %male cyclists
td$bicycle_male <- round(td$v12 * td$malecyc/(td$malecyc+td$femalecyc), 0)
td$bicycle_female <- td$v12 - td$bicycle_male

# td$bicycle_male <- round(td$v12 * td$malecyc, 0)  #male cyclists=Nmales.cyclists * % male cyclists
# td$bicycle_female <- round(td$v12 * td$femalecyc, 0) #female cyclists=N.females * % female cyclists

#delete NaN's
# td[!is.finite(td[target,24]),24]<-0
# td[!is.finite(td[target,25]),25]<-0

#sex ratio for GM MSOAs
td <-td[,1:9]
save.dta13(td, file.choose())        #save as <msoa_t2w_sex_GM.dta>
#write.csv(td,file.choose(),col.names = T,row.names = F)   #OR save <msoa_t2w_sex_v1.csv>, then convert .dta


#### ----------------> run Anna's scenarios
#
#produces 1: pct_lines_GM.dta
#         2: pct_areas_GM.dta ..........  (layer for Greater Manchester)

##############################################
#  AD-HOC procedure:
#  ONCE SCENARIOS ARE THERE,
# 'NORMALISATION' TAKES PLACE: the file is made as similar as
# possible to the existing PCT files (l.Rds, c.Rds, .......)


#NORM. STEP 1:   read pct_lines file + get ready for next stage
#pct <-read.dta13(file.choose())        #pct_lines_GM.dta, the flows file, read from GM folder
pct <-read.csv(file.choose(),header = T,as.is = T)      #read <pct_lines_GM.dta>
pct <-pct[pct$msoa2!='other', ]
colnames(pct)[1:2] <-c('Area.of.residence','Area.of.workplace')
pct <- pct[pct$all!=0, ]
pct <-cbind.data.frame(id=(paste(pct$Area.of.residence,pct$Area.of.workplace, sep=' ')),pct)

#keeping only same flows as PCT (optional)
path <-'V:/Group/GitHub/pct-bigdata/'
flow_nat <- readRDS(file.path(path,'pct_lines_oneway_shapes.Rds'))  
pct <-inner_join(pct,flow_nat@data[,c(2,3,83,84)])    #6531

#match PCT colnames (as most are uppercase)
pct <- dplyr::rename(.data = pct, All=all,
                                 Train=train,
                                 Bus=bus,
                                 Taxi=taxi,
                                  Motorbike=motorbike,
                                  Car_driver=car_driver,
                                  Car_passenger=car_passenger,
                                  Bicycle = bicycle,
                                  Foot = foot,
                                  Other = other)

#colnames(pct)[4:14] <- c( "All","light_rail","Train","Bus","Taxi","Motorbike","Car_driver","Car_passenger","Bicycle","Foot","Other")
#read c.Rds
pathGM <- 'V:/Group/GitHub/pct-data/greater-manchester/all-trips/'  #before w/o: -NC
c <-readRDS(file.path(pathGM,'c.Rds'))   #c.Rds generated from build_region.R


###ADD DISTANCE FROM flow_nat

###### TRANSFORMATION required for PCT
#create Spatial Lines object (l1=DF, c=Spatial Polygons/Points DF).
l <- od2line(pct,c)

#cols. 4-14 if you have col. "id", 3:13 otherwise
l <- onewayid(l, attrib = c(3:13), id1 ='Area.of.residence', id2 = 'Area.of.workplace' )
#l <- onewayid(l, attrib = c(4:14), id1 ='Area.of.residence', id2 = 'Area.of.workplace' )

saveRDS(l,file.choose())    #save as l.rDS in pathGM
write.csv(l, file.choose(),row.names = F) #NEEDED!!: csv does NOT contain garbage, dta DOES

#######  ========================

#NORM. STEP 3:   read pct_areas file -> produce z.Rds
#pct <-read.dta13(file.choose())        #pct_lines_GM.dta, the flows file
pctzones <-read.csv(file.choose(),header = T, as.is = T)
pctzones <- pctzones[pctzones$all!=0, ]

pctzones <- dplyr::rename(.data = pctzones,
                          geo_code = home_msoa,
                          geo_label=home_msoa_name,
                          All=all,
                          Car=car_driver,
                          Bicycle = bicycle)

#only col. missing is avslope
z <- od2line(pctzones, c)
saveRDS(z,file.choose())    #save as z.rDS in pathGM -------->


# #create Spatial Lines object
# l.df <- l@data
#
# #make flows single
# l <- onewayid(l, attrib = c(3:6), id1 ='Area.of.residence', id2 = 'Area.of.workplace' )
