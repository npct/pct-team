
##########PREPARE the RAW FILE
rm(list=ls())
filename <- 'Cycle Tool Yr1_3 GM TRADS export.csv'
ruta <- 'V:/Studies/MOVED/HealthImpact/Modelling/DfT_2.0/Manchester/Manchester Travel Survey/Data'

####GM travel Survey
td <- read.csv(file.choose(),header = T,as.is = T)
colnames(td)
colnames(td)[1]='id'

#
unique(td$StartLSOA)  #2154
unique(td$EndLSOA)    #2123 
unique(td$MainMode)   #10 modes, like Census
sum(td$MainMode=='Bicycle')    #485 cyclists (28782 trips)

ruta1 <- '//me-filer1/home$/au232/My Documents/1.CEDAR/3_Studies !!/28-DfT2.0/8-Census& lookups/LSOAlookup/'
#lookup LSOA<>MSOA
filename1<- 'LSOA_MSOA.csv'
lsoa <-read.csv(paste0(ruta1,filename1),header = T,as.is = T)
colnames(td)
colnames(lsoa)

#link
tdmsoa <- inner_join(td,lsoa[,c(1,3)],by=c('StartLSOA'='LSOA11CD') )

#rename cols

tdmsoa <- inner_join(tdmsoa,lsoa[,c(1, 3)],by=c('EndLSOA'='LSOA11CD'))
colnames(tdmsoa)[22] <-'StartMSOA'
colnames(tdmsoa)[23] <-'EndMSOA'
colnames(tdmsoa)


#reorder cols
tdmsoa <- tdmsoa[ , c(1:17,22,18,23, 19:21) ]
colnames(tdmsoa)

ruta2 <- '//me-filer1/home$/au232/My Documents/1.CEDAR/3_Studies !!/28-DfT2.0/4-Manchester/1-Model OD data DFT2.0/modelODdata~/3-Rds/'
#lookup LSOA<>MSOA
filename2 <- 'tdmsoa.Rds'


#save GM.T.S with MSOAs-non-grouped
saveRDS(tdmsoa, paste0(ruta2,filename2))



