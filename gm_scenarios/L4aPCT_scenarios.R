#setup
rm(list=ls())

library("readstata13")
library(dplyr)
library(data.table)
#library(taRifx)

pass_aggregate <- function  (var1, byconcept, oper) {
#aggregates entries by one/two linking fields

if (length(byconcept)==1) {
   agg <- aggregate(var1, by=list(l[[byconcept ]]),FUN=oper, na.rm=T )
   colnames(agg) <- c(byconcept, 'x')
   l <- inner_join(l, agg, by=byconcept)
   }

if (length(byconcept)==2) {
   agg <- aggregate(var1, by=list(l[,byconcept[1]], l[,byconcept[2]]),FUN=oper, na.rm=T )
   colnames(agg) <- c(byconcept[1], byconcept[2], 'x')
   l <- inner_join(l, agg, by=c(byconcept[1], byconcept[2]))
      }


#assign result
return(l$x)
   }


#read flow file (from private data)
old_dir = setwd("gm_scenarios/")
l <- read.csv('./Input/l_scenariosGM.csv', as.is=T, header=T)   #read from  ../3-Rds

#RENAMES v1..v14 to FINAL NAMES
l <- dplyr::rename(.data = l,   home_msoa = v1,
                                    work_msoa  = v2,
                                    all = v3,
                                    from_home  = v4,
                                    light_rail  = v5,
                                    train  = v6,
                                    bus  = v7,
                                    taxi  = v8,
                                    motorbike  = v9,
                                    car_driver  = v10,
                                    car_passenger  = v11,
                                    bicycle  = v12,
                                    foot  = v13,
                                    other = v14     )

## SECTION 2

#read mortality + add 3 cols. (from private data)
mortality <- read.dta13('./Input/mortrate_msoa.dta')    #test w. .CSV if .dta not 100% OK
colnames(mortality)[1] <- 'home_msoa'
mortality <-mortality[, c(1:4)]
l <-inner_join(l, mortality, by=c('home_msoa'))
rm(mortality)

#add cyclestreet variables: dist, dist_fast, av_slope (from private data)
cyclestreet <- read.dta13('./Input/cyclestreets_speedhilliness.dta')
cyclestreet <- cyclestreet[,1:7]
l <- left_join(l, cyclestreet[, -c(3, 4)], by=c('home_msoa', 'work_msoa'))
l[is.na(l)] <- 0         #replace NA from missing cyclestreet routes
rm(cyclestreet)

#drop _merge==2 (elements only in right set)
# GROUP OVERSEAS WORKPLACES TO 'OTHER', GENERATE WORKPLACE TYPE
l$hometemp <- substr(l$home_msoa,start = 1,stop = 1)     #picks first letter
l$worktemp <- substr(l$work_msoa,start = 1,stop = 1)

#####
l$work_msoa[l$work_msoa=="OD0000002" | l$worktemp %in% c("W","S",'0','1','2','3','4','5','9') ] <- 'other'

#generate & assign flowtypes
l$flowtype <- NA

l$flowtype[l$worktemp=="E" & l$dist_fast<30 & l$home_msoa!=l$work_msoa] <- 1  # the 'standard' commuter
l$flowtype[l$home_msoa==l$work_msoa]      <- 2     #work from home
l$flowtype[l$work_msoa=="OD0000003" ]     <- 3     #overseas and funny ones
l$flowtype[l$work_msoa=="other"]         <- 4      # other
l$flowtype[ l$worktemp=="E" & l$dist_fast>=30 ] <- 4    #long distance commuters

summary(l$flowtype[which(l$hometemp=="E" & l$work_msoa!="OD0000001")] )    #compare w. Stata

#start of various aggregations
#REPLACE 'all' & 'other' by their sum by OD
l$all <- pass_aggregate(l$all, c('home_msoa', 'work_msoa'), sum)
l$other <- pass_aggregate(l$other, c('home_msoa', 'work_msoa'), sum)

#del temp cols.
drops <- c('hometemp','worktemp')
l <- l[, !names(l) %in% drops]

#duplicates drop : all entries with same cols. (none in GM !!)
l <- l[!duplicated(l),]


#read sex ratio file
msoa_sex <- read.dta13("./Input/msoa_t2w_sex_GM.dta")
msoa_sex <- msoa_sex[, 1:9]
l <- inner_join(l,msoa_sex,by=c('home_msoa', 'work_msoa'))

sum((l$allcom_male+  l$allcom_female)!= l$all)  #check is 0

#order by home_msoa_names & work_msoa_name
l <- arrange(l, home_msoa_name, home_la_name)

l <-l[ l$work_msoa!="OD0000001",]
l$hometemp <- substr(l$home_msoa, start = 1, stop = 1)     #picks first letter
l <- l[l$hometemp=='E',]   #subset only to people in England (all for GM layer)


#################
## STEP 1: COLLAPSE TYPE 4 FLOWS TO 'OTHER'
#################
# COLLAPSE ALL OUT-OF-SCOPE FLOWS TO 'OTHER' (2 of 2)
l$work_msoa[l$flowtype==4] <-    'other'

#these lines not needed for G.M layer
l$all <- pass_aggregate(l$all, c('home_msoa', 'work_msoa'), sum)
l$other <- pass_aggregate(l$other, c('home_msoa', 'work_msoa'), sum)


l$allcom_male <- pass_aggregate(l$allcom_male, c('home_msoa', 'work_msoa'), sum)
l$bicycle_female <- pass_aggregate(l$bicycle_female, c('home_msoa', 'work_msoa'), sum)

#remove operational cols.
# drops <- grep(pattern ='agg_',x = names(l),fixed = T)
# l <- l[, -drops]

l$dist[l$work_msoa=='other'] <- NA
l$dist_fast[l$work_msoa=='other'] <- NA
l$avslope_perc[l$work_msoa=='other']    <-   NA

l <- l[!duplicated(l),]


#################
## STEP 2: ASSIGN VALUES IF DISTANCE/HILLINESS UNKNOWN
#################
## ASSIGN HILLINESS + DISTANCE VALUES IF START AND END IN SAME PLACE
#by home_msoa (dist_fast), sort: gen littlen=_n

#l <- transform(l, Sequence=ave(seq_along(littlen), home_msoa,dist_fast, FUN=seq_along))

l <- arrange(l, home_msoa, dist_fast)
l$littlen <- with(l, ave(rep(1, nrow(l)), l$home_msoa, l$dist_fast,  FUN = seq_along))  #for GM: mostly 1's

#check following lines
l$dist_fasttemp <-l$dist_fast
l$avslope_perctemp <- l$avslope_perc
l$dist_fasttemp[l$littlegen > 3] <- NA
l$avslope_perctemp [l$littlegen > 3] <- NA
l$dist_fasttemp[is.na(l$dist_fasttemp)] <- 0 #??

l$dist_fasttemp2 <- NA  #check if NEEDED
l$dist_fasttemp2 <- pass_aggregate(l$dist_fasttemp, 'home_msoa', mean)

l$avslope_perctemp2 <- NA
l$avslope_perctemp2 <- pass_aggregate(l$avslope_perctemp, 'home_msoa', mean)

l$dist_fasttemp[l$flowtype==2 & is.na(l$dist_fast)]        <-  l$dist_fasttemp2
l$avslope_perctemp [ l$flowtype==2 & is.na(l$avslope_perctemp) ]   <- l$avslope_perctemp2

# DISTANCE = A THIRD OF THE MEAN DISTANCE OF SHORTEST 3 FLOWS
# HILLINESS = MEAN HILLINESS OF SHORTEST 3 FLOWS
l$dist_fast[l$flowtype==2 ] = l$dist_fast[l$flowtype==2 ] / 3

#special case:  isles of Scilly
l$dist_fast[l$home_msoa=='E02006781' &l$work_msoa=='E02006781'] = 0.79
l$avslope_perc[l$home_msoa=='E02006781' &l$work_msoa=='E02006781'] = 0.2

###get rid of temp columns
#altern: regex
drops <- grep(pattern ='temp',x =names(l)  )    ##WATCH OUT !!!!! hometemp col. ALSO deleted
n <- which(names(l)=='littlen')
drops <- c(n, drops)
l <- l[,-drops]


## ASSIGN DISTANCE AMONG CYCLISTS VALUES IF NO FIXED PLACE : MEAN DIST AMONG CYCLISTS TRAVELLING <15KM
l$cycdist_fast <- l$dist_fast
l$dist_fast15  <- l$dist_fast30 <- l$dist_fast
l$bicycle15 <- l$bicycle30 <- l$bicycle

l$dist_fast15[l$dist_fast>15 | l$flowtype>2] <- NA
l$dist_fast30 [l$dist_fast>30 | l$flowtype>2] <- NA


l$bicycle15 [is.na(l$dist_fast15)] <- NA
l$bicycle30 [is.na(l$dist_fast30)] <- NA


l$numndist_fast15 <- pass_aggregate(l$dist_fast15 * l$bicycle15, 'home_msoa', sum)
l$dendist_fast15  <- pass_aggregate(l$bicycle15, 'home_msoa', sum)


l$meandist_fast15 <- l$numndist_fast15  /   l$dendist_fast15
l$cycdist_fast[l$flowtype==3 ]    <-  l$meandist_fast15
#OK

## ASSIGN DISTANCE VALUES IF OVERSEAS OR >30KM: MEAN DIST AMONG CYCLISTS TRAVELLING <30KM

# watch out l$dist_fast30, l$bicycle30 *have* NAs
l$numndist_fast30   <-  sum(l$dist_fast30[!is.na(l$dist_fast30)] * l$bicycle30[!is.na(l$bicycle30)] )
l$dendist_fast30    <-  sum(l$bicycle30[!is.na(l$bicycle30)] )
l$meandist_fast30   <-   l$numndist_fast30/ l$dendist_fast30

l$cycdist_fast[l$flowtype==4]    <-  l$meandist_fast30[l$flowtype==4]
#OK

#add new columns -- clarify w. Anna
l$dist_fastmissing <- l$dist_fast
l$cycdist_fastmissing <- l$cycdist_fast

l$dist_fastmissing[is.na(l$dist_fast)] <- NA
l$cycdist_fastmissing [is.na(l$cycdist_fast) ] <- NA

# table(l$flowtype)
# table(l$cycdist_fastmissing)

#this needs to be ommited in R, otherwise you lose key cols like allcom_female ?
#KEEP RELEVANT VARIABLES --TO FIT INDIVIDUAL MODEL
# l <-l[,c('home_msoa', 'work_msoa', 'home_msoa_name', 'all', 'other', 'flowtype',
#          'allcom_male', 'bicycle_female', 'mortrate_govtarget', 'mortrate_gendereq',
#          'mortrate_dutch', 'dist', 'dist_fast', 'avslope_perc', 'cycdist_fast')]

saveRDS(object =l[, c('home_msoa','cycdist_fast')],file="./Output/MSOA_ODpairs_process2.1.Rds")


#########

####### STEP 3A:    CALCULATE PROPENSITY to CYCLE

##MODEL FITTING FOR TRIPS <30KM

l$dist_fastsq       <-  l$dist_fast^2
l$dist_fastsqrt     <- sqrt(l$dist_fast)
l$ned_avslope_perc  <- l$avslope_perc-0.57
l$interact          <-   l$dist_fast * l$ned_avslope_perc
l$interactsqrt      <-  l$dist_fastsqrt * l$ned_avslope_perc

# FIT REGRESSION EQUATION

l$pred_base <- -3.894 + (-0.5872 * l$dist_fast) + (1.832 * l$dist_fastsqrt) + (0.007956 * l$dist_fastsq)
+ (-0.2872 * l$ned_avslope_perc) + (0.01784 * l$dist_fast* l$ned_avslope_perc)
+ (-0.09770 * l$dist_fastsqrt * l$ned_avslope_perc)

l$bdutch <-  2.499+(-0.07384 * l$dist_fast)                 #Dutch travel survey

l$bdutch[which(l$flowtype ==3)] <- NA
l$bebike <- (0.05710 * l$dist_fast) + (-0.0001087 * l$dist_fastsq)
l$bebike <- l$bebike + (-0.67 * -0.2872 * l$ned_avslope_perc)  #Swiss travel survey

l$pred_dutch <- l$pred_base + l$bdutch
l$pred_ebike <- l$pred_dutch + l$bebike

for (x in c('base','dutch','ebike')) {
    l[[paste0('pred_',x)]] <- exp( l[[paste0('pred_',x)]] ) / (1 +  exp( l[[paste0('pred_',x)]] ) )
}


## MODEL FITTING FOR TRIPS WITH NO FIXED PLACE= NA
# INPUT PARAMETERS
l$pred_base [l$flowtype!=1 & l$flowtype!=2] <- l$bdutch[l$flowtype!=1 & l$flowtype!=2] <- l$bebike[l$flowtype!=1 & l$flowtype!=2] <- NA

############
# all temp thing
############
l$nummeanpred_base <-  l$nummeanbdutch     <- l$nummeanbebike <- 0
cond <- l$flowtype!=1 & l$flowtype!=2
l$nummeanpred_base[cond] <-  l$nummeanbdutch[cond] <- l$nummeanbebike [cond] <- NA

l$nummeanpred_base <-  pass_aggregate(l$pred_base*l$all, 'home_msoa', sum)
l$nummeanbdutch     <- pass_aggregate(l$bdutch * l$all, 'home_msoa', sum)
l$nummeanbebike     <- pass_aggregate(l$bebike * l$all, 'home_msoa', sum )

x <-  pass_aggregate(l$all, 'home_msoa', sum)
l$denmeanpred_base  <- l$denmeanbdutch     <- l$denmeanbebike     <- x


l$meanpred_base  <- l$nummeanpred_base / l$denmeanpred_base
l$meanbdutch     <- l$nummeanbdutch  / l$denmeanbdutch
l$meanbebike     <- l$nummeanbebike  / l$denmeanbebike

l <-l[, -grep(pattern =('nummean|denmean'),x =names(l) )]   #alternative

l$meanpred_basesq   <-     l$meanpred_base^2
l$meanpred_basesqrt <-     l$meanpred_base^0.5


##FIT REGRESSION EQUATION
l$pred2_base<-  -6.218 + (189.9 * l$meanpred_basesq) + (9.275 * l$meanpred_basesqrt)
l$pred2_dutch<-  l$pred2_base + l$meanbdutch
l$pred2_ebike<-  l$pred2_dutch + l$meanbebike

l$pred2_base  <-   exp(l$pred2_base) / (1 +exp(l$pred2_base) )
l$pred2_dutch <-    exp(l$pred2_dutch) / (1 +exp(l$pred2_dutch) )
l$pred2_ebike  <-  exp(l$pred2_ebike) / (1 +exp(l$pred2_ebike) )


l$pred_base [l$flowtype==3] <-  l$pred2_base[which(l$flowtype==3)]
l$pred_dutch [l$flowtype==3] <- l$pred2_dutch[which(l$flowtype==3)]
l$pred_ebike [l$flowtype==3] <- l$pred2_ebike[which(l$flowtype==3)]

#delete pred2 cols.
l <- l[,-grep(pattern='pred2',x = names(l))]   #check
drops <- c('dist_fastsq' ,'dist_fastsqrt' ,'ned_avslope_perc' ,'interact' ,
           'interactsqrt' ,'meanpred_base' ,'bdutch' ,'bebike' ,'meanbdutch',
           'meanbebike' ,'meanpred_basesq' ,'meanpred_basesqrt')
l <- l[, !(names(l) %in% drops)]

########################################
## PART 3B: APPLY SCENARIOS TO MSOA DATA
#########################################
## CALCULATE NO. CYCLISTS IN EACH SCENARIO

l$nocyclists_slc<- 0
l$nocyclists_sic <-    l$nocyclists_slc - l$bicycle

l$govtarget_slc <-  l$bicycle+ (l$pred_base * l$all)
sel = which(l$govtarget_slc > l$all & ! is.na(l$govtarget_slc))
l$govtarget_slc [sel]  <- l$all[sel]
l$govtarget_sic  <-  l$govtarget_slc   -  l$bicycle

#??????? order govtarget_slc, before(govtarget_sic)

l$gendereq_slc <- (l$bicycle_male  * (1 + (l$allcom_female/l$allcom_male)))

l$gendereq_slc[which(l$gendereq_slc > l$all & !is.na(l$gendereq_slc)) ] <-
  l$all[which(l$gendereq_slc > l$all & !is.na(l$gendereq_slc)) ]


#tab all if female==0 | male==0
l$gendereq_slc[l$allcom_female==0 | l$allcom_male==0 | (l$gendereq_slc< l$bicycle)] <-
  l$bicycle[l$allcom_female==0 | l$allcom_male==0 | (l$gendereq_slc< l$bicycle)]

# [not needed] NO CHANGE IF NO FEMALES IN FLOW-NO MALES IN FLOW- SLC < BASELINE
l$gendereq_sic      <-     l$gendereq_slc - l$bicycle     #ajuste final

l$dutch_slc  <- l$pred_dutch * l$all
l$ebike_slc  <- l$pred_ebike * l$all

l$dutch_slc[l$dutch_slc >  l$all  & !is.na(l$dutch_slc) ]  <-    l$all  #max. is 100%
l$ebike_slc[l$ebike_slc >  l$all  & !is.na(l$ebike_slc) ]    <-  l$all

#check NAS in this line
sel_dutch = l$dutch_slc <  l$bicycle
sel_ebike = l$ebike_slc <  l$bicycle

sel_dutch[is.na(sel_dutch)] = 0
sel_ebike[is.na(sel_ebike)] = 0

l$dutch_slc[sel_dutch]  <-    l$bicycle[sel_dutch]  #min  IS BASELINE
l$ebike_slc[sel_ebike]    <-  l$bicycle[sel_ebike]

#increase in no. cyclists
l$dutch_sic  <- l$dutch_slc  - l$bicycle
l$ebike_sic  <- l$ebike_slc - l$bicycle


l$govtarget_slc[l$work_msoa=="other"]  <-    l$bicycle[l$work_msoa=="other"]
l$gendereq_slc [l$work_msoa=="other"]  <-    l$bicycle[l$work_msoa=="other"]
l$dutch_slc [l$work_msoa=="other"]  <-    l$bicycle[l$work_msoa=="other"]
l$ebike_slc [l$work_msoa=="other"]  <-    l$bicycle[l$work_msoa=="other"]

l$govtarget_sic[l$work_msoa=="other"]  <-    0
l$gendereq_sic [l$work_msoa=="other"]  <-    0
l$dutch_sic [l$work_msoa=="other"]  <-    0
l$ebike_sic [l$work_msoa=="other"]  <-    0


## CALCULATE % NON-CYCLISTS MADE CYCLISTS IN EACH SCENARIO: TURN THAT % AWAY FROM WALKING

l$pchange_nocyclists=(l$all- l$nocyclists_slc)/(l$all - l$bicycle)
l$nocyclists_slw = l$foot * l$pchange_nocyclists					# most flows - scale walking according to %change

sel=(l$bicycle==l$all)
l$nocyclists_slw[sel] =((l$all[sel]-l$nocyclists_slc[sel]) * 0.31) 	# Flows with pure bicycles at baseline - make walking 31% of new flows
l$nocyclists_siw=l$nocyclists_slw - l$foot
l$nocyclists_sld= l$car_driver * l$pchange_nocyclists

l$nocyclists_sld[ sel ]= (l$all[ sel ]- l$nocyclists_slc[ sel ]) * 0.35
# Flows with pure bicycles at baseline - make driving 35% of new flows

l$nocyclists_sid = l$nocyclists_sld -  l$car_driver
#order `x'_slw `x'_siw `x'_sld `x'_sid, after(`x'_sic)

# Create new variables - slw, siw, sld, sid for walking and driving
for (x in c('govtarget','gendereq', 'dutch','ebike') ) {

    l[[paste0('pchange_',x)]] <- NA
    l[[paste0('pchange_',x)]] = l$all - l[[paste0(x,'_slc')]] / (l$all - l$bicycle)

    l[l$all==l$bicycle, paste0('pchange_',x)] = 1

    l[[paste0(x,'_slw')]] = l$foot * l[[paste0('pchange_',x)]]
    l[[paste0(x,'_siw')]] = l[[paste0(x,'_slw')]] - l$foot
    l[[paste0(x,'_sld')]] = l$car_driver  * l[[paste0('pchange_',x)]]
    l[[paste0(x,'_sid')]] = l[[paste0(x,'_sld')]] - l$car_driver

                                        }

# order `x'_slw `x'_siw `x'_sld `x'_sid, after(`x'_sic)

## DROP INTERMEDIARY VARIABLES
drops <- c('pred_base', 'pred_dutch', 'pred_ebike', 'pchange_nocyclists', 'pchange_govtarget', 'pchange_gendereq', 'pchange_dutch', 'pchange_ebike')
l <- l[, !names(l) %in% drops]

#################
## STEP 4: DO HEAT
#################
# INPUT PARAMETERS (constants)

cyclecommute_tripspertypicalweek  <-  7.16
cspeed  <-  14
wspeed  <-  4.8
ebikespeed  <-  15.8
ebikemetreduction  <-  0.648

l$percentebike_dutch <- l$percentebike_ebike <- NA
l$percentebike_dutch [min(l$cycdist_fast)<=l$cycdist_fast & l$cycdist_fast<4.9999999] <- 0.06
l$percentebike_dutch [5<=l$cycdist_fast & l$cycdist_fast < 9.9999999] <-                  0.11
l$percentebike_dutch [10<=l$cycdist_fast  & l$cycdist_fast <=19.999999 ] <-               0.17
l$percentebike_dutch [20<= l$cycdist_fast    & l$cycdist_fast< max(l$cycdist_fast) ] <-   0.23


l$percentebike_ebike [min(l$cycdist_fast)<=l$cycdist_fast & l$cycdist_fast <4.9999999] <- 0.71
l$percentebike_ebike [5 <=l$cycdist_fast & l$cycdist_fast< 19.9999999]       <-           0.92
l$percentebike_ebike [20<= l$cycdist_fast  & l$cycdist_fast< max(l$cycdist_fast) ]   <-   1


# all constants
crr_webtag <- 0.72
crr_heat <- 0.9
cdur_ref_webtag <- 180
cdur_ref_heat <- 100
wrr_webtag <- 0.78
wrr_heat <- 0.89
wdur_ref_webtag <- 203
wdur_ref_heat <- 168
l$mortrate_nocyclists <- l$mortrate_govtarget
l$mortrate_ebike <- l$mortrate_dutch
vsl <- 1855315		# VALUE IN POUNDS

# DURATION OF CYCLING/WALKING

# TIME CYCLING PER DAY min. AMONG NEW CYCLISTS  -- CHECK WHY l$cycdist_fast is missing
l$cdur_obs  <- l$cdur_obs_dutch <- l$cdur_obs_ebike <- l$wdur_obs  <-  NA

l$cdur_obs  <-  60 * ((l$cycdist_fast * cyclecommute_tripspertypicalweek) / cspeed)
l$cdur_obs_dutch <- ((1-l$percentebike_dutch) * l$cdur_obs)+(l$percentebike_dutch * l$cdur_obs * ebikemetreduction * (cspeed/ ebikespeed))
l$cdur_obs_ebike <- ((1-l$percentebike_ebike) * l$cdur_obs)+(l$percentebike_ebike * l$cdur_obs * ebikemetreduction * (cspeed / ebikespeed))

# TIME WALKING PER DAY IN MINUTES AMONG THOSE NOW SWITCHING TO CYCLING
l$wdur_obs  <-  60 * ((l$cycdist_fast  * cyclecommute_tripspertypicalweek) / wspeed)

#	drop cyclecommute_tripspertypicalweek cspeed wspeed ebiketimereduction ebikemetreduction percentebike_dutch percentebike_ebike


# MORTALITY PROTECTION
## alternative: loop

#var <-paste0(cprotection_govtarget,z)    # SCALE RR DEPENDING ON HOW DURATION IN THIS POP COMPARES TO REF
l$cprotection_govtarget_webtag <- (1-crr_webtag) * (l$cdur_obs/cdur_ref_webtag)
l$cprotection_nocyclists_webtag <- l$cprotection_govtarget_webtag

l$cprotection_gendereq_webtag <- l$cprotection_govtarget_webtag
l$cprotection_dutch_webtag <-  (1-crr_webtag) * (l$cdur_obs_dutch / cdur_ref_webtag)
l$cprotection_ebike_webtag <- (1-crr_webtag) * (l$cdur_obs_ebike / cdur_ref_webtag)

l$cprotection_govtarget_heat <- (1-crr_heat) * (l$cdur_obs / cdur_ref_heat)
l$cprotection_nocyclists_heat <- l$cprotection_govtarget_heat
l$cprotection_gendereq_heat <- l$cprotection_govtarget_heat
l$cprotection_dutch_heat= (1-crr_heat) * (l$cdur_obs_dutch / cdur_ref_heat)
l$cprotection_ebike_heat= (1-crr_heat) * (l$cdur_obs_ebike /cdur_ref_heat)


cols1 <- paste0('cprotection_',c('nocyclists','govtarget','gendereq','dutch','ebike'),'_webtag')
cols2 <- paste0('cprotection_',c('nocyclists','govtarget','gendereq','dutch','ebike'),'_heat')

for (i in cols1)   {
  sel = which(l[,i]> 0.5)
  l[sel, i] <- 0.5   }

for (i in cols2)   {
  sel = which(l[,i]> 0.45)
  l[sel, i] <- 0.45   }



l$wprotection_webtag <-  (1- wrr_webtag) * (l$wdur_obs/  wdur_ref_webtag)
l$wprotection_heat <-  (1- wrr_heat) * (l$wdur_obs/ wdur_ref_heat)

sel = l$wprotection_webtag> 0.50
l$wprotection_webtag[sel]= 0.5

sel = l$wprotection_heat> 0.30
l$wprotection_heat[sel] = 0.30


# DEATHS AND VALUES
target1 <- c('webtag', 'heat')
target2 <- c('nocyclists', 'govtarget','gendereq','dutch','ebike')
target3 <- c('govtarget','gendereq','dutch','ebike')

# # Error in this code
# for (z in target1) {
#
#     for (x in target2) {
#
#           l[[paste0(x,'_sic_death_',z)]]  <- -1 * l[[paste0(x,'_sic')]]  * l[[paste0('mortrate_',x) ]]* l[[paste0('cprotection_',x,'_',z)]]
#
#           l[[paste0(x,'_siw_death_',z)]]  <- -1 * l[[paste0(x,'_siw')]] * l[[paste0('mortrate_',x) ]] * l[[paste0('cprotection_', x,'_',z)]]
#
#           l[[paste0(x,'_sic_death_',z)]]  <-  l[[paste0(x,'_sic_death_',z)]] + l[[paste0(x,'_siw_death_',z) ]]
#
#           # one var MISSING !!
#           l[[paste0(x,'_sivalue_',z)]]  <- -1 * l[[paste0(x,'_sideath_',z)]] * vsl   #long ommited!
#
#
#        #drop `x'_sic_death_`z' `x'_siw_death_`z'
#                            } #for x
#
# l[[paste0('base_sldeath_',z)]]  <- -1 * l[[paste0(nocyclists,'_sideath_',z )]]
# # BASELINE LEVEL IS INVERSE OF 'NO CYCLISTS' SCENARIO
#
# l[[paste0(base,'_slvalue_', z) ]] <- -1 * l[[paste0(nocyclists,'_sivalue_l', z)  ]]
#
#
#          for (x in target3)  {
#          l[[x,'_sldeath_' , z]]  = l[[x,'_sideath_' , z]] + l[['base_sldeath_' , z]]
#          l[[x,'_slvalue_' , z]] = l[[x,'_sivalue_' , z]] + l[['base_slvalue_', z]]
#
#          #order `x'_sideath_`z' `x'_sivalue_`z', after(`x'_slvalue_`z')
#                        }#for x
#
#                }  #for z

# DROP INTERMEDIARY VARIABLES
drops <- c('mortrate_govtarget', 'mortrate_gendereq', 'mortrate_dutch', 'cyclecommute_tripspertypicalweek', 'wprotection_heat',
           'nocyclists_sideath_webtag', 'nocyclists_sivalue_webtag', 'nocyclists_sideath_heat', 'nocyclists_sivalue_heat')

l <- l[ , !names(l) %in% drops]


#################
##  STEP 5: DO CO2 EMISSIONS CALCS
#################
#constants
cyclecommute_tripsperweek  <- 5.24
co2kg_km  <- 0.186

target <- c('nocyclists','govtarget', 'gendereq', 'dutch', 'ebike')

for (x in target)    {
      l[[paste0(x,'_sico2')]] <- l[[x,'_sid']] * l$cycdist_fast *
      cyclecommute_tripsperweek * 52.2 * co2kg_km 	# NO CYCLISTS *DIST * COMMUTE PER DAY * CO2 EMISSIONS FACToR
                     }

      l$base_slco2 <- -1 * l$nocyclists_sico2	## BASELINE LEVEL IS INVERSE OF 'NO CYCLISTS' SCENARIO INCREASE


   target <-target[-1]  #amend list

for (x in target) {
    l[[x,'_slco2']]<- l[[x, '_sico2']] + l$base_slco2

   #order `x'_sico2 , after(`x'_slco2)
                  }

#drop columns
drops <- c('nocyclists', 'cyclecommute_tripsperweek', 'co2kg_km', 'cycdist_fast')
l <- l[ , !names %in% drops]

#################
## FINISH: SAVE TEMPORARY DATASET, PRE-AGGREGATION
#################

saveRDS(l,file='./Output/MSOA_ODpairs_process2.5.Rds')


#################
## PART 3A: AGGREGATE TO AREA LEVEL
#################

# AGGREGATE UP AREA FIGURES

target <-c('all', ' other', ' govtarget_slc', 'ebike_sico2')

###########  ADAPT TO pass_aggregate function

for (x in target)  {
                l[[paste0('a_', x)]] <- aggregate(l[[x]], by=home_msoa,FUN=sum, na.rm=T)

                l[[paste0('a_', x)]] <- l[[paste0('a_', x,'$','x')]]
                }

# AREA FILE KEEP+RENAME+ORDER
tcols <- grep(pattern ='home_',names(l) )
tcols1 <- grep(pattern = 'a_', names(l) )
l  <- l[, c(tcols, tcols1)]
#   rename a_* *

l <- l[,-c('from_home')]

#order home_msoa home_msoa_name all light_rail- other
l <- l[!duplicated(l),]

# ROUND to 3, 5, 1 decimals
for (x in  c('govtarget_slc','ebike_sid')) {
    l[[x]]  = round(l[[x]], 3)  }

for (x in c('base_sl', 'govtarget_sl', 'govtarget_si', 'gendereq_sl', 'gendereq_si', 'dutch_sl', 'dutch_si', 'ebike_sl', 'ebike_si')) {
for (y in c('death_webtag', 'death_heat') ) {
    l[[paste0(x, y)]] =round(l[[paste0(x, y)]], 5)
                                    }

for (y in c('value_webtag', 'value_heat') ){
    l[[paste0(x, y)]]= round(l[[paste0(x, y)]],1)
                                        }

    l[[paste0(x,'co2')]] =  round(l[[paste0(x,'co2')]], 4)
}

# save lines
saveRDS(l,file='./Output/l_processed.Rds')


#################
## PART 3B: AGGREGATE TO FLOW LEVEL
#################

# MAKE BIDIRECTIONAL MSOAS
l$homesub=substr(l$home_msoa,2, 10 )
l$worksub=substr(l$work_msoa,2, 10)    #or replace 10 by real length

l$homesub <- as.numeric(l$homesub)
l$worksub <- as.numeric(l$worksub)

l$msoa1= as.character (min(l$homesub, l$worksub))
l$msoa2= as.character (max(l$homesub, l$worksub))    #check this


l$msoa1="E0"+msoa1
l$msoa2="E0"+msoa2

if (l$work_msoa=="OD0000003" | l$work_msoa=="other")  {
    l$msoa1=l$home_msoa
    l$msoa2=l$work_msoa             }

########## CHECK FUNCTION !!!!!!!!!
# AGGREGATE UP FLOW FIGURES                       =======>  LOOOOOOONGEST OF ALL !!!
for  (x in  c(all, other, govtarget_slc, ebike_sico2)  )  {

    l[[ paste0('f_',x)]]= aggregate(l[[x]], by=c('msoa1','msoa2'), na.rm=T)

                                                        }
# FLOW FILE KEEP + RENAME + ORDER + no duplicates
colstokeep <- grep(pattern=('msoa1'|'msoa2'|'f_') ,x=names(l))

#keep msoa1 msoa2 f_*

#rename f_* *
#drop from_home
#order msoa1 msoa2 all light_rail- other
l <- l[!duplicated(l),]

#vars to round to 2D
target<- c('govtarget_slc', 'ebike_sid')
l[target] <- round(l[target],digits =2)

#vars to round to 5D
x<- c('base_sl', 'govtarget_sl', 'govtarget_si', 'gendereq_sl', 'gendereq_si', 'dutch_sl', 'dutch_si', 'ebike_sl', 'ebike_si')
y = c('death_webtag', 'death_heat')
z = c('value_webtag', 'value_heat')

target1 <- expand.grid(x,y)
target2 <- expand.grid(x,z)
target3 <- expand.grid(x,'co2')

#set appropriate rounding per var
l[[target1]] <- round(l[target1], digits =5)
l[[target2]] <- round(l[target1], digits =1)
l[[target3]] <- round(l[target1], digits = 2)

saveRDS(l,"./Output/pct_lines_v1.Rds")
#it seems library("readstata13") does not always reads .dta files correctly, getting extra 'attr' rows
#so the source might need to be .CSV / .RDS


