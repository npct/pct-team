#Generate lines and save in batches
# Aim: find and re-add 'missing lines'
source("set-up.R")
library(foreach)
library(doParallel)
cents = geojsonio::geojson_read("../pct-bigdata/cents-scenarios.geojson", what = "sp")
# load OD data - source http://wicid.ukdataservice.ac.uk/
unzip("../pct-bigdata/wu03ew_msoa.zip")
flow_cens = readr::read_csv("wu03ew_msoa.csv")
file.remove("wu03ew_msoa.csv")
nrow(flow_cens) # 2.4 m

  #plot(cents)
  cents$geo_code = as.character(cents$geo_code)
  o <- flow_cens$`Area of residence` %in% cents$geo_code
  d <- flow_cens$`Area of workplace` %in% cents$geo_code

  flow <- flow_cens[o & d, ] # subset OD pairs with o and d in study area
  geodist = stplanr::od_dist(flow,cents)/1000 # find straight line distance in km
  
  hist(geodist, breaks = 0:800)
  flow$dist = geodist
  flow = flow[!is.na(flow$dist),] # there are 36k destinations with no matching cents - remove
  flow = flow[flow$dist < 20,] # subset based on euclidean distance
  names(flow) = gsub(pattern = " ", "_", names(flow))
  flow_twoway = flow
  flow = onewayid(flow, attrib = 3:14)
  flow[1:2] = cbind(pmin(flow[[1]], flow[[2]]), pmax(flow[[1]], flow[[2]]))
  nrow(flow) # down to 0.9m, removed majority of lines
  # flow = flow[1:50000,]
  
  lines = od2line2(flow = flow, zones = cents)
  #plot(lines)
  
  #class(lines)
  #length(lines)
  lines = SpatialLinesDataFrame(sl = lines, data = flow)
  #names(lines)
  proj4string(lines) = CRS("+init=epsg:4326") # set crs
  
  sum(lines$`All_categories:_Method_of_travel_to_work`)
  #summary(lines$`All_categories:_Method_of_travel_to_work`)
  
  lines$dist = od_dist(flow = lines@data, zones = cents) / 1000
  
  #summary(lines$dist)
  
  lines@data <- dplyr::rename(lines@data,
                              msoa1 = Area_of_residence,
                              msoa2 = Area_of_workplace,
                              all = `All_categories:_Method_of_travel_to_work`,
                              bicycle = Bicycle,
                              train = Train,
                              bus = `Bus,_minibus_or_coach`,
                              car_driver = `Driving_a_car_or_van`,
                              car_passenger = `Passenger_in_a_car_or_van`,
                              foot = On_foot,
                              taxi = Taxi,
                              motorbike = `Motorcycle,_scooter_or_moped`,
                              light_rail = `Underground,_metro,_light_rail,_tram`,
                              other = Other_method_of_travel_to_work
  )
  
  lines$Work_mainlwary_at_or_from_home <- NULL
  
  #names(lines)
  
  l = lines
  
  ### RObin chunking
  
  # n = 99
 # x = 1:nrow(l)
 # sel = split(x, cut(x, n, labels = FALSE))
 # rf_n = rq_n = as.list(1:n)
 # num_names = formatC(1:n, flag = "0", width = 2)
 # sel_names = paste0("rf", num_names, ".Rds")
 # for(i in 1:99){
 #   print(sel_names[i])
 #   rf_n[[i]] = line2route(l = l[sel[[i]],], route_fun = "route_cyclestreet", plan = "fastest")
 #   rq_n[[i]] = line2route(l = l[sel[[i]],], route_fun = "route_cyclestreet", plan = "quietest")
 #   saveRDS(rf_n[[i]], paste0("D:/Git/pct-bigdata/chunks/", sel_names[i]))
 #  saveRDS(rq_n[[i]], paste0("D:/Git/pct-bigdata/chunks/", sel_names[i]))
 # }
### Malcolm Parallel Chuncking
  cl <- makeCluster(5)
  registerDoParallel(cl)
  n = 2
  x = 1:nrow(l)
  sel = split(x, cut(x, n, labels = FALSE))
  rf_n = rq_n = as.list(1:n)
  num_names = formatC(1:n, flag = "0", width = 2)
  sel_names = paste0("rf", num_names, ".Rds")
  par <- foreach(i=1:99,.errorhandling='pass',.packages = 'stplanr') %dopar% {
    rf_n[[i]] = line2route(l = l[sel[[i]],], route_fun = "route_cyclestreet", plan = "fastest")
    rq_n[[i]] = line2route(l = l[sel[[i]],], route_fun = "route_cyclestreet", plan = "quietest")
    saveRDS(rf_n[[i]], paste0("D:/Git/pct-bigdata/chunks/", sel_names[i]))
    saveRDS(rq_n[[i]], paste0("D:/Git/pct-bigdata/chunks/", sel_names[i]))
    print(sel_names[i]) 
    print(Sys.time())
  }
  
  
  # bind the routes together
  l = l[l$dist > 0,]
  n = 99
  x = 1:nrow(l)
  sel = split(x, cut(x, n, labels = FALSE))
  rf_n = as.list(1:n)
  num_names = formatC(1:n, flag = "0", width = 2)
  sel_names = paste0("rf", num_names, ".Rds")
  f = list.files(path = "D:/Git/pct-bigdata/chunks/", pattern = "rf[0-9]", full.names = T)
  for(i in 1:length(f)){
    if(i == 1){
      rf = readRDS(f[i])
      rf$id = l$id[sel[[i]]]
    }else{
      rn = readRDS(f[i])
      rn$id = l$id[sel[[i]]]
      rf = sbind(rf, rn)
      print(i)
    }
  }
  nrow(rf) == nrow(l)
  summary(rf$id == l$id)



