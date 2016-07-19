# Aim: find and re-add 'missing lines'

# Start: get data for Cambridge
source("set-up.R")
region = "cambridgeshire"
# source("build_region.R") # comment out to skip build

# load excel file containing them
# download.file("https://github.com/npct/pct-shiny/files/333471/160624_MissingCambLines.xlsx", "160624_MissingCambLines.xlsx")
# flow_ag = readxl::read_excel("160624_MissingCambLines.xlsx")
l = readRDS("../pct-data/cambridgeshire/l.Rds")
zones = readRDS("../pct-data/cambridgeshire/z.Rds")
cents = readRDS("../pct-data/cambridgeshire/c.Rds")

# sum(flow_ag$visualised_anna)
nrow(l)
# Finding: there are more lines in 'visualised anna' (1347) than the pct (1110)
# Which ones are missing?

# Start with the flow data from the census
flow_cens = readr::read_csv("../pct-bigdata/wu03ew_v2.csv")

# Subset by zones in the study area
o <- flow_cens$`Area of residence` %in% cents$geo_code
d <- flow_cens$`Area of workplace` %in% cents$geo_code
flow_cam <- flow_cens[o & d, ] # subset OD pairs with o and d in study area
# Remove flows with all < 10
# flow_cam = flow_cam[flow_cam$`All categories: Method of travel to work` > 10,]
# Convert to SpatialLines
flow_cam_sp = od2line(flow = flow_cam, zones = cents)
flow_cam_sp$dist = gprojected(flow_cam_sp, byid = T) / 1000
plot(flow_cam_sp)
summary(flow_cam_sp$dist)
# cor(flow_cam_sp$dist, flow_cam_sp$dist3) # testing the method works
flow_cam_sp = flow_cam_sp[flow_cam_sp$dist < 20, ]
plot(flow_cam_sp, lwd = flow_cam_sp$`All categories: Method of travel to work` /
       mean(flow_cam_sp$`All categories: Method of travel to work`))# Finding: 2304 lines
sum(flow_cam_sp$`All categories: Method of travel to work`)
# Finding: 227166 commuters in Cambridgeshire
sum(l$all)
# Finding: 175204 commuters, 77% of Census data


# flow_cam_oneway = onewaygeo(flow_cam_sp, attrib = 3:ncol(flow_cam_sp)) # finding: fewer flows still
# Finding: results in 1294 flows: lines are lost in original onewayid

# Solution: update stplanr::onewayid() function
flow_cam_oneway = onewayid(flow_cam_sp@data, attrib = 3:14)
# flow_cam_oneway = onewayid(flow_cam_sp@data, attrib = 3:ncol(flow_cam_sp))
sum(flow_cam_oneway$`All categories: Method of travel to work`) ==
  sum(flow_cam_sp$`All categories: Method of travel to work`)
# Finding: fixed, they have the same total flow now
flow_cam_oneway = flow_cam_oneway[flow_cam_oneway$`All categories: Method of travel to work` > 10,]
# swap ids of lines where msoa1 > msoa2
sel_msoa1_big = flow_cam_oneway$`Area of residence` > flow_cam_oneway$`Area of workplace`
summary(sel_msoa1_big)
o = flow_cam_oneway$`Area of residence`[sel_msoa1_big]
d = flow_cam_oneway$`Area of workplace`[sel_msoa1_big]
flow_cam_oneway$`Area of residence`[sel_msoa1_big] = d
flow_cam_oneway$`Area of workplace`[sel_msoa1_big] = o
flow_cam_oneway = arrange(flow_cam_oneway, `Area of residence`, `Area of workplace`)
# summary(flow_cam_oneway$dist)
# flow_cam_oneway$dist = flow_cam_oneway$dist / 2

flow_cam_oneway_sp = od2line(flow_cam_oneway, cents)
flow_cam_oneway_sp$dist = gprojected(flow_cam_oneway_sp, byid = T) / 1000
summary(flow_cam_oneway_sp$dist)
plot(flow_cam_oneway_sp, lwd = flow_cam_oneway_sp$`All categories: Method of travel to work` /
       mean(flow_cam_oneway_sp$`All categories: Method of travel to work`))

df = flow_cam_oneway_sp@data
write_csv(df, "/tmp/cam-oneway-updated-rl.csv")

# side note: testing od2line
df_test = flow_cam_sp@data
summary(flow_cam_sp$dist) # only up to 20 km
sp_test = od2line(df_test, cents)
sp_test2 = od2line2(df_test, cents)
proj4string(sp_test2) = proj4string(sp_test)
bbox(sp_test)
flow_cam_sp$dist4 = gprojected(sp_test, byid = T) / 1000
flow_cam_sp$dist5 = gprojected(sp_test2, byid = T) / 1000
plot(flow_cam_sp$dist4, flow_cam_sp$dist)
plot(flow_cam_sp$dist5, flow_cam_sp$dist)
plot(flow_cam_sp)
plot(sp_test, col = "red", add = T)
plot(sp_test2, col = "green", add = T)
