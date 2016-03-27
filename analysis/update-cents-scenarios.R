# Aim: update centroids used in the PCT

# load centroids
source("set-up.R")
centsa = readOGR("../pct-bigdata/cents.geojson", "OGRGeoJSON")

head(centsa)
names(centsa)[names(centsa) == "MSOA11NM"] = "geo_label"
l_new <- readstata13::read.dta13("C:/Users/georl/Dropbox/PCT/160229_AreaLines/pct_lines_160229.dta")

l_new <- dplyr::rename(l_new,
                       Area.of.residence = msoa1,
                       Area.of.workplace = msoa2,
                       All = all,
                       Bicycle = bicycle,
                       avslope = avslope_perc,
                       Rail = light_rail,
                       Bus = bus,
                       Car_driver = car_driver,
                       Car_passenger = car_passenger,
                       Foot = foot,
                       Other = other
)

# second attempt (after 1st round)
l@data <- rename(l@data,
                 Taxi = taxi,
                 Motorbike = motorbike)

# remove excess columns
l_new$Rail <- l_new$Rail + l_new$train
l_new$train <- NULL

summary(sel <- l_new$Area.of.residence == l_new$Area.of.workplace)
l_new_cents = l_new[sel,]
names(l_new)

l_new_cents$Area.of.workplace = NULL
l_new_cents = rename(l_new_cents, geo_code = Area.of.residence)

head(l_new_cents[1:5])
head(centsa)
# merge new data into existing data
summary(centsa$geo_code %in% l_new_cents$geo_code) # no matching variables
head(centsa$geo_code)
head(l_new_cents$geo_code)
centsa$geo_code = as.character(centsa$geo_code)
head(sort(centsa$geo_code))
head(sort(l_new_cents$geo_code))
summary(centsa$geo_code %in% l_new_cents$geo_code)
names(centsa)
l_new_cents$avslope = NULL

centsa@data = left_join(centsa@data, l_new_cents)
head(centsa@data)
plot(centsa)
cents_df = centsa@data
View(cents_df)
geojson_write(centsa, file = "../pct-bigdata/cents-scenarios.geojson")
