# Renaming/updating data for pct-shiny

library(dplyr)

# load original data for zones
ukmsoas <- readRDS("~/npct/pct-bigdata/ukmsoas-scenarios.Rds")
names(ukmsoas)
# ukmsoas_new_old <- foreign::read.dta("~/Dropbox/PCT/160229_AreaLines/pct_area_160229.dta")
ukmsoas_new <- foreign::read.dta("/tmp/newdata/160401_AreaLines/pct_area.dta")
# identical(ukmsoas_new, ukmsoas_new_old)

names(ukmsoas_new)
ukmsoas_new <- dplyr::rename(ukmsoas_new,
                       geo_code = home_msoa,
                       geo_label = home_msoa_name
                       )

ukmsoas = ukmsoas[ukmsoas$geo_code %in% ukmsoas_new$geo_code,]

# check they are the same - nope!
plot(ukmsoas$All, ukmsoas_new$all)

ukmsoas_new <- left_join(ukmsoas@data["geo_code"], ukmsoas_new)

plot(ukmsoas$All, ukmsoas_new$all) # now they fit!
ukmsoas@data <- ukmsoas_new

names(ukmsoas)
head(ukmsoas)

# plot to ensure it makes sense
library(tmap)
tm_shape(ukmsoas) +
  tm_fill(col = "bicycle", breaks = c(0, 30, 300, 3000))

# saveRDS(ukmsoas, "~/npct/pct-bigdata/ukmsoas-scenarios.Rds")


# testing avslop addition
# p <- spsample(l, 10000, type = "regular")
# ltest <- l[1:10,]
# plot(ltest)
# utest <- ukmsoas[ltest,]
# plot(utest, add = T)
# ul = aggregate(ltest["avslope"], utest, FUN = mean)
# head(ul@data)
for(i in 5:50){
  # print(summary(ukmsoas@data[i] - ukmsoas@data[i]))
  print(summary(ukmsoas_new[i] - ukmsoas_new_old[i]))
}

# add avslope
l = readRDS("~/npct/pct-bigdata/pct_lines_oneway_shapes.Rds")
proj4string(l) <- proj4string(ukmsoas)
lmini <- l[l$dist < 10,] # reduce n. lines
nrow(lmini)
rf_all = readRDS("../pct-bigdata/rf.Rds")
nrow(rf_all)
nrow(l)
head(rf_all$id)
id_good = lmini$id
lmini$id = gsub(pattern = " ", "", lmini$id)
head(lmini$id)
lmini@data = left_join(lmini@data, rf_all@data[c("id", "av_incline")])
head(lmini@data["av_incline"])
ul = aggregate(lmini@data[,"av_incline"] ~ lmini$Area.of.residence, FUN = mean)
summary(ul)
names(ul) = c("geo_code", "avslope")

# ukmsoas$avslope <- ul[2]
ukmsoas@data = left_join(ukmsoas@data, ul)
tm_shape(ukmsoas) +
  tm_fill(col = "avslope")

ukmsoas@data <- rename(ukmsoas@data,
                       Bicycle = bicycle,
                       All = all,
                       Car = car_driver)

saveRDS(ukmsoas, "~/npct/pct-bigdata/ukmsoas-scenarios.Rds")

# knitr::spin("../pct/analysis/rename-zone-vars.R", format = "Rtex")
