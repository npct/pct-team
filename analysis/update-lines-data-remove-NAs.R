# Aim: update l.Rds to remove NA values and lines with distances above and below 30km in either direction
library(sp)
library(dplyr)

# get new data
curl::curl_download("https://github.com/npct/pct-shiny/files/419964/New.folder.zip", "/tmp/New.folder.zip")
unzip(zipfile = "/tmp/New.folder.zip", exdir = "/tmp/")
missing_lines = read.csv("/tmp/New folder/oneway_delete_lines.csv")
head(missing_lines) # take a look - just 32 ids

l = readRDS("../pct-bigdata/lines_oneway_shapes_updated.Rds")
summary("E02002485 E02002517" %in% l$id) # in updated lines? no
summary("E02002485 E02002517" %in% missing_lines$id) # in missing lines? yes

l_clev = readRDS("../pct-data/cleveland/l.Rds")
summary("E02002485 E02002517" %in% l_clev$id) # in cleveland new? no
l_clev = read.csv("https://cdn.rawgit.com/npct/pct-data/46946ee/cleveland/l.csv")
summary("E02002485 E02002517" %in% l_clev$id) # in cleveland data download? NO.

# solution - update the data sha in pct-shiny :)

# check data sha of pct:
readLines("../pct-shiny/data_sha")
# aha - not the latest sha has been downloaded...
l_clev = read.csv("https://cdn.rawgit.com/npct/pct-data/cf727e4/cleveland/l.csv") # with latest sha
summary("E02002485 E02002517" %in% l_clev$id) # in cleveland updated data download? yes!


nrow(l) # 233478 lines, including with 0 length

sel_missing = l$id %in% missing_lines$id
which(sel_missing) # 32 missing, distributed throughout the rows

sel_na_all = is.na(l$all)
summary(sel_na_all) # no NAs there...

sel_na_scenarios = is.na(l$dutch_slc)
summary(sel_na_scenarios)
summary(l$dist[sel_na_scenarios])

sel_na_lines = sel_na_scenarios & l$dist > 0
summary(sel_na_lines) # 8519 missing

to_remove = sel_missing | sel_na_lines
summary(to_remove) # 8551 to remove
l = l[!to_remove,]
nrow(l) # 224927 lines now
summary(l$dist)

saveRDS(l, "../pct-bigdata/lines_oneway_shapes_updated.Rds")

old = setwd("../pct-bigdata/")
system("git commit -am 'Update lines_oneway_shapes_updated.Rds'")
system("git push origin master")
