l11 <- readRDS("../pct-bigdata/pct_lines_oneway_shapes.Rds")
lredox <- readxl::read_excel("D://tmp/linestoredo_rf_160225.xls")

head(l11@data$id)
lredox$id = paste(lredox$home_msoa, lredox$work_msoa)
lredox$id2 = paste(lredox$work_msoa, lredox$home_msoa)

sel1 = l11$id %in% lredox$id
sel2 = l11$id %in% lredox$id2

sum(sel1) # there are only 312 matches
sum(sel2) # there are many more - 2324 matches

l11 = l11[sel1 | sel2,]

sel3 <- !lredox$id %in% l11$id
sel4 <- !lredox$id2 %in% l11$id
sum(sel3)
sum(sel4)

lnot_linked = lredox[sel3 & sel4,]
summary(lnot_linked$id %in% lredox$id2) # there are 700 2 way flows in lredox

# And example of a 2way flow in lredox:

test1 = lnot_linked[lnot_linked$id %in% lredox$id2, ][1,]
test2 = lredox[lredox$id2 %in% test1$id,] 

test1 # line in one direction
test2 # line in another

# plan the routes

library("stplanr")

# rfmini <- line2route(l11, n_print = 100)
# rqmini <- line2route(l11, plan = "quietest",
#                      n_print = 100)
# saveRDS(rfmini, "rfmini.Rds")
# saveRDS(rqmini, "rqmini.Rds")

rfmini = readRDS("rfmini.Rds")
rqmini = readRDS("rqmini.Rds")

# join the data
l11$dist_fast <- rfmini$length
l11$dist_quiet <- rqmini$length
l11$time_fast <- rfmini$time
l11$time_quiet <- rqmini$time
l11$cirquity <- rfmini$length / l11$dist
l11$distq_f <- rqmini$length / rfmini$length
l11$avslope <- rfmini$av_incline
l11$co2_saving <- rfmini$co2_saving
l11$calories <- rfmini$calories
l11$busyness <- rfmini$busyness
l11$avslope_q <- rqmini$av_incline
l11$co2_saving_q <- rqmini$co2_saving
l11$calories_q <- rqmini$calories
l11$busyness_q <- rqmini$busyness


# test the output
head(l11@data)
n = 999
plot(l11[n,])
lines(rfmini[n,], add = T, col = "red")
lines(rqmini[n,], add = T, col = "green")

# save the result
# write.csv(l11@data, "D://tmp/lines-mini-2016-02-26-rl.csv")

# make the results clear to all
library(knitr)
spin(hair = "../pct/analysis/lredo-mini.R", "../pct/lredo-mini.html")
