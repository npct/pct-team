l <- readRDS("../../pct-bigdata/pct_lines_oneway_shapes.Rds")

c(
  "msoa1" = "Area.of.residence",
  "msoa2" = "Area.of.workplace",
  "all" = "All",
  "bicycle" = "Bicycle",
  "train" = "Train",
  "bus" = "Bus",
  "car_driver" = "Car_driver",
  "car_passenger" = "Car_passenger",
  "foot" = "Foot",
  "taxi" = "Taxi",
  "motorbike" = "Motorbike",
  "other" = "Other"
)

for(r in names(rname)){ names(l)[which(names(l) == rname[r])] <- r }

saveRDS(l, "../../pct-bigdata/pct_lines_oneway_shapes.Rds")
