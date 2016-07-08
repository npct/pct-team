pct_bigdata <- file.path("..", "..", "pct-bigdata")

l <- readRDS(file.path(pct_bigdata, "pct_lines_oneway_shapes.Rds"))

l@data <- dplyr::rename(l@data,
                        msoa1 = Area.of.residence,
                        msoa2 = Area.of.workplace,
                        all = All,
                        bicycle = Bicycle,
                        train = Train,
                        bus = Bus,
                        car_driver = Car_driver,
                        car_passenger = Car_passenger,
                        foot = Foot,
                        taxi = Taxi,
                        motorbike = Motorbike,
                        rail = Rail,
                        other = Other
)

saveRDS(l, file.path(pct_bigdata, "pct_lines_oneway_shapes.Rds"))

z <- readRDS(file.path(pct_bigdata, "ukmsoas-scenarios.Rds"))

z@data <- dplyr::rename(z@data,
  all = All,
  bicycle = Bicycle,
  car_driver = Car
)

saveRDS(z, file.path(pct_bigdata, "ukmsoas-scenarios.Rds"))
