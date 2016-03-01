# Aim generate output data for a cluster of regions
source("set-up.R")
regnames <- c("tiverton", "birmingham", "nottingham")
ireg <- 1 # start with one region for testing

dout <- "/tmp/pct-output-for-ncwip"
dir.create(dout)

for(ireg in seq_along(regnames)){
  ddir <- paste0("../pct-data/", regnames[ireg])
  doutr <- file.path(dout, regnames[ireg])
  dir.create(doutr)
  f <- (list.files(ddir, full.names = T))
  z <- readRDS(f[12])
  plot(z) # zones: OK
  head(z)
  l <- readRDS(f[4])
  plot(l)
  head(l)
  rf <- readRDS(f[8])
  rq <- readRDS(f[10])
  rnet <- readRDS(f[9])
  geojson_write(z, file = file.path(doutr, "z"))
  shapefile(l, file = file.path(doutr, "l"))
  geojson_write(l, file = file.path(doutr, "l"))
  geojson_write(rf, file = file.path(doutr, "rf"))
  geojson_write(rq, file = file.path(doutr, "rq"))
  shapefile(rnet, file = file.path(doutr, "rnet"))
  geojson_write(rnet, file = file.path(doutr, "rnet"))
}
