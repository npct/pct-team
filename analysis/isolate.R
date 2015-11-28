# Add isolate file to pct-data directories that overlap
r <- c("West-Yorkshire", "gm")

for(i in r){
  file.create(paste0("pct-data/", i, "/isolated"))
}
