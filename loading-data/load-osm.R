# download road network
system.time({
  r = get_lines(bbox = b, key = "highway", value = "tertiary") # slow
})
plot(r)
alt90_all = alt90
alt_mask = rasterize(x = r, y = alt90, mask = TRUE)
plot(r)
plot(alt_mask, add = TRUE)