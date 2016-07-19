flow <- readRDS("../../pct-bigdata/flow.Rds")

df_same  <- flow[flow$Area.of.residence == flow$Area.of.workplace,]
df_diff  <- inner_join(flow, flow, by=c('Area.of.workplace'='Area.of.residence', 'Area.of.residence'='Area.of.workplace'))
dupped_x <- names(df_diff)[grepl("\\.x", names(df_diff))]
dupped_x <- dupped_x[dupped_x != "id.x"]
dupped_y <- unlist(lapply(dupped_x, function(x){sub("\\.x", ".y", x)}))
unduped  <- unlist(lapply(dupped_x, function(x){sub("\\.x", "", x)}))

df_diff[,unduped]  <- df_diff[, dupped_x] +  df_diff[, dupped_y]
df_diff <- df_diff[,!grepl("\\.x|\\.y", names(df_diff))]

flow_one_way <- rbind(df_same, df_diff)
saveRDS(flow_one_way, "../../pct-bigdata/flow_one_way.Rds")
