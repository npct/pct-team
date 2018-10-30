# aim: create plot showing change in schoo
library(tidyverse)

d = readxl::read_excel("~/h/Dropbox/PCT/2_WorkInProgress/Anna/PCTSchoolsPaper/4_FirstRevision/2_NTS_ActiveTravel.xlsx")
names(d)[2] = "p_active"
d$Year = seq(from = 1976, to = 2016, length.out = nrow(d))
d$Year
ggplot(d) +
  geom_point(aes(Year, p_active * 100)) +
  ylab("% Active modes") +
  ylim(c(0, NA)) +
  theme_bw()
ggsave("figures/schools-cycling-time-plot.png")
browseURL("figures/schools-cycling-time-plot.png")
