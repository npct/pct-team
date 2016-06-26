library(dplyr)
library(readxl)
library(ggplot2)
library(stringr)
download.file("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/456236/nts0101.xls",
              "nts0101.xls")
nts0101 = readxl::read_excel("nts0101.xls", skip = 8)
nts0101 = filter(nts0101, !is.na(`All trips1`))

newdate = function(x) {
  if(grepl("/", x)){
  x = gsub(" ", "", x)
  date_list = str_split(string =  x, pattern = "/")
  sel = nchar(date_list[[1]][1]) - nchar(date_list[[1]][2])
  base_date = str_sub(date_list[[1]][1], end = sel )
  newdate1 = str_sub(date_list[[1]][1], start = sel + 1)
  newdate2 = date_list[[1]][2]
  newdate = mean(c(as.numeric(newdate1), as.numeric(newdate2)))
  x = paste0(base_date, newdate)
  }
  return(x)
}


nts0101$Year = sapply(nts0101$Year, newdate)
nts0101$Year = as.numeric(nts0101$Year)
# bodge wrong years
nts0101$Year[nts0101$Year == 1949] = 1999
nts0101$Year[nts0101$Year == 1950.5] = 1991
names(nts0101)[2] = "Trips/year/person (all modes)"
alltrips19170 = ggplot(nts0101) + geom_point(aes(Year, `Trips/year/person (all modes)`))
dir.create("ggplots")
ggsave("ggplots/allplots1970s.png")

str(nts0101)
