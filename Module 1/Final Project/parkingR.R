rm(list=ls())

library(lubridate)
library(ggmap)
library(ggplot2)
library(stringr)
library(tidyr)
library(geojsonio)

neighDF <- read.csv('~/Desktop/neigh.csv', stringsAsFactors = FALSE)
mainDF <- read.csv('~/Desktop/parking_data_2016_trimmed.csv', stringsAsFactors = FALSE)
houseDF <- read.csv('~/Desktop/houseData.csv', stringsAsFactors = FALSE)

names(mainDF)[9] <- "lon" # rename variables
names(mainDF)[1] <- "lat"

names(neighDF)[1] <- "lon" # rename variables
names(neighDF)[2] <- "lat"

# save for merge later
mDF <- mainDF
nDF <- neighDF
hDF <- houseDF

# mainDF$allPlateState <- mainDF$RP_PLATE_STATE # copy original 
# mainDF$RP_PLATE_STATE[!mainDF$RP_PLATE_STATE %in% c("MD", "VA", "DC") ] <- "other"# replace non MD/VA/DC with other
# mainDF$RP_PLATE_STATE <- as.factor(mainDF$RP_PLATE_STATE)


tixMain    <- "P076"
tixZone    <- c("P001", "P002", "P003", "P004", "P005", "P007", "P008", "P010", "P011", "P012", "P013", "P014", "P015", "P016", "P017", "P019", "P020", "P022", "P023", "P024", "P025", "P027", "P031", "P041", "P042", "P043", "P044", "P045", "P046", "P047", "P048", "P050", "P053", "P054", "P055", "P056", "P057", "P058", "P059", "P157", "P159", "P173", "P198", "P199", "P200", "P259", "P269", "P270", "P271", "P282", "P301", "P302", "P303", "P304", "P306", "P307", "P311", "P312", "P318", "P320", "P322", "P332", "P333", "P334", "P344", "P346", "P385", "P403", "P409", "P410", "P413", "P416", "P417", "P418", "P419")
tixVehicle <- c("P029", "P030", "P040", "P072", "P077", "P109", "P178", "P300", "P338", "P360", "P361", "P386", "P412", "P675")
tixMeter   <- c("P032", "P034", "P035", "P036", "P037", "P039", "P213", "P278", "P279", "P280", "P281", "P284", "P285", "P287", "P289", "P373", "P387", "P425")
tixPermit  <- c("P090", "P093", "P104", "P111", "P112", "P113", "P168", "P169", "P170", "P172", "P190", "P191", "P250", "P309", "P314", "P316", "P330", "P421")
tixTaxi    <- c("P120", "P122", "P123", "P127", "P205", "P212", "P232", "P239")

mainDF$ticket_group <- "Main" # fill with default category 
mainDF$ticket_group[mainDF$VIOLATION_CODE %in%  tixMain    ] <- "Main"   
mainDF$ticket_group[mainDF$VIOLATION_CODE %in%  tixZone    ] <- "Zone"   
mainDF$ticket_group[mainDF$VIOLATION_CODE %in%  tixVehicle ] <- "Vehicle"
mainDF$ticket_group[mainDF$VIOLATION_CODE %in%  tixMeter   ] <- "Meter"  
mainDF$ticket_group[mainDF$VIOLATION_CODE %in%  tixPermit  ] <- "Tags" 
mainDF$ticket_group[mainDF$VIOLATION_CODE %in%  tixTaxi    ] <- "Taxi"  

prkDF <- geojson_read("~/Desktop/prk.geojson", what = "sp")

sp::coordinates(mainDF) <- ~lon+lat
sp::proj4string(mainDF) <- sp::proj4string(prkDF)

sp::coordinates(neighDF) <- ~lon+lat
sp::proj4string(neighDF) <- sp::proj4string(prkDF)

sp::coordinates(houseDF) <- ~lon+lat
sp::proj4string(houseDF) <- sp::proj4string(prkDF)

test <-sp:: over(mainDF, prkDF)
testn <-sp:: over(neighDF, prkDF)
testh <-sp:: over(houseDF, prkDF)


mDF <- cbind(mDF, test[,1])
nDF <- cbind(nDF, testn[,1])
hDF <- cbind(hDF, testh[,1])

names(mDF)[10] <- "OBJECTID"
names(nDF)[4] <- "OBJECTID"
names(hDF)[4] <- "OBJECTID"
merged <- merge(nDF, mDF, by="OBJECTID")
mergedH <- merge(nDF, hDF, by="OBJECTID")

# make bounding box for data: bottom left, top right 
mapBox <- c(min(mainDF$lon), min(mainDF$lat), max(mainDF$lon), max(mainDF$lat))

# download background map courtesy of Google map
baseMapBox <- get_map(location = mapBox, maptype = 'roadmap',scale = 1) # no zoom, instead box

showBaseMap <- ggmap(baseMapBox) # save for later in plot form

p<- showBaseMap +
  geom_point(data= merged[1:10000,], aes(x=lon, y=lat, color = OBJECTID  ) ) +
  #annotate("text", label = "all states", x = -77.08, y = 38.83, size = 6, color = "red")+
  guides(color =  guide_legend(title = "plate\nstate")) + # change legend title
  scale_colour_brewer(palette = "Set1")
ggsave("allStates.png", units = "cm", width = 13, height = 13) # save a copy of pic


p <-showBaseMap +
  geom_point(data= mainDF[mainDF$RP_PLATE_STATE =="DC",], aes(x=lon, y=lat ), color = "red", alpha = 0.05 ) +
  annotate("text", label = "District", x = -77.08, y = 38.83, size = 6, color = "red")+
  scale_colour_discrete(guide = FALSE) 
ggsave("district.png", units = "cm", width = 11, height = 11) # save a copy of pic

showBaseMap +
  geom_point(data= mainDF[mainDF$RP_PLATE_STATE =="MD",], aes(x=lon, y=lat), color = "blue" , alpha = 0.05  ) +
  annotate("text", label = "Maryland", x = -77.08, y = 38.83, size = 6, color = "blue")+
  scale_colour_discrete(guide = FALSE) 
ggsave("maryland.png", units = "cm", width = 11, height = 11) # save a copy of pic

showBaseMap +
  geom_point(data= mainDF[mainDF$RP_PLATE_STATE =="VA",], aes(x=lon, y=lat ), color = "green", alpha = 0.05  ) +
  annotate("text", label = "Virginia", x = -77.08, y = 38.83, size = 6, color = "green")+
  scale_colour_discrete(guide = FALSE) 
ggsave("virginia.png", units = "cm", width = 11, height = 11) # save a copy of pic


showBaseMap +
  geom_point(data= mainDF[mainDF$RP_PLATE_STATE =="other",], aes(x=lon, y=lat ), color = "red", alpha = 0.05  ) +
  annotate("text", label = "other states", x = -77.08, y = 38.83, size = 6, color = "red")+
  scale_colour_discrete(guide = FALSE)
ggsave("others.png", units = "cm", width = 11, height = 11) # save a copy of pic