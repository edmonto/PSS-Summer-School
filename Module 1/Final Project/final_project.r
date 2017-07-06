# Eduard Danalache
# Federal Reserve Board of Governors
# OCDO, DMID Section

library(ggplot2)
library(dplyr)
library(timeDate)
library(ggthemes)

#clear workspace
rm(list=ls())

# identify home directory
home.dir    <- "C:/Users/eduar/Documents/Programming/PSS-Summer-School/Module 1/Final Project/"

# load in csv
parking_violations <- read.csv(paste0(home.dir, "parking_data_2016_trimmed.csv"))

# map different violation types
tixMain    <- "P076"
tixZone    <- c("P001", "P002", "P003", "P004", "P005", "P007", "P008", "P010", "P011", "P012", "P013", "P014", "P015", "P016", "P017", "P019", "P020", "P022", "P023", "P024", "P025", "P027", "P031", "P041", "P042", "P043", "P044", "P045", "P046", "P047", "P048", "P050", "P053", "P054", "P055", "P056", "P057", "P058", "P059", "P157", "P159", "P173", "P198", "P199", "P200", "P259", "P269", "P270", "P271", "P282", "P301", "P302", "P303", "P304", "P306", "P307", "P311", "P312", "P318", "P320", "P322", "P332", "P333", "P334", "P344", "P346", "P385", "P403", "P409", "P410", "P413", "P416", "P417", "P418", "P419")
tixVehicle <- c("P029", "P030", "P040", "P072", "P077", "P109", "P178", "P300", "P338", "P360", "P361", "P386", "P412", "P675")
tixMeter   <- c("P032", "P034", "P035", "P036", "P037", "P039", "P213", "P278", "P279", "P280", "P281", "P284", "P285", "P287", "P289", "P373", "P387", "P425")
tixPermit  <- c("P090", "P093", "P104", "P111", "P112", "P113", "P168", "P169", "P170", "P172", "P190", "P191", "P250", "P309", "P314", "P316", "P330", "P421")
tixTaxi    <- c("P120", "P122", "P123", "P127", "P205", "P212", "P232", "P239")


body_styles_trimmed <- parking_violations

# filter by frequency of each body style
body_styles_trimmed <- body_styles_trimmed %>% group_by(BODY_STYLE) %>% summarise(count=n())

# remove missing values and frequencies less than 1000
body_styles_trimmed <- body_styles_trimmed[which(body_styles_trimmed$count > 100000),]

body_style_g <- ggplot(body_styles_trimmed, aes(BODY_STYLE, count)) + geom_bar(stat="identity") +
  labs(x="Body Style", y="Number of Parking Tickets", title="Frequency of Parking Violations vs Most Common Car Styles") +
  theme_igray() + theme(plot.title = element_text(hjust = 0.5))
pdf(paste0(home.dir, "body_styles.pdf"))
plot(body_style_g)
dev.off()


violation_code_trimmed <- parking_violations

temp <- vector(mode="character", length=length(violation_code_trimmed$TICKET_ISSUE_DATE))
for (i in 1:length(temp)) {
  if (violation_code_trimmed$VIOLATION_CODE[i] %in% tixMain) {
    temp[i] <- "Main"
  } else if (violation_code_trimmed$VIOLATION_CODE[i] %in% tixZone) {
    temp[i] <- "Zone"
  } else if (violation_code_trimmed$VIOLATION_CODE[i] %in% tixVehicle) {
    temp[i] <- "Vehicle"
  } else if (violation_code_trimmed$VIOLATION_CODE[i] %in% tixMeter) {
    temp[i] <- "Meter"
  } else if (violation_code_trimmed$VIOLATION_CODE[i] %in% tixPermit) {
    temp[i] <- "Permit"
  } else if (violation_code_trimmed$VIOLATION_CODE[i] %in% tixTaxi) {
    temp[i] <- "Taxi"
  } else {
    temp[i] <- "Other"
  }
}


# filter by frequency of each violation
violation_code_trimmed <- violation_code_trimmed %>% group_by(VIOLATION_CODE) %>% summarise(count=n())

# remove frequencies less than 1000
violation_code_trimmed <- violation_code_trimmed[which(violation_code_trimmed$count > 10000),]

violation_code_g <- ggplot(violation_code_trimmed, aes(VIOLATION_CODE, count)) + geom_bar(stat="identity") +
  labs(x="Violation Code", y="Number of Parking Tickets", title="Frequency of Parking Violations vs Neighborhood Affluency") +
  theme_igray() + theme(plot.title = element_text(hjust = 0.5))
pdf(paste0(home.dir, "violation_codes.pdf"))
plot(violation_code_g)
dev.off()


neighborhood_prices <- read.csv(paste0(home.dir, "houses_with_neighborhoods.csv"))
parking_with_hoods <- read.csv(paste0(home.dir, "2016_with_neighborhoods.csv"))


is_holiday <- function(x) { 
  years = as.POSIXlt(x)$year+1900 
  years = unique(years) 
  holidays <- NULL 
  for (y in years) { 
    if (y >= 1885) 
      holidays <- c(holidays, as.character(USNewYearsDay(y))) 
    if (y >= 1885) 
      holidays <- c(holidays, as.character(USIndependenceDay(y))) 
    if (y >= 1885) 
      holidays <- c(holidays, as.character(USThanksgivingDay(y))) 
    if (y >= 1885) 
      holidays <- c(holidays, as.character(USChristmasDay(y))) 
  } 
  holidays = as.Date(holidays,format="%Y-%m-%d") 
  ans = x %in% holidays 
  return(ans) 
} 


#temp <- vector(mode="character", length=length(parking_with_hoods$TICKET_ISSUE_DATE))

#for (i in 1:length(parking_with_hoods$TICKET_ISSUE_DATE)) {
#  temp[i] <- is_holiday(parking_with_hoods$TICKET_ISSUE_DATE[i])
#  print(i)
#}

average_hood_price <- neighborhood_prices %>% group_by(LABEL_NAME) %>% summarise(average_price=mean(SALEPRICE))
tickets_per_hood <- parking_with_hoods %>% group_by(LABEL_NAME) %>% summarise(count=n())

parking_with_hoods_prices <- merge(x=tickets_per_hood, y=average_hood_price)

regression_line <- lm(data=parking_with_hoods_prices, formula=count ~ average_price)

affluent_tickets <- ggplot(data=parking_with_hoods_prices, aes(x=average_price, y=count)) + geom_point() + 
  scale_x_continuous(labels=comma, limits=c(0, 3100000)) + geom_smooth(method="lm") + 
  labs(x="Average Price of House ($)", y="Number of Parking Tickets", title="Frequency of Parking Violations vs Neighborhood Affluency") +
  theme_igray() + theme(plot.title = element_text(hjust = 0.5))
pdf(paste0(home.dir, "affluent_tickets.pdf"))
plot(affluent_tickets)
dev.off()









