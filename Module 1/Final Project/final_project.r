# Eduard Danalache
# Federal Reserve Board of Governors
# OCDO, DMID Section

library(ggplot2)
library(dplyr)

#clear workspace
rm(list=ls())

# identify home directory
home.dir    <- "C:/Users/eduar/Documents/Programming/PSS-Summer-School/Module 1/Final Project/"

# load in csv
parking_violations <- read.csv(paste0(home.dir, "parking_data_trimmed.csv"))

body_styles_trimmed <- parking_violations

# filter by frequency of each body style
body_styles_trimmed <- body_styles_trimmed %>% group_by(BODY_STYLE) %>% summarise(count=n())

# remove missing values and frequencies less than 1000
body_styles_trimmed <- body_styles_trimmed[-c(1),]
body_styles_trimmed <- body_styles_trimmed[which(body_styles_trimmed$count > 200000),]

body_style_g <- ggplot(body_styles_trimmed, aes(BODY_STYLE, count)) + geom_bar(stat="identity")
pdf(paste0(home.dir, "body_styles.pdf"))
plot(body_style_g)
dev.off()


violation_code_trimmed <- parking_violations

# filter by frequency of each violation
violation_code_trimmed <- violation_code_trimmed %>% group_by(VIOLATION_CODE) %>% summarise(count=n())

# remove frequencies less than 1000
violation_code_trimmed <- violation_code_trimmed[which(violation_code_trimmed$count > 500000),]

violation_code_g <- ggplot(violation_code_trimmed, aes(VIOLATION_CODE, count)) + geom_bar(stat="identity")
pdf(paste0(home.dir, "violation_codes.pdf"))
plot(violation_code_g)
dev.off()
