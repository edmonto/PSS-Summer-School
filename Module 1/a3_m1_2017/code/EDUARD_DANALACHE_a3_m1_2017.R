# Eduard Danalache
# Federal Reserve Board of Governors
# OCDO, DMID Section

library(dplyr)
library(reshape2)
library(zoo)

#clear workspace
rm(list=ls())

# identify home directory
home.dir    <- "C:/Users/Edi/Documents/Programming/R/PSS-Summer-School/Module 1/a3_m1_2017/"

# identify auxillary directories, based off of home.dir
code.dir    <- paste0(home.dir, "code/")
data.dir    <- paste0(home.dir, "data/")
plots.dir   <- paste0(home.dir, "plots/")

# identify input and output directories, based of of data.dir
input.dir   <- paste0(data.dir, "input/")
output.dir  <- paste0(data.dir, "output/")

# load provided data
load(file=paste0(input.dir, "trips_2010Q4_2014Q4.RData"))
load(file=paste0(input.dir, "weather_2010_2015.RData"))

# initialize answers list
answers <- list()


# problem 4.1
# part a
sub_whole_years_df <- trips[which(format(as.Date(trips$date), "%Y") != 2010 & format(as.Date(trips$date), "%Y") != 2014),]
answers$sub_whole_years_df <- sub_whole_years_df

# part b
agg_trips_by_ridryr <- sub_whole_years_df %>% mutate(year=format(as.Date(date), "%Y")) %>% group_by(type, year) %>% summarise(ntrips=sum(ntrips))
answers$agg_trips_by_ridryr <- agg_trips_by_ridryr

# part c
cas_reg_ridr <- dcast(agg_trips_by_ridryr, type ~ year)

# part d
mean_dur_by_ridryr <- sub_whole_years_df %>% mutate(year=format(as.Date(date), "%Y")) %>% group_by(type, year) %>% summarise(mduration=mean(mduration))
cas_reg_dur <- dcast(mean_dur_by_ridryr, type ~ year)
dur_list <- list(mean_dur=mean_dur_by_ridryr, cas_reg=cas_reg_dur)
answers$q41d <- dur_list

# part e
agg_trips_season <- (sub_whole_years_df %>% mutate(season=factor(format(as.yearqtr(as.yearmon(date) + 1/12), "%q"), levels = 1:4,
                                                                labels = c("winter", "spring", "summer", "fall"))) %>%
                    group_by(type, season) %>% summarise(ntrips=sum(ntrips)) %>% dcast(type ~ season) %>% mutate(measure="ntrips"))[,c(6, 1:5)]
mean_dur_season  <- (sub_whole_years_df %>% mutate(season=factor(format(as.yearqtr(as.yearmon(date) + 1/12), "%q"), levels = 1:4,
                                                                labels = c("winter", "spring", "summer", "fall"))) %>%
                    group_by(type, season) %>% summarise(mduration=mean(mduration)) %>% dcast(type ~ season) %>% mutate(measure="mduration"))[,c(6, 1:5)]
season_df <- rbind(agg_trips_season, mean_dur_season)
answers$season_df <- season_df


# problem 4.2
# part a
weather_is_raining <- weather %>% mutate(is_raining=ifelse(precip > 0, 1, 0))
answers$is_raining <- weather_is_raining


# part b
uniq_df <- weather_is_raining %>% group_by(date, hour) %>%
           summarise(temp=mean(temp), windspeed=mean(windspeed), precip=mean(precip, na.rm=T), is_raining=ifelse(max(is_raining, na.rm=T) == -Inf,
                                                                                                                 NA, max(is_raining, na.rm=T)))
answers$uniq_df <- uniq_df

# part c
variables <- c("temp", "windspeed", "precip", "is_raining")
median_vals <- c(median(weather_is_raining$temp), median(weather_is_raining$windspeed, na.rm=T),
                 median(weather_is_raining$precip, na.rm=T), median(weather_is_raining$is_raining, na.rm=T))
mean_vals <- c(mean(weather_is_raining$temp), mean(weather_is_raining$windspeed, na.rm=T),
               mean(weather_is_raining$precip, na.rm=T), mean(weather_is_raining$is_raining, na.rm=T))
sd_vals <- c(sd(weather_is_raining$temp), sd(weather_is_raining$windspeed, na.rm=T),
             sd(weather_is_raining$precip, na.rm=T), sd(weather_is_raining$is_raining, na.rm=T))
min_vals <- c(min(weather_is_raining$temp), min(weather_is_raining$windspeed, na.rm=T),
              min(weather_is_raining$precip, na.rm=T), min(weather_is_raining$is_raining, na.rm=T))
max_vals <- c(max(weather_is_raining$temp), max(weather_is_raining$windspeed, na.rm=T),
              max(weather_is_raining$precip, na.rm=T), max(weather_is_raining$is_raining, na.rm=T))

summarize_df <- data.frame(variables, median_vals, mean_vals, sd_vals, min_vals, max_vals)
answers$summarize_df <- summarize_df


# problem 5.3
# part a
merged_df <- merge(sub_whole_years_df, weather_is_raining)
answers$merged_df <- merged_df

# part b
lm_res <- lm(formula=ntrips ~ temp + windspeed + precip + is_raining, data=merged_df)
answers$lm_res <- lm_res


output.file <- "EDUARD_DANALACHE_a3_m1_2017.RData"
save(answers, file=paste0(output.dir, output.file))





















