# Eduard Danalache
# Federal Reserve Board of Governors
# OCDO, DMID Section

library(ggplot2)
library(dplyr)

#clear workspace
rm(list=ls())

# identify home directory
home.dir    <- "C:/Users/eduar/Documents/Programming/PSS-Summer-School/Module 1/a4_m1_2017/"

# identify auxillary directories, based off of home.dir
code.dir    <- paste0(home.dir, "code/")
data.dir    <- paste0(home.dir, "data/")
plots.dir   <- paste0(home.dir, "plots/")

# identify input and output directories, based of of data.dir
input.dir   <- paste0(data.dir, "input/")
output.dir  <- paste0(data.dir, "output/")

# load provided data
load(file=paste0(input.dir, "a4_m1_2017.Rdata"))

# initialize answers list
answers <- list()


# problem 2.1
# part 2.1.1
amzn_perf <- trades %>% filter(ticker=="AMZN") %>% mutate(lag=lag(price)) %>% mutate(amzn_return=(price-lag)/lag)

index_perf <- trades %>% group_by(date) %>% summarise(ticker="S&P 500", price=sum(price)) %>% mutate(lag=lag(price)) %>% mutate(index_return=(price-lag)/lag)

amzn_vs_index <- index_perf %>% select(date, index_return) %>% cbind(select(amzn_perf, amzn_return)) %>% na.omit()

calc_beta <- ggplot(data=amzn_vs_index, aes(x=index_return, y=amzn_return)) + geom_point() + labs(x="Market Return", y="AMZN Return", title="Calculating Beta") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_smooth(method="lm", color="black", se=F, fullrange=T)

pdf(paste0(plots.dir, "calc_beta.pdf"))
plot(calc_beta)
dev.off()

answers$q2.1.1 <- calc_beta


# part 2.1.2
trades_w_sec <- merge(trades, desc, by="ticker") %>% filter(date > as.Date("2005-12-31"))

nvda_perf_2006 <- trades %>% filter(ticker=="NVDA" & date > as.Date("2005-12-31")) %>% group_by(date) %>% summarise(ticker="NVDA", price=sum(price))
nvda_shares <- 1000 / nvda_perf_2006$price[1]
nvda_perf_2006 <- transmute(nvda_perf_2006, date=date, ticker=ticker, price=price*nvda_shares)

it_perf_2006 <- trades_w_sec %>% filter(gics_sector=="Information Technology" & date > as.Date("2005-12-31")) %>% group_by(date) %>% 
  summarise(ticker="IT Sector", price=sum(price))
it_shares <- 1000 / it_perf_2006$price[1]
it_perf_2006 <- transmute(it_perf_2006, date=date, ticker=ticker, price=price*it_shares)

index_perf_2006 <- trades %>% group_by(date) %>%  filter(date > as.Date("2005-12-31")) %>% summarise(ticker="Market", price=sum(price))
index_shares <- 1000 / index_perf_2006$price[1]
index_perf_2006 <- transmute(index_perf_2006, date=date, ticker=ticker, price=price*index_shares)

nvda_vs_it_vs_index_2006 <- index_perf_2006 %>% rbind(nvda_perf_2006) %>% rbind(it_perf_2006)

hyp_line <- ggplot(data=nvda_vs_it_vs_index_2006, aes(x=date, y=price, color=ticker, group=ticker)) + geom_line() + 
  labs(x="Date", y="Portfolio Value", title="Hypothetical $1,000 Investment", color="Investment") + theme(plot.title = element_text(hjust = 0.5))

pdf(paste0(plots.dir, "hyp_line.pdf"))
plot(hyp_line)
dev.off()

answers$q2.1.2 <- hyp_line


# part 2.1.3
nvda_returns_2014 <- trades %>% filter(ticker=="NVDA" & date > as.Date("2013-12-31") & date < as.Date("2015-01-01")) %>% 
  mutate(lag=lag(price)) %>% mutate(nvda_return=(price-lag)/lag, year=2014) %>% na.omit()
ret_hist <- ggplot(data=nvda_returns_2014, aes(x=nvda_return)) + geom_histogram() + labs(x="Daily Return (%)", y="Frequency", title="NVDA 2014 Returns") +
  theme(plot.title = element_text(hjust = 0.5))

pdf(paste0(plots.dir, "ret_hist.pdf"))
plot(ret_hist)
dev.off()

answers$q2.1.3 <- ret_hist


# part 2.1.4
nvda_returns_2008 <- trades %>% filter(ticker=="NVDA" & date > as.Date("2007-12-31") & date < as.Date("2009-01-01")) %>% 
  mutate(lag=lag(price)) %>% mutate(nvda_return=(price-lag)/lag, year=2008) %>% na.omit()
nvda_returns_compared <- rbind(nvda_returns_2014, nvda_returns_2008)
ret_kern <- ggplot(data=nvda_returns_compared, aes(x=nvda_return, color=factor(year), fill=factor(year), group=year)) + geom_density(alpha=.4, size=1) + 
  labs(x="Daily Returns (%)", y="Density", title="NVDA 2008 vs 2014 Returns", color="Year") + theme(plot.title = element_text(hjust = 0.5)) + guides(fill=F)

pdf(paste0(plots.dir, "ret_kern.pdf"))
plot(ret_kern)
dev.off()


sector_returns_2008 <- trades_w_sec %>% filter(gics_sector=="Consumer Discretionary" & date > as.Date("2007-12-31") & date < as.Date("2009-01-01")) %>% 
  group_by(date) %>% summarise(price=sum(price)) %>% mutate(lag=lag(price)) %>% mutate(return=(price-lag)/lag, year=2008) %>% na.omit()
sector_returns_2014 <- trades_w_sec %>% filter(gics_sector=="Consumer Discretionary" & date > as.Date("2013-12-31") & date < as.Date("2015-01-01")) %>% 
  group_by(date) %>% summarise(price=sum(price)) %>% mutate(lag=lag(price)) %>% mutate(return=(price-lag)/lag, year=2014) %>% na.omit()
sector_returns_compared <- rbind(sector_returns_2008, sector_returns_2014)
ret_kern_sector <- ggplot(data=sector_returns_compared, aes(x=return, color=factor(year), fill=factor(year), group=year)) + geom_density(alpha=.4, size=1) + 
  labs(x="Daily Returns (%)", y="Density", title="Consumer Discretionary Sector 2008 vs 2014 Returns", color="Year") + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(fill=F)

pdf(paste0(plots.dir, "sector_kern1.pdf"))
plot(ret_kern_sector)
dev.off()

sector_returns_2008 <- trades_w_sec %>% filter(gics_sector=="Consumer Staples" & date > as.Date("2007-12-31") & date < as.Date("2009-01-01")) %>% 
  group_by(date) %>% summarise(price=sum(price)) %>% mutate(lag=lag(price)) %>% mutate(return=(price-lag)/lag, year=2008) %>% na.omit()
sector_returns_2014 <- trades_w_sec %>% filter(gics_sector=="Consumer Staples" & date > as.Date("2013-12-31") & date < as.Date("2015-01-01")) %>% 
  group_by(date) %>% summarise(price=sum(price)) %>% mutate(lag=lag(price)) %>% mutate(return=(price-lag)/lag, year=2014) %>% na.omit()
sector_returns_compared <- rbind(sector_returns_2008, sector_returns_2014)
ret_kern_sector <- ggplot(data=sector_returns_compared, aes(x=return, color=factor(year), fill=factor(year), group=year)) + geom_density(alpha=.4, size=1) + 
  labs(x="Daily Returns (%)", y="Density", title="Consumer Staples Sector 2008 vs 2014 Returns", color="Year") + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(fill=F)

pdf(paste0(plots.dir, "sector_kern2.pdf"))
plot(ret_kern_sector)
dev.off()

sector_returns_2008 <- trades_w_sec %>% filter(gics_sector=="Energy" & date > as.Date("2007-12-31") & date < as.Date("2009-01-01")) %>% 
  group_by(date) %>% summarise(price=sum(price)) %>% mutate(lag=lag(price)) %>% mutate(return=(price-lag)/lag, year=2008) %>% na.omit()
sector_returns_2014 <- trades_w_sec %>% filter(gics_sector=="Energy" & date > as.Date("2013-12-31") & date < as.Date("2015-01-01")) %>% 
  group_by(date) %>% summarise(price=sum(price)) %>% mutate(lag=lag(price)) %>% mutate(return=(price-lag)/lag, year=2014) %>% na.omit()
sector_returns_compared <- rbind(sector_returns_2008, sector_returns_2014)
ret_kern_sector <- ggplot(data=sector_returns_compared, aes(x=return, color=factor(year), fill=factor(year), group=year)) + geom_density(alpha=.4, size=1) + 
  labs(x="Daily Returns (%)", y="Density", title="Energy Sector 2008 vs 2014 Returns", color="Year") + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(fill=F)

pdf(paste0(plots.dir, "sector_kern3.pdf"))
plot(ret_kern_sector)
dev.off()

sector_returns_2008 <- trades_w_sec %>% filter(gics_sector=="Financials" & date > as.Date("2007-12-31") & date < as.Date("2009-01-01")) %>% 
  group_by(date) %>% summarise(price=sum(price)) %>% mutate(lag=lag(price)) %>% mutate(return=(price-lag)/lag, year=2008) %>% na.omit()
sector_returns_2014 <- trades_w_sec %>% filter(gics_sector=="Financials" & date > as.Date("2013-12-31") & date < as.Date("2015-01-01")) %>% 
  group_by(date) %>% summarise(price=sum(price)) %>% mutate(lag=lag(price)) %>% mutate(return=(price-lag)/lag, year=2014) %>% na.omit()
sector_returns_compared <- rbind(sector_returns_2008, sector_returns_2014)
ret_kern_sector <- ggplot(data=sector_returns_compared, aes(x=return, color=factor(year), fill=factor(year), group=year)) + geom_density(alpha=.4, size=1) + 
  labs(x="Daily Returns (%)", y="Density", title="Financials Sector 2008 vs 2014 Returns", color="Year") + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(fill=F)

pdf(paste0(plots.dir, "sector_kern4.pdf"))
plot(ret_kern_sector)
dev.off()

sector_returns_2008 <- trades_w_sec %>% filter(gics_sector=="Health Care" & date > as.Date("2007-12-31") & date < as.Date("2009-01-01")) %>% 
  group_by(date) %>% summarise(price=sum(price)) %>% mutate(lag=lag(price)) %>% mutate(return=(price-lag)/lag, year=2008) %>% na.omit()
sector_returns_2014 <- trades_w_sec %>% filter(gics_sector=="Health Care" & date > as.Date("2013-12-31") & date < as.Date("2015-01-01")) %>% 
  group_by(date) %>% summarise(price=sum(price)) %>% mutate(lag=lag(price)) %>% mutate(return=(price-lag)/lag, year=2014) %>% na.omit()
sector_returns_compared <- rbind(sector_returns_2008, sector_returns_2014)
ret_kern_sector <- ggplot(data=sector_returns_compared, aes(x=return, color=factor(year), fill=factor(year), group=year)) + geom_density(alpha=.4, size=1) + 
  labs(x="Daily Returns (%)", y="Density", title="Health Care Sector 2008 vs 2014 Returns", color="Year") + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(fill=F)

pdf(paste0(plots.dir, "sector_kern5.pdf"))
plot(ret_kern_sector)
dev.off()

sector_returns_2008 <- trades_w_sec %>% filter(gics_sector=="Industrials" & date > as.Date("2007-12-31") & date < as.Date("2009-01-01")) %>% 
  group_by(date) %>% summarise(price=sum(price)) %>% mutate(lag=lag(price)) %>% mutate(return=(price-lag)/lag, year=2008) %>% na.omit()
sector_returns_2014 <- trades_w_sec %>% filter(gics_sector=="Industrials" & date > as.Date("2013-12-31") & date < as.Date("2015-01-01")) %>% 
  group_by(date) %>% summarise(price=sum(price)) %>% mutate(lag=lag(price)) %>% mutate(return=(price-lag)/lag, year=2014) %>% na.omit()
sector_returns_compared <- rbind(sector_returns_2008, sector_returns_2014)
ret_kern_sector <- ggplot(data=sector_returns_compared, aes(x=return, color=factor(year), fill=factor(year), group=year)) + geom_density(alpha=.4, size=1) + 
  labs(x="Daily Returns (%)", y="Density", title="Industrials Sector 2008 vs 2014 Returns", color="Year") + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(fill=F)

pdf(paste0(plots.dir, "sector_kern6.pdf"))
plot(ret_kern_sector)
dev.off()

sector_returns_2008 <- trades_w_sec %>% filter(gics_sector=="Information Technology" & date > as.Date("2007-12-31") & date < as.Date("2009-01-01")) %>% 
  group_by(date) %>% summarise(price=sum(price)) %>% mutate(lag=lag(price)) %>% mutate(return=(price-lag)/lag, year=2008) %>% na.omit()
sector_returns_2014 <- trades_w_sec %>% filter(gics_sector=="Information Technology" & date > as.Date("2013-12-31") & date < as.Date("2015-01-01")) %>% 
  group_by(date) %>% summarise(price=sum(price)) %>% mutate(lag=lag(price)) %>% mutate(return=(price-lag)/lag, year=2014) %>% na.omit()
sector_returns_compared <- rbind(sector_returns_2008, sector_returns_2014)
ret_kern_sector <- ggplot(data=sector_returns_compared, aes(x=return, color=factor(year), fill=factor(year), group=year)) + geom_density(alpha=.4, size=1) + 
  labs(x="Daily Returns (%)", y="Density", title="Information Technology Sector 2008 vs 2014 Returns", color="Year") + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(fill=F)

pdf(paste0(plots.dir, "sector_kern7.pdf"))
plot(ret_kern_sector)
dev.off()

sector_returns_2008 <- trades_w_sec %>% filter(gics_sector=="Materials" & date > as.Date("2007-12-31") & date < as.Date("2009-01-01")) %>% 
  group_by(date) %>% summarise(price=sum(price)) %>% mutate(lag=lag(price)) %>% mutate(return=(price-lag)/lag, year=2008) %>% na.omit()
sector_returns_2014 <- trades_w_sec %>% filter(gics_sector=="Materials" & date > as.Date("2013-12-31") & date < as.Date("2015-01-01")) %>% 
  group_by(date) %>% summarise(price=sum(price)) %>% mutate(lag=lag(price)) %>% mutate(return=(price-lag)/lag, year=2014) %>% na.omit()
sector_returns_compared <- rbind(sector_returns_2008, sector_returns_2014)
ret_kern_sector <- ggplot(data=sector_returns_compared, aes(x=return, color=factor(year), fill=factor(year), group=year)) + geom_density(alpha=.4, size=1) + 
  labs(x="Daily Returns (%)", y="Density", title="Materials Sector 2008 vs 2014 Returns", color="Year") + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(fill=F)

pdf(paste0(plots.dir, "sector_kern8.pdf"))
plot(ret_kern_sector)
dev.off()

sector_returns_2008 <- trades_w_sec %>% filter(gics_sector=="Telecommunications Services" & date > as.Date("2007-12-31") & date < as.Date("2009-01-01")) %>% 
  group_by(date) %>% summarise(price=sum(price)) %>% mutate(lag=lag(price)) %>% mutate(return=(price-lag)/lag, year=2008) %>% na.omit()
sector_returns_2014 <- trades_w_sec %>% filter(gics_sector=="Telecommunications Services" & date > as.Date("2013-12-31") & date < as.Date("2015-01-01")) %>% 
  group_by(date) %>% summarise(price=sum(price)) %>% mutate(lag=lag(price)) %>% mutate(return=(price-lag)/lag, year=2014) %>% na.omit()
sector_returns_compared <- rbind(sector_returns_2008, sector_returns_2014)
ret_kern_sector <- ggplot(data=sector_returns_compared, aes(x=return, color=factor(year), fill=factor(year), group=year)) + geom_density(alpha=.4, size=1) + 
  labs(x="Daily Returns (%)", y="Density", title="Telecommunications Services Sector 2008 vs 2014 Returns", color="Year") + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(fill=F)

pdf(paste0(plots.dir, "sector_kern9.pdf"))
plot(ret_kern_sector)
dev.off()

sector_returns_2008 <- trades_w_sec %>% filter(gics_sector=="Utilities" & date > as.Date("2007-12-31") & date < as.Date("2009-01-01")) %>% 
  group_by(date) %>% summarise(price=sum(price)) %>% mutate(lag=lag(price)) %>% mutate(return=(price-lag)/lag, year=2008) %>% na.omit()
sector_returns_2014 <- trades_w_sec %>% filter(gics_sector=="Utilities" & date > as.Date("2013-12-31") & date < as.Date("2015-01-01")) %>% 
  group_by(date) %>% summarise(price=sum(price)) %>% mutate(lag=lag(price)) %>% mutate(return=(price-lag)/lag, year=2014) %>% na.omit()
sector_returns_compared <- rbind(sector_returns_2008, sector_returns_2014)
ret_kern_sector <- ggplot(data=sector_returns_compared, aes(x=return, color=factor(year), fill=factor(year), group=year)) + geom_density(alpha=.4, size=1) + 
  labs(x="Daily Returns (%)", y="Density", title="Utilities Sector 2008 vs 2014 Returns", color="Year") + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(fill=F)

pdf(paste0(plots.dir, "sector_kern10.pdf"))
plot(ret_kern_sector)
dev.off()


#part 2.1.5



# problem 3.2
# part 




output.file <- "EDUARD_DANALACHE_a4_m1_2017.RData"
save(answers, file=paste0(output.dir, output.file))














