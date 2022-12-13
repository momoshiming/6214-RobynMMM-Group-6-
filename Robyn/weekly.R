####################Input Data##############################
data_mmm <- read.csv('data_mmm.csv')
data_mmm$date <- as.Date(data_mmm$date, format='%Y-%m-%d')
data_mmm$year <- substr(data_mmm$date, 1, 4)
data_mmm$year <- as.factor(data_mmm$year)

#insert macroeconomic variables data
retail_trade <- read.csv('RSXFS.csv')
GDP <- read.csv('GDP.csv')
inflation_rate <- read.csv('T10YIE.csv')
Income <- read.csv('DSPIC96.csv')

data_mmm$date <- as.Date(data_mmm$date, format='%Y-%m-%d')
retail_trade$DATE <- as.Date(retail_trade$DATE, format='%Y-%m-%d')
GDP$DATE <- as.Date(GDP$DATE, format='%Y-%m-%d')
inflation_rate$DATE <- as.Date(inflation_rate$DATE, format='%Y-%m-%d')
Income$DATE <- as.Date(Income$DATE, format='%Y-%m-%d')


###################Merging Data##############################

#inflation_rate
library(dplyr)
#install.packages('xts')
library('xts')
inflation_rate.T10YIE <- xts(inflation_rate$T10YIE, order.by = inflation_rate$DATE)
# fill the NA with mean 
inflation_rate<-na.locf(merge(inflation_rate.T10YIE, foo=zoo(NA, order.by=seq(start(inflation_rate.T10YIE), end(inflation_rate.T10YIE),"day",drop=F)))[, 1])
data_new <- cbind(data_mmm, inflation_rate)


#retail_trade
retail_trade.RSXFS <- xts(retail_trade$RSXFS,order.by = retail_trade$DATE)
retail_trade<-na.locf(merge(retail_trade.RSXFS, foo=zoo(NA, order.by=seq(start(retail_trade.RSXFS), end(retail_trade.RSXFS),
                                                                         "day",drop=F)))[, 1])
data_new <- cbind(data_new, retail_trade)


#GDP
GDP.GDP <- xts(GDP$GDP,order.by = GDP$DATE)
GDP<-na.locf(merge(GDP.GDP, foo=zoo(NA, order.by=seq(start(GDP.GDP), end(GDP.GDP),
                                                     "day",drop=F)))[, 1])
data_new <- cbind(data_new, GDP)

#Income
Income.DSPIC96 <- xts(Income$DSPIC96,order.by = Income$DATE)
Income<-na.locf(merge(Income.DSPIC96, foo=zoo(NA, order.by=seq(start(Income.DSPIC96), end(Income.DSPIC96),
                                                               "day",drop=F)))[, 1])
data_new <- cbind(data_new, Income)

#reset index and rename column
rownames(data_new) <- 1:nrow(data_new)
#check the column name
colnames(data_new)
colnames(data_new)[colnames(data_new) == 'inflation_rate.T10YIE'] <- 'Inflation_rate'
colnames(data_new)[colnames(data_new) == 'retail_trade.RSXFS'] <- 'Retail_trade'
colnames(data_new)[colnames(data_new) == 'GDP.GDP'] <- 'GDP'
colnames(data_new)[colnames(data_new) == 'Income.DSPIC96'] <- 'Income'


##################Convert data to week####################
#Convert data to week
Week <- as.Date(cut(data_new$date, "week",start.on.monday=FALSE))

data_new$week <- Week

data_week <- data_new %>%
  group_by(week)%>%
  summarise_all(mean)

data_week <- data_week[,-c(2,3)]

write.csv(data_week,"weekly.csv", row.names = FALSE)
