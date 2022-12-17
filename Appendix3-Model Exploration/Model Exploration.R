#############################

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
#install.packages('zoo')
library(xts)
library(zoo)
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


##################Convert data to week, month, quarter####################
#Convert data to month
data_new$Month <- as.yearmon(data_new$date)
data_month <- data_new %>%
  group_by(Month)%>%
  summarise_all(mean)
data_month <- data_month[,-2]
data_month <- data_month[,-18]

#Convert data to quater
data_new$Quarter <- as.yearqtr(data_new$date)
data_quarter <- data_new %>%
  group_by(Quarter)%>%
  summarise_all(mean)

data_quarter <- data_quarter[,-2]
data_quarter <- data_quarter[,-18]

#Convert data to week
#install.packages('data.table')
Week <- as.Date(cut(data_new$date, "week",start.on.monday=FALSE))

data_new$week <- Week

data_week <- data_new %>%
  group_by(week)%>%
  summarise_all(mean)

data_week <- data_week[,-c(2,3)]

## linear regression: daily 
data_sd <- data_new
install.packages("caret")
install.packages("mlbench")
library(caret)
library(mlbench)
lm1 <- train(sales ~ unemployment
             + temperature
             + facebook_newsfeed_spend
             + youtube_brand_spend
             + search_spend
             + youtube_performance_spend
             + newspaper_spend
             + tv_spend
             ,
             data = data_sd,
             #trControl=cross_train,
             method="lm")
summary(lm1)

## multiple linear regression: daily
data_md = data_new
data_md <- data_md[,c('sales','unemployment'
                   ,'temperature'
                   ,'facebook_newsfeed_spend'
                   ,'youtube_brand_spend'
                   ,'search_spend'
                   ,'youtube_performance_spend'
                   ,'newspaper_spend'
                   ,'tv_spend'
                   ,'GDP'
                   ,'Income'
                   ,'Inflation_rate'
                   , 'Retail_trade'
                   )]
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(data_md$sales, SplitRatio = 0.7)
training_set = subset(data_md, split == TRUE)
test_set = subset(data_md, split == FALSE)
regressor = lm(formula = sales ~ unemployment
               + temperature
               + facebook_newsfeed_spend
               + youtube_brand_spend
               + search_spend
               + youtube_performance_spend
               + newspaper_spend
               + tv_spend
               + GDP
               + Income
               + Inflation_rate
               + Retail_trade,
               data = training_set)
y_pred = predict(regressor, newdata = test_set)

summary(regressor)

#simple linear regression: weekly
data_sw <- data_week
#install.packages("caret")
#install.packages("mlbench")
library(caret)
library(mlbench)
lm2 <- train(sales ~ unemployment
             + temperature
             + facebook_newsfeed_spend
             + youtube_brand_spend
             + search_spend
             + youtube_performance_spend
             + newspaper_spend
             + tv_spend
             ,
             data = data_sw,
             #trControl=cross_train,
             method="lm")
summary(lm2)

#multiple linear regression: weekly
data_mw = data_week
data_mw<- data_mw[,c('sales','unemployment'
                     ,'temperature'
                     ,'facebook_newsfeed_spend'
                     ,'youtube_brand_spend'
                     ,'search_spend'
                     ,'youtube_performance_spend'
                     ,'newspaper_spend'
                     ,'tv_spend'
                     ,'GDP'
                     ,'Income'
                     ,'Inflation_rate'
                     , 'Retail_trade'
)]
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(data_mw$sales, SplitRatio = 0.7)
training_set = subset(data_mw, split == TRUE)
test_set = subset(data_mw, split == FALSE)
regressor = lm(formula = sales ~ unemployment
               + temperature
               + facebook_newsfeed_spend
               + youtube_brand_spend
               + search_spend
               + youtube_performance_spend
               + newspaper_spend
               + tv_spend
               + GDP
               + Income
               + Inflation_rate
               + Retail_trade,
               ,
               data = training_set)
y_pred = predict(regressor, newdata = test_set)

summary(regressor)


## random forest
install.packages('randomForest')
library(randomForest)
rf<-randomForest(sales~.,data=data_mw,importance=TRUE, ntree=1000)
print(rf)


############################################################
#Figure 3.3: Past spend & Current sales (lagged) all the time
############################################################
##lagged spend-sales correlations all the time（weekly dataset)
data_mw$total_spend <- data_mw$facebook_newsfeed_spend
+ data_mw$youtube_brand_spend
+ data_mw$search_spend
+ data_mw$youtube_performance_spend
+ data_mw$newspaper_spend
+ data_mw$tv_spend

lagged_corr_alltime_1_40 <- ccf(spend_ts <- ts(data_mw$total_spend), sales_ts <- ts(data_mw$sales), lag=40,
                                main = "Lagged correlations between past spend and current sales (All-time)",
                                xlim = c(-40,-1))

lagged_corr_alltime_11_16 <- ccf(spend_ts <- ts(data_mw$total_spend), sales_ts <- ts(data_mw$sales), lag=16,
                                 main = "Lagged correlations between past spend and current sales (All-time)",
                                 xlim = c(-10,0))