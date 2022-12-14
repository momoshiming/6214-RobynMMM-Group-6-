## simple linear regression: daily 
data_sd <- read.csv("~/Desktop/MISM 6214/group/data_daily_sr.csv", header = TRUE)
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
data_md = read.csv("~/Desktop/MISM 6214/group/data_daily_mr.csv")
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(data_md$sales, SplitRatio = 0.7)
training_set = subset(data_md, split == TRUE)
test_set = subset(data_md, split == FALSE)
regressor = lm(formula = sales ~ .,
               data = training_set)
y_pred = predict(regressor, newdata = test_set)

summary(regressor)

#simple linear regression: weekly
data_sw <- read.csv("~/Desktop/MISM 6214/group/data_week_sr.csv", header = TRUE)
install.packages("caret")
install.packages("mlbench")
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
data_mw = read.csv("~/Desktop/MISM 6214/group/data_week_mr.csv")
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(data_mw$sales, SplitRatio = 0.7)
training_set = subset(data_mw, split == TRUE)
test_set = subset(data_mw, split == FALSE)
regressor = lm(formula = sales ~ .,
               data = training_set)
y_pred = predict(regressor, newdata = test_set)

summary(regressor)


## random forest
install.packages('randomForest')
library(randomForest)
rf<-randomForest(sales~.,data=data_mw,importance=TRUE, ntree=1000)
print(rf)

##lagged spend-sales correlations all the time???weekly dataset)
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
