### Add four macroeconomic variables, and generate four new datasets of different time intervals by average
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
data_new$Quater <- as.yearqtr(data_new$date)
data_quater <- data_new %>%
  group_by(Quater)%>%
  summarise_all(mean)

data_quater <- data_quater[,-2]
data_quater <- data_quater[,-18]

#Convert data to week
#install.packages('data.table')
library(data.table)
data_new$week <- week(data_new$date)
library(tidyr)
data_new <- data_new %>% 
  unite(data_week, c("year", "week"), sep = "_")
data_week <- data_new %>%
  group_by(data_week)%>%
  summarise_all(mean)

data_week <- data_week[,!colnames(data_week) %in% c("X", "week")]


### Figure 2.1: The Correlation Heatmap in different time intervals

#################day##################
data_day = read.csv('data_day.csv')

### correlation heatmap
# select variables used in analysis for heatmap
cor_vars1 <- c("sales","unemployment","temperature","facebook_newsfeed_spend","facebook_newsfeed_impressions",
               "youtube_brand_spend","youtube_brand_impressions","search_spend","search_clicks","youtube_performance_spend", 
               "youtube_performance_impressions", "newspaper_spend", "newspaper_readership",
               "tv_spend", "tv_gross_rating_points", "inflation_rate", "retail_trade", "gdp", "income")

cor_data1 <- data_day[cor_vars1]

# create correlation matrix
# fill all missings with 0 to allow for correlation calculation
cor_data1[is.na(cor_data1) == TRUE] <- 0
cormat1 <- round(cor(cor_data1),2)

# helper function to get upper triangle of the correlation matrix
get_upper_tri1 <- function(cormat1){
  cormat1[lower.tri(cormat1)]<- NA
  return(cormat1)
}

upper_tri1 <- get_upper_tri1(cormat1)

melted_cormat1 <- melt(upper_tri1, na.rm = TRUE)

# label levels of factors to make heatmap nice to read
#levels(melted_cormat$Var1)[levels(melted_cormat$Var1)=="years_since_first_spend"] <- "Years active on platform"
#levels(melted_cormat$Var2)[levels(melted_cormat$Var2)=="years_since_first_spend"] <- "Years active on platform"
library(ggplot2)
cor_heat1 <- ggplot(data = melted_cormat1, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 8, hjust = 1))+
  theme(axis.text.y = element_text(size = ))+
  coord_fixed()

#######week###################
data_week <- read.csv("data_week.csv", header = TRUE)
data_week$date <- as.Date(data_week$date, format='%Y-%m-%d')


### correlation heatmap
# select variables used in analysis for heatmap
cor_vars2 <- c("sales","unemployment","temperature","facebook_newsfeed_spend","facebook_newsfeed_impressions",
               "youtube_brand_spend","youtube_brand_impressions","search_spend","search_clicks","youtube_performance_spend", 
               "youtube_performance_impressions", "youtube_performance_impressions", "newspaper_spend", "newspaper_readership",
               "tv_spend", "tv_gross_rating_points", "inflation_rate", "retail_trade", "gdp", "income")

cor_data2 <- data_week[cor_vars2]

# create correlation matrix
# fill all missings with 0 to allow for correlation calculation
cor_data2[is.na(cor_data2) == TRUE] <- 0
cormat2 <- round(cor(cor_data2),2)

# helper function to get upper triangle of the correlation matrix
get_upper_tri2 <- function(cormat2){
  cormat2[lower.tri(cormat2)]<- NA
  return(cormat2)
}

upper_tri2 <- get_upper_tri2(cormat2)

melted_cormat2 <- melt(upper_tri2, na.rm = TRUE)

# label levels of factors to make heatmap nice to read
#levels(melted_cormat$Var1)[levels(melted_cormat$Var1)=="years_since_first_spend"] <- "Years active on platform"
#levels(melted_cormat$Var2)[levels(melted_cormat$Var2)=="years_since_first_spend"] <- "Years active on platform"

cor_heat2 <- ggplot(data = melted_cormat2, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 8, hjust = 1))+
  theme(axis.text.y = element_text(size = ))+
  coord_fixed()

############quarter##########
data_quarter = read.csv('data_quarter.csv')

### correlation heatmap
# select variables used in analysis for heatmap
cor_vars3 <- c("sales","unemployment","temperature","facebook_newsfeed_spend","facebook_newsfeed_impressions",
               "youtube_brand_spend","youtube_brand_impressions","search_spend","search_clicks","youtube_performance_spend", 
               "youtube_performance_impressions", "newspaper_spend", "newspaper_readership",
               "tv_spend", "tv_gross_rating_points", "inflation_rate", "retail_trade", "gdp", "income")

cor_data3 <- data_quarter[cor_vars3]

# create correlation matrix
# fill all missings with 0 to allow for correlation calculation
cor_data3[is.na(cor_data3) == TRUE] <- 0
cormat3 <- round(cor(cor_data3),2)

# helper function to get upper triangle of the correlation matrix
get_upper_tri3 <- function(cormat3){
  cormat3[lower.tri(cormat3)]<- NA
  return(cormat3)
}

upper_tri3 <- get_upper_tri3(cormat3)

melted_cormat3 <- melt(upper_tri3, na.rm = TRUE)

# label levels of factors to make heatmap nice to read
#levels(melted_cormat$Var1)[levels(melted_cormat$Var1)=="years_since_first_spend"] <- "Years active on platform"
#levels(melted_cormat$Var2)[levels(melted_cormat$Var2)=="years_since_first_spend"] <- "Years active on platform"

cor_heat3 <- ggplot(data = melted_cormat3, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 8, hjust = 1))+
  theme(axis.text.y = element_text(size = ))+
  coord_fixed()

############month##########
data_month = read.csv('data_month.csv')

### correlation heatmap
# select variables used in analysis for heatmap
cor_vars4 <- c("sales","unemployment","temperature","facebook_newsfeed_spend","facebook_newsfeed_impressions",
               "youtube_brand_spend","youtube_brand_impressions","search_spend","search_clicks","youtube_performance_spend", 
               "youtube_performance_impressions", "newspaper_spend", "newspaper_readership",
               "tv_spend", "tv_gross_rating_points", "inflation_rate", "retail_trade", "gdp", "income")

cor_data4 <- data_month[cor_vars4]

# create correlation matrix
# fill all missings with 0 to allow for correlation calculation
cor_data4[is.na(cor_data4) == TRUE] <- 0
cormat4 <- round(cor(cor_data4),2)

# helper function to get upper triangle of the correlation matrix
get_upper_tri4 <- function(cormat4){
  cormat4[lower.tri(cormat4)]<- NA
  return(cormat4)
}

upper_tri4 <- get_upper_tri4(cormat4)

melted_cormat4 <- melt(upper_tri4, na.rm = TRUE)

# label levels of factors to make heatmap nice to read
#levels(melted_cormat$Var1)[levels(melted_cormat$Var1)=="years_since_first_spend"] <- "Years active on platform"
#levels(melted_cormat$Var2)[levels(melted_cormat$Var2)=="years_since_first_spend"] <- "Years active on platform"

cor_heat4 <- ggplot(data = melted_cormat4, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 8, hjust = 1))+
  theme(axis.text.y = element_text(size = ))+
  coord_fixed()


### Figure 2.2: Figure 2.2: Visualization of the number of missing values for impressions variables
data_week <- read.csv('data_week.csv')
data_week[data_week == 0] <- NA
df1 <- data_week[,c(7,9,11,13,15,17)]
#install.packages('tidyverse')
library(tidyverse)
theme_set(theme_bw(base_size=16))

df1 %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col()+
  scale_fill_manual(values=c("skyblue3","gold"))+
  theme(axis.title.y=element_blank())



### Figure 2.3: Total Sales and Spending by Year

###add total spend
data_mmm$total_spend <- data_mmm$facebook_newsfeed_spend
+ data_mmm$youtube_brand_spend
+ data_mmm$search_spend
+ data_mmm$youtube_performance_spend
+ data_mmm$newspaper_spend
+ data_mmm$tv_spend

# spend and sales associations
sales_spend <- ggplot(data_mmm, aes(x=total_spend, y=sales, color=year)) + 
  geom_point()+
  geom_smooth() +
  #geom_text(x = 7.8, y = 9.5, label = lm_eqn(df), parse = TRUE) +
  xlab("Total ad spend") +
  ylab("Total sales") +
  #ylim(0,100000) +
  labs(title = "Sales & Spend by Year") 


### Figure 2.4: The Spendings for Six Channels
library(lubridate)
library(ggplot2)

#preparation
data_month <- read.csv('data_month_1.csv')
data_month$Month<-month(data_month$date)
data_month$Year <- year(data_month$date)
data_month$Month <- paste(data_month$Year,data_month$Month,sep="-")

#plot
plot_month_spend <- ggplot(data_month, aes(x=Month)) +
  geom_line(aes(y = facebook_newsfeed_spend, group=1, colour="Facebook")) +
  geom_line(aes(y = youtube_brand_spend, group=1, colour="Youtube Brand"))+
  geom_line(aes(y = search_spend, group=1, colour="Search"))+
  geom_line(aes(y = youtube_performance_spend, group=1, colour="Youtube Performance"))+
  geom_line(aes(y = newspaper_spend, group=1, colour="Newspaper"))+
  geom_line(aes(y = tv_spend, group=1, colour="TV"))+
  
  ggtitle("The Spendings for Six Channels")+
  xlab("Month from 2019 to 2022")+
  ylab("Spendings for Each Month(Average)")+
  scale_y_continuous(breaks = c(5000,10000,15000, 20000,25000,30000),limits = c(0,30000))+
  theme_classic()+
  theme(
    # Adjust the x-axis label to 90 degrees
    axis.text.x = element_text(angle = 90, hjust = 1),
    # Set blank legend name 
    legend.title=element_blank(),
    # Set the text size of the legend to 10
    legend.text=element_text(size=10),
    # Set the figure title position and its size
    plot.title = element_text(hjust=0.5, size=15),
    axis.text=element_text(size=10),
    # Set the title text size and bold for axes
    axis.title=element_text(size=12,face="bold")
  )


### Figure 2.5: Past spend & Current sales (lagged) Each year
data_mmm <- read.csv("data_mmm.csv", header = TRUE)

### some data formatting
data_mmm$date <- as.Date(data_mmm$date, format='%Y-%m-%d')
data_mmm$year <- substr(data_mmm$date, 1, 4)
data_mmm$year <- as.factor(data_mmm$year)

data_mmm$total_spend <- data_mmm$facebook_newsfeed_spend
+ data_mmm$youtube_brand_spend
+ data_mmm$search_spend
+ data_mmm$youtube_performance_spend
+ data_mmm$newspaper_spend
+ data_mmm$tv_spend

data_mmm_2019 = filter(data_mmm,data_mmm$year==2019)
data_mmm_2020 = filter(data_mmm,data_mmm$year==2020)
data_mmm_2021 = filter(data_mmm,data_mmm$year==2021)
data_mmm_2022 = filter(data_mmm,data_mmm$year==2022)

lagged_corr_alltime_1_40 <- ccf(spend_ts <- ts(data_mmm$total_spend), sales_ts <- ts(data_mmm$sales), lag=40,
                                main = "Lagged correlations between past spend and current sales (All-time)",
                                xlim = c(-40,-1))

lagged_corr_alltime_11_16 <- ccf(spend_ts <- ts(data_mmm$total_spend), sales_ts <- ts(data_mmm$sales), lag=16,
                                 main = "Lagged correlations between past spend and current sales (All-time)",
                                 xlim = c(-16,-11))
#2019
lagged_corr_2019_1_40 <- ccf(spend_ts <- ts(data_mmm_2019$total_spend), sales_ts <- ts(data_mmm_2019$sales), lag=40,
                             main = "Lagged correlations between past spend and current sales (2019)",
                             xlim = c(-40,-1))
lagged_corr_2019_1_5 <- ccf(spend_ts <- ts(data_mmm_2019$total_spend), sales_ts <- ts(data_mmm_2019$sales), lag=5,
                            main = "Lagged correlations between past spend and current sales (2019)",
                            xlim = c(-5,-1))

#2020
lagged_corr_2020_1_40 <- ccf(spend_ts <- ts(data_mmm_2020$total_spend), sales_ts <- ts(data_mmm_2020$sales), lag=40,
                             main = "Lagged correlations between past spend and current sales (2020)",
                             xlim = c(-40,-1))
lagged_corr_2020_10_16 <- ccf(spend_ts <- ts(data_mmm_2020$total_spend), sales_ts <- ts(data_mmm_2020$sales), lag=16,
                              main = "Lagged correlations between past spend and current sales (2020)",
                              xlim = c(-16,-10))

#2021
lagged_corr_2021_1_40 <- ccf(spend_ts <- ts(data_mmm_2021$total_spend), sales_ts <- ts(data_mmm_2021$sales), lag=40,
                             main = "Lagged correlations between past spend and current sales (2021)",
                             xlim = c(-40,-1))
lagged_corr_2021_15_20 <- ccf(spend_ts <- ts(data_mmm_2021$total_spend), sales_ts <- ts(data_mmm_2021$sales), lag=20,
                              main = "Lagged correlations between past spend and current sales (2021)",
                              xlim = c(-20,-15))

#2022
lagged_corr_2022_1_40 <- ccf(spend_ts <- ts(data_mmm_2022$total_spend), sales_ts <- ts(data_mmm_2022$sales), lag=40,
                             main = "Lagged correlations between past spend and current sales (2022)",
                             xlim = c(-40,-1))
lagged_corr_2022_20_26 <- ccf(spend_ts <- ts(data_mmm_2022$total_spend), sales_ts <- ts(data_mmm_2022$sales), lag=26,
                              main = "Lagged correlations between past spend and current sales (2022)",
                              xlim = c(-26,-20))



