####EDA#######
library(lubridate)
library(ggplot2)

#preparation
data_month <- read.csv('data_month.csv')
data_month$Month<-month(data_month$date)
data_month$Year <- year(data_month$date)
data_month$Month <- paste(data_month$Year,data_month$Month,sep="-")
data_month$Month <- as.factor(data_month$Month)
data_month$Month<- factor(data_month$Month,levels=data_month$Month)

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


###find missing value
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



df2 <- data_week[,c(6,8,10,12,14,16)]
df2 %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col()+
  scale_fill_manual(values=c("skyblue3","red"))+
  theme(axis.title.y=element_blank())

#install.packages('naniar')
is.na(data_week$youtube_performance_impressions)
library(naniar)

vis_miss(data_month)


