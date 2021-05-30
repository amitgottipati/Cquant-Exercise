##########################################################
############## Cquant Exercise ##########################
##########################################################

## Name: Amit Gottipati

rm(list=ls(all=TRUE))

setwd("C:\\Users\\19726\\Desktop\\cquant_exercise")

library(dplyr)
library(tidyverse)
library(lubridate)
library(janitor)
library(zoo)

df16 <- read_csv("C:\\Users\\19726\\Desktop\\cquant_exercise\\Data\\ERCOT_DA_Prices_2016.csv")
df17 <- read_csv("C:\\Users\\19726\\Desktop\\cquant_exercise\\Data\\ERCOT_DA_Prices_2017.csv")
df18 <- read_csv("C:\\Users\\19726\\Desktop\\cquant_exercise\\Data\\ERCOT_DA_Prices_2018.csv")
df19 <- read_csv("C:\\Users\\19726\\Desktop\\cquant_exercise\\Data\\ERCOT_DA_Prices_2019.csv")

head(df16)
head(df17)
head(df18)
head(df19)

## Date in dttm format year,month,day,time
## SettlementPoint in che, convert to factor
## Price in numeric

########################### Task 1 - Merging all historical data  #######################


df <- bind_rows(df16,df17,df18,df19)

head(df)
tail(df)

summary(df)
str(df)

#df$SettlementPoint =as.factor(df$SettlementPoint)

## no missing values 
sum(is.na(df))

## No duplicates
df %>% get_dupes()

#########################################################################################
################################ Task 2 #################################################
########## Avg Price for each settlement point and year-month ###########################
#########################################################################################

df_avg_ym <- df %>% 
  mutate( SettlementPoint = factor(SettlementPoint),
          Year = year(Date),
          Month = month(Date),
          ) %>% 
  select(SettlementPoint,Year,Month,Price)%>% 
  group_by(SettlementPoint,Year,Month) %>% 
  summarise( AveragePrice = mean(Price))

head(df_avg_ym,20)
tail(df_avg_ym,20)

df_avg_ym %>% get_dupes()

############################## Task 3 ###################################################
#################### Output to csv file #################################################

write.csv(df_avg_ym, "AveragePriceByMonth.csv")


############################## Task 4 ###################################################
############### Price Volatility #########################################################

## hourly price volatility
## std(log(price)) , log > 0, Remive 0, -ve

## Removing Load Zones
df2 <- df[!grepl("LZ_", df$SettlementPoint),]

df2 %>% get_dupes()

df2 %>% distinct()

df_hour_year_volatility <- df2 %>% 
  filter(Price > 0) %>% ## filtering price
  mutate(Year = year(Date),
         Hour = hour(Date)) %>% 
  select(SettlementPoint,Year, Hour, Price) %>%
  group_by(SettlementPoint,Year) %>% 
  summarise(HourlyVolatility = sd(log(Price))) ## volatility

df_hour_year_volatility %>%  get_dupes()

df_hour_year_volatility

##################### Task 5 ##############################################################

write.csv(df_hour_year_volatility, "HourlyVolatilityByYear.csv")

####################### Task 6 ########################################################

df_max_volatility_year <- df_hour_year_volatility %>% 
  filter(HourlyVolatility == max(HourlyVolatility)) %>% 
  arrange(desc(HourlyVolatility))

df_max_volatility_year <- df_hour_year_volatility %>% 
  group_by(Year) %>% 
  filter(HourlyVolatility == max(HourlyVolatility)) %>% 
  arrange(Year)

df_max_volatility_year

## Settlement HB_PAN for year 2019 showed maximum volatility

write.csv(df_max_volatility_year, "MaxVolatilityByYear.csv")

########################### Task 7 #######################################################

## Pseudocode

## X1 - X24 tell us Hourly prices for 3 variables in 3 files
## We need to convert data from long format to wide format
## selecting one station

df_7 <- df %>% 
  mutate(Hour = hour(Date),
        SettlementPoint= factor(SettlementPoint)) %>% 
        filter(SettlementPoint== "HB_BUSAVG") %>% 
        select(Date,Price,Hour)

df_7 %>% 
  pivot_wider(names_from = Hour, values_from = Price)


################## Bonus - Mean Plots #########################################

df_avg_ym

df_LZ <- df_avg_ym[grepl("LZ_", df_avg_ym$SettlementPoint),]
df_LZ

df_HB <- df_avg_ym[grepl("HB_", df_avg_ym$SettlementPoint),]
df_HB

df_HB <- df_HB %>% 
  mutate(Year_Month = as.yearmon(paste(Year, Month), "%Y %m")) %>% 
  select(SettlementPoint,Year_Month,AveragePrice)

df_HB

df_LZ <- df_LZ %>% 
  mutate(Year_Month = as.yearmon(paste(Year, Month), "%Y %m")) %>% 
  select(SettlementPoint,Year_Month,AveragePrice)

df_LZ

p<-ggplot(df_HB, aes(x=Year_Month, y=AveragePrice, group=SettlementPoint)) +
  geom_line(aes(color=SettlementPoint))+
  geom_point(aes(color=SettlementPoint))
p


q<-ggplot(df_LZ, aes(x=Year_Month, y=AveragePrice, group=SettlementPoint)) +
  geom_line(aes(color=SettlementPoint))+
  geom_point(aes(color=SettlementPoint))
q

########################### Bonus Volatility Plots ####################################

df_hour_year_volatility

r<-ggplot(df_hour_year_volatility, aes(x=Year, y=HourlyVolatility, group=SettlementPoint)) +
  geom_line(aes(color=SettlementPoint))+
  geom_point(aes(color=SettlementPoint))
r

########################## Bonus - Open Ended Analysis ###################

######## Univariate Time Series Analysis ############################

library(forecast)
library(MLmetrics)
library(tseries)

dfts = df_avg_ym
dfts

## Analysis for a one hub --> HB_BUSAVG

dfts <- dfts %>% 
  filter(SettlementPoint == "HB_BUSAVG") %>% 
  mutate(Year_Month = as.yearmon(paste(Year, Month), "%Y %m")) %>% 
  select(SettlementPoint,Year_Month,AveragePrice)

dfts

ggplot(dfts, aes(Year_Month, AveragePrice)) + geom_line()  + ylab("Economic Cost") +
  xlab("")

dfts_new = ts(dfts[, c('AveragePrice')], start=c(2016,1), frequency = 12)
dfts_new

dfts_new = tsclean(cost_ec)

ndiffs(dfts_new,test = 'kpss')

dfts_new_diff = diff(dfts_new,differences = 1)

ndiffs(dfts_new_diff,test = 'kpss')

plot(dfts_new_diff)
## There is no Trend in Data
## There is mo Seasonality in Data
## Data follows a Random Walk Process

acf(dfts_new_diff)
pacf(dfts_new_diff)

model =auto.arima(dfts_new_diff,seasonal = TRUE,trace = TRUE)
summary(model)

checkresiduals(model)
## passes Box-ljung test

## We See that auto - arima chooses (0,0,0) model indicating a random walk 
## There is no trend and seasonality to model our data with an ARIMA model
## This method is not suitable for forecasting Average Prices 
## We need to look incorporate variables which help us explain the variation in prices better

## we can look at Dynamic Regression models or Exponential Smoothing




































































