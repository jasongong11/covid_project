## Timeseries Analysis of Covid Data


# packages required
# install.packages("gridExtra")
#packages = c("gtrendsR","tidyverse","usmap")
#install.packages("mapproj")
# install.packages("forecast")
# install.packages("aTSA")
# install.packages("zoo")
# importing libraries
library(dplyr)
library(tidyverse)
library(data.table)
library(ggplot2)  
library(scales)
library(gridExtra)
library(lmtest)
library(forecast)

# library(aTSA)

# read in NYtimes Covid data tables 

# Cumulative Daily Cases and Deaths by 
# 1 - County Level
cty_data <-read.csv("us-counties.csv") 

# 2 - State Level
state_data <-read.csv("us-states.csv")
state_pop <-read.csv("state_pop.csv")

# 3 - 
us_data <-read.csv("us.csv")

# transforming date column from CHARACTER to DATE class
cty_data$date <- as.Date(cty_data$date)
state_data$date <- as.Date(state_data$date,format="%Y-%m-%d")
us_data$date <- as.Date(us_data$date)

# filter out dates before March 1st, 2020
cty_data <- base::subset(cty_data,date>"2020-02-29")
state_data <- base::subset(state_data,date>"2020-02-29")
us_data <- base::subset(us_data,date>"2020-02-29")

# Examination of US-wide Temporal Trends

# Use `filter()` to add a column named `new_cases` and 'new_deaths'
us_data$new_cases = as.numeric(stats::filter(us_data$cases,filter=c(1,-1), sides=1))

us_data$new_deaths = as.numeric(stats::filter(us_data$deaths,filter=c(1,-1), sides=1))

# Change first entry fo new deaths and new cases from NA to 0
us_data$new_cases[1]<-0
us_data$new_deaths[1]<-0

# Add Data on Season
season <- c("Spring","Summer","Winter","Fall")

# create df for seasons
season_df = data.frame(c(1:12),c("Winter","Winter","Spring","Spring","Spring","Summer","Summer","Summer","Fall","Fall","Fall","Winter"))
names(season_df)[1] <- "month"
names(season_df)[2] <- "season"

for (i in 1:length(us_data$date))
{
  if (month(us_data$date[i]) >= 3 & month(us_data$date[i]) < 6) {
    us_data$season[i] = season[1]
  } else if (month(us_data$date[i]) >= 6 & month(us_data$date[i]) < 9) {
    us_data$season[i] = season[2]
  } else if (month(us_data$date[i]) >= 9 & month(us_data$date[i]) < 12) {
    us_data$season[i] = season[3]
  } else {
    us_data$season[i] = season[4]
  }
}
us_data$season = as.factor(us_data$season)

## Plotting Below

# Timeseries of Covid Cases and Deaths through time
# Value used to transform the data
p1 <- ggplot(us_data, aes(x = date, y = new_cases)) +
  geom_line(color = "darkred") +
  labs(title="US Covid-19 Cases and Deaths",  y="Daily New Cases", x="")


p2 <- ggplot(us_data, aes(x = date, y = new_deaths)) +
  geom_line(color = "black") +
  labs(  y="Daily New Deaths", x="Date")

grid.arrange(p1,p2,nrow=2)

############################################################################
# Timeseries Analysis

# Converting to timeseries
new_cases.ts <- ts(us_data$new_cases,frequency = 7)
new_deaths.ts <- ts(us_data$new_deaths,frequency = 7)

## CASES
## plot the ts
autoplot(new_cases.ts)+
  xlab("Weeks from from March 1st, 2020") + ylab("Daily New Covid Cases in US") +
 ggtitle("Timeseries of New Covid 19 Cases in US") 

## Decompose
components.ts = decompose(new_cases.ts)
plot(components.ts,xlab = "Weeks since March 1st, 2020")

# Test for Stationarity
tseries::adf.test(new_cases.ts)


## Remove Non Stationrity through differencing
diff_new_cases.ts <- diff(new_cases.ts)

components.ts2 = decompose(diff_new_cases.ts)
plot(components.ts2,xlab = "Weeks since March 1st, 2020")

# using differencing method
tseries::adf.test(diff(new_cases.ts))

## examine autocorrelation function
new_cases.ts %>% diff() %>% ggtsdisplay(main="")

## Calculate optimal coefficient for model
auto.arima(new_cases.ts, trace=TRUE) 

## Determining AR model
fitARIMA_cases <- arima(new_cases.ts, order=c(0,1,1),seasonal = list(order = c(0,0,2), period = 7),method="ML")

## Check residuals
checkresiduals(fitARIMA_cases)+
  ggtitle("Residual Analysis for Ar Lag 7 Covid Case Model")

## plot forecast
autoplot(forecast(fitARIMA_cases))+
  xlab("Weeks from from March 1st, 2020") + ylab("Daily New Covid Cases in US") +
  ggtitle("AR Lag 7 Model for Covid Cases")


## DEATHS
## plot the ts
autoplot(new_deaths.ts)+
  xlab("Weeks from from March 1st, 2020") + ylab("Daily New Covid Cases in US") +
  ggtitle("Timeseries of New Covid 19 Cases in US") 

## Decompose
components.ts = decompose(new_deaths.ts)
plot(components.ts,xlab = "Weeks since March 1st, 2020")

# Test for Stationarity
tseries::adf.test(new_deaths.ts)


## Remove Non Stationrity through differencing
diff_new_deaths.ts <- diff(new_deaths.ts)

components.ts2 = decompose(diff_new_deaths.ts)
plot(components.ts2,xlab = "Weeks since March 1st, 2020")

# using differencing method
tseries::adf.test(diff(new_deaths.ts))

## examine autocorrelation function
new_deaths.ts %>% diff() %>% ggtsdisplay(main="")

## Calculate optimal coefficient for model
auto.arima(new_deaths.ts, trace=TRUE) 

## Determining AR model
fitARIMA_deaths <- arima(new_deaths.ts, order=c(1,0,2),seasonal = list(order = c(0,1,1), period = 7),method="ML")

## Check residuals
checkresiduals(fitARIMA_deaths)+
  ggtitle("Residual Analysis for Ar Lag 7 Covid Death Model")

## plot forecast
autoplot(forecast(fitARIMA_deaths))+
  xlab("Weeks from from March 1st, 2020") + ylab("Daily New Covid Deaths in US") +
  ggtitle("AR Lag 7 Model for Covid Deaths")