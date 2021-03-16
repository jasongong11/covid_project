## Timeseries Analysis of Covid Data


# packages required
# install.packages("gridExtra")
#packages = c("gtrendsR","tidyverse","usmap")
#install.packages("mapproj")
# install.packages("forecast")
# install.packages("aTSA")
# importing libraries
library(dplyr)
library(tidyverse)
library(data.table)
library(ggplot2)  
library(scales)
library(gridExtra)
# library(forecast)
library(aTSA)

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


## Time Series Analhysis


# Converting to timeseries
new_cases.ts <- ts(us_data$new_cases, frequency=7)
new_deaths.ts <- ts(us_data$new_deaths, frequency=7)

## plot the ts
plot.ts(new_cases.ts, ylab = expression(paste("New Cases")))


## weights for moving avg
fltr <- c(1/2, rep(1, times = 7), 1/2)/12

## estimate of trend
cases_trend <- stats::filter(new_cases.ts, filter = fltr, method = "convo", 
                             sides = 2)
## plot the trend
plot.ts(cases_trend, ylab = "Trend", cex = 1)

## oscillatory effect over time
cases_seas <- new_cases.ts - cases_trend

## plot the weekly trends
plot.ts(cases_seas, ylab = "Weekly effect", xlab = "Week", cex = 1)

# decomposition of cases
new_cases_decomp <- decompose(new_cases.ts)
Y <- msts(new_cases.ts, seasonal.periods=c(7,365,25))

## plot the obs ts, trend & seasonal effect
plot(new_cases_decomp, yax.flip = TRUE)

# plot autocorrelatiuon function
acf(new_cases.ts, lag.max = 90)

#Fitting the AR Model to the time series
AR_cases <- arima(new_cases.ts, order = c(1,0,0))
print(AR_cases)

#plotting the series along with the fitted values
ts.plot(new_cases.ts)
AR_cases_fit <- new_cases.ts - residuals(AR_cases)
points(AR_cases_fit, type = "l", col = 2, lty = 2)


#Using predict() to make a 1-step forecast
predict_AR_cases <- predict(AR_cases)

#plotting the AirPassenger series plus the forecast and 95% prediction intervals
ts.plot(new_cases.ts,xlab = "Week from March 1st, 2020",
        ylab = "New Covid-19 Cases in US",xlim = c(30,55))
title("AR Model for US Covid-19 Cases")
AR_cases_forecast <- predict(AR_cases, n.ahead = 30)$pred
AR_cases_forecast_se <- predict(AR_cases, n.ahead = 30)$se
points(AR_cases_forecast, type = "l", col = 2)
points(AR_cases_forecast - 2*AR_cases_forecast_se, type = "l", col = 2, lty = 2)
points(AR_cases_forecast + 2*AR_cases_forecast_se, type = "l", col = 2, lty = 2)


## Plotting with MA model
MA_cases <- arima(new_cases.ts, order = c(0,0,1))
print(MA)

#plotting the series along with the MA fitted values
ts.plot(new_cases.ts)
MA_cases_fit <- new_cases.ts - resid(MA_cases)
points(MA_cases_fit, type = "l", col = 2, lty = 2)

# Predict wiht MA
#Plotting the AIrPAssenger series plus the forecast and 95% prediction intervals
ts.plot(new_cases.ts, xlim = c(30, 55),xlab = "Week from March 1st, 2020")
MA_cases_forecasts <- predict(MA_cases, n.ahead = 30)$pred
MA_cases_forecast_se <- predict(MA_cases, n.ahead = 30)$se
points(MA_cases_forecasts, type = "l", col = 2)
points(MA_cases_forecasts - 2*MA_cases_forecast_se, type = "l", col = 2, lty = 2)
points(MA_cases_forecasts + 2*MA_cases_forecast_se, type = "l", col = 2, lty = 2)

# Choose Model
# Find correlation between AR_fit and MA_fit
cor(AR_cases_fit, MA_cases_fit)

# Find AIC of MA
AIC(MA_cases)

# Find BIC of AR
AIC(AR_cases)

tseries::adf.test(diff(log(new_cases.ts)), alternative="stationary", k=0)