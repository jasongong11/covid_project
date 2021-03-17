---
  title: "Statistical Analysis of Temporal and Spatial Trends in US Covid-19 Cases and Deaths"
author: "Jason Gong and Micah Swann"
affiliation: "UC Davis"
---

## REQUIRED PACKAGES
# install.packages("gridExtra")
# packages = c("gtrendsR","tidyverse","usmap")
# install.packages("mapproj")
# importing libraries

## REQUIRED LIBRARIES
library(dplyr)
library(tidyverse)
library(data.table)
library(ggplot2)  
library(scales)
library(gridExtra)
library(lmtest)
library(forecast)


# Introduction

Covid-19 is a novel, highly contagious, acute respiratory virus that was first identified in December 2019 in Wuhan, China. Over the course of the following 14 months, this virus spread rapidly to every corner of the globe, becoming one of the deadliest pandemics in recorded history. In the United States, the first confirmed Covid-19 case was identified in January 2020 and by mid-March there were confirmed cases in every single state and North American territory. In the midst of this rapid pandemic spread, epidemiologists and modelers struggled to accurately forecast the spatial and temporal trends in cases and deaths. However, with regularly updated, publicly-available covid tracking data, a sufficient amount of data now exists to retroactively examine how cases and deaths evolved over the course of this 14 month period. This study utilizes the New York Times Covid Tracking Data to statistically analyze trends in the timeseries of Covid-19 cases and deaths as well as the spatial development of cases at the state level across the united states. Through the us of this timeseries data, an autoregressive integrated moving average (ARIMA) forecast model is employed to predict future cases and deaths at the national level. Furthermore cluster analysis is employed to find clustering trends in cases and deaths at the state level. Demographic data is further incoropated to examine the signficance of different factors in the model development process. 

# Data Description

## Data Sources

## Data Formatting


# Exploratory Data Analysis

## Timeseries Visualization

{r, echo = FALSE}
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

# Use filter() to add a column named new_cases and 'new_deaths'
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
  labs(title="US Covid-19 Cases and Deaths",  y="Daily New Cases", x="")+
  theme(plot.title = element_text(hjust = 0.5))


p2 <- ggplot(us_data, aes(x = date, y = new_deaths)) +
  geom_line(color = "black") +
  labs(  y="Daily New Deaths", x="Date")

grid.arrange(p1,p2,nrow=2)


## Case-Death Scatter Plot

# scatter plot comparison of Cases Vs Deaths
gg <- ggplot(us_data,aes(x = new_cases, y = new_deaths, colour = season)) +
  geom_point() +
  labs(title="Cases Vs Deaths (US-wide)",y="Daily Deaths", x="Daily Cases")+
  theme(plot.title = element_text(hjust = 0.5))

plot(gg)


## State-level case and death boxplots

## Examining State Spatial Trends

# 1 - Formatting Data

# remove leading character in State_Pop$State
state_pop$State <- substring(state_pop$State,2)

# remove commas from population and convert to numeric
state_pop$Population <- as.numeric(gsub(",","",state_pop$Population))


# add state population data
state_data <- merge(state_data, state_pop, by.x="state", by.y="State")


# get population density
# remove DC from state_pop list
state_pop <-state_pop [-c(9), ] 
state.density = state_pop$Population/state.area

## get state regional data
states_info <- data.frame(state.name,state.region,state.density)
names(states_info)[1] <- "Name"
names(states_info)[2] <- "Region"
names(states_info)[3] <- "Density"

# add state region data
state_data <- merge(state_data, states_info, by.x="state", by.y="Name")

# categorize into density groups

for (i in 1:length(state_data$Density))
{
  if (state_data$Density[i] <= 50) {
     state_data$Density2[i] <- "<=50"
  } else if (state_data$Density[i] >  50 & state_data$Density[i] <= 100) {
    state_data$Density2[i] <- "50-100"
  } else if (state_data$Density[i] >  100 & state_data$Density[i] <= 400) {
    state_data$Density2[i] <- "100-400"
  } else {
    state_data$Density2[i] <- ">400"
  }
}
state_data$Density2 <- factor(state_data$Density2,
    levels = c('<=50','50-100','100-400','>400'),ordered = TRUE)

# sort by date
state_data<-state_data[order((state_data$date)),]

# Find unique fips values to find unique counties
fips_vect <- unique(cty_data$fips)

# Find unique dates
dates_vect <- unique(cty_data$date)

# find unique states (Exclude territories)
states_vect <-state.name;

# Find number of daily new cases and deaths by state
i <-1

# looping through unique state names
for (val in states_vect)
{
  # subset by state
  dum <- subset(state_data, state == val)
  
  #Use filter() to add a column named new_cases 
  dum$new_cases = as.numeric(stats::filter(dum$cases,filter=c(1,-1), sides=1))

  # Set Na values to 0. These occur on the first date of obervations
  dum$new_cases[is.na(dum$new_cases)] <- 0
  
  #Use filter() to add a column named new_deaths 
  dum$new_deaths = as.numeric(stats::filter(dum$deaths,filter=c(1,-1), sides=1))
  
    # Set NaN values to 0. These occur on the first date of obervations
  dum$new_deaths[is.na(dum$new_deaths)] <- 0
  
  # append to dataframe
  if (i>1){
    df1 <- rbind(df1,dum)
  }else{
    df1 <- dum
  }
  i <- i+1
  rm(dum)
}
# final new data frame
state_data2 <- df1

#calculate per capita case and death rates
state_data2$pc_new_cases <- state_data2$new_cases / state_data2$Population
state_data2$pc_new_deaths <- state_data2$new_deaths / state_data2$Population

# extracting year and month
month <- data.frame(format(state_data2$date,format="%m/%y"))
# month <- data.frame(format(state_data2$date,format="%m"))

# add as columns to df
# state_data2<-bind_cols(state_data2,year)
state_data2<-bind_cols(state_data2,month)

# sorting columns
state_data3 <- state_data2[,c(1,14,2:13)]

# renaming columns
names(state_data3)[2] <- "month"

# Calculate pc avg cases for each state
state_mnthly_cases = as.data.table(state_data3)[, mean(pc_new_cases), by = .(month,state, Region,Density2)]
names(state_mnthly_cases)[5] <- "avg_pc_cases"
state_mnthly_cases$Density2 = as.factor(state_mnthly_cases$Density2)
names(state_mnthly_cases)[4] <- "Density"

# Calculate avg deaths for each state
state_mnthly_deaths = as.data.table(state_data3)[, mean(pc_new_deaths), by = .(month,state,Region,Density2)]
names(state_mnthly_deaths)[5] <- "avg_pc_deaths"
state_mnthly_deaths$Density2 = as.factor(state_mnthly_deaths$Density2)
names(state_mnthly_deaths)[4] <- "Density"

# Boxplot of Monthly Cumulative Cases by State

# getting dates in order
dum = unique(state_mnthly_cases$month)
state_mnthly_cases$month <- factor(state_mnthly_cases$month , levels=dum)
state_mnthly_deaths$month <- factor(state_mnthly_deaths$month , levels=dum)

# plotting below


# boxplot of cases
mnthly_case_bplot = ggplot(state_mnthly_cases,aes(month, avg_pc_cases)) + 
geom_boxplot() +
  theme(plot.title = element_text(hjust = 0.5))
mnthly_case_bplot + labs(y = "Mean Per Capita Daily Cases by State",x = "Month",
                   title = "Box Plot of Monthly Per Capita Covid-19 Cases by State") 

# boxplot of deaths
mnthly_death_bplot = ggplot(state_mnthly_deaths,aes(month, avg_pc_deaths)) + geom_boxplot() +
  theme(plot.title = element_text(hjust = 0.5))
mnthly_death_bplot + labs(y = "Mean Per Capita Daily Deaths by State",x = "Month",
                   title = "Box Plot of Monthly Per Capita Covid-19 Deaths by State")  




## Regional and Demographic Trends
# Region Map
all_states <- map_data("state")
all_states$region <- str_to_title(all_states$region)  
stateData <- merge(all_states,states_info,by.x="region",by.y = "Name" )

regionplot <- ggplot()+geom_polygon(data=stateData,aes(x=long, y=lat, group = group, fill=Region),color="grey50")+coord_map()
regionplot


# Color by region
mnthly_case_bplot2 = ggplot(state_mnthly_cases,aes(month, avg_pc_cases)) + 
geom_boxplot(aes(color = Region)) 
mnthly_case_bplot2 + labs(y = "Mean Per Capita Daily Cases by Region",x = "Month",
                   title = "Box Plot of Monthly Per Capita Covid-19 Cases by Region")  


mnthly_death_bplot2 = ggplot(state_mnthly_deaths,aes(month, avg_pc_deaths)) + 
geom_boxplot(aes(color = Region)) 
mnthly_death_bplot2 + labs(y = "Mean Daily Per Capita Deaths by Region",x = "Month",
                   title = "Box Plot of Monthly Per Capita Covid-19 Deaths by Region") 


# Color by density
mnthly_case_bplot3 = ggplot(state_mnthly_cases,aes(month, avg_pc_cases)) + 
geom_boxplot(aes(color = Density)) 
mnthly_case_bplot3 + labs(y = "Mean Per Capita Daily Cases by Density",x = "Month",
                   title = "Box Plot of Monthly Per Capita Covid-19 Cases by Density",fill = "Density [pop/mi^2]")  


mnthly_death_bplot3 = ggplot(state_mnthly_deaths,aes(month, avg_pc_deaths)) + 
geom_boxplot(aes(color = Density)) 
mnthly_death_bplot3 + labs(y = "Mean Daily Per Capita Deaths by Density",x = "Month",
                   title = "Box Plot of Monthly Per Capita Covid-19 Deaths by Density",fill = "Density [pop/mi^2]") 




# Algorithms
## ARIMA Timeseries Forecast Model



### ARIMA US Covid Cases Model Development Process

# Timeseries Analysis of Cases

# Converting to timeseries
new_cases.ts <- ts(us_data$new_cases,frequency = 7)
new_deaths.ts <- ts(us_data$new_deaths,frequency = 7)

## Decompose
components.ts = decompose(new_cases.ts)
plot(components.ts,xlab = "Weeks since March 1st, 2020")

# Test for Stationarity
tseries::adf.test(new_cases.ts)

## Remove Non Stationrity through differencing
diff_new_cases.ts <- diff(new_cases.ts)

components.ts2 = decompose(diff_new_cases.ts)
plot(components.ts2,xlab = "Weeks since March 1st, 2020")



# Test for Stationarity with differencing
tseries::adf.test(diff(new_cases.ts))

## Calculate optimal coefficient for model
auto.arima(new_cases.ts, trace=TRUE) 

## Determining AR model
fitARIMA_cases <- arima(new_cases.ts, order=c(0,1,1),seasonal = list(order = c(0,0,2), period = 7),method="ML")



## Check residuals
checkresiduals(fitARIMA_cases)

### ARIMA US Covid Death Model Development Process


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
checkresiduals(fitARIMA_deaths)



## Cluster Analysis

# Results

## US-wide Covid-19 Case ARIMA Forecast Model
## plot forecast
autoplot(forecast(fitARIMA_cases))+
  xlab("Weeks from from March 1st, 2020") + ylab("Daily New Covid Cases in US") +
  ggtitle("AR Lag 7 Model for Covid Cases")


## US-wide Covid-19 Death ARIMA Forecast Model
## plot forecast
autoplot(forecast(fitARIMA_deaths))+
  xlab("Weeks from from March 1st, 2020") + ylab("Daily New Covid Deaths in US") +
  ggtitle("AR Lag 7 Model for Covid Deaths")

# Conclusions

## Temporal Trends

## Spatial Trends