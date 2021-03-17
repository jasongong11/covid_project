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
library(dtwclust)
library(ggpubr)
library(gplots)
library(RColorBrewer)
library(nnet)
library(summarytools)
library(lmtest)


# Introduction

# Covid-19 is a novel, highly contagious, acute respiratory virus that was first identified in December 2019 in Wuhan, China. Over the course of the following 14 months, this virus spread rapidly to every corner of the globe, becoming one of the deadliest pandemics in recorded history. In the United States, the first confirmed Covid-19 case was identified in January 2020 and by mid-March there were confirmed cases in every single state and North American territory. In the midst of this rapid pandemic spread, epidemiologists and modelers struggled to accurately forecast the spatial and temporal trends in cases and deaths. However, with regularly updated, publicly-available covid tracking data, a sufficient amount of data now exists to retroactively examine how cases and deaths evolved over the course of this 14 month period. This study utilizes the New York Times Covid Tracking Data to statistically analyze trends in the timeseries of Covid-19 cases and deaths as well as the spatial development of cases at the state level across the united states. Through the us of this timeseries data, an autoregressive integrated moving average (ARIMA) forecast model is employed to predict future cases and deaths at the national level. Furthermore cluster analysis is employed to find clustering trends in cases and deaths at the state level. Demographic data is further incoropated to examine the signficance of different factors in the model development process. 

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

#========= Spatial Trends =====

# build time series matrix
state_data2 <- state_data2 %>%
  mutate(avg_new_cases = stats::filter(new_cases, filter=rep(1/7,7), side=1),
         avg_new_deaths = stats::filter(new_deaths, filter=rep(1/7,7), side=1))


ts_avg_case_matrix <- matrix( nrow = length(states_vect), ncol = length(dates_vect))
ts_avg_death_matrix <- matrix( nrow = length(states_vect), ncol = length(dates_vect))

for (i in 1:length(states_vect)) {
  for (j in 1:length(dates_vect)) {
    tryCatch({ts_avg_case_matrix[i,j] <- state_data2$avg_new_cases[(state_data2$state == states_vect[i] 
                                                                    & state_data2$date == dates_vect[j])]},
             error=function(e){ts_avg_case_matrix[i,j] <- NA})
    tryCatch({ts_avg_death_matrix[i,j] <- state_data2$avg_new_deaths[(state_data2$state == states_vect[i] &
                                                                        state_data2$date == dates_vect[j])]},
             error=function(e){ts_avg_death_matrix[i,j] <- NA})
  }
}

rownames(ts_avg_case_matrix) <- states_vect
rownames(ts_avg_death_matrix) <- states_vect
colnames(ts_avg_case_matrix) <- dates_vect
colnames(ts_avg_death_matrix) <- dates_vect

ts_avg_case_matrix <- ts_avg_case_matrix[ , colSums(is.na(ts_avg_case_matrix)) == 0]
ts_avg_death_matrix <- ts_avg_death_matrix[ , colSums(is.na(ts_avg_death_matrix)) == 0]

#=========plot the time series data======
cl <- rainbow(50)

par(mfrow=c(1,2)) 
plot(0,0,xlim = c(0,330),ylim = c(0,10),type = "n", main= 'Cases time series data for each state', xlab='Time', ylab='Log num of cases')
for (i in 1:50){
  lines(log(ts_avg_case_matrix[i,]),col = cl[i], type = 'l')
}

plot(0,0,xlim = c(0,330),ylim = c(0,8),type = "n", main='Deaths time series data for each state', xlab='Time', ylab='Log num of deaths')
for (i in 1:50){
  lines(log(ts_avg_death_matrix[i,]),col = cl[i], type = 'l')
}

#=====SBD distance and correaltion with geo distance====
suppressPackageStartupMessages(library(dtw))

# calculate the SBD distance between time series
dtw_distmat_case <- proxy::dist(ts_avg_case_matrix, method = "SBD", 
                                upper = TRUE, diag = TRUE)
dtw_distmat_death <- proxy::dist(ts_avg_death_matrix, method = "SBD", 
                                 upper = TRUE, diag = TRUE)

# manage the dataset
dtw_distmat_case <- as.matrix(dtw_distmat_case)
dtw_distmat_death <- as.matrix(dtw_distmat_death)
us_distance_matrix <- as.matrix(dist(cbind(state.center$x, state.center$y)))
rownames(us_distance_matrix) <- rownames(dtw_distmat_death)
colnames(us_distance_matrix) <- colnames(dtw_distmat_death)
df <- data.frame(case=dtw_distmat_case[lower.tri(dtw_distmat_case, diag=FALSE)],
                 death=dtw_distmat_death[lower.tri(dtw_distmat_death, diag=FALSE)],
                 geo_dis=us_distance_matrix[lower.tri(us_distance_matrix)])

# plot the heatmap for the distances matrix
heatmap(dtw_distmat_case, Colv = NA, Rowv = NA, symm =T,  cex.main=0.8, margins = c(3, 3), 
        main = 'K-shape distance cases', cexCol = 0.5, cexRow = 0.5)
heatmap(dtw_distmat_death, Colv = NA, Rowv = NA, symm =T, margins = c(3, 3), 
        main = 'K-shape distance deaths', cexCol = 0.5, cexRow = 0.5)
heatmap(us_distance_matrix, Colv = NA, Rowv = NA, symm =T, margins = c(3, 3), 
        main = 'Euclidean Geo distance', cexCol = 0.5, cexRow = 0.5)

# correlation testing
cor.test(df$case, df$geo_dis, method = "pearson")
cor.test(df$death, df$geo_dis, method = "pearson")

ggscatter(df, x = "case", y = "geo_dis", size = 0.8,
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "geo distance", ylab = "case K-shape distance")

ggscatter(df, x = "death", y = "geo_dis", size = 0.8,
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "geo distance", ylab = "death K-shape distance")

#====Download the US demographical data====
library(choroplethr)
library(acs)
api.key.install('a094e7b6969767fb50b6af3cd246ff66d0eec50a')
us_data <- get_state_demographics(endyear = 2013, span = 5)
us_data_2020 <- get_state_demographics(endyear = 2019, span = 5)

#=======Determine the size of K for clustering====
print("Partition clustering for cases")
pc_k_case <- tsclust(ts_avg_case_matrix, k = 5L:9L,
                     distance = "dtw_basic", centroid = "dba",norm = "L2",
                     preproc = zscore,norm = "L2",
                     window.size = 20L)

names(pc_k_case) <- paste0("k_", 5L:9L)
sapply(as.list(pc_k_case), cvi, type = "internal")

print("Partition clustering for deaths")
pc_k_death <- tsclust(ts_avg_death_matrix, k = 5L:9L,
                      distance = "dtw_basic", centroid = "dba",norm = "L2",
                      preproc = zscore,norm = "L2",
                      window.size = 20L)

names(pc_k_death) <- paste0("k_", 5L:9L)
sapply(as.list(pc_k_death), cvi, type = "internal")

#======Apply the clustering analysis for partition and hierarchical clustering====

# Partition clustering
pc_case <- tsclust(ts_avg_case_matrix, type = "partitional", k = 6L, preproc = zscore,norm = "L2",
                    distance = "dtw_basic", centroid = "dba",
                    trace = TRUE, seed = 8,
                    norm = "L2", window.size = 20L,
                    args = tsclust_args(cent = list(trace = TRUE)))
case_cluster <- pc_case@cluster
case_cluster_list <- as.list(setNames(case_cluster, tolower(states_vect)))

pc_death <- tsclust(ts_avg_death_matrix, type = "partitional", k = 6L, preproc = zscore,norm = "L2",
                    distance = "dtw_basic", centroid = "dba",
                    trace = TRUE, seed = 8,
                    norm = "L2", window.size = 20L,
                    args = tsclust_args(cent = list(trace = TRUE)))
death_cluster <- pc_death@cluster
death_cluster_list <- as.list(setNames(death_cluster, tolower(states_vect)))

# hierarchical clustering
hc_case <- tsclust(ts_avg_case_matrix, type = "hierarchical", k =7, 
                   distance = "dtw_basic", trace = TRUE, preproc = zscore,norm = "L2",
                   control = hierarchical_control(method = "average"))

hc_death <- tsclust(ts_avg_death_matrix, type = "hierarchical", k = 7, 
                    distance = "sbd", trace = TRUE, preproc = zscore,norm = "L2",
                    control = hierarchical_control(method = "average"))

# plot the partition clustering
pc_case_plot <- plot(pc_case)
pc_death_plot <- plot(pc_death)
hc_case_plot <- plot(hc_case)
hc_death_plot <- plot(hc_death)
grid.arrange(pc_case_plot,pc_death_plot,nrow=2)
# plot the hierarchical clustering
par(mfrow=c(1,2)) 
plot(hc_case, cex=0.5)
xlab('')
plot(hc_death, cex=0.5)

#=====Ploting the centroid time series for each clusters=====
centroids_plots_case <- vector('list', 6)
for (i in 1:6) {
  temp <- pc_case@centroids[[i]]
  temp_df <- data.frame(t=1:length(temp), cases=temp)
  centroids_plots_case[[i]] <- ggplot(temp_df, aes(x=t, y=cases)) +  
    geom_line(linetype = "solid", color = brewer.pal(n = 7, name = "Set2")[i+1], size=1) +
    ggtitle(paste("Case Cluster", i))
}

centroids_plots_death <- vector('list', 6)
for (i in 1:6) {
  temp <- pc_death@centroids[[i]]
  temp_df <- data.frame(t=1:length(temp), death=temp)
  centroids_plots_death[[i]] <- ggplot(temp_df, aes(x=t, y=death)) +  
    geom_line(linetype = "solid", color = brewer.pal(n = 7, name = "Set2")[i+1], size=1)+
    ggtitle(paste("Death Cluster", i))
}
grid.arrange(grobs = c(centroids_plots_case, centroids_plots_death), ncol = 3)

#====Plot the cluster on US map====
# manage the US map dataset
state <- map_data("state")
setdiff(unique(state$region), names(death_cluster_list))

death_cluster_list['district of columbia'] = 0
case_cluster_list['district of columbia'] = 0

state$case_cluster <- unlist(case_cluster_list[state$region], use.names=FALSE)
state$death_cluster <- unlist(death_cluster_list[state$region], use.names=FALSE)

state$case_cluster <- as.character(state$case_cluster)
state$death_cluster <- as.character(state$death_cluster)

# plot the cluster on US map
us_cluster_case <- ggplot(data=state, aes(x=long, y=lat, fill=case_cluster, group=group)) + 
  geom_polygon(color = "white") + 
  guides(fill=FALSE) + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  scale_fill_manual(values=brewer.pal(n = 7, name = "Set2")) + 
  ggtitle('Clustering of new cases US states') + 
  coord_fixed(1.3)


us_cluster_death <- ggplot(data=state, aes(x=long, y=lat, fill=death_cluster, group=group)) + 
  geom_polygon(color = "white") + 
  guides(fill=FALSE) + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  scale_fill_manual(values=brewer.pal(n = 7, name = "Set2")) + 
  ggtitle('Clustering of new deaths US states') + 
  coord_fixed(1.3)

grid.arrange(us_cluster_case, us_cluster_death, ncol = 2)

#====Multinomial logistic regression model to predict cluteres====
# manage the dataset
prediction_clusters_case <- unlist(case_cluster_list[us_data_2020$region], use.names=FALSE)
prediction_clusters_death <- unlist(death_cluster_list[us_data_2020$region], use.names=FALSE)

# the party affliation for each state, 0 rep, 1 dem
vote <- c(0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0,0,0,0,0, 1, 1,1,1,1, 0,0,0,0, 1,1,1,1,1, 0,0,0,0, 1,1,1, 0,0,0,0,0, 1,1,1,0,1,0)

us_data_2020$cluster_case <- prediction_clusters_case
us_data_2020$cluster_death <- prediction_clusters_death
us_data_2020_subset <- subset(us_data_2020, region %in% tolower(states_vect))
us_data_2020_subset$vote <- vote

us_data_2020_subset$cluster_case <- as.factor(us_data_2020_subset$cluster_case)
us_data_2020_subset$cluster_death <- as.factor(us_data_2020_subset$cluster_death)
us_data_2020_subset$vote <- as.factor(us_data_2020_subset$vote)

# standardize the predictors
us_data_2020_subset$total_population_scale <- scale(us_data_2020_subset$total_population)
us_data_2020_subset$percent_white_scale <- scale(us_data_2020_subset$percent_white)
us_data_2020_subset$per_capita_income_scale <- scale(us_data_2020_subset$per_capita_income)
us_data_2020_subset$median_rent_scale <- scale(us_data_2020_subset$median_rent)
us_data_2020_subset$median_age_scale <- scale(us_data_2020_subset$median_age)
us_data_2020_subset$density <- scale(state.density)

# apply the multinomial model fitting
multi_mo_case <- multinom(cluster_case ~ total_population_scale + percent_white_scale +
                            per_capita_income_scale + vote + density + 
                            median_age_scale, data = us_data_2020_subset, model=TRUE)
multi_mo_death<- multinom(cluster_death ~ total_population_scale + percent_white_scale +
                            per_capita_income_scale + vote + density + 
                            median_age_scale, data = us_data_2020_subset, model=TRUE)

#====Inference testing for the multinomial model====
# chi square testing
print('Chi square testing for the full model, case')
chisq.test(us_data_2020_subset$cluster_case,predict(multi_mo_case))
print('Chi square testing for the full model, death')
chisq.test(us_data_2020_subset$cluster_death,predict(multi_mo_death))

# repeated Likelihood Ratio Test
result1 <- lrtest(multi_mo_case, "total_population_scale")
result2 <- lrtest(multi_mo_case, "percent_white_scale")
result3 <- lrtest(multi_mo_case, "median_age_scale")
result4 <- lrtest(multi_mo_case, "per_capita_income_scale")
result5 <- lrtest(multi_mo_case, "vote")
result6 <- lrtest(multi_mo_case, "density")

result01 <- lrtest(multi_mo_death, "total_population_scale")
result02 <- lrtest(multi_mo_death, "percent_white_scale")
result03 <- lrtest(multi_mo_death, "median_age_scale")
result04 <- lrtest(multi_mo_death, "per_capita_income_scale")
result05 <- lrtest(multi_mo_death, "vote")
result06 <- lrtest(multi_mo_death, "density")

# prediction tatble
ctable <- table(us_data_2020_subset$cluster_case,predict(multi_mo_case))
ctable

ctable <- table(us_data_2020_subset$cluster_case,predict(multi_mo_case))
ctable
