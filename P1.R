## PART1 - EUR time series plot ##
rate0 <- read.csv(file.choose())
attach(rate0)

colnames(rate0) <- c("Date", "EUR", "HKD")     # change columns' names
rate <- rate0[-c(1:5),]     # remove first five rows of description
rate <- rate[rate[,2]!="ND",]     # remove NA data and create a new data set

eur <- rate[, c("Date", "EUR")]     # extract data for EURO
i <- rownames(eur)     # add an index column
eur <- cbind(i=i, eur)     # merge to a new data set
View(eur)

head(eur)
library(tseries)     # for time series analysis
library(dplyr)     # for manipulating data.frames
ts_eur <-  ts(eur$EUR, start = 2000, frequency = 365)     # create a time series object
plot.ts(ts_eur)     # time series plot

library(date)     # for handling dates
library(lubridate)     # for working with dates
library(scales)     # to access breaks/formatting functions
library(ggplot2)     # for creating graphs
library(ggthemes)     #for changing themes
eur$Date <- as.Date(eur$Date)     #convert data class

ggplot(eur, aes(x = 1 , y = Date)) + geom_boxplot() + xlab("Currency Rate") + ylab("Date")    # check outliers

EURplot1 <- ggplot(eur, aes(eur$Date, EUR)) + geom_point(na.rm=TRUE, color="purple", size=1) + scale_x_date(date_labels = "%b-%y") + ggtitle("EUR per USD\n 2000-2015") + xlab("Date") + ylab("Currency Rate")    # use ggplot2 to plot EUR rate
EURplot1 <- EURplot1 + theme_stata() +  stat_smooth(color = "green")    #add a theme and a trend
EURplot1

eur$year <- year(eur$Date)     # create year variables
eur$month <- month(eur$Date, label = TRUE)     # create month variables
eur <- eur[which(eur$year >= 2000), ]     # subset the data
EURplot2 <- ggplot(eur, aes(x = month, y = EUR))     # build the foundation of the plot
EURplot2 <- EURplot2 + geom_line(aes(color = factor(year), group = year)) +  scale_color_discrete(name = "Year") + labs(title = "EUR per USD\n 2000-2015", x = "Month", y = "Currency Rate")     # add lines color coded and grouped by year, name the legend "Year", format the y-axis, and add a title and labels
EURplot2


## PART2 - HKD time series plot ##
hkd <- rate[, c("Date", "HKD")]     # extract data for HKD
j <- rownames(hkd)     # add an index column
hkd <- cbind(j=j, hkd)     # merge to a new data set
head(hkd)
hkd$Date <- as.Date(hkd$Date)     #convert data class

ts_hkd <-  ts(hkd$HKD, start = 2000, frequency = 365)     # create a time series object
plot.ts(ts_hkd)     # time series plot

ggplot(hkd, aes(x = 1 , y = Date)) + geom_boxplot() + xlab("Currency Rate") + ylab("Date")     # check outliers

HKDplot1 <- ggplot(hkd, aes(hkd$Date, HKD)) + geom_point(na.rm=TRUE, color="blue", size=1) + scale_x_date(date_labels = "%b-%y") + ggtitle("HKD per USD\n 2000-2015") + xlab("Date") + ylab("Currency Rate")    # time series plot for HKD rate
HKDplot1 <- HKDplot1 + theme_stata() + stat_smooth(color = "red")     #add a theme and a trend
HKDplot1

hkd$year <- year(hkd$Date)     # create year variables
hkd$month <- month(hkd$Date, label = TRUE)     # create month variables
hkd <- hkd[which(hkd$year >= 2000), ]     # subset the data
HKDplot2 <- ggplot(hkd, aes(x = month, y = HKD))     # build the foundation of the plot
HKDplot2 <- HKDplot2 + geom_line(aes(color = factor(year), group = year)) +  scale_color_discrete(name = "Year") + labs(title = "HKD per USD\n 2000-2015", x = "Month", y = "Currency Rate")     # add lines color coded and grouped by year, name the legend "Year", format the y-axis, and add a title and labels
HKDplot2


## PART3 - forcast currency rate for next ten years ##
library(zoo)     #for handling regular and irregular time series
library(forecast)     #for forecasting functions for time series
eur_tbats = tbats(ts_eur)     # detect daily patterns in time series data
eur_fc = forecast(eur_tbats, h = 120)     # forecast EUR rate for next 120 months=10 years
plot(eur_fc)

hkd_tbats = tbats(ts_hkd)     # detect daily patterns in time series data
hkd_fc = forecast(hkd_tbats, h = 120)     # forecast HKD rate for next 120 months=10 years
plot(hkd_fc)


## PART4 -  find a relation between EUR and HKD ##
rate$EUR <- as.numeric(as.character(rate$EUR))     # covert data to numeric value
rate$HKD <- as.numeric(as.character(rate$HKD))
cor.test(rate$EUR, rate$HKD)     # test the correlation between EUR and HKD
m1 <- lm(rate$EUR ~ rate$HKD)     # build a linear regression model
summary(m1)     #  produce summaries of the results of the model 
rate$Date <- as.Date(rate$Date)     # cover data class
ggplot(rate, aes(rate$HKD, rate$EUR)) + geom_line() + theme_economist() +  ggtitle("Linear Regression Model \n lm(EUR ~ HKD)") + xlab("HKD") + ylab("EUR")     # use ggplot2 to plot the model


## PART5 - find a correlation between EUR and Ti ##
dim(rate0)     # check the number of entires in each column
eur$i <- as.numeric(i)     # covert data class 
Ti = eur$i / 4180     #define Ti
eur <- cbind(eur, Ti)     # merge Ti to eur

eur$EUR <- as.numeric(as.character(eur$EUR))     #covert data to numeric values
cor.test(eur$EUR, Ti)     #test correlation between EUR and Ti
m2 <- lm(eur$EUR ~ Ti)     #build a linear regression model
summary(m2)
ggplot(eur, aes(Ti, eur$EUR)) + geom_line() + theme_economist() +  ggtitle("Linear Regression Model \n lm(EUR ~ Ti)") + xlab("Time") + ylab("EUR Rate") 


## PART6 - find a correlation between HKD and Tj ##
hkd$j <- as.numeric(j)     # covert data class 
Tj = hkd$j / 4180     #define Tj
hkd <- cbind(hkd, Tj)     # merge Tj to hkd

hkd$HKD <- as.numeric(as.character(hkd$HKD))     #covert data to numeric values
cor.test(hkd$HKD, Tj)     #test correlation between HKD and Tj
m3 <- lm(hkd$HKD ~ Tj)     #build a linear model
summary(m3)     #produce result summaries of model fitting function
ggplot(hkd, aes(Tj, hkd$HKD)) + geom_line() + theme_economist() +  ggtitle("Linear Regression Model \n lm(HKD ~ Tj)") + xlab("Time") + ylab("HKD Rate")


## PAR7 - update final data set ##
rate <- cbind(rate, eur$month, eur$year, Ti, Tj)
