setwd("C:/Users/User10/Desktop/CA2/Datasets")
library(tidyverse)
library(readxl)
library(forecast)
library(fpp2)
library(dplyr)
library(ggplot2)
library(readr)

# Overall LFPR
data <- read_excel("overall.xlsx", sheet = "Malaysia", trim_ws = TRUE)
years <- data$Years
overall_lfpr <- data$`Labour force participation rate`

# Linear interpolation.
data_approx <- approx(years, overall_lfpr, n = 40)
data_approx

# Create time series data from 1982 to 2021.
library(TTR)
overall_lfpr_ts <- ts(data_approx$y, start = c(1982))
overall_lfpr_ts

### training set
### use data from 2016 to 2021 for forecasting

train <- window(overall_lfpr_ts, start=1982, end=c(2015,1))

### test set
### use remaining data from 1957 to 1960 to test accuracy

test <- window(overall_lfpr_ts, start=2016, end=c(2021,1))
accuracy(ses(train, h = 6), test)

# Build the ses model
se_model <- ses(overall_lfpr_ts, h = 5)
summary(se_model)

data_approx$simpleexp
checkresiduals(se_model)
autoplot(overall_lfpr_ts, xlab = "Years", ylab = "Overall LFPR in Malaysia (%)", main = "Time Series Plot of Overall LFPR in Malaysia from 1982 to 2021")
autoplot(se_model, xlab = "Years", ylab = "Overall LFPR in Malaysia (%)", main = "Forecasts from SES on Overall LFPR in Malaysia from 1982 to 2021")

# ---------------------------------------------------------------------- #

# Male LFPR
data2 <- read_excel("overall.xlsx", sheet = "Male", trim_ws = TRUE)
years <- data$Years
male_lfpr <- data2$`Labour force participation rate`

# Linear interpolation.
data_approx2 <- approx(years, male_lfpr, n = 40)
data_approx2

# Create time series data from 1982 to 2021.
library(TTR)
male_lfpr_ts <- ts(data_approx2$y, start = c(1982))
male_lfpr_ts

### use data from 2016 to 2021 for forecasting

train2 = window(male_lfpr_ts, start=1982, end=c(2015,1))

### test set
### use remaining data from 1957 to 1960 to test accuracy

test2 = window(male_lfpr_ts, start=2016, end=c(2021,1))
accuracy(ses(train2, h = 6), test2)

# Build the ses model
se_model2 <- ses(male_lfpr_ts, h = 5)
summary(se_model2)

data_approx2$simpleexp
checkresiduals(se_model2)
autoplot(male_lfpr_ts, xlab = "Years", ylab = "Male LFPR in Malaysia (%)", main = "Time Series Plot of Male LFPR in Malaysia from 1982 to 2021")
autoplot(se_model2, xlab = "Years", ylab = "Male LFPR in Malaysia (%)", main = "Forecasts from SES on Male LFPR in Malaysia from 1982 to 2021")

# ---------------------------------------------------------------------- #

# Female LFPR
data3 <- read_excel("overall.xlsx", sheet = "Female", trim_ws = TRUE)
years <- data$Years
female_lfpr <- data3$`Labour force participation rate`

# Linear interpolation.
data_approx3 <- approx(years, female_lfpr, n = 40)
data_approx3

# Create time series data from 1982 to 2021.
library(TTR)
female_lfpr_ts <- ts(data_approx3$y, start = c(1982))
female_lfpr_ts

### use data from 2016 to 2021 for forecasting

train3 = window(female_lfpr_ts, start=1982, end=c(2015,1))

### test set
### use remaining data from 1957 to 1960 to test accuracy

test3 = window(female_lfpr_ts, start=2016, end=c(2021,1))
accuracy(ses(train3, h = 6), test3)

# Build the ses model
se_model3 <- ses(female_lfpr_ts, h = 5)
summary(se_model3)

data_approx3$simpleexp
checkresiduals(se_model3)
autoplot(female_lfpr_ts, xlab = "Years", ylab = "Female LFPR in Malaysia (%)", main = "Time Series Plot of Female LFPR in Malaysia from 1982 to 2021")
autoplot(se_model3, xlab = "Years", ylab = "Female LFPR in Malaysia (%)", main = "Forecasts from SES on Female LFPR in Malaysia from 1982 to 2021")