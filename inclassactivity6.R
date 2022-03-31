###### In class activity 6

install.packages("lubridate","ggplot2","forecast","dplyr","olsrr","PerformanceAnalytics")
library(lubridate)
library(ggplot2)
library(forecast)
library(dplyr)
library(olsrr)
library(PerformanceAnalytics)

ghg <- read.csv("/cloud/project/activity05/Deemer_GHG_Data.csv")
ETdat <- read.csv("/cloud/project/activity06/ETdata.csv")

unique(ETdat$crop)

ET_all <- ETdat %>%
  group_by(date, crop) %>%
  summarise(ET.in = mean(Ensemble.ET))

Pistachios <- ET_all %>%
  filter(crop == "Pistachios")

Pistachio_ts <- na.omit(ts(Pistachios$ET.in,
                   start = c(2016,1),
                   frequency = 12))

Pistachio_decompose <- decompose(Pistachio_ts)
plot(Pistachio_decompose)
Pistachio_decompose


acf(na.omit(ts(Pistachios$ET.in,
               start = c(2016,1),
               frequency = 12)),
    lag.max = 24)

pacf(na.omit(ts(Pistachios$ET.in,
               start = c(2016,1),
               frequency = 12)),
    lag.max = 24)


model1 <- arima(Pistachio_ts,
                order = c(1,0,0))
model1
