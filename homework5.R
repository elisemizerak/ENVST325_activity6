##### Homework 5 ----

##### Question 1 ----
#Create a regression for water managers

ghg <- read.csv("/cloud/project/activity05/Deemer_GHG_Data.csv")
ghg$log.co2 <- 1/(ghg$co2+1000)
ghg$log.age <- log(ghg$age)
ghg$log.chlorophyll.a <- log(ghg$chlorophyll.a)
ghg$log.DIP <- log(ghg$DIP+1)

unique(ghg$Region)

ghg$TropicalV <- ifelse(ghg$Region == "Tropical", 1, 0)
ghg$HydroV <- ifelse(ghg$hydropower == "yes",1,0)

co2_regression <- lm(log.co2 ~ airTemp+
                       log.age+
                       log.chlorophyll.a+
                       log.DIP+ TropicalV, data=ghg)
summary(co2_regression)

res.co2 <- rstandard(co2_regression)
fit.co2 <- fitted.values(co2_regression)

# check for normality
# qq plot
qqnorm(res.co2, pch=19, col="grey50")
qqline(res.co2)

# checking residuals
plot(fit.co2,res.co2, pch=19, col="grey50")
abline(h=0)

# checking multicollinearity 

# isolate continuous model variables into data frame:

reg.co2 <- data.frame(ghg$airTemp,
                       ghg$log.age, 
                      ghg$log.chlorophyll.a,
                       ghg$log.DIP)

# make a correlation matrix 
chart.Correlation(reg.co2, histogram=TRUE, pch=19)
# run stepwise
full.step.CO2 <- ols_step_forward_aic(co2_regression)
# view table
full.step.CO2 

install.packages("jtools")
library(jtools)
summ(co2_regression)


##### Question 2 ----

ETdat <- read.csv("/cloud/project/activity06/ETdata.csv")
unique(ETdat$crop)
library(lubridate)
library(ggplot2)
library(forecast)
library(dplyr)

# Decompose the evapotranspiration time series for almonds, pistachios, 
# fallow/idle fields, corn, and table grapes

# almonds 
almonds <- ETdat %>%
  filter(crop == "Almonds") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))
ggplot(almonds, aes(x=ymd(date),y=ET.in))+
  geom_point() +
  geom_line() +
  labs(x="year", y="Monthly evapotranspiration (in)")
almonds_ts <- ts(almonds$ET.in,
                 start = c(2016,1),
                 frequency = 12)
almonds_dec <- decompose(almonds_ts)
plot(almonds_dec)

#pistachios
pistachios <- ETdat %>%
  filter(crop == "Pistachios") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))
ggplot(pistachios, aes(x=ymd(date),y=ET.in))+
  geom_point() +
  geom_line() +
  labs(x="year", y="Monthly evapotranspiration (in)")
pistachios_ts <- ts(pistachios$ET.in,
                 start = c(2016,1),
                 frequency = 12)
pistachios_dec <- decompose(pistachios_ts)
plot(pistachios_dec)

# Fallow/Idle Cropland
fallowidlecropland <- ETdat %>%
  filter(crop == "Fallow/Idle Cropland") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))
ggplot(fallowidlecropland, aes(x=ymd(date),y=ET.in))+
  geom_point() +
  geom_line() +
  labs(x="year", y="Monthly evapotranspiration (in)")
fallowidlecropland_ts <- ts(fallowidlecropland$ET.in,
                    start = c(2016,1),
                    frequency = 12)
fallowidlecropland_dec <- decompose(fallowidlecropland_ts)
plot(fallowidlecropland_dec)

# Corn
corn <- ETdat %>%
  filter(crop == "Corn") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))
ggplot(corn, aes(x=ymd(date),y=ET.in))+
  geom_point() +
  geom_line() +
  labs(x="year", y="Monthly evapotranspiration (in)")
corn_ts <- ts(corn$ET.in,
              start = c(2016,1),
              frequency = 12)
corn_dec <- decompose(corn_ts)
plot(corn_dec)

# table grapes
grapes <- ETdat %>%
  filter(crop == "Grapes (Table/Raisin)") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))
ggplot(grapes, aes(x=ymd(date),y=ET.in))+
  geom_point() +
  geom_line() +
  labs(x="year", y="Monthly evapotranspiration (in)")
grapes_ts <- ts(grapes$ET.in,
              start = c(2016,1),
              frequency = 12)
grapes_dec <- decompose(grapes_ts)
plot(grapes_dec)

##### Question 3 -----
# Design an autoregressive model for pistachios, corn, and fallow/idle fields.

#pistachios
acf(na.omit(pistachios_ts), 
    lag.max = 24) 
pacf.plot <- pacf(na.omit(pistachios_ts))

pistachios_y <- na.omit(pistachios_ts)
model1 <- arima(pistachios_y , 
                order = c(1,0,0)) 
model1

model4 <- arima(pistachios_y , 
                order = c(4,0,0))
model4


# calculate fit
AR_fit1 <- pistachios_y - residuals(model1)
AR_fit4 <- pistachios_y - residuals(model4)
#plot data
plot(pistachios_y)
# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")

#forecast future data
newPistachio <- forecast(model4)
newPistachio

#make dataframe for plotting
newPistachioF <- data.frame(newPistachio)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newPistachioF$dateF <- ymd(paste(years,"/",month,"/",1))

#plot of data and predictions
ggplot() +
  geom_line(data = pistachios, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(pistachios$date[1]),newPistachioF$dateF[24])+  
  geom_line(data = newPistachioF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newPistachioF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

#corn
acf(na.omit(corn_ts), 
    lag.max = 24) 
pacf.plot <- pacf(na.omit(corn_ts))

corn_y <- na.omit(corn_ts)
model1 <- arima(corn_y , 
                order = c(1,0,0)) 
model1

model4 <- arima(corn_y , 
                order = c(4,0,0))
model4


# calculate fit
AR_fit1 <- corn_y - residuals(model1)
AR_fit4 <- corn_y - residuals(model4)
#plot data
plot(corn_y)
# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")

#forecast future data
newCorn <- forecast(model4)
newCorn

#make dataframe for plotting
newCornF <- data.frame(newCorn)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newCornF$dateF <- ymd(paste(years,"/",month,"/",1))

#plot of data and predictions
ggplot() +
  geom_line(data = corn, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(corn$date[1]),newCornF$dateF[24])+  
  geom_line(data = newCornF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newCornF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

# fallow/idle fields

acf(na.omit(fallowidlecropland_ts), 
    lag.max = 24) 
pacf.plot <- pacf(na.omit(fallowidlecropland_ts))

fallowidlecropland_y <- na.omit(fallowidlecropland_ts)
model1 <- arima(fallowidlecropland_y , 
                order = c(1,0,0)) 
model1

model4 <- arima(fallowidlecropland_y , 
                order = c(4,0,0))
model4

# calculate fit
AR_fit1 <- fallowidlecropland_y - residuals(model1)
AR_fit4 <- fallowidlecropland_y - residuals(model4)
#plot data
plot(fallowidlecropland_y)
# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")

#forecast future data
newFallow <- forecast(model4)
newFallow

#make dataframe for plotting
newFallowF <- data.frame(newFallow)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newFallowF$dateF <- ymd(paste(years,"/",month,"/",1))

#plot of data and predictions
ggplot() +
  geom_line(data = fallowidlecropland, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(fallowidlecropland$date[1]),newFallowF$dateF[24])+  
  geom_line(data = newFallowF, aes(x = dateF, y = Point.Forecast),
            col="red") +  
  geom_ribbon(data=newFallowF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ 
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")








