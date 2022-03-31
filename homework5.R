##### Homework 5 ----

##### Question 1 ----
#Create a regression for water managers

ghg <- read.csv("/cloud/project/activity05/Deemer_GHG_Data.csv")
ghg$log.co2 <- 1/(ghg$co2+1000)

ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP)
ghg$log.precipitation <- log(ghg$precipitation)
ghg$log.airTemp <- log(ghg$airTemp)

unique(ghg$Region)

ghg$BorealV <- ifelse(ghg$Region == "Boreal", 1, 0)
ghg$TropicalV <- ifelse(ghg$Region == "Tropical", 1, 0)

co2_regression <- lm(log.co2 ~ log.airTemp+
                       log.age+
                       log.DIP+
                       log.precipitation+
                       TropicalV, data = ghg)
summary(co2_regression)
str(co2_regression)
regTable <- summary(co2_regression)$coefficients
write.csv()