# clear environment - rm (list = ls())
# clear console - ctrl l
# change font - Tools->Global Options->Appearance
# install.packages("lubridate")
# install.packages("dplyr)
# install.packages("tibble")
# install.packages("ggplot2")
# 1 - homicide, 2 - rape, 3 - robbery, 4 - assault, 5 - burglary, 6 - larceny
# 7 - auto-theft, 8 - arson, 9 - shooting

library(dplyr)
library(lubridate)
library(tibble)
library(ggplot2)

setwd("C:/Jonathan's School Work/college/2/HONR238V/crime-in-baltimore")

baltCrime <- read.csv("BPD_Part_1_Victim_Based_Crime_Data.csv")
as_tibble(baltCrime)
salaries2012 <- read.csv("Baltimore_City_Employee_Salaries_FY2012.csv")
salaries2013 <- read.csv("Baltimore_City_Employee_Salaries_FY2013.csv")
salaries2014 <- read.csv("Baltimore_City_Employee_Salaries_FY2014.csv")
salaries2015 <- read.csv("Baltimore_City_Employee_Salaries_FY2015.csv")
salaries2016 <- read.csv("Baltimore_City_Employee_Salaries_FY2016.csv")
salaries2017 <- read.csv("Baltimore_City_Employee_Salaries_FY2017.csv")
salaries2012 <- select(salaries2012, c(AnnualSalary))
salaries2013 <- select(salaries2013, c(ANNUAL_RT))
salaries2014 <- select(salaries2014, c(AnnualSalary))
salaries2015 <- select(salaries2015, c(AnnualSalary))
salaries2016 <- select(salaries2016, c(AnnualSalary))
salaries2017 <- select(salaries2017, c(ANNUAL_RT))
as_tibble(salaries2013)
as_tibble(salaries2017)
avg2012 <- mean(salaries2012$AnnualSalary)
avg2012 <- round(avg2012, digits = 2)
avg2013 <- mean(salaries2013$ANNUAL_RT)
avg2013 <- round(avg2013, digits = 2)
avg2014 <- mean(salaries2014$AnnualSalary)
avg2014 <- round(avg2014, digits = 2)
avg2015 <- mean(salaries2015$AnnualSalary)
avg2015 <- round(avg2015, digits = 2)
avg2016 <- mean(salaries2016$AnnualSalary)
avg2016 <- round(avg2016, digits = 2)
avg2017 <- mean(salaries2017$ANNUAL_RT)
avg2017 <- round(avg2017, digits = 2)

averages <- c(avg2012, avg2013, avg2014, avg2015, avg2016, avg2017)
year <- c(2012, 2013, 2014, 2015, 2016, 2017)
avgFrame = data.frame(year, averages)
ggplot(avgFrame, aes(x = year, y = averages)) + geom_point() + ylab("Average salary") + xlab("Year") + ggtitle("Baltimore Average Salary, 2012-2017")

baltCrime <- select(baltCrime, -c(CrimeTime, Inside.Outside, Premise, Total.Incidents, Location.1, Longitude, Latitude, Location, Post))

as_tibble(baltCrime)

baltCrime$CrimeDate <- as.Date(baltCrime$CrimeDate, format = "%d/%m/%Y")
baltCrime$CrimeDate <- format(baltCrime$CrimeDate, "%Y")
baltCrime <- arrange(baltCrime, CrimeDate, CrimeCode)
baltCrime$CrimeCode <- as.character(baltCrime$CrimeCode)
baltCrime$CrimeCode <- substr(baltCrime$CrimeCode, 1, 1)
baltCrime$CrimeCode <- as.integer(baltCrime$CrimeCode)
as_tibble(baltCrime)
baltCrime <- filter(baltCrime, !is.na(baltCrime$CrimeDate))

crimeFreq2012 <- sum(baltCrime$CrimeDate == '2012')
crimeFreq2013 <- sum(baltCrime$CrimeDate == '2013')
crimeFreq2014 <- sum(baltCrime$CrimeDate == '2014')
crimeFreq2015 <- sum(baltCrime$CrimeDate == '2015')
crimeFreq2016 <- sum(baltCrime$CrimeDate == '2016')
crimeFreq2017 <- sum(baltCrime$CrimeDate == '2017')

crimeFreq2012_1 <- sum(baltCrime$CrimeCode == 1 & baltCrime$CrimeDate == '2012')
crimeFreq2013_1 <- sum(baltCrime$CrimeCode == 1 & baltCrime$CrimeDate == '2013')
crimeFreq2014_1 <- sum(baltCrime$CrimeCode == 1 & baltCrime$CrimeDate == '2014')
crimeFreq2015_1 <- sum(baltCrime$CrimeCode == 1 & baltCrime$CrimeDate == '2015')
crimeFreq2016_1 <- sum(baltCrime$CrimeCode == 1 & baltCrime$CrimeDate == '2016')
crimeFreq2017_1 <- sum(baltCrime$CrimeCode == 1 & baltCrime$CrimeDate == '2017')

crimeFreq2012_2 <- sum(baltCrime$CrimeCode == 2 & baltCrime$CrimeDate == '2012')
crimeFreq2013_2 <- sum(baltCrime$CrimeCode == 2 & baltCrime$CrimeDate == '2013')
crimeFreq2014_2 <- sum(baltCrime$CrimeCode == 2 & baltCrime$CrimeDate == '2014')
crimeFreq2015_2 <- sum(baltCrime$CrimeCode == 2 & baltCrime$CrimeDate == '2015')
crimeFreq2016_2 <- sum(baltCrime$CrimeCode == 2 & baltCrime$CrimeDate == '2016')
crimeFreq2017_2 <- sum(baltCrime$CrimeCode == 2 & baltCrime$CrimeDate == '2017')

crimeFreq2012_3 <- sum(baltCrime$CrimeCode == 3 & baltCrime$CrimeDate == '2012')
crimeFreq2013_3 <- sum(baltCrime$CrimeCode == 3 & baltCrime$CrimeDate == '2013')
crimeFreq2014_3 <- sum(baltCrime$CrimeCode == 3 & baltCrime$CrimeDate == '2014')
crimeFreq2015_3 <- sum(baltCrime$CrimeCode == 3 & baltCrime$CrimeDate == '2015')
crimeFreq2016_3 <- sum(baltCrime$CrimeCode == 3 & baltCrime$CrimeDate == '2016')
crimeFreq2017_3 <- sum(baltCrime$CrimeCode == 3 & baltCrime$CrimeDate == '2017')

crimeFreq2012_4 <- sum(baltCrime$CrimeCode == 4 & baltCrime$CrimeDate == '2012')
crimeFreq2013_4 <- sum(baltCrime$CrimeCode == 4 & baltCrime$CrimeDate == '2013')
crimeFreq2014_4 <- sum(baltCrime$CrimeCode == 4 & baltCrime$CrimeDate == '2014')
crimeFreq2015_4 <- sum(baltCrime$CrimeCode == 4 & baltCrime$CrimeDate == '2015')
crimeFreq2016_4 <- sum(baltCrime$CrimeCode == 4 & baltCrime$CrimeDate == '2016')
crimeFreq2017_4 <- sum(baltCrime$CrimeCode == 4 & baltCrime$CrimeDate == '2017')

crimeFreq2012_5 <- sum(baltCrime$CrimeCode == 5 & baltCrime$CrimeDate == '2012')
crimeFreq2013_5 <- sum(baltCrime$CrimeCode == 5 & baltCrime$CrimeDate == '2013')
crimeFreq2014_5 <- sum(baltCrime$CrimeCode == 5 & baltCrime$CrimeDate == '2014')
crimeFreq2015_5 <- sum(baltCrime$CrimeCode == 5 & baltCrime$CrimeDate == '2015')
crimeFreq2016_5 <- sum(baltCrime$CrimeCode == 5 & baltCrime$CrimeDate == '2016')
crimeFreq2017_5 <- sum(baltCrime$CrimeCode == 5 & baltCrime$CrimeDate == '2017')

crimeFreq2012_6 <- sum(baltCrime$CrimeCode == 6 & baltCrime$CrimeDate == '2012')
crimeFreq2013_6 <- sum(baltCrime$CrimeCode == 6 & baltCrime$CrimeDate == '2013')
crimeFreq2014_6 <- sum(baltCrime$CrimeCode == 6 & baltCrime$CrimeDate == '2014')
crimeFreq2015_6 <- sum(baltCrime$CrimeCode == 6 & baltCrime$CrimeDate == '2015')
crimeFreq2016_6 <- sum(baltCrime$CrimeCode == 6 & baltCrime$CrimeDate == '2016')
crimeFreq2017_6 <- sum(baltCrime$CrimeCode == 6 & baltCrime$CrimeDate == '2017')

crimeFreq2012_7 <- sum(baltCrime$CrimeCode == 7 & baltCrime$CrimeDate == '2012')
crimeFreq2013_7 <- sum(baltCrime$CrimeCode == 7 & baltCrime$CrimeDate == '2013')
crimeFreq2014_7 <- sum(baltCrime$CrimeCode == 7 & baltCrime$CrimeDate == '2014')
crimeFreq2015_7 <- sum(baltCrime$CrimeCode == 7 & baltCrime$CrimeDate == '2015')
crimeFreq2016_7 <- sum(baltCrime$CrimeCode == 7 & baltCrime$CrimeDate == '2016')
crimeFreq2017_7 <- sum(baltCrime$CrimeCode == 7 & baltCrime$CrimeDate == '2017')

crimeFreq2012_8 <- sum(baltCrime$CrimeCode == 8 & baltCrime$CrimeDate == '2012')
crimeFreq2013_8 <- sum(baltCrime$CrimeCode == 8 & baltCrime$CrimeDate == '2013')
crimeFreq2014_8 <- sum(baltCrime$CrimeCode == 8 & baltCrime$CrimeDate == '2014')
crimeFreq2015_8 <- sum(baltCrime$CrimeCode == 8 & baltCrime$CrimeDate == '2015')
crimeFreq2016_8 <- sum(baltCrime$CrimeCode == 8 & baltCrime$CrimeDate == '2016')
crimeFreq2017_8 <- sum(baltCrime$CrimeCode == 8 & baltCrime$CrimeDate == '2017')

crimeFreq2012_9 <- sum(baltCrime$CrimeCode == 9 & baltCrime$CrimeDate == '2012')
crimeFreq2013_9 <- sum(baltCrime$CrimeCode == 9 & baltCrime$CrimeDate == '2013')
crimeFreq2014_9 <- sum(baltCrime$CrimeCode == 9 & baltCrime$CrimeDate == '2014')
crimeFreq2015_9 <- sum(baltCrime$CrimeCode == 9 & baltCrime$CrimeDate == '2015')
crimeFreq2016_9 <- sum(baltCrime$CrimeCode == 9 & baltCrime$CrimeDate == '2016')
crimeFreq2017_9 <- sum(baltCrime$CrimeCode == 9 & baltCrime$CrimeDate == '2017')

crimeFreq2 <- sum(baltCrime$CrimeCode == 2)
crimeFreq3 <- sum(baltCrime$CrimeCode == 3)
crimeFreq4 <- sum(baltCrime$CrimeCode == 4)
crimeFreq5 <- sum(baltCrime$CrimeCode == 5)
crimeFreq6 <- sum(baltCrime$CrimeCode == 6)
crimeFreq7 <- sum(baltCrime$CrimeCode == 7)
crimeFreq8 <- sum(baltCrime$CrimeCode == 8)
crimeFreq9 <- sum(baltCrime$CrimeCode == 9)

yearVector <- c(2012, 2013, 2014, 2015, 2016, 2017)
freqVector <- c(crimeFreq2012, crimeFreq2013, crimeFreq2014, crimeFreq2015, crimeFreq2016, crimeFreq2017)
freq1Vector <- c(crimeFreq2012_1, crimeFreq2013_1, crimeFreq2014_1, crimeFreq2015_1, crimeFreq2016_1, crimeFreq2017_1)
freq2Vector <- c(crimeFreq2012_2, crimeFreq2013_2, crimeFreq2014_2, crimeFreq2015_2, crimeFreq2016_2, crimeFreq2017_2)
freq3Vector <- c(crimeFreq2012_3, crimeFreq2013_3, crimeFreq2014_3, crimeFreq2015_3, crimeFreq2016_3, crimeFreq2017_3)
freq4Vector <- c(crimeFreq2012_4, crimeFreq2013_4, crimeFreq2014_4, crimeFreq2015_4, crimeFreq2016_4, crimeFreq2017_4)
freq5Vector <- c(crimeFreq2012_5, crimeFreq2013_5, crimeFreq2014_5, crimeFreq2015_5, crimeFreq2016_5, crimeFreq2017_5)
freq6Vector <- c(crimeFreq2012_6, crimeFreq2013_6, crimeFreq2014_6, crimeFreq2015_6, crimeFreq2016_6, crimeFreq2017_6)
freq7Vector <- c(crimeFreq2012_7, crimeFreq2013_7, crimeFreq2014_7, crimeFreq2015_7, crimeFreq2016_7, crimeFreq2017_7)
freq8Vector <- c(crimeFreq2012_8, crimeFreq2013_8, crimeFreq2014_8, crimeFreq2015_8, crimeFreq2016_8, crimeFreq2017_8)
freq9Vector <- c(crimeFreq2012_9, crimeFreq2013_9, crimeFreq2014_9, crimeFreq2015_9, crimeFreq2016_9, crimeFreq2017_9)

freqMatrix <- cbind(yearVector, freqVector, freq1Vector, freq2Vector, freq3Vector, freq4Vector, freq5Vector, freq6Vector, freq7Vector, freq8Vector, freq9Vector)
colnames(freqMatrix) <- c("Year", "Total # Crimes", "Homicide Freq", "Rape Freq", "Robbery Freq", "Assault Freq", "Burglary Freq", "Larceny Freq", "Auto-theft Freq", "Arson Freq", "Shooting Freq")
freqFrame <- as_tibble(freqMatrix)
ggplot(freqFrame, aes(x = yearVector, y, color = variable)) + 
  geom_point(aes(y = freq3Vector, col = "Robbery")) + 
  geom_point(aes(y = freq4Vector, col = "Assault")) + 
  geom_point(aes(y = freq5Vector, col = "Burglary")) + 
  geom_point(aes(y = freq6Vector, col = "Larceny")) + 
  geom_point(aes(y = freq7Vector, col = "Auto-theft")) +
  xlab("Year") + 
  ylab("Frequency") + 
  ggtitle("High Frequency Crime in Baltimore, 2012-2017")

ggplot(freqFrame, aes(x = yearVector, y, color = variable)) + 
  geom_point(aes(y = freq1Vector, col = "Homicide")) + 
  geom_point(aes(y = freq2Vector, col = "Rape")) + 
  geom_point(aes(y = freq8Vector, col = "Arson")) + 
  geom_point(aes(y = freq9Vector, col = "Shooting")) + 
  xlab("Year") + 
  ylab("Frequency") + 
  ggtitle("Low Frequency Crime in Baltimore, 2012-2017")
# as_tibble(baltCrime)