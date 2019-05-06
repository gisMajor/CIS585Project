##CIS585 "Data Analytics in R" Project
##Crime and Weather: Effect of Weather on Crime in Chicago
##Austin Johnessee

##Set Working Directory
dirc <- "D:/Krotov/BaseData"
setwd(dirc)

##Load in Data
Data2015 <- read.csv("2015Data.csv",header=TRUE)

##Plot Overview
##Save as PNG
png(filename = "Overview2015.png",width=10,height=6,units="in",res=300)

plot(Data2015$Date,Data2015$DateTotal,main="Overview of 2015 Crimes",
     xlab="Date",ylab="Total")

dev.off()

##Summarize Monthly Crimes
julian_dayBegin <- c(1,32,60,91,121,152,182,213,244,274,305,335)
julian_dayEnd <- c(31,59,90,120,151,181,212,243,273,304,334,365)
month <- c(1:12)
monthCrimeSum <- array(NA,c(12))
monthArsonSum <- array(NA,c(12))
monthBurglarySum <- array(NA,c(12))
monthDeceptivePracticeSum <- array(NA,c(12))
monthHomicideSum <- array(NA,c(12))
monthDaySum <- array(NA,c(12))
monthNightSum <- array(NA,c(12))
for(i in 1:length(julian_dayBegin)) {
  monthCrimeSum[i] <- sum(Data2015$DateTotal[julian_dayBegin[i]:julian_dayEnd[i]])
  monthHomicideSum[i] <- sum(Data2015$HomicideTotal[julian_dayBegin[i]:julian_dayEnd[i]])
  monthArsonSum[i] <- sum(Data2015$ArsonTotal[julian_dayBegin[i]:julian_dayEnd[i]])
  monthBurglarySum[i] <- sum(Data2015$BurglaryTotal[julian_dayBegin[i]:julian_dayEnd[i]])
  monthDeceptivePracticeSum[i] <- sum(Data2015$DeceptivePracticeTotal[julian_dayBegin[i]:julian_dayEnd[i]])
  monthDaySum[i] <- sum(Data2015$DayTotal[julian_dayBegin[i]:julian_dayEnd[i]])
  monthNightSum[i] <- sum(Data2015$NightTotal[julian_dayBegin[i]:julian_dayEnd[i]])
}

##Plot Monthly Totals for All Crimes
##Save as PNG
png(filename="OverviewMonthlyTotal2015.png",width=10,height=6,units="in",res=300)

plot(month,monthCrimeSum,main="Monthly Totals",xlab="Months",ylab="Totals")

dev.off()

##Plot Monthly Totals for Crime Categories
##Save as PNG
png(filename="OverviewMonthlyCategoryTotals2015.png",width=10,height=6,units="in",res=300)

par(mfrow=c(2,2),mar=c(4,5,3,1),cex.lab=1.8)

##Plot Monthly Arson Totals
plot(month,monthArsonSum,main="Monthly Arson Totals",xlab="Months",
     ylab="Arson Totals")

##Plot Monthly Burglary Totals
plot(month,monthBurglarySum,main="Monthly Burglary Totals",xlab="Months",
     ylab="Burglary Totals")

##Plot Monthly Deceptive Practice Totals
plot(month,monthDeceptivePracticeSum,main="Monthly Deceptive Practice Totals",xlab="Months",
     ylab="Deceptive Practice Totals")

##Plot Homicde Monthly Totals
plot(month,monthHomicideSum,main="Monthly Homicide Totals",xlab="Months",
     ylab="Homicide Totals")

dev.off()

##Average Monthly Crimes
monthCrimeMean <- array(NA,c(12))
monthArsonMean <- array(NA,c(12))
monthBurglaryMean <- array(NA,c(12))
monthDeceptivePracticeMean <- array(NA,c(12))
monthHomicideMean <- array(NA,c(12))
monthDayMean <- array(NA,c(12))
monthNightMean <- array(NA,c(12))
for(j in 1:length(julian_dayBegin)) {
  monthCrimeMean[j] <- mean(Data2015$DateTotal[julian_dayBegin[j]:julian_dayEnd[j]])
  monthArsonMean[j] <- mean(Data2015$ArsonTotal[julian_dayBegin[j]:julian_dayEnd[j]])
  monthBurglaryMean[j] <- mean(Data2015$BurglaryTotal[julian_dayBegin[j]:julian_dayEnd[j]])
  monthDeceptivePracticeMean[j] <- mean(Data2015$DeceptivePracticeTotal[julian_dayBegin[j]:julian_dayEnd[j]])
  monthHomicideMean[j] <- mean(Data2015$HomicideTotal[julian_dayBegin[j]:julian_dayEnd[j]])
  monthDayMean[j] <- mean(Data2015$DayTotal[julian_dayBegin[j]:julian_dayEnd[j]])
  monthNightMean[j] <- mean(Data2015$NightTotal[julian_dayBegin[j]:julian_dayEnd[j]])
}

##Plot Monthly Averages for All Crimes
##Save as PNG
png(filename="OverviewMonthlyAverage2015.png",width=10,height=6,units="in",res=300)

plot(month,monthCrimeMean,main="Monthly Averages",xlab="Months",ylab="Averages")

dev.off()

##Plot Monthly Averages for Crime Categories
##Save as PNG
png(filename="OverviewMonthlyCategoryAverages2015.png",width=10,height=6,units="in",res=300)

par(mfrow=c(3,3),mar=c(4,5,3,1),cex.lab=1.8)

##Plot Monthly Arson Averages
plot(month,monthArsonMean,main="Monthly Arson Averages",xlab="Months",
     ylab="Arson Averages")

##Plot Monthly Burglary Averages
plot(month,monthBurglaryMean,main="Monthly Burglary Averages",xlab="Months",
     ylab="Burglary Averages")

##Plot Monthly Deceptive Practice Averages
plot(month,monthDeceptivePracticeMean,main="Monthly Deceptive Practice Averages",xlab="Months",
     ylab="Deceptive Practice Totals")

##Plot Homicde Monthly Averages
plot(month,monthHomicideMean,main="Monthly Homicide Averages",xlab="Months",
     ylab="Homicide Averages")

##Plot Day Crime Monthly Averages
plot(month,monthDayMean,main="Monthly Day Crime Averages",xlab="Months",
     ylab="Day Crime Averages")

##Plot Night Crime Monthly Averages
plot(month,monthNightMean,main="Monthly Night Crime Averages",xlab="Months",
     ylab="Night Crime Averages")

dev.off()

##Average Monthly Weather Conditions
monthAvgTempMean <- array(NA,c(12))
monthMaxTempMean <- array(NA,c(12))
monthMinTempMean <- array(NA,c(12))
monthPrecipAmountMean <- array(NA,c(12))
monthSnowAmountMean <- array(NA,c(12))
monthWindSpeedMean <- array(NA,c(12))
monthFogMean <- array(NA,c(12))
monthHeavyFogMean <- array(NA,c(12))
monthThunderMean <- array(NA,c(12))
monthSleetMean <- array(NA,c(12))
monthHailMean <- array(NA,c(12))
monthGlazeMean <- array(NA,c(12))
monthHazeMean <- array(NA,c(12))
monthSnowMean <- array(NA,c(12))
monthSunnyMean <- array(NA,c(12))
monthWeatherMean <- array(NA,c(12))
monthDayLengthMean <- array(NA,c(12))
for(k in 1:length(julian_dayBegin)) {
  monthAvgTempMean[k] <- mean(Data2015$Temp_Avg.oF.[julian_dayBegin[k]:julian_dayEnd[k]])
  monthMaxTempMean[k] <- mean(Data2015$Temp_Max.oF.[julian_dayBegin[k]:julian_dayEnd[k]])
  monthMinTempMean[k] <- mean(Data2015$Temp_Min.oF.[julian_dayBegin[k]:julian_dayEnd[k]])
  monthPrecipAmountMean[k] <- mean(Data2015$Precip.in.[julian_dayBegin[k]:julian_dayEnd[k]])
  monthSnowAmountMean[k] <- mean(Data2015$Snow.in.[julian_dayBegin[k]:julian_dayEnd[k]])
  monthWindSpeedMean[k] <- mean(Data2015$Wind.mph.[julian_dayBegin[k]:julian_dayEnd[k]])
  monthFogMean[k] <- mean(Data2015$Fog[julian_dayBegin[k]:julian_dayEnd[k]])
  monthHeavyFogMean[k] <- mean(Data2015$HeavyFog[julian_dayBegin[k]:julian_dayEnd[k]])
  monthThunderMean[k] <- mean(Data2015$Thunder[julian_dayBegin[k]:julian_dayEnd[k]])
  monthSleetMean[k] <- mean(Data2015$Sleet[julian_dayBegin[k]:julian_dayEnd[k]])
  monthHailMean[k] <- mean(Data2015$Hail[julian_dayBegin[k]:julian_dayEnd[k]])
  monthGlazeMean[k] <- mean(Data2015$Glaze[julian_dayBegin[k]:julian_dayEnd[k]])
  monthHazeMean[k] <- mean(Data2015$Haze[julian_dayBegin[k]:julian_dayEnd[k]])
  monthSnowMean[k] <- mean(Data2015$Snow[julian_dayBegin[k]:julian_dayEnd[k]])
  monthSunnyMean[k] <- mean(Data2015$Sunny[julian_dayBegin[k]:julian_dayEnd[k]])
  monthWeatherMean[k] <- mean(Data2015$Weather[julian_dayBegin[k]:julian_dayEnd[k]])
  monthDayLengthMean[k] <- mean(Data2015$DayLengthPercent[julian_dayBegin[k]:julian_dayEnd[k]])
}

##Linear Regression of Day Length Average Across Monthly Crime Average
show(sumRegAnalAllCrimeDayLengthMonth <- summary(lm(monthDayLengthMean~monthCrimeMean)))
show(sumRegAnalArsonDayLengthMonth <- summary(lm(monthDayLengthMean~monthArsonMean)))
show(sumRegAnalBurglaryDayLengthMonth <- summary(lm(monthDayLengthMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeDayLengthMonth <- summary(lm(monthDayLengthMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideDayLengthMonth <- summary(lm(monthDayLengthMean~monthHomicideMean)))
show(sumRegAnalDayDayLengthMonth <- summary(lm(monthDayLengthMean~monthDayMean)))
show(sumRegAnalNightDayLengthMonth <- summary(lm(monthDayLengthMean~monthNightMean)))

##Linear Regression of Maximum Temperature Average Across Monthly Crime Average
show(sumRegAnalAllCrimeMaxTempMonth <- summary(lm(monthMaxTempMean~monthCrimeMean)))
show(sumRegAnalArsonMaxTempMonth <- summary(lm(monthMaxTempMean~monthArsonMean)))
show(sumRegAnalBurglaryMaxTempMonth <- summary(lm(monthMaxTempMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeMaxTempMonth <- summary(lm(monthMaxTempMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideMaxTempMonth <- summary(lm(monthMaxTempMean~monthHomicideMean)))
show(sumRegAnalDayMaxTempMonth <- summary(lm(monthMaxTempMean~monthDayMean)))
show(sumRegAnalNightMaxTempMonth <- summary(lm(monthMaxTempMean~monthNightMean)))

##Linear Regression of Average Temperature Average Across Monthly Crime Average
show(sumRegAnalAllCrimeAvgTempMonth <- summary(lm(monthAvgTempMean~monthCrimeMean)))
show(sumRegAnalArsonAvgTempMonth <- summary(lm(monthAvgTempMean~monthArsonMean)))
show(sumRegAnalBurglaryAvgTempMonth <- summary(lm(monthAvgTempMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeAvgTempMonth <- summary(lm(monthAvgTempMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideAvgTempMonth <- summary(lm(monthAvgTempMean~monthHomicideMean)))
show(sumRegAnalDayAvgTempMonth <- summary(lm(monthAvgTempMean~monthDayMean)))
show(sumRegAnalNightAvgTempMonth <- summary(lm(monthAvgTempMean~monthNightMean)))

##Linear Regression of Minimum Temperature Average Across Monthly Crime Average
show(sumRegAnalAllCrimeMinTempMonth <- summary(lm(monthMinTempMean~monthCrimeMean)))
show(sumRegAnalArsonMinTempMonth <- summary(lm(monthMinTempMean~monthArsonMean)))
show(sumRegAnalBurglaryMinTempMonth <- summary(lm(monthMinTempMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeMinTempMonth <- summary(lm(monthMinTempMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideMinTempMonth <- summary(lm(monthMinTempMean~monthHomicideMean)))
show(sumRegAnalDayMinTempMonth <- summary(lm(monthMinTempMean~monthDayMean)))
show(sumRegAnalNightMinTempMonth <- summary(lm(monthMinTempMean~monthNightMean)))

##Linear Regression of Precipiation Amount Average Across Monthly Crime Average
show(sumRegAnalAllCrimePrecipAmountMonth <- summary(lm(monthPrecipAmountMean~monthCrimeMean)))
show(sumRegAnalArsonPrecipAmountMonth <- summary(lm(monthPrecipAmountMean~monthArsonMean)))
show(sumRegAnalBurglaryPrecipAmountMonth <- summary(lm(monthPrecipAmountMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticePrecipAmountMonth <- summary(lm(monthPrecipAmountMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicidePrecipAmountMonth <- summary(lm(monthPrecipAmountMean~monthHomicideMean)))
show(sumRegAnalDayPrecipAmountMonth <- summary(lm(monthPrecipAmountMean~monthDayMean)))
show(sumRegAnalNightPrecipAmountMonth <- summary(lm(monthPrecipAmountMean~monthNightMean)))

##Linear Regression of Snow Amount Average Across Monthly Crime Average
show(sumRegAnalAllCrimeSnowAmountMonth <- summary(lm(monthSnowAmountMean~monthCrimeMean)))
show(sumRegAnalArsonSnowAmountMonth <- summary(lm(monthSnowAmountMean~monthArsonMean)))
show(sumRegAnalBurglarySnowAmountMonth <- summary(lm(monthSnowAmountMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeSnowAmountMonth <- summary(lm(monthSnowAmountMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideSnowAmountMonth <- summary(lm(monthSnowAmountMean~monthHomicideMean)))
show(sumRegAnalDaySnowAmountMonth <- summary(lm(monthSnowAmountMean~monthDayMean)))
show(sumRegAnalNightSnowAmountMonth <- summary(lm(monthSnowAmountMean~monthNightMean)))

##Linear Regression of Wind Speed Average Across Monthly Crime Average
show(sumRegAnalAllCrimeWindSpeedMonth <- summary(lm(monthWindSpeedMean~monthCrimeMean)))
show(sumRegAnalArsonWindSpeedMonth <- summary(lm(monthWindSpeedMean~monthArsonMean)))
show(sumRegAnalBurglaryWindSpeedMonth <- summary(lm(monthWindSpeedMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeWindSpeedMonth <- summary(lm(monthWindSpeedMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideWindSpeedMonth <- summary(lm(monthWindSpeedMean~monthHomicideMean)))
show(sumRegAnalDayWindSpeedMonth <- summary(lm(monthDayLengthMean~monthDayMean)))
show(sumRegAnalNightWindSpeedMonth <- summary(lm(monthWindSpeedMean~monthNightMean)))

##Linear Regression of Fog Average Across Monthly Crime Average
show(sumRegAnalAllCrimeFogMonth <- summary(lm(monthFogMean~monthCrimeMean)))
show(sumRegAnalArsonFogMonth <- summary(lm(monthFogMean~monthArsonMean)))
show(sumRegAnalBurglaryFogMonth <- summary(lm(monthFogMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeFogMonth <- summary(lm(monthFogMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideFogMonth <- summary(lm(monthFogMean~monthHomicideMean)))
show(sumRegAnalDayFogMonth <- summary(lm(monthFogMean~monthDayMean)))
show(sumRegAnalNightFogMonth <- summary(lm(monthFogMean~monthNightMean)))

##Linear Regression of Heavy Fog Average Across Monthly Crime Average
show(sumRegAnalAllCrimeHeavyFogMonth <- summary(lm(monthHeavyFogMean~monthCrimeMean)))
show(sumRegAnalArsonHeavyFogMonth <- summary(lm(monthHeavyFogMean~monthArsonMean)))
show(sumRegAnalBurglaryHeavyFogMonth <- summary(lm(monthHeavyFogMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeHeavyFogMonth <- summary(lm(monthHeavyFogMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideHeavyFogMonth <- summary(lm(monthHeavyFogMean~monthHomicideMean)))
show(sumRegAnalDayHeavyFogMonth <- summary(lm(monthHeavyFogMean~monthDayMean)))
show(sumRegAnalNightHeavyFogMonth <- summary(lm(monthHeavyFogMean~monthNightMean)))

##Linear Regression of Thunder Average Across Monthly Crime Average
show(sumRegAnalAllCrimeThunderMonth <- summary(lm(monthThunderMean~monthCrimeMean)))
show(sumRegAnalArsonThunderMonth <- summary(lm(monthThunderMean~monthArsonMean)))
show(sumRegAnalBurglaryThunderMonth <- summary(lm(monthThunderMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeThunderMonth <- summary(lm(monthThunderMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideThunderMonth <- summary(lm(monthThunderMean~monthHomicideMean)))
show(sumRegAnalDayThunderMonth <- summary(lm(monthThunderMean~monthDayMean)))
show(sumRegAnalNightThunderMonth <- summary(lm(monthThunderMean~monthNightMean)))

##Linear Regression of Sleet Average Across Monthly Crime Average
show(sumRegAnalAllCrimeSleetMonth <- summary(lm(monthSleetMean~monthCrimeMean)))
show(sumRegAnalArsonSleetMonth <- summary(lm(monthSleetMean~monthArsonMean)))
show(sumRegAnalBurglarySleetMonth <- summary(lm(monthSleetMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeSleetMonth <- summary(lm(monthSleetMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideSleetMonth <- summary(lm(monthSleetMean~monthHomicideMean)))
show(sumRegAnalDaySleetMonth <- summary(lm(monthSleetMean~monthDayMean)))
show(sumRegAnalNightSleetMonth <- summary(lm(monthSleetMean~monthNightMean)))

##Linear Regression of Hail Average Across Monthly Crime Average
show(sumRegAnalAllCrimeHailMonth <- summary(lm(monthHailMean~monthCrimeMean)))
show(sumRegAnalArsonHailMonth <- summary(lm(monthHailMean~monthArsonMean)))
show(sumRegAnalBurglaryHailMonth <- summary(lm(monthHailMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeHailMonth <- summary(lm(monthHailMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideHailMonth <- summary(lm(monthHailMean~monthHomicideMean)))
show(sumRegAnalDayHailMonth <- summary(lm(monthHailMean~monthDayMean)))
show(sumRegAnalNightHailMonth <- summary(lm(monthHailMean~monthNightMean)))

##Linear Regression of Glaze Average Across Monthly Crime Average
show(sumRegAnalAllCrimeGlazeMonth <- summary(lm(monthGlazeMean~monthCrimeMean)))
show(sumRegAnalArsonGlazeMonth <- summary(lm(monthGlazeMean~monthArsonMean)))
show(sumRegAnalBurglaryGlazeMonth <- summary(lm(monthGlazeMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeGlazeMonth <- summary(lm(monthGlazeMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideGlazeMonth <- summary(lm(monthGlazeMean~monthHomicideMean)))
show(sumRegAnalDayGlazeMonth <- summary(lm(monthGlazeMean~monthDayMean)))
show(sumRegAnalNightGlazeMonth <- summary(lm(monthGlazeMean~monthNightMean)))

##Linear Regression of Haze Average Across Monthly Crime Average
show(sumRegAnalAllCrimeHazeMonth <- summary(lm(monthHazeMean~monthCrimeMean)))
show(sumRegAnalArsonHazeMonth <- summary(lm(monthHazeMean~monthArsonMean)))
show(sumRegAnalBurglaryHazeMonth <- summary(lm(monthHazeMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeHazeMonth <- summary(lm(monthHazeMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideHazeMonth <- summary(lm(monthHazeMean~monthHomicideMean)))
show(sumRegAnalDayHazeMonth <- summary(lm(monthHazeMean~monthDayMean)))
show(sumRegAnalNightHazeMonth <- summary(lm(monthHazeMean~monthNightMean)))

##Linear Regression of Snow Average Across Monthly Crime Average
show(sumRegAnalAllCrimeHazeMonth <- summary(lm(monthHazeMean~monthCrimeMean)))
show(sumRegAnalArsonHazeMonth <- summary(lm(monthHazeMean~monthArsonMean)))
show(sumRegAnalBurglaryHazeMonth <- summary(lm(monthHazeMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeHazeMonth <- summary(lm(monthHazeMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideHazeMonth <- summary(lm(monthHazeMean~monthHomicideMean)))
show(sumRegAnalDaySnowMonth <- summary(lm(monthSnowMean~monthDayMean)))
show(sumRegAnalNightSnowMonth <- summary(lm(monthSnowMean~monthNightMean)))

##Linear Regression of Sunny Average Across Monthly Crime Average
show(sumRegAnalAllCrimeSunnyMonth <- summary(lm(monthSunnyMean~monthCrimeMean)))
show(sumRegAnalArsonSunnyMonth <- summary(lm(monthSunnyMean~monthArsonMean)))
show(sumRegAnalBurglarySunnyMonth <- summary(lm(monthSunnyMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeSunnyMonth <- summary(lm(monthSunnyMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideSunnyMonth <- summary(lm(monthSunnyMean~monthHomicideMean)))
show(sumRegAnalDaySunnyMonth <- summary(lm(monthSunnyMean~monthDayMean)))
show(sumRegAnalNightSunnyMonth <- summary(lm(monthSunnyMean~monthNightMean)))

##Linear Regression of Weather Condition Average Across Monthly Crime Average
show(sumRegAnalAllCrimeWeatherMonth <- summary(lm(monthWeatherMean~monthCrimeMean)))
show(sumRegAnalArsonWeatherMonth <- summary(lm(monthWeatherMean~monthArsonMean)))
show(sumRegAnalBurglaryWeatherMonth <- summary(lm(monthWeatherMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeWeatherMonth <- summary(lm(monthWeatherMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideWeatherMonth <- summary(lm(monthWeatherMean~monthHomicideMean)))
show(sumRegAnalDayWeatherMonth <- summary(lm(monthWeatherMean~monthDayMean)))
show(sumRegAnalNightWeatherMonth <- summary(lm(monthWeatherMean~monthNightMean)))

##Multiple Regression Analysis of Daylength, Maximum Temperature, and Sunny Averages Across Monthly Crime Averages
show(sumRegAnalAllCrimeDayMaxSunnyMonth <- summary(lm(monthDayLengthMean+monthMaxTempMean+monthSunnyMean~monthCrimeMean)))
show(sumRegAnalArsonDayMaxSunnyMonth <- summary(lm(monthDayLengthMean+monthMaxTempMean+monthSunnyMean~monthArsonMean)))
show(sumRegAnalBurglaryDayMaxSunnyMonth <- summary(lm(monthDayLengthMean+monthMaxTempMean+monthSunnyMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeDayMaxSunnyMonth <- summary(lm(monthDayLengthMean+monthMaxTempMean+monthSunnyMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideDayMaxSunnyMonth <- summary(lm(monthDayLengthMean+monthMaxTempMean+monthSunnyMean~monthHomicideMean)))
show(sumRegAnalDayDayMaxSunnyMonth <- summary(lm(monthDayLengthMean+monthMaxTempMean+monthSunnyMean~monthDayMean)))
show(sumRegAnalNightDayMaxSunnyMonth <- summary(lm(monthDayLengthMean+monthMaxTempMean+monthSunnyMean~monthNightMean)))

##Multiple Regression Analysis of Daylength, Average Temperature, and Sunny Averages Across Monthly Crime Averages
show(sumRegAnalAllCrimeDayAvgSunnyMonth <- summary(lm(monthDayLengthMean+monthAvgTempMean+monthSunnyMean~monthCrimeMean)))
show(sumRegAnalArsonDayAvgSunnyMonth <- summary(lm(monthDayLengthMean+monthAvgTempMean+monthSunnyMean~monthArsonMean)))
show(sumRegAnalBurglaryDayAvgSunnyMonth <- summary(lm(monthDayLengthMean+monthAvgTempMean+monthSunnyMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeDayAvgSunnyMonth <- summary(lm(monthDayLengthMean+monthAvgTempMean+monthSunnyMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideDayAvgSunnyMonth <- summary(lm(monthDayLengthMean+monthAvgTempMean+monthSunnyMean~monthHomicideMean)))
show(sumRegAnalDayDayAvgSunnyMonth <- summary(lm(monthDayLengthMean+monthAvgTempMean+monthSunnyMean~monthDayMean)))
show(sumRegAnalNightDayAvgSunnyMonth <- summary(lm(monthDayLengthMean+monthAvgTempMean+monthSunnyMean~monthNightMean)))

##Multiple Regression Analysis of Daylength, Minimum Temperature, and Sunny Averages Across Monthly Crime Averages
show(sumRegAnalAllCrimeDayMinSunnyMonth <- summary(lm(monthDayLengthMean+monthMinTempMean+monthSunnyMean~monthCrimeMean)))
show(sumRegAnalArsonDayMinSunnyMonth <- summary(lm(monthDayLengthMean+monthMinTempMean+monthSunnyMean~monthArsonMean)))
show(sumRegAnalBurglaryDayMinSunnyMonth <- summary(lm(monthDayLengthMean+monthMinTempMean+monthSunnyMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeDayMinSunnyMonth <- summary(lm(monthDayLengthMean+monthMinTempMean+monthSunnyMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideDayMinSunnyMonth <- summary(lm(monthDayLengthMean+monthMinTempMean+monthSunnyMean~monthHomicideMean)))
show(sumRegAnalDayDayMinSunnyMonth <- summary(lm(monthDayLengthMean+monthMinTempMean+monthSunnyMean~monthDayMean)))
show(sumRegAnalNightDayMinSunnyMonth <- summary(lm(monthDayLengthMean+monthMinTempMean+monthSunnyMean~monthNightMean)))

##Multiple Regression Analysis of All Weather Variables (using Maximum Temperature) Across Monthly Crime Averages
show(sumRegAnalAllCrimeAllWeatherMaxTempMonth <- summary(lm(monthDayLengthMean+monthMaxTempMean+monthSunnyMean+monthSnowAmountMean+monthSnowMean+
                                                              monthHailMean+monthHazeMean+monthGlazeMean+monthSleetMean+monthWindSpeedMean+
                                                              monthFogMean+monthHeavyFogMean+monthPrecipAmountMean+monthThunderMean+monthWeatherMean~monthCrimeMean)))
show(sumRegAnalArsonAllWeatherMaxTempMonth <- summary(lm(monthDayLengthMean+monthMaxTempMean+monthSunnyMean+monthSnowAmountMean+monthSnowMean+
                                                           monthHailMean+monthHazeMean+monthGlazeMean+monthSleetMean+monthWindSpeedMean+
                                                           monthFogMean+monthHeavyFogMean+monthPrecipAmountMean+monthThunderMean+monthWeatherMean~monthArsonMean)))
show(sumRegAnalBurglaryAllWeatherMaxTempMonth <- summary(lm(monthDayLengthMean+monthMaxTempMean+monthSunnyMean+monthSnowAmountMean+monthSnowMean+
                                                              monthHailMean+monthHazeMean+monthGlazeMean+monthSleetMean+monthWindSpeedMean+
                                                              monthFogMean+monthHeavyFogMean+monthPrecipAmountMean+monthThunderMean+monthWeatherMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeAllWeatherMaxTempMonth <- summary(lm(monthDayLengthMean+monthMaxTempMean+monthSunnyMean+monthSnowAmountMean+monthSnowMean+
                                                                       monthHailMean+monthHazeMean+monthGlazeMean+monthSleetMean+monthWindSpeedMean+
                                                                       monthFogMean+monthHeavyFogMean+monthPrecipAmountMean+monthThunderMean+monthWeatherMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideAllWeatherMaxTempMonth <- summary(lm(monthDayLengthMean+monthMaxTempMean+monthSunnyMean+monthSnowAmountMean+monthSnowMean+
                                                              monthHailMean+monthHazeMean+monthGlazeMean+monthSleetMean+monthWindSpeedMean+
                                                              monthFogMean+monthHeavyFogMean+monthPrecipAmountMean+monthThunderMean+monthWeatherMean~monthHomicideMean)))
show(sumRegAnalDayAllWeatherMaxTempMonth <- summary(lm(monthDayLengthMean+monthMaxTempMean+monthSunnyMean+monthSnowAmountMean+monthSnowMean+
                                                         monthHailMean+monthHazeMean+monthGlazeMean+monthSleetMean+monthWindSpeedMean+
                                                         monthFogMean+monthHeavyFogMean+monthPrecipAmountMean+monthThunderMean+monthWeatherMean~monthDayMean)))
show(sumRegAnalNightAllWeatherMaxTempMonth <- summary(lm(monthDayLengthMean+monthMaxTempMean+monthSunnyMean+monthSnowAmountMean+monthSnowMean+
                                                           monthHailMean+monthHazeMean+monthGlazeMean+monthSleetMean+monthWindSpeedMean+
                                                           monthFogMean+monthHeavyFogMean+monthPrecipAmountMean+monthThunderMean+monthWeatherMean~monthNightMean)))

##Multiple Regression Analysis of All Weather Variables (using Average Temperature) Across Monthly Crime Averages
show(sumRegAnalAllCrimeAllWeatherAvgTempMonth <- summary(lm(monthDayLengthMean+monthAvgTempMean+monthSunnyMean+monthSnowAmountMean+monthSnowMean+
                                                              monthHailMean+monthHazeMean+monthGlazeMean+monthSleetMean+monthWindSpeedMean+
                                                              monthFogMean+monthHeavyFogMean+monthPrecipAmountMean+monthThunderMean+monthWeatherMean~monthCrimeMean)))
show(sumRegAnalArsonAllWeatherAvgTempMonth <- summary(lm(monthDayLengthMean+monthAvgTempMean+monthSunnyMean+monthSnowAmountMean+monthSnowMean+
                                                           monthHailMean+monthHazeMean+monthGlazeMean+monthSleetMean+monthWindSpeedMean+
                                                           monthFogMean+monthHeavyFogMean+monthPrecipAmountMean+monthThunderMean+monthWeatherMean~monthArsonMean)))
show(sumRegAnalBurglaryAllWeatherAvgTempMonth <- summary(lm(monthDayLengthMean+monthAvgTempMean+monthSunnyMean+monthSnowAmountMean+monthSnowMean+
                                                              monthHailMean+monthHazeMean+monthGlazeMean+monthSleetMean+monthWindSpeedMean+
                                                              monthFogMean+monthHeavyFogMean+monthPrecipAmountMean+monthThunderMean+monthWeatherMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeAllWeatherAvgTempMonth <- summary(lm(monthDayLengthMean+monthAvgTempMean+monthSunnyMean+monthSnowAmountMean+monthSnowMean+
                                                                       monthHailMean+monthHazeMean+monthGlazeMean+monthSleetMean+monthWindSpeedMean+
                                                                       monthFogMean+monthHeavyFogMean+monthPrecipAmountMean+monthThunderMean+monthWeatherMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideAllWeatherAvgTempMonth <- summary(lm(monthDayLengthMean+monthAvgTempMean+monthSunnyMean+monthSnowAmountMean+monthSnowMean+
                                                              monthHailMean+monthHazeMean+monthGlazeMean+monthSleetMean+monthWindSpeedMean+
                                                              monthFogMean+monthHeavyFogMean+monthPrecipAmountMean+monthThunderMean+monthWeatherMean~monthHomicideMean)))
show(sumRegAnalDayAllWeatherAvgTempMonth <- summary(lm(monthDayLengthMean+monthAvgTempMean+monthSunnyMean+monthSnowAmountMean+monthSnowMean+
                                                         monthHailMean+monthHazeMean+monthGlazeMean+monthSleetMean+monthWindSpeedMean+
                                                         monthFogMean+monthHeavyFogMean+monthPrecipAmountMean+monthThunderMean+monthWeatherMean~monthDayMean)))
show(sumRegAnalNightAllWeatherAvgTempMonth <- summary(lm(monthDayLengthMean+monthAvgTempMean+monthSunnyMean+monthSnowAmountMean+monthSnowMean+
                                                           monthHailMean+monthHazeMean+monthGlazeMean+monthSleetMean+monthWindSpeedMean+
                                                           monthFogMean+monthHeavyFogMean+monthPrecipAmountMean+monthThunderMean+monthWeatherMean~monthNightMean)))

##Multiple Regression Analysis of All Weather Variables (using Minimum Temperature) Across Monthly Crime Averages
show(sumRegAnalAllCrimeAllWeatherMinTempMonth <- summary(lm(monthDayLengthMean+monthMinTempMean+monthSunnyMean+monthSnowAmountMean+monthSnowMean+
                                                              monthHailMean+monthHazeMean+monthGlazeMean+monthSleetMean+monthWindSpeedMean+
                                                              monthFogMean+monthHeavyFogMean+monthPrecipAmountMean+monthThunderMean+monthWeatherMean~monthCrimeMean)))
show(sumRegAnalArsonAllWeatherMinTempMonth <- summary(lm(monthDayLengthMean+monthMinTempMean+monthSunnyMean+monthSnowAmountMean+monthSnowMean+
                                                           monthHailMean+monthHazeMean+monthGlazeMean+monthSleetMean+monthWindSpeedMean+
                                                           monthFogMean+monthHeavyFogMean+monthPrecipAmountMean+monthThunderMean+monthWeatherMean~monthArsonMean)))
show(sumRegAnalBurglaryAllWeatherMinTempMonth <- summary(lm(monthDayLengthMean+monthMinTempMean+monthSunnyMean+monthSnowAmountMean+monthSnowMean+
                                                              monthHailMean+monthHazeMean+monthGlazeMean+monthSleetMean+monthWindSpeedMean+
                                                              monthFogMean+monthHeavyFogMean+monthPrecipAmountMean+monthThunderMean+monthWeatherMean~monthBurglaryMean)))
show(sumRegAnalDeceptivePracticeAllWeatherMinTempMonth <- summary(lm(monthDayLengthMean+monthMinTempMean+monthSunnyMean+monthSnowAmountMean+monthSnowMean+
                                                                       monthHailMean+monthHazeMean+monthGlazeMean+monthSleetMean+monthWindSpeedMean+
                                                                       monthFogMean+monthHeavyFogMean+monthPrecipAmountMean+monthThunderMean+monthWeatherMean~monthDeceptivePracticeMean)))
show(sumRegAnalHomicideAllWeatherMinTempMonth <- summary(lm(monthDayLengthMean+monthMinTempMean+monthSunnyMean+monthSnowAmountMean+monthSnowMean+
                                                              monthHailMean+monthHazeMean+monthGlazeMean+monthSleetMean+monthWindSpeedMean+
                                                              monthFogMean+monthHeavyFogMean+monthPrecipAmountMean+monthThunderMean+monthWeatherMean~monthHomicideMean)))
show(sumRegAnalDayAllWeatherMinTempMonth <- summary(lm(monthDayLengthMean+monthMinTempMean+monthSunnyMean+monthSnowAmountMean+monthSnowMean+
                                                         monthHailMean+monthHazeMean+monthGlazeMean+monthSleetMean+monthWindSpeedMean+
                                                         monthFogMean+monthHeavyFogMean+monthPrecipAmountMean+monthThunderMean+monthWeatherMean~monthDayMean)))
show(sumRegAnalNightAllWeatherMinTempMonth <- summary(lm(monthDayLengthMean+monthMinTempMean+monthSunnyMean+monthSnowAmountMean+monthSnowMean+
                                                           monthHailMean+monthHazeMean+monthGlazeMean+monthSleetMean+monthWindSpeedMean+
                                                           monthFogMean+monthHeavyFogMean+monthPrecipAmountMean+monthThunderMean+monthWeatherMean~monthNightMean)))

##Linear Regression of Day Length Across Crime Totals
show(sumRegAnalAllCrimeDayLength <- summary(lm(Data2015$DayLengthPercent~Data2015$DateTotal)))
show(sumRegAnalArsonDayLength <- summary(lm(Data2015$DayLengthPercent~Data2015$ArsonTotal)))
show(sumRegAnalBurglaryDayLength <- summary(lm(Data2015$DayLengthPercent~Data2015$BurglaryTotal)))
show(sumRegAnalDeceptivePracticeDayLength <- summary(lm(Data2015$DayLengthPercent~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideDayLength <- summary(lm(Data2015$DayLengthPercent~Data2015$HomicideTotal)))
show(sumRegAnalDayDayLength <- summary(lm(Data2015$DayLengthPercent~Data2015$DayTotal)))
show(sumRegAnalNightDayLength <- summary(lm(Data2015$DayLengthPercent~Data2015$NightTotal)))

##Linear Regression of Maximum Temperature Across Crime Totals
show(sumRegAnalAllCrimeMaxTemp <- summary(lm(Data2015$Temp_Max.oF.~Data2015$DateTotal)))
show(sumRegAnalArsonMaxTemp <- summary(lm(Data2015$Temp_Max.oF.~Data2015$ArsonTotal)))
show(sumRegAnalBurglaryMaxTemp <- summary(lm(Data2015$Temp_Max.oF.~Data2015$BurglaryTotal)))
show(sumRegAnalDeceptivePracticeMaxTemp <- summary(lm(Data2015$Temp_Max.oF.~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideMaxTemp <- summary(lm(Data2015$Temp_Max.oF.~Data2015$HomicideTotal)))
show(sumRegAnalDayMaxTemp <- summary(lm(Data2015$Temp_Max.oF.~Data2015$DayTotal)))
show(sumRegAnalNightMaxTemp <- summary(lm(Data2015$Temp_Max.oF.~Data2015$NightTotal)))

##Linear Regression of Average Temperature Across Crime TOtals
show(sumRegAnalAllCrimeAvgTemp <- summary(lm(Data2015$Temp_Avg.oF.~Data2015$DateTotal)))
show(sumRegAnalArsonAvgTemp <- summary(lm(Data2015$Temp_Avg.oF.~Data2015$ArsonTotal)))
show(sumRegAnalBurglaryAvgTemp <- summary(lm(Data2015$Temp_Avg.oF.~Data2015$ArsonTotal)))
show(sumRegAnalDeceptivePracticeAvgTemp <- summary(lm(Data2015$Temp_Avg.oF.~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideAvgTemp <- summary(lm(Data2015$Temp_Avg.oF.~Data2015$HomicideTotal)))
show(sumRegAnalDayAvgTemp <- summary(lm(Data2015$Temp_Avg.oF.~Data2015$DayTotal)))
show(sumRegAnalNightAvgTemp <- summary(lm(Data2015$Temp_Avg.oF.~Data2015$NightTotal)))

##Linear Regression of Minimum Temperature Across Crime Totals
show(sumRegAnalAllCrimeMinTemp <- summary(lm(Data2015$Temp_Min.oF.~Data2015$DateTotal)))
show(sumRegAnalArsonMinTemp <- summary(lm(Data2015$Temp_Min.oF.~Data2015$ArsonTotal)))
show(sumRegAnalBurglaryMinTemp <- summary(lm(Data2015$Temp_Min.oF.~Data2015$ArsonTotal)))
show(sumRegAnalDeceptivePracticeMinTemp <- summary(lm(Data2015$Temp_Min.oF.~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideMinTemp <- summary(lm(Data2015$Temp_Min.oF.~Data2015$HomicideTotal)))
show(sumRegAnalDayMinTemp <- summary(lm(Data2015$Temp_Min.oF.~Data2015$DayTotal)))
show(sumRegAnalNightMinTemp <- summary(lm(Data2015$Temp_Min.oF.~Data2015$NightTotal)))

##Linear Regression of Precipiation Amount Across Crime Totals
show(sumRegAnalAllCrimePrecipAmount <- summary(lm(Data2015$Precip.in.~Data2015$DateTotal)))
show(sumRegAnalArsonPrecipAmount <- summary(lm(Data2015$Precip.in.~Data2015$ArsonTotal)))
show(sumRegAnalBurglaryPrecipAmount <- summary(lm(Data2015$Precip.in.~Data2015$ArsonTotal)))
show(sumRegAnalDeceptivePracticePrecipAmount <- summary(lm(Data2015$Precip.in.~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicidePrecipAmount <- summary(lm(Data2015$Precip.in.~Data2015$HomicideTotal)))
show(sumRegAnalDayPrecipAmount <- summary(lm(Data2015$Precip.in.~Data2015$DayTotal)))
show(sumRegAnalNightPrecipAmount <- summary(lm(Data2015$Precip.in.~Data2015$NightTotal)))

##Linear Regression of Snow Amount Across Crime Totals
show(sumRegAnalAllCrimeSnowAmount <- summary(lm(Data2015$Snow.in.~Data2015$DateTotal)))
show(sumRegAnalArsonSnowAmount <- summary(lm(Data2015$Snow.in.~Data2015$ArsonTotal)))
show(sumRegAnalBurglarySnowAmount <- summary(lm(Data2015$Snow.in.~Data2015$ArsonTotal)))
show(sumRegAnalDeceptivePracticeSnowAmount <- summary(lm(Data2015$Snow.in.~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideSnowAmount <- summary(lm(Data2015$Snow.in.~Data2015$HomicideTotal)))
show(sumRegAnalDaySnowAmount <- summary(lm(Data2015$Snow.in.~Data2015$DayTotal)))
show(sumRegAnalNightSnowAmount <- summary(lm(Data2015$Snow.in.~Data2015$NightTotal)))

##Linear Regression of Wind Speed Across Crime Totals
show(sumRegAnalAllCrimeWindSpeed <- summary(lm(Data2015$Wind.mph.~Data2015$DateTotal)))
show(sumRegAnalArsonWindSpeed <- summary(lm(Data2015$Wind.mph.~Data2015$ArsonTotal)))
show(sumRegAnalBurglaryWindSpeed <- summary(lm(Data2015$Wind.mph.~Data2015$ArsonTotal)))
show(sumRegAnalDeceptivePracticeWindSpeed <- summary(lm(Data2015$Wind.mph.~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideWindSpeed <- summary(lm(Data2015$Wind.mph.~Data2015$HomicideTotal)))
show(sumRegAnalDayWindSpeed <- summary(lm(Data2015$Wind.mph.~Data2015$DayTotal)))
show(sumRegAnalNightWindSpeed <- summary(lm(Data2015$Wind.mph.~Data2015$NightTotal)))

##Linear Regression of Fog Across Crime Totals
show(sumRegAnalAllCrimeFog <- summary(lm(Data2015$Fog~Data2015$DateTotal)))
show(sumRegAnalArsonFog <- summary(lm(Data2015$Fog~Data2015$ArsonTotal)))
show(sumRegAnalBurglaryFog <- summary(lm(Data2015$Fog~Data2015$ArsonTotal)))
show(sumRegAnalDeceptivePracticeFog <- summary(lm(Data2015$Fog~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideFog <- summary(lm(Data2015$Fog~Data2015$HomicideTotal)))
show(sumRegAnalDayFog <- summary(lm(Data2015$Fog~Data2015$DayTotal)))
show(sumRegAnalNightFog <- summary(lm(Data2015$Fog~Data2015$NightTotal)))

##Linear Regression of Heavy Fog Across Crime Totals
show(sumRegAnalAllCrimeHeavyFog <- summary(lm(Data2015$HeavyFog~Data2015$DateTotal)))
show(sumRegAnalArsonHeavyFog <- summary(lm(Data2015$HeavyFog~Data2015$ArsonTotal)))
show(sumRegAnalBurglaryHeavyFog <- summary(lm(Data2015$HeavyFog~Data2015$ArsonTotal)))
show(sumRegAnalDeceptivePracticeHeavyFog <- summary(lm(Data2015$HeavyFog~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideHeavyFog <- summary(lm(Data2015$HeavyFog~Data2015$HomicideTotal)))
show(sumRegAnalDayHeavyFog <- summary(lm(Data2015$HeavyFog~Data2015$DayTotal)))
show(sumRegAnalNightHeavyFog <- summary(lm(Data2015$HeavyFog~Data2015$NightTotal)))

##Linear Regression of Thunder Across Crime Totals
show(sumRegAnalAllCrimeThunder <- summary(lm(Data2015$Thunder~Data2015$DateTotal)))
show(sumRegAnalArsonThunder <- summary(lm(Data2015$Thunder~Data2015$ArsonTotal)))
show(sumRegAnalBurglaryThunder <- summary(lm(Data2015$Thunder~Data2015$ArsonTotal)))
show(sumRegAnalDeceptivePracticeThunder <- summary(lm(Data2015$Thunder~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideThunder <- summary(lm(Data2015$Thunder~Data2015$HomicideTotal)))
show(sumRegAnalDayThunder <- summary(lm(Data2015$Thunder~Data2015$DayTotal)))
show(sumRegAnalNightThunder <- summary(lm(Data2015$Thunder~Data2015$NightTotal)))

##Linear Regression of Sleet Across Crime Totals
show(sumRegAnalAllCrimeSleet <- summary(lm(Data2015$Sleet~Data2015$DateTotal)))
show(sumRegAnalArsonSleet <- summary(lm(Data2015$Sleet~Data2015$ArsonTotal)))
show(sumRegAnalBurglarySleet <- summary(lm(Data2015$Sleet~Data2015$ArsonTotal)))
show(sumRegAnalDeceptivePracticeSleet <- summary(lm(Data2015$Sleet~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideSleet <- summary(lm(Data2015$Sleet~Data2015$HomicideTotal)))
show(sumRegAnalDaySleet <- summary(lm(Data2015$Sleet~Data2015$DayTotal)))
show(sumRegAnalNightSleet <- summary(lm(Data2015$Sleet~Data2015$NightTotal)))

##Linear Regression of Hail Across Crime Totals
show(sumRegAnalAllCrimeHail <- summary(lm(Data2015$Hail~Data2015$DateTotal)))
show(sumRegAnalArsonHail <- summary(lm(Data2015$Hail~Data2015$ArsonTotal)))
show(sumRegAnalBurglaryHail <- summary(lm(Data2015$Hail~Data2015$ArsonTotal)))
show(sumRegAnalDeceptivePracticeHail <- summary(lm(Data2015$Hail~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideHail <- summary(lm(Data2015$Hail~Data2015$HomicideTotal)))
show(sumRegAnalDayHail <- summary(lm(Data2015$Hail~Data2015$DayTotal)))
show(sumRegAnalNightHail <- summary(lm(Data2015$Hail~Data2015$NightTotal)))

##Linear Regression of Glaze Across Crime Totals
show(sumRegAnalAllCrimeGlaze <- summary(lm(Data2015$Glaze~Data2015$DateTotal)))
show(sumRegAnalArsonGlaze <- summary(lm(Data2015$Glaze~Data2015$ArsonTotal)))
show(sumRegAnalBurglaryGlaze <- summary(lm(Data2015$Glaze~Data2015$ArsonTotal)))
show(sumRegAnalDeceptivePracticeGlaze <- summary(lm(Data2015$Glaze~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideGlaze <- summary(lm(Data2015$Glaze~Data2015$HomicideTotal)))
show(sumRegAnalDayGlaze <- summary(lm(Data2015$Glaze~Data2015$DayTotal)))
show(sumRegAnalNightGlaze <- summary(lm(Data2015$Glaze~Data2015$NightTotal)))

##Linear Regression of Haze Across Crime Totals
show(sumRegAnalAllCrimeHaze <- summary(lm(Data2015$Haze~Data2015$DateTotal)))
show(sumRegAnalArsonHaze <- summary(lm(Data2015$Haze~Data2015$ArsonTotal)))
show(sumRegAnalBurglaryHaze <- summary(lm(Data2015$Haze~Data2015$ArsonTotal)))
show(sumRegAnalDeceptivePracticeHaze <- summary(lm(Data2015$Haze~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideHaze <- summary(lm(Data2015$Haze~Data2015$HomicideTotal)))
show(sumRegAnalDayHaze <- summary(lm(Data2015$Haze~Data2015$DayTotal)))
show(sumRegAnalNightHaze <- summary(lm(Data2015$Haze~Data2015$NightTotal)))

##Linear Regression of Snow Across Crime Totals
show(sumRegAnalAllCrimeSnow <- summary(lm(Data2015$Snow~Data2015$DateTotal)))
show(sumRegAnalArsonSnow <- summary(lm(Data2015$Snow~Data2015$ArsonTotal)))
show(sumRegAnalBurglarySnow <- summary(lm(Data2015$Snow~Data2015$ArsonTotal)))
show(sumRegAnalDeceptivePracticeSnow <- summary(lm(Data2015$Snow~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideSnow <- summary(lm(Data2015$Snow~Data2015$HomicideTotal)))
show(sumRegAnalDaySnow <- summary(lm(Data2015$Snow~Data2015$DayTotal)))
show(sumRegAnalNightSnow <- summary(lm(Data2015$Snow~Data2015$NightTotal)))

##Linear Regression of Sunny Across Crime Totals
show(sumRegAnalAllCrimeSunny <- summary(lm(Data2015$Sunny~Data2015$DateTotal)))
show(sumRegAnalArsonSunny <- summary(lm(Data2015$Sunny~Data2015$ArsonTotal)))
show(sumRegAnalBurglarySunny <- summary(lm(Data2015$Sunny~Data2015$ArsonTotal)))
show(sumRegAnalDeceptivePracticeSunny <- summary(lm(Data2015$Sunny~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideSunny <- summary(lm(Data2015$Sunny~Data2015$HomicideTotal)))
show(sumRegAnalDaySunny <- summary(lm(Data2015$Sunny~Data2015$DayTotal)))
show(sumRegAnalNightSunny <- summary(lm(Data2015$Sunny~Data2015$NightTotal)))

##Linear Regression of Weather Condition Across Crime Totals
show(sumRegAnalAllCrimeWeather <- summary(lm(Data2015$WeatherCondition~Data2015$DateTotal)))
show(sumRegAnalArsonWeather <- summary(lm(Data2015$WeatherCondition~Data2015$ArsonTotal)))
show(sumRegAnalBurglaryWeather <- summary(lm(Data2015$WeatherCondition~Data2015$ArsonTotal)))
show(sumRegAnalDeceptivePracticeWeather <- summary(lm(Data2015$WeatherCondition~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideWeather <- summary(lm(Data2015$WeatherCondition~Data2015$HomicideTotal)))
show(sumRegAnalDayWeather <- summary(lm(Data2015$WeatherCondition~Data2015$DayTotal)))
show(sumRegAnalNightWeather <- summary(lm(Data2015$WeatherCondition~Data2015$NightTotal)))

##Multiple Regression Analysis of Daylength, Maximum Temperature, and Sunny Totals Across Crime Totals
show(sumRegAnalAllCrimeDayMaxSunnyDaily <- summary(lm(Data2015$DayLengthPercent+Data2015$Temp_Max.oF.+Data2015$Sunny~Data2015$DateTotal)))
show(sumRegAnalArsonDayMaxSunnyDaily <- summary(lm(Data2015$DayLengthPercent+Data2015$Temp_Max.oF.+Data2015$Sunny~Data2015$ArsonTotal)))
show(sumRegAnalBurglaryDayMaxSunnyDaily <- summary(lm(Data2015$DayLengthPercent+Data2015$Temp_Max.oF.+Data2015$Sunny~Data2015$BurglaryTotal)))
show(sumRegAnalDeceptivePracticeDayMaxSunnyDaily <- summary(lm(Data2015$DayLengthPercent+Data2015$Temp_Max.oF.+Data2015$Sunny~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideDayMaxSunnyDaily <- summary(lm(Data2015$DayLengthPercent+Data2015$Temp_Max.oF.+Data2015$Sunny~Data2015$HomicideTotal)))
show(sumRegAnalDayDayMaxSunnyDaily <- summary(lm(Data2015$DayLengthPercent+Data2015$Temp_Max.oF.+Data2015$Sunny~Data2015$DayTotal)))
show(sumRegAnalNightDayMaxSunnyDaily <- summary(lm(Data2015$DayLengthPercent+Data2015$Temp_Max.oF.+Data2015$Sunny~Data2015$NightTotal)))

##Multiple Regression Analysis of Daylength, Average Temperature, and Sunny Totals Across Crime Totals
show(sumRegAnalAllCrimeDayAvgSunnyDaily <- summary(lm(Data2015$DayLengthPercent+Data2015$Temp_Avg.oF.+Data2015$Sunny~Data2015$DateTotal)))
show(sumRegAnalArsonDayAvgSunnyDaily <- summary(lm(Data2015$DayLengthPercent+Data2015$Temp_Avg.oF.+Data2015$Sunny~Data2015$ArsonTotal)))
show(sumRegAnalBurglaryDayAvgSunnyDaily <- summary(lm(Data2015$DayLengthPercent+Data2015$Temp_Avg.oF.+Data2015$Sunny~Data2015$BurglaryTotal)))
show(sumRegAnalDeceptivePracticeDayAvgSunnyDaily <- summary(lm(Data2015$DayLengthPercent+Data2015$Temp_Avg.oF.+Data2015$Sunny~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideDayAvgSunnyDaily <- summary(lm(Data2015$DayLengthPercent+Data2015$Temp_Avg.oF.+Data2015$Sunny~Data2015$HomicideTotal)))
show(sumRegAnalDayDayAvgSunnyDaily <- summary(lm(Data2015$DayLengthPercent+Data2015$Temp_Avg.oF.+Data2015$Sunny~Data2015$DayTotal)))
show(sumRegAnalNightDayAvgSunnyDaily <- summary(lm(Data2015$DayLengthPercent+Data2015$Temp_Avg.oF.+Data2015$Sunny~Data2015$NightTotal)))

##Multiple Regression Analysis of Daylength, Minimum Temperature, and Sunny Totals Across Crime Totals
show(sumRegAnalAllCrimeDayMinSunnyDaily <- summary(lm(Data2015$DayLengthPercent+Data2015$Temp_Min.oF.+Data2015$Sunny~Data2015$DateTotal)))
show(sumRegAnalArsonDayMinSunnyDaily <- summary(lm(Data2015$DayLengthPercent+Data2015$Temp_Min.oF.+Data2015$Sunny~Data2015$ArsonTotal)))
show(sumRegAnalBurglaryDayMinSunnyDaily <- summary(lm(Data2015$DayLengthPercent+Data2015$Temp_Min.oF.+Data2015$Sunny~Data2015$BurglaryTotal)))
show(sumRegAnalDeceptivePracticeDayMinSunnyDaily <- summary(lm(Data2015$DayLengthPercent+Data2015$Temp_Min.oF.+Data2015$Sunny~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideDayMinSunnyDaily <- summary(lm(Data2015$DayLengthPercent+Data2015$Temp_Min.oF.+Data2015$Sunny~Data2015$HomicideTotal)))
show(sumRegAnalDayDayMinSunnyDaily <- summary(lm(Data2015$DayLengthPercent+Data2015$Temp_Min.oF.+Data2015$Sunny~Data2015$DayTotal)))
show(sumRegAnalNightDayMinSunnyDaily <- summary(lm(Data2015$DayLengthPercent+Data2015$Temp_Min.oF.+Data2015$Sunny~Data2015$NightTotal)))

##Multiple Regression Analysis of All Weather Variables (using Maximum Temperature) Across Monthly Crime Averages
show(sumRegAnalAllCrimeAllWeatherMaxTempDaily <- summary(lm(Data2015$Temp_Max.oF.+Data2015$DayLengthPercent+Data2015$Sunny+Data2015$Snow+Data2015$Snow.in.+
                                                              Data2015$Haze+Data2015$Glaze+Data2015$Hail+Data2015$Sleet+Data2015$Thunder+
                                                              Data2015$HeavyFog+Data2015$Fog+Data2015$WeatherCondition+Data2015$Wind.mph.+Data2015$Precip.in.~Data2015$DateTotal)))
show(sumRegAnalArsonAllWeatherMaxTempDaily <- summary(lm(Data2015$Temp_Max.oF.+Data2015$DayLengthPercent+Data2015$Sunny+Data2015$Snow+Data2015$Snow.in.+
                                                           Data2015$Haze+Data2015$Glaze+Data2015$Hail+Data2015$Sleet+Data2015$Thunder+
                                                           Data2015$HeavyFog+Data2015$Fog+Data2015$WeatherCondition+Data2015$Wind.mph.+Data2015$Precip.in.~Data2015$ArsonTotal)))
show(sumRegAnalBurglaryAllWeatherMaxTempDaily <- summary(lm(Data2015$Temp_Max.oF.+Data2015$DayLengthPercent+Data2015$Sunny+Data2015$Snow+Data2015$Snow.in.+
                                                              Data2015$Haze+Data2015$Glaze+Data2015$Hail+Data2015$Sleet+Data2015$Thunder+
                                                              Data2015$HeavyFog+Data2015$Fog+Data2015$WeatherCondition+Data2015$Wind.mph.+Data2015$Precip.in.~Data2015$BurglaryTotal)))
show(sumRegAnalDeceptivePracticeAllWeatherMaxTempDaily <- summary(lm(Data2015$Temp_Max.oF.+Data2015$DayLengthPercent+Data2015$Sunny+Data2015$Snow+Data2015$Snow.in.+
                                                                       Data2015$Haze+Data2015$Glaze+Data2015$Hail+Data2015$Sleet+Data2015$Thunder+
                                                                       Data2015$HeavyFog+Data2015$Fog+Data2015$WeatherCondition+Data2015$Wind.mph.+
                                                                       Data2015$Precip.in.~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideAllWeatherMaxTempDaily <- summary(lm(Data2015$Temp_Max.oF.+Data2015$DayLengthPercent+Data2015$Sunny+Data2015$Snow+Data2015$Snow.in.+
                                                              Data2015$Haze+Data2015$Glaze+Data2015$Hail+Data2015$Sleet+Data2015$Thunder+
                                                              Data2015$HeavyFog+Data2015$Fog+Data2015$WeatherCondition+Data2015$Wind.mph.+Data2015$Precip.in.~Data2015$HomicideTotal)))
show(sumRegAnalDayAllWeatherMaxTempDaily <- summary(lm(Data2015$Temp_Max.oF.+Data2015$DayLengthPercent+Data2015$Sunny+Data2015$Snow+Data2015$Snow.in.+
                                                         Data2015$Haze+Data2015$Glaze+Data2015$Hail+Data2015$Sleet+Data2015$Thunder+
                                                         Data2015$HeavyFog+Data2015$Fog+Data2015$WeatherCondition+Data2015$Wind.mph.+Data2015$Precip.in.~Data2015$DayTotal)))
show(sumRegAnalNightAllWeatherMaxTempDaily <- summary(lm(Data2015$Temp_Max.oF.+Data2015$DayLengthPercent+Data2015$Sunny+Data2015$Snow+Data2015$Snow.in.+
                                                           Data2015$Haze+Data2015$Glaze+Data2015$Hail+Data2015$Sleet+Data2015$Thunder+
                                                           Data2015$HeavyFog+Data2015$Fog+Data2015$WeatherCondition+Data2015$Wind.mph.+Data2015$Precip.in.~Data2015$NightTotal)))

##Multiple Regression Analysis of All Weather Variables (using Average Temperature) Across Crime Totals
show(sumRegAnalAllCrimeAllWeatherAvgTempDaily <- summary(lm(Data2015$Temp_Avg.oF.+Data2015$DayLengthPercent+Data2015$Sunny+Data2015$Snow+Data2015$Snow.in.+
                                                              Data2015$Haze+Data2015$Glaze+Data2015$Hail+Data2015$Sleet+Data2015$Thunder+
                                                              Data2015$HeavyFog+Data2015$Fog+Data2015$WeatherCondition+Data2015$Wind.mph.+Data2015$Precip.in.~Data2015$DateTotal)))
show(sumRegAnalArsonAllWeatherAvgTempDaily <- summary(lm(Data2015$Temp_Avg.oF.+Data2015$DayLengthPercent+Data2015$Sunny+Data2015$Snow+Data2015$Snow.in.+
                                                           Data2015$Haze+Data2015$Glaze+Data2015$Hail+Data2015$Sleet+Data2015$Thunder+
                                                           Data2015$HeavyFog+Data2015$Fog+Data2015$WeatherCondition+Data2015$Wind.mph.+Data2015$Precip.in.~Data2015$ArsonTotal)))
show(sumRegAnalBurglaryAllWeatherAvgTempDaily <- summary(lm(Data2015$Temp_Avg.oF.+Data2015$DayLengthPercent+Data2015$Sunny+Data2015$Snow+Data2015$Snow.in.+
                                                              Data2015$Haze+Data2015$Glaze+Data2015$Hail+Data2015$Sleet+Data2015$Thunder+
                                                              Data2015$HeavyFog+Data2015$Fog+Data2015$WeatherCondition+Data2015$Wind.mph.+Data2015$Precip.in.~Data2015$BurglaryTotal)))
show(sumRegAnalDeceptivePracticeAllWeatherAvgTempDaily <- summary(lm(Data2015$Temp_Avg.oF.+Data2015$DayLengthPercent+Data2015$Sunny+Data2015$Snow+Data2015$Snow.in.+
                                                                       Data2015$Haze+Data2015$Glaze+Data2015$Hail+Data2015$Sleet+Data2015$Thunder+
                                                                       Data2015$HeavyFog+Data2015$Fog+Data2015$WeatherCondition+Data2015$Wind.mph.+
                                                                       Data2015$Precip.in.~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideAllWeatherAvgTempDaily <- summary(lm(Data2015$Temp_Avg.oF.+Data2015$DayLengthPercent+Data2015$Sunny+Data2015$Snow+Data2015$Snow.in.+
                                                              Data2015$Haze+Data2015$Glaze+Data2015$Hail+Data2015$Sleet+Data2015$Thunder+
                                                              Data2015$HeavyFog+Data2015$Fog+Data2015$WeatherCondition+Data2015$Wind.mph.+Data2015$Precip.in.~Data2015$HomicideTotal)))
show(sumRegAnalDayAllWeatherAvgTempDaily <- summary(lm(Data2015$Temp_Avg.oF.+Data2015$DayLengthPercent+Data2015$Sunny+Data2015$Snow+Data2015$Snow.in.+
                                                         Data2015$Haze+Data2015$Glaze+Data2015$Hail+Data2015$Sleet+Data2015$Thunder+
                                                         Data2015$HeavyFog+Data2015$Fog+Data2015$WeatherCondition+Data2015$Wind.mph.+Data2015$Precip.in.~Data2015$DayTotal)))
show(sumRegAnalNightAllWeatherAvgTempDaily <- summary(lm(Data2015$Temp_Avg.oF.+Data2015$DayLengthPercent+Data2015$Sunny+Data2015$Snow+Data2015$Snow.in.+
                                                           Data2015$Haze+Data2015$Glaze+Data2015$Hail+Data2015$Sleet+Data2015$Thunder+
                                                           Data2015$HeavyFog+Data2015$Fog+Data2015$WeatherCondition+Data2015$Wind.mph.+Data2015$Precip.in.~Data2015$NightTotal)))

##Multiple Regression Analysis of All Weather Variables (using Minimum Temperature) Across Monthly Crime Averages
show(sumRegAnalAllCrimeAllWeatherMinTempDaily <- summary(lm(Data2015$Temp_Min.oF.+Data2015$DayLengthPercent+Data2015$Sunny+Data2015$Snow+Data2015$Snow.in.+
                                                              Data2015$Haze+Data2015$Glaze+Data2015$Hail+Data2015$Sleet+Data2015$Thunder+
                                                              Data2015$HeavyFog+Data2015$Fog+Data2015$WeatherCondition+Data2015$Wind.mph.+Data2015$Precip.in.~Data2015$DateTotal)))
show(sumRegAnalArsonAllWeatherMinTempDaily <- summary(lm(Data2015$Temp_Min.oF.+Data2015$DayLengthPercent+Data2015$Sunny+Data2015$Snow+Data2015$Snow.in.+
                                                           Data2015$Haze+Data2015$Glaze+Data2015$Hail+Data2015$Sleet+Data2015$Thunder+
                                                           Data2015$HeavyFog+Data2015$Fog+Data2015$WeatherCondition+Data2015$Wind.mph.+Data2015$Precip.in.~Data2015$ArsonTotal)))
show(sumRegAnalBurglaryAllWeatherMinTempDaily <- summary(lm(Data2015$Temp_Min.oF.+Data2015$DayLengthPercent+Data2015$Sunny+Data2015$Snow+Data2015$Snow.in.+
                                                              Data2015$Haze+Data2015$Glaze+Data2015$Hail+Data2015$Sleet+Data2015$Thunder+
                                                              Data2015$HeavyFog+Data2015$Fog+Data2015$WeatherCondition+Data2015$Wind.mph.+Data2015$Precip.in.~Data2015$BurglaryTotal)))
show(sumRegAnalDeceptivePracticeAllWeatherMinTempDaily <- summary(lm(Data2015$Temp_Min.oF.+Data2015$DayLengthPercent+Data2015$Sunny+Data2015$Snow+Data2015$Snow.in.+
                                                                       Data2015$Haze+Data2015$Glaze+Data2015$Hail+Data2015$Sleet+Data2015$Thunder+
                                                                       Data2015$HeavyFog+Data2015$Fog+Data2015$WeatherCondition+Data2015$Wind.mph.+
                                                                       Data2015$Precip.in.~Data2015$DeceptivePracticeTotal)))
show(sumRegAnalHomicideAllWeatherMinTempDaily <- summary(lm(Data2015$Temp_Min.oF.+Data2015$DayLengthPercent+Data2015$Sunny+Data2015$Snow+Data2015$Snow.in.+
                                                              Data2015$Haze+Data2015$Glaze+Data2015$Hail+Data2015$Sleet+Data2015$Thunder+
                                                              Data2015$HeavyFog+Data2015$Fog+Data2015$WeatherCondition+Data2015$Wind.mph.+Data2015$Precip.in.~Data2015$HomicideTotal)))
show(sumRegAnalDayAllWeatherMinTempDaily <- summary(lm(Data2015$Temp_Min.oF.+Data2015$DayLengthPercent+Data2015$Sunny+Data2015$Snow+Data2015$Snow.in.+
                                                         Data2015$Haze+Data2015$Glaze+Data2015$Hail+Data2015$Sleet+Data2015$Thunder+
                                                         Data2015$HeavyFog+Data2015$Fog+Data2015$WeatherCondition+Data2015$Wind.mph.+Data2015$Precip.in.~Data2015$DayTotal)))
show(sumRegAnalNightAllWeatherMinTempDaily <- summary(lm(Data2015$Temp_Min.oF.+Data2015$DayLengthPercent+Data2015$Sunny+Data2015$Snow+Data2015$Snow.in.+
                                                           Data2015$Haze+Data2015$Glaze+Data2015$Hail+Data2015$Sleet+Data2015$Thunder+
                                                           Data2015$HeavyFog+Data2015$Fog+Data2015$WeatherCondition+Data2015$Wind.mph.+Data2015$Precip.in.~Data2015$NightTotal)))