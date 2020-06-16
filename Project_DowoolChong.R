racedata <- read.csv("Provisional_Death_Count_by_Race.csv" , header = T)
agedata <- read.csv("Provisional_Death_Count_by_Age.csv", header = T)
ihme <- read.csv("IHME_Data.csv", header = T)

#####################  Data Cleaning ##################### 
racedata <- as.data.frame(racedata)
summary(racedata)
racedata$state <- as.factor(racedata$state)
racedata$age <- as.factor(racedata$age)
racedata$race <- as.factor(racedata$race)
racedata$covid <- as.numeric(racedata$covid)
racedata$tot_death <- as.numeric(racedata$tot_death)
racedata$pneumonia <- as.numeric(racedata$pneumonia)
racedata$pneum_covid <- as.numeric(racedata$pneum_covid)
racedata$influenza <- as.numeric(racedata$influenza)
racedata$pneu_flu_covid <- as.numeric(racedata$pneu_flu_covid)

#####################  Merge ##################### 
finaldeathcounts <- merge(racedata, agedata, by = c("state", "age", "covid", "tot_death", "pneumonia", "pneum_covid", "influenza", "pneu_flu_covid"), all = T)

summary(finaldeathcounts)

finaldeathcounts$sex <- as.factor(finaldeathcounts$sex)

##  Reorder the columns in a data frame
finaldeathcounts <- finaldeathcounts[c(1,2,9,10,3,4,5,6,7,8)]

## Order of Age Group
finaldeathcounts$age <- factor(finaldeathcounts$age, levels = c("25-34 years", "35-44 years", "45-54 years", "55-64 years", "65-74 years", "75-84 years", "85 years and over"))

## Remove NAs in All Death Counts Variables
finaldeathcounts$covid[is.na(finaldeathcounts$covid)] <- 0
finaldeathcounts$tot_death[is.na(finaldeathcounts$tot_death)] <- 0
finaldeathcounts$pneumonia[is.na(finaldeathcounts$pneumonia)] <- 0
finaldeathcounts$pneum_covid[is.na(finaldeathcounts$pneum_covid)] <- 0
finaldeathcounts$influenza[is.na(finaldeathcounts$influenza)] <- 0
finaldeathcounts$pneu_flu_covid[is.na(finaldeathcounts$pneu_flu_covid)] <- 0


##################### SUMMARY STATISTICS #####################

## Box Plot (Deaths Counts by Race)

## Remove NAs for Box Plots by Race
counts_by_race <- finaldeathcounts[!is.na(finaldeathcounts$race),]
counts_by_race$sex <- NULL

## Order of Race Group
counts_by_race$race <- factor(counts_by_race$race, levels = c("White", "Black", "Asian", "Hispanic", "More race"))

boxplot(finaldeathcounts$covid~finaldeathcounts$race, main = "COVID-19 Deaths by Race", xlab = "Race", ylab = "Count", col = "skyblue")
boxplot(finaldeathcounts$pneumonia~finaldeathcounts$race, main = "Pneumonia Deaths by Race", xlab = "Race", ylab = "Count", col = "orange")
boxplot(finaldeathcounts$influenza~finaldeathcounts$race, main = "Influenza Deaths by Race", xlab = "Race", ylab = "Count", col = "yellow")

## Box Plot (Deaths Counts by Sex)
boxplot(finaldeathcounts$covid~finaldeathcounts$sex, main = "COVID-19 Deaths by Sex", xlab = "Sex", ylab = "Count", col = "red")
boxplot(finaldeathcounts$pneumonia~finaldeathcounts$sex, main = "Pneumonia Deaths by Sex", xlab = "Sex", ylab = "Count", col = "red")
boxplot(finaldeathcounts$influenza~finaldeathcounts$sex, main = "Influenza Deaths by Sex", xlab = "Sex", ylab = "Count", col = "red")

## Box Plot (Deaths Counts by Age Group)
boxplot(finaldeathcounts$covid~finaldeathcounts$age, main = "COVID-19 Deaths by Age", xlab = "Age", ylab = "Count", col = "Orange")
boxplot(finaldeathcounts$pneumonia~finaldeathcounts$age, main = "Pneumonia Deaths by Age", xlab = "Age", ylab = "Count", col = "Orange")
boxplot(finaldeathcounts$influenza~finaldeathcounts$age, main = "Influenza Deaths by Age", xlab = "Age", ylab = "Count", col = "Orange")

## Scatter Plot (Relationship betweem Pneumonia & COVID-19 Deaths by State)
plot(finaldeathcounts$pneumonia[finaldeathcounts$state == "California"], finaldeathcounts$covid[finaldeathcounts$state == "California"], main = "Relationship betweem Pneumonia & COVID-19 Deaths by State", xlab = "Pnuemonia Deaths", ylab = "COVID-19 Deaths", pch = 17, col = "blue")
points(finaldeathcounts$pneumonia[finaldeathcounts$state == "Massachusetts"], finaldeathcounts$covid[finaldeathcounts$state == "Massachusetts"], pch = 18, col = "green")
points(finaldeathcounts$pneumonia[finaldeathcounts$state == "New Jersey"], finaldeathcounts$covid[finaldeathcounts$state == "New Jersey"], pch = 19, col = "black")
points(finaldeathcounts$pneumonia[finaldeathcounts$state == "New York"], finaldeathcounts$covid[finaldeathcounts$state == "New York"], pch = 20, col = "orange")
points(finaldeathcounts$pneumonia[finaldeathcounts$state == "New York City"], finaldeathcounts$covid[finaldeathcounts$state == "New York City"], pch = 21, col = "red")
points(finaldeathcounts$pneumonia[finaldeathcounts$state == "Pennsylvania"], finaldeathcounts$covid[finaldeathcounts$state == "Pennsylvania"], pch = 22, col = "purple")
legend("topright", c("California", "Massachusetts", "New Jersey", "New York", "New York City", "Pennsylvania"), col = c("blue", "green", "black", "orange", "red", "purple"),pch = c(17,18,19,20,21,22))

## Histogram (Average Daily COVID-19 Death by States)
hist(ihme$mean_daily_death[ihme$state == "California"], col = rgb(1,0,0,0.5), main = "Average Daily COVID-19 Death in California", xlab = "California(06/01 - 08/04/2020)", xlim = c(0, 15), breaks = 10)
hist(ihme$mean_daily_death[ihme$state == "Massachusetts"], col = rgb(0,0,1,0.5), main = "Average Daily COVID-19 Death in Massachusetts", xlab = "Massachusetts(06/01 - 08/04/2020)", xlim = c(0, 100), breaks = 10)
hist(ihme$mean_daily_death[ihme$state == "New Jersey"], col = rgb(0,1,0,0.5), main = "Average Daily COVID-19 Death in New Jersey", xlab = "New Jersey(06/01 - 08/04/2020)", xlim = c(0, 100), breaks = 10)
hist(ihme$mean_daily_death[ihme$state == "New York"], col = rgb(1,0,1,0.5), main = "Average Daily COVID-19 Death in New York", xlab = "New York(06/01 - 08/04/2020)", xlim = c(0, 100), breaks = 10)
hist(ihme$mean_daily_death[ihme$state == "Pennsylvania"], col = rgb(0,1,1,0.5), main = "Average Daily COVID-19 Death in Pennsylvania", xlab = "Pennsylvania(06/01 - 08/04/2020)", xlim = c(0, 120), breaks = 10)
hist(ihme$mean_daily_death[ihme$state == "United States of America"], col = rgb(1,1,0,0.5), main = "Average Daily COVID-19 Death in United States", xlab = "United States(06/01 - 08/04/2020)", xlim = c(0, 1200), breaks = 10)

## Histogram (Average Daily Hospital Admission by States)
hist(ihme$admis_mean[ihme$state == "California"], col = rgb(1,0,0,0.5), main = "Average Daily Hospital Admission in California", xlab = "California(06/01 - 08/04/2020)", xlim = c(0, 100), breaks = 10)
hist(ihme$admis_mean[ihme$state == "Massachusetts"], col = rgb(0,0,1,0.5), main = "Average Daily Hospital Admission in Massachusetts", xlab = "Massachusetts(06/01 - 08/04/2020)", xlim = c(0, 200), breaks = 10)
hist(ihme$admis_mean[ihme$state == "New Jersey"], col = rgb(0,1,0,0.5), main = "Average Daily Hospital Admission in New Jersey", xlab = "New Jersey(06/01 - 08/04/2020)", xlim = c(0, 400), breaks = 10)
hist(ihme$admis_mean[ihme$state == "New York"], col = rgb(1,0,1,0.5), main = "Average Daily Hospital Admission in New York", xlab = "New York(06/01 - 08/04/2020)", xlim = c(0, 100), breaks = 10)
hist(ihme$admis_mean[ihme$state == "Pennsylvania"], col = rgb(0,1,1,0.5), main = "Average Daily Hospital Admission in Pennsylvania", xlab = "Pennsylvania(06/01 - 08/04/2020)", xlim = c(0, 500), breaks = 10)
hist(ihme$admis_mean[ihme$state == "United States of America"], col = rgb(1,1,0,0.5), main = "Average Daily Hospital Admission in United States", xlab = "United States(06/01 - 08/04/2020)", xlim = c(0, 5000), breaks = 10)

## Histogram (Average New Daily ICU Admission by States)
hist(ihme$newICU_mean[ihme$state == "California"], col = rgb(1,0,0,0.5), main = "Average New Daily ICU Admission in California", xlab = "California(06/01 - 08/04/2020)", xlim = c(0, 20), breaks = 10)
hist(ihme$newICU_mean[ihme$state == "Massachusetts"], col = rgb(0,0,1,0.5), main = "Average New Daily ICU Admission in Massachusetts", xlab = "Massachusetts(06/01 - 08/04/2020)", xlim = c(0, 80), breaks = 10)
hist(ihme$newICU_mean[ihme$state == "New Jersey"], col = rgb(0,1,0,0.5), main = "Average New Daily ICU Admission in New Jersey", xlab = "New Jersey(06/01 - 08/04/2020)", xlim = c(0, 120), breaks = 10)
hist(ihme$newICU_mean[ihme$state == "New York"], col = rgb(1,0,1,0.5), main = "Average New Daily ICU Admission in New York", xlab = "New York(06/01 - 08/04/2020)", xlim = c(0, 100), breaks = 10)
hist(ihme$newICU_mean[ihme$state == "Pennsylvania"], col = rgb(0,1,1,0.5), main = "Average New Daily ICU Admission in Pennsylvania", xlab = "Pennsylvania(06/01 - 08/04/2020)", xlim = c(0, 150), breaks = 10)
hist(ihme$newICU_mean[ihme$state == "United States of America"], col = rgb(1,1,0,0.5), main = "Average New Daily ICU Admission in United States", xlab = "United States(06/01 - 08/04/2020)", xlim = c(0, 1500), breaks = 10)


write.csv(finaldeathcounts,file = "finaldeathcounts.csv")


