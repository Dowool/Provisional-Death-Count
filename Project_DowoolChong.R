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


############### Use various EDA and simple statistical analysis techniques to gain a deep understanding for the data. #############
# Using summary() and str() for summary statistics
summary(finaldeathcounts$covid)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   22.25   99.00  330.79  356.00 2629.00

summary(finaldeathcounts$pneumonia)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.0    20.0    85.0   271.4   375.8  2233.0 

summary(finaldeathcounts$influenza)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    0.00    0.00   14.39   20.75  145.00 

str(finaldeathcounts)
# 'data.frame':	266 obs. of  10 variables:
# $ state         : Factor w/ 6 levels "California","Massachusetts",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ age           : Factor w/ 7 levels "25-34 years",..: 1 1 1 1 1 1 1 2 2 2 ...
# $ race          : Factor w/ 6 levels "Asian","Black",..: NA 4 1 2 NA 6 3 4 1 NA ...
# $ sex           : Factor w/ 2 levels "Female","Male": 2 NA NA NA 1 NA NA NA NA 1 ...
# $ covid         : num  16 0 0 0 0 0 0 0 1 17 ...
# $ tot_death     : num  1291 39 118 219 477 ...
# $ pneumonia     : num  37 0 0 0 17 16 26 0 15 46 ...
# $ pneum_covid   : num  0 0 0 0 0 0 0 0 0 0 ...
# $ influenza     : num  0 0 0 0 0 0 0 0 0 0 ...
# $ pneu_flu_covid: num  53 0 0 10 27 22 39 0 16 65 ...


# Variable correlation with cor() in R. 
# The Pearson correlation assumes the random variables to be 
# normally distributed.

# highly correlated variables
cor(finaldeathcounts$covid, finaldeathcounts$pneumonia)
# [1] 0.8613225

cor(finaldeathcounts$covid, finaldeathcounts$influenza)
# [1] 0.7209334

cor(finaldeathcounts$pneumonia, finaldeathcounts$influenza)
# [1] 0.7316998

# Example: weak correlation, negative correlation: larger Wind 
# tends to have smaller Temp
cor(airquality$Wind, airquality$Temp, method="pearson")
# [1] -0.4579879



##################### SUMMARY STATISTICS #####################

## Box Plot (Deaths Counts by Race)

## Remove NAs for Box Plots by Race
counts_by_race <- finaldeathcounts[!is.na(finaldeathcounts$race),]
counts_by_race$sex <- NULL

## Order of Race Group
counts_by_race$race <- factor(counts_by_race$race, levels = c("White", "Black", "Asian", "Hispanic", "More race"))

##  Box Plot (Deaths Counts by Race)
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


## Pie Chart of Population by Race in State ##
capop <- read.csv("Cali_Population.csv", header = T)
capop$Race <- as.factor(capop$Race)
capop$Population <- as.numeric(capop$Population)
capop$Total.Percentage < as.numeric(capop$Total.Percentage)
summary(capop)
pie(capop$Population[capop$Race != "All Races"], labels = capop$Race[capop$Race != "All Races"], main="Population by Race in California")

mapop <- read.csv("MA_Population.csv", header = T)
mapop$Race <- as.factor(mapop$Race)
mapop$Population <- as.numeric(mapop$Population)
mapop$Total.Percentage < as.numeric(mapop$Total.Percentage)
summary(mapop)
pie(mapop$Population[mapop$Race != "Total Population"], labels = mapop$Race[mapop$Race != "Total Population"], main="Population by Race in Masschusetts")

njpop <- read.csv("NJ_Population.csv", header = T)
njpop$Race <- as.factor(njpop$Race)
njpop$Population <- as.numeric(njpop$Population)
njpop$Total.Percentage < as.numeric(njpop$Total.Percentage)
summary(njpop)
pie(njpop$Population[njpop$Race != "Total Population"], labels = njpop$Race[njpop$Race != "Total Population"], main="Population by Race in New Jersey")

nypop <- read.csv("NY_Population.csv", header = T)
nypop$Race <- as.factor(nypop$Race)
nypop$Population <- as.numeric(nypop$Population)
nypop$Total.Percentage < as.numeric(nypop$Total.Percentage)
summary(nypop)
pie(nypop$Population[nypop$Race != "Total Population"], labels = nypop$Race[nypop$Race != "Total Population"], main="Population by Race in New York")

papop <- read.csv("PA_Population.csv", header = T)
papop$Race <- as.factor(papop$Race)
papop$Population <- as.numeric(papop$Population)
papop$Total.Percentage < as.numeric(papop$Total.Percentage)
summary(papop)
pie(papop$Population[papop$Race != "Total Population"], labels = papop$Race[papop$Race != "Total Population"], main="Population by Race in Pennsylvania")




library(UsingR)

#plot pneumonia by covid
#ok
plot(finaldeathcounts$pneumonia, finaldeathcounts$covid)

#only transformation that works
plot(sqrt(finaldeathcounts$pneumonia), finaldeathcounts$covid, col="green", pch=19)
#not good
plot((finaldeathcounts$pneumonia)^2, finaldeathcounts$covid)
#not good
plot(log(finaldeathcounts$pneumonia), finaldeathcounts$covid)


#plot influenza by covid; no linear relationship
plot(finaldeathcounts$influenza, finaldeathcounts$covid)

plot(log(finaldeathcounts$influenza), finaldeathcounts$covid)
#not good
plot(sqrt(finaldeathcounts$influenza), finaldeathcounts$covid)
#not good
plot((finaldeathcounts$influenza)^2, finaldeathcounts$covid)


# from the above plots,these variables will be used for the diagnostic plots
# finaldeathcounts$pneumonia, finaldeathcounts$covid, sqrt(finaldeathcounts$pneumonia)

m0 <- lm(covid ~ pneumonia + influenza, data = finaldeathcounts)
summary(m0)

# delete influenza, it is not significant
# Call:
#   lm(formula = covid ~ pneumonia + influenza, data = finaldeathcounts)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1362.78   -16.22     1.05    69.01   820.31 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 12.88791   19.00980   0.678    0.498    
# pneumonia    0.95608    0.05903  16.198  < 2e-16 ***
# influenza    4.05666    0.92171   4.401 1.57e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 252.5 on 263 degrees of freedom
# Multiple R-squared:  0.7596,	Adjusted R-squared:  0.7578 
# F-statistic: 415.5 on 2 and 263 DF,  p-value: < 2.2e-16

plot(finaldeathcounts$pneumonia, finaldeathcounts$covid, col="green", pch=19)

m1 <- lm(covid~pneumonia, data = finaldeathcounts)
summary(m1)

#significant, adjusted r^2 = 0.7409
par(mfrow = c(1,1))
plot(m1)

## Residuals vs Fitted
# Because residuals have non-linear patterns and equally spread around a horizontal line without distinct patterns. 
# This is a good indication this doesn’t have non-linear relationships.

##  Normal Q-Q
# The plot should show if residuals are not normally distributed. 
# This plot concerns me because of the larger value observations at the end than what we would expect under the standard modeling assumptions.

##  Scale-Location
# The  plot should show if residuals are spread equally along with the ranges of predictors. 
# In the plot, because the residuals spread wider, the red smooth line is not horizontal and shows a steep angle.

## Resideuals vs Leverage
# The points I need to focus on are values in the upper right or lower right corners, which are outside the red dashed Cook’s distance line because these are points that would be influential in the model and removing them would likely noticeably alter the regression results. 
# qqplot is off, cooks distance is off by 47 & 48


# FINAL MODEL
m2 <- lm(finaldeathcounts$covid ~ I(sqrt(finaldeathcounts$pneumonia)))
summary(m2)
# Call:
#   lm(formula = finaldeathcounts$covid ~ I(sqrt(finaldeathcounts$pneumonia)))

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1009.29   -95.08     5.44   130.21  1359.76 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         -173.156     27.098   -6.39 7.47e-10 ***
#   I(sqrt(finaldeathcounts$pneumonia))   40.051      1.645   24.35  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 285.3 on 264 degrees of freedom
# Multiple R-squared:  0.6919,	Adjusted R-squared:  0.6908 
# F-statistic:   593 on 1 and 264 DF,  p-value: < 2.2e-16

#significant, adjusted r^2 = 0.6908 
plot(m2)

## Residuals vs Fitted
# Because residuals have non-linear patterns and equally spread around a horizontal line without distinct patterns. 
# This is a good indication this doesn’t have non-linear relationships.

##  Normal Q-Q
# The plot should show if residuals are not normally distributed. 
# This plot concerns me because of the larger value observations at the end than what we would expect under the standard modeling assumptions.

##  Scale-Location
# The  plot should show if residuals are spread equally along with the ranges of predictors. 
# In the plot, because the residuals spread wider, the red smooth line is not horizontal and shows a steep angle.

## Resideuals vs Leverage
# The points I need to focus on are values in the upper right or lower right corners, which are outside the red dashed Cook’s distance line because these are points that would be influential in the model and removing them would likely noticeably alter the regression results. 
# There are no point beyond the cook's distance

write.csv(finaldeathcounts,file = "finaldeathcounts.csv")


