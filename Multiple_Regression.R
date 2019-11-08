##Author: Priyam Saxena
########  Multiple Regression Analysis ##########

library(data.table)
library(ggplot2)  # tidyverse data visualization package
library(stringr)
library(corrplot)
library(psych)


#Importing csv file from my local computer
airbnbOriginalDF =read.csv("D:/Priyam/FirstSemester/MVA project/airbnb-host-analysis-for-newyork/Airbnb Host Data For Newyork City.csv")

##Converting data frame to data table
setDT(airbnbOriginalDF)

#Removing values which are null and storing in new table.
airbnbNoNADT = airbnbOriginalDF[airbnbOriginalDF$reviews_per_month != 'NA']

#Converting datatype of last review date to DAte Format.
airbnbNoNADT[,last_review:=as.Date(last_review, '%m/%d/%Y')]

#As the neighbourhood_group column has 5 categorical values, we can factor it, and convert our string data type.
airbnbNoNADT[,neighbourhood_group:= factor(neighbourhood_group)]

#For room type, we get 3 unique categorical values. we can factor it, and convert our string datatype.
airbnbNoNADT[,room_type:= factor(room_type)]

#With earlier analysis/ summary and plot we found few ouliers, therefore that data  we have dropped below, conforming it is not impact our main dataset.
airbnbCleaned = airbnbNoNADT[price<2500 & number_of_reviews<400 & reviews_per_month<10]
##Manhattan area dataset
airbnbManhattan = airbnbCleaned[neighbourhood_group=='Manhattan']
nrow(airbnbManhattan)



library(dplyr)
library(data.table)

##Taking the numeric columns that will contribute for variance in data
airbnbManhattanLM = data.frame(
  airbnbManhattan$id,
  airbnbManhattan$host_id,
  airbnbManhattan$room_type,
  airbnbManhattan$price, 
  airbnbManhattan$minimum_nights, 
  airbnbManhattan$number_of_reviews,
  airbnbManhattan$reviews_per_month,
  airbnbManhattan$availability_365)

setDT(airbnbManhattanLM)

##Setting column names for our new dataframe
names(airbnbManhattanLM) <- c(
  'id',
  'host_id',
  'room_type',
  'price',
  'minimum_nights',
  'number_of_reviews', 
  'reviews_per_month',
  'availability_365')

head(airbnbManhattanLM, 5)

# Performing multiple regression on Airbnb Manhattan dataset
fit_airbnb <- lm(price~number_of_reviews+availability_365+minimum_nights+room_type, data=airbnbManhattanLM)

#show the results
#Section1: How well does the model fit the data (before Coefficients). 
#Section2: Is the hypothesis supported? (until sifnif codes). 
#Section3: How well does data fit the model (again).

summary(fit_airbnb)

#The coefficients describe the mathematical relationship between each independent variable and the dependent variable.
#The p-values for the coefficients indicate whether these relationships are statistically significant.

#From the summary no_of_reviews , availability_365, minimum_nights are statistically significant because their p-values are very small.



#It is standard practice to use the coefficient p-values to decide whether to include variables in the final model.
#For the results above, we would consider all variables. Keeping variables that are not statistically significant can reduce the model's precision.

#The coefficient value signifies how much the mean of the dependent variable changes given a one-unit shift in the independent variable
#while holding other variables in the model constant. This property of holding the other variables constant is crucial because it allows
#you to assess the effect of each variable in isolation from the others.

#After fitting a regression model, check the residual plots first to be sure that you have unbiased estimates.
#for combining plots into a matrix through the ggpairs function.
#matrix of plots can be useful for quickly exploring the relationships between multiple columns of data in a data frame.
library(GGally)
ggpairs(data=airbnbManhattanLM,title="Property Data")


#To extract fitted values from objects returned by modeling functions
#fitted(fit_airbnb)
#To check residuals
#residuals(fit_airbnb)
library(car)
outlierTest(fit_airbnb)
#The result gives values at given row number are outliers.

# Cook's D plot 
##it's a way to identify points that negatively affect your regression model.
#The measurement is a combination of each observation's leverage and residual 
#values; the higher the leverage and residuals, the higher the Cook's distance. Cook's distance
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(airbnbManhattanLM)-length(fit_airbnb$coefficients)-2))
plot(fit_airbnb, which=4, cook.levels=cutoff)

# Representation of above using Influence Plot
influencePlot(fit_airbnb, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

##THIS SHOWS THE RESULTING POINTS HAVE MUCH NEGATIVE EFFECT ON OUR MODEL.

#Extract Studentized Residuals From A Linear Model
library(MASS)
#A studentized residual is calculated by dividing the residual by an estimate of its 
#standard deviation. The standard deviation for each residual is computed with the observation excluded. 
#For this reason, studentized residuals are sometimes referred to as externally studentized residuals
sresid <- studres(fit_airbnb)

##Lets view the distribution of theses studentized residuals.
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

# Test for Autocorrelated Errors
#Computes residual autocorrelations and generalized Durbin-Watson statistics and their bootstrapped p-values
#Non-independence of Errors
durbinWatsonTest(fit_airbnb)

# Global test of model assumptions
library(gvlma)

## The gvlma( ) function in the gvlma package, performs a global validation of
#linear model assumptions as well separate evaluations of skewness, kurtosis,
#and heteroscedasticity
gvmodel <- gvlma(fit_airbnb)
summary(gvmodel)



##The stepAIC() function performs backward model selection by starting from a 
#"maximal" model, which is then trimmed down. The "maximal" model is a linear
#regression model which assumes independent model errors and includes only main
#effects for the predictor variables

#The Akaike information criterion (AIC) is an estimator of the relative quality of statistical models for a given set of data. Given a collection of models
#for the data, AIC estimates the quality of each model, relative to each of the other models.
#Thus, AIC provides a means for model selection.
library(MASS)
step <- stepAIC(fit_airbnb, direction="both")
step$anova # display results
summary(step)$coeff
summary(step)$r.squared

#The adjusted R^2 is 18.89% which means that the model explains 18% of the variation in mpg 
#indicating it is a robust and highly predictive model.

#Stepwise selection
fit1 <- lm(price ~ number_of_reviews,data = airbnbManhattanLM)
fit2 <- lm(price ~ number_of_reviews+availability_365, data = airbnbManhattanLM)
fit3 <- lm(price ~ number_of_reviews+availability_365+minimum_nights, data = airbnbManhattanLM)
fit4 <- lm(price ~ number_of_reviews+availability_365+minimum_nights+room_type, data = airbnbManhattanLM)

anova(fit1, fit2, fit3, fit4)

#The above shows that result is consistent with stepwise selection model

# Calculate Relative Importance for Each Predictor
library(relaimpo)
calc.relimp(fit_airbnb)
# Bootstrap Measures of Relative Importance (1000 samples)
bootresults<-boot.relimp(fit_airbnb, b=1000)
rel_imp <-booteval.relimp(bootresults) # print result
plot(rel_imp) # plot result

predict.lm(fit_airbnb, data.frame(number_of_reviews = 45, availability_365 =365,minimum_nights = 1, room_type = "Entire home/apt"))

#Here we have throw the values sample from our dataset and got approx same price.
#Thus it shows our multinomial regression model is good to predict price.