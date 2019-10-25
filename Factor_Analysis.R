##Author: Priyam Saxena
########  FACTOR ANALYSIS ##########

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
airbnbManhattanPCA = data.frame(
  airbnbManhattan$id,
  airbnbManhattan$host_id,
  airbnbManhattan$room_type,
  airbnbManhattan$price, 
  airbnbManhattan$minimum_nights, 
  airbnbManhattan$number_of_reviews,
  airbnbManhattan$reviews_per_month,
  airbnbManhattan$availability_365)

setDT(airbnbManhattanPCA)

##Setting column names for our new dataframe
names(airbnbManhattanPCA) <- c(
  'id',
  'host_id',
  'room_type',
  'price',
  'minimum_nights',
  'number_of_reviews', 
  'reviews_per_month',
  'availability_365')

head(airbnbManhattanPCA, 5)

##Lets first check the correlation to see whether FA is good to apply
corrM = cor(airbnbManhattanPCA[,-1:-3])
corrplot(corrM, method = "number")
#The variables are not very correlated, however we see that no of reviews and reviews_per_month are corelated



## To check acceptable number of factors and generate scree plot we use parallel analysis.
## Parallel analysis suggests that the number of factors =  5  and the number of components =  NA"
parallel <- fa.parallel(airbnbManhattanPCA[,-1:-3], fm = 'minres', fa = 'fa')

##when look at the large drops in the actual data (its 2 in this case) and spot the point where it levels off to the right.
##Also we locate the point of inflection - the point
##where the gap between simulated data and actual data tends to be minimum(its between 3 and 4)
##Factor we can take between 2 and 4
## So we will take 3 as factors here
threefactor <- principal(airbnbManhattanPCA[,-1:-3],nfactors = 3,rotate = "varimax")
print(threefactor)

class(threefactor)
#Displaying factor values.
threefactor$values
round(threefactor$values, 3)

#Displaying factor loadings
threefactor$loadings

# Communalities
threefactor$communality

# Rotated factor scores.
head(threefactor$scores)

# Play with FA utilities


##Lets look at the factor mapping of different variables
fa.diagram(threefactor)
#Here we found that all the factors have good contribution in respective factors and are singly mapped.
#Hence we can make three factor, i.e reduce 5 variable sto 3.


##Here we plot the factors and can rename the factors analyzed with three column names
colnames(threefactor$loadings) <- c("Reviews","Price","Minimum Night/Availability")
colnames(threefactor$loadings)

plot(threefactor)

##In factor analysis we model the observed variables as linear functions of the "factors."

