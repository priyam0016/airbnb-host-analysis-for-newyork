library(data.table)
library(ggplot2)  # tidyverse data visualization package
library(sf)
library(raster)
library(dplyr)
library(spData)
library(stringr)

library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(shiny)   # for web applications

setDT(airbnbOriginalDF)
str(airbnbOriginalDF)

#Checking null/missing value in table
table(is.na(airbnbOriginalDF))
str_detect(airbnbOriginalDF, ' ')
table(is.na(airbnbOriginalDF$reviews_per_month))

extracat::visna(airbnbOriginalDF)


#Removing values which are null and storing in new table.
airbnbNoNADT = airbnbOriginalDF[airbnbOriginalDF$reviews_per_month != 'NA']
table(is.na(airbnbNoNADT))
table(is.na(airbnbNoNADT$reviews_per_month))

#airbnbNoNADT is datatable with not any null values

#Converting datatype of last review date to DAte Format.
airbnbNoNADT[,last_review:=as.Date(last_review, '%m/%d/%Y')]
str(airbnbNoNADT)


#Lets try to further analyze our data by analysing data types.
str(airbnbNoNADT)
unique(airbnbNoNADT$neighbourhood_group)
#As the neighbourhood_group column has 5 categorical values, we can factor it, and convert our string data type.
airbnbNoNADT[,neighbourhood_group:= factor(neighbourhood_group)]


unique(airbnbNoNADT$neighbourhood)
#For neighbourhood, we get 217 unique values. Here to reduce storage we can covert all similar type to lower case and also trim white spaces, so that each anme is unique.
airbnbNoNADT[,neighbourhood:=tolower(neighbourhood)]
airbnbNoNADT[,neighbourhood:=trimws(neighbourhood)]
#We will refrain ourselves from factoring it at this stage, as we can do it later when required.

#For room type, we get 3 unique categorical values. we can factor it, and convert our string datatype.
unique(airbnbNoNADT$room_type)          
airbnbNoNADT[,room_type:= factor(room_type)]

#Analysing longitude data. The distribution is fair
summary(airbnbNoNADT$longitude)
str(airbnbNoNADT)

#Analysing avialbility data. THe data is fair and no extreme values.
summary(airbnbNoNADT$availability_365)

#Analysing price data. Could see extremely large values. Lets draw a plot to see the distribution.
summary(airbnbNoNADT$price)
ggplot(airbnbNoNADT,aes(y=price))+geom_boxplot(fill='yellow')
#In plot we can see some outliers. lets run below and see how many are such properties that have price greater than 2500.
nrow(airbnbNoNADT[price>2500])  #By runing this, we find only 25 such properties. This can be dropped as we 38k plus data

#Analysing number of reviews data. Could see extremely large values. Lets draw a plot to see the distrinution.
summary(airbnbNoNADT$number_of_reviews)
ggplot(airbnbNoNADT,aes(y=number_of_reviews))+geom_boxplot(fill ='red')
#In plot we can see some outliers. lets run below and see how many are such properties that have no of reviews greater than 400.
#Such a huge review for one or two property seems to be some spam or fake. We shall how many such rows are there in our data.
nrow(airbnbNoNADT[number_of_reviews>400])
#We found 39 rows which have number of reviews greater than 400.
airbnbNoNADT[number_of_reviews>400,unique(neighbourhood_group)]
#When we checked for which areas this spam review is , it shows Manhattan, Brooklyn and Queens. So there is no clear indication by this data, we will drop this to further clean our data and remove outliers.

#Analysingreviews per month Could see extremely large values. Lets draw a plot to see the distrinution.
summary(airbnbNoNADT$reviews_per_month)
ggplot(airbnbNoNADT,aes(y=reviews_per_month))+geom_boxplot(fill='purple')
#In plot we can see some outliers. lets run below and see how many are such properties that have reviews per month greater than 10.
#Most of the data is located below 5. We shall how many such rows rae there in our data which have review per month greater than 10
nrow(airbnbNoNADT[reviews_per_month>10])
airbnbNoNADT[reviews_per_month>10,unique(neighbourhood_group)]
#When we tried checking if any particular locality has more reviews, it does not give any indication. The result is spread out for all localities. We can drop this rows, as it wont yield anything peculiar.

#With above summary and plot we found few ouliers, therefore that data  we have dropped below, conforming it is not impact our main dataset.
airbnbCleaned = airbnbNoNADT[price<2500 & number_of_reviews<400 & reviews_per_month<10]
#airbnbCleaned is our Final cleaned data


#Attach is used to access column directly without using data table name.
attach(airbnbCleaned)

summary(airbnbCleaned)
names(airbnbCleaned)
ggplot(airbnbCleaned, aes(x=price, y = name ))+geom_bar()+facet_wrap(neighbourhood_group)

#Analysing the price distribution based on location
plot(neighbourhood_group,price, xlab= 'Boroughs', ylab='Price distribution across boroughs')
?plot()

#Analysing the availability across borouhgs
plot(airbnbCleaned$neighbourhood_group, airbnbCleaned$availability_365, xlab ='Boroughs', ylab= 'Availablity in days')

names(airbnbCleaned)

#Analysing the room types which are preferred and mostly listed across all boroughs
ggplot(airbnbCleaned, aes(x=neighbourhood_group, fill = room_type))+geom_bar(position = "dodge") + xlab("Borough") + ylab("Count")



#Analysis:
#We can see that Entire home apartment listings are highest in number except Queens and Bronx. Queens has more ‘Private’ style property than ‘Apartments’.
#The maximum apartment style listings are located in Manhattan, constituting 90% of all properties in that neighborhood. Next is Brooklyn with 75% Apartment style listing.

ggplot(airbnbCleaned, aes(x=neighbourhood_group, fill = number_of_reviews))+geom_bar(color='black', fill='maroon') + xlab("Borough") + ylab("Number of reviews")
names(airbnbCleaned)
head(airbnbCleaned)

#Analysis:
#We can see that properties in Manhattan has recieved most of customer review , followed by Brooklyn.

ggplot(airbnbCleaned, aes(x= number_of_reviews, fill= room_type )) + geom_histogram(binwidth = 30)+facet_wrap(room_type)

#With above data, we can see that Apartment type properties are mostly preferred, since they are the ones
#receiving maximum ratings. After which people prefer private rooms. Shared rooms have received very few 
#rating. This would be helpful for other business to avoid providing shared rooms

#Lets see the correlation among 
detach(airbnbCleaned)

#Seperating the data for boroughs
airbnbManhattan = airbnbCleaned[neighbourhood_group=='Manhattan']
nrow(airbnbManhattan)
airbnbQueens = airbnbCleaned[neighbourhood_group=='Queens']
nrow(airbnbQueens)
airbnbBrooklyn = airbnbCleaned[neighbourhood_group=='Brooklyn']
nrow(airbnbBrooklyn)
airbnbBronx = airbnbCleaned[neighbourhood_group=='Bronx']
nrow(airbnbBronx)
airbnbStatenIsland = airbnbCleaned[neighbourhood_group=='Staten Island']
nrow(airbnbStatenIsland)

diagnolcol = c("price","minimum_nights","reviews/month", "numberOfReviews", "availabilityFor365")
#Priyam below#
pairs(data.table(
  airbnbManhattan$price, 
  airbnbManhattan$minimum_nights, 
  airbnbManhattan$reviews_per_month, 
  airbnbManhattan$number_of_reviews, 
  airbnbManhattan$availability_365), labels = diag.col)

pairs(data.table(
  airbnbBrooklyn$price, 
  airbnbBrooklyn$minimum_nights, 
  airbnbBrooklyn$reviews_per_month, 
  airbnbBrooklyn$number_of_reviews, 
  airbnbBrooklyn$availability_365), labels = diag.col)

pairs(data.table(
  airbnbQueens$price, 
  airbnbQueens$minimum_nights, 
  airbnbQueens$reviews_per_month, 
  airbnbQueens$number_of_reviews, 
  airbnbQueens$availability_365), labels = diag.col)

pairs(data.table(
  airbnbStatenIsland$price, 
  airbnbStatenIsland$minimum_nights, 
  airbnbStatenIsland$reviews_per_month, 
  airbnbStatenIsland$number_of_reviews, 
  airbnbStatenIsland$availability_365), labels = diag.col)

pairs(data.table(
  airbnbBronx$price, 
  airbnbBronx$minimum_nights, 
  airbnbBronx$reviews_per_month, 
  airbnbBronx$number_of_reviews, 
  airbnbBronx$availability_365), labels = diag.col)


cor(airbnbCleaned[,c("host_id","reviews_per_month","availability_365")])

summary(airbnbCleaned)

mapnyc <- data.frame(latitude = c(40.7032815,40.7082398),
                     longitude = c(-74.0192166, -74.0054436),
                     logprice = c(120,56))
ggplot(airbnbCleaned, aes(x=longitude, y=latitude, COLOR= 'red'),alpha =0.03) +
  geom_point(size=0.8)

