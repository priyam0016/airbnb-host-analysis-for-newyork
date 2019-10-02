## Analyzing New York City's Airbnb Dataset 

#### Problem Statement
_Airbnb has become one of the most preferred websites when it comes for rental accomodation while travelling, among tourists. Airbnb has a huge customer base in New York City as New York City is among the top tourist destinations in the world.An analyis on airbnb data provides us insight on tourists preferences and the ratings of various listings in different NYC boroughs. There are several
other variables in the dataset which can be analyzed to determine these patterns, preferences and ratings.
The analysis on this dataset will help potential travellers to make a better choice regarding their accomodation. It will also be helpful for business models like airbnb to forsee the most popular areas and listing based on user preferences to scale their business and maximize profit._


Variable Name |  Description | Datatype |  Accepts Null Value
------------ | ------------- | ------------- | ------------- |
id | Property ID | int64 | N
name | Name of the property | object | N
host_id | ID number of the host | int64 | N
host_name | Name of the host | object | N
neighbourhood_group | 5 boroughs of NYC | object | N
neighbourhood | Sub areas under 5 boroughs | object | N
latitude | Latitude coordiantes | float64 | N
longitude | Longitude coordinates | float64 | N
room_type | Type of space in property | object | N
price | Rates charged per night in dollars | int64 | N
minimum_nights | Minimum days of reservation one should make | int64 | N
number_of_reviews | Number of reviews of property | int64 | Y
last_review | Latest review date | object | Y
reviews_per_month | reviews of property per month | float64 | Y
calculated_host_listings_count | amount of property per host | int64 | N
availability_365 | rnumber of days in a year when property is available for booking | int64 | N
