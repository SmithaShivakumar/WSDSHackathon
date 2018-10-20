library(readxl)
library(plyr)         # Data manipulation
library(dplyr)        # Data manipulation
library(ggplot2)      # Data visualization
library(plotly)       # Dynamic data visualization
library(caTools)      # Setting seeds
library(rpart)        # Decision Trees
library(randomForest) # Random Forest
library(maps)         # World Map
library(countrycode)  # Gets country code 
library(leaflet)      # Interactive maps
library(corrplot)     # Nice correlation matrix
library(RColorBrewer) # Color brewer
library(leaflet.extras)
library(readxl)

CollegeListUS <- read_excel("CountCollegeListUS.xlsx")
poverty <- read.csv("child_poverty.csv")
musicMajor <- read.csv("MusicMajor.csv")
number_of_vol <- read.csv('number of volunteers in millions.csv')


CollegeListUS$State <- tolower(CollegeListUS$State)

states <- map_data("state") 

map.df <- merge(states,CollegeListUS, by.x="region", by.y = "State", all.x=T)
map.df <- map.df[order(map.df$order),]
#head(map.df)

poverty$State <- tolower(poverty$State)

poverty.df <- merge(states,poverty, by.x="region", by.y = "State")
poverty.df <- poverty.df[order(poverty.df$order),]


musicMajor$StateNames <- tolower(musicMajor$StateNames)

MusicMap.df <- merge(states,musicMajor, by.x="region", by.y = "StateNames")
MusicMap.df <- MusicMap.df[order(MusicMap.df$order),]
#head(MusicMap.df)




# 
# leaflet(data = MusicMajorByState) %>%  
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addHeatmap(lng=~MusicMajorByState$lon, lat=~MusicMajorByState$lon)

# +   addMarkers(~MusicMajorByState$lon, ~MusicMajorByState$lat, 
#                  popup = ~as.character(MusicMajorByState$Volunteer), 
#                  label = ~as.character(MusicMajorByState$'Music Major'))
