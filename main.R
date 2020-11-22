### Dataset link
###

### Clear console
cat("\014")  

### Constants
RESULT_FILE_PATH <- "./results.xlsx"

### Initialize libraries
library(xlsx)
library(dummies)
library(dplyr)
library(mlbench)
library(caret)
library(corrplot)
library('randomForest')

### Load data
weatherData <- read.csv(file = './weatherAUS.csv')

### Preview data
head(weatherData)

### Summary of weather data
summary <- summary(weatherData)
summary

#### ++++++++++++++++++++++++++++++
#### Data preparation
#### ++++++++++++++++++++++++++++++

### Empty rows count
columnNames <-colnames(weatherData)
emptyRowCount <-matrix(ncol=2, nrow=length(columnNames))

for ( col in 1:ncol(weatherData))
{
  emptyRowCount[col, 2] <-sum(is.na(weatherData[,col]))
  emptyRowCount[col, 1] <- columnNames[col]
}


write.xlsx(
  emptyRowCount,
  RESULT_FILE_PATH,
  sheetName = "empty_column_rows_count",
  col.names = FALSE, row.names = FALSE, 
  append = TRUE,
)

rm(col, columnNames, summary) # Clear values

### Clean data

# Sunshine - number of hours of brightness in the day.
#	* Too little data.
# Cloud3pm - fraction of sky obscured by clouds at 3pm.
#	* Too little data.
# Cloud9am - fraction of sky obscured by clouds at 9am.
#	* Too little data.
# Evaporation - The so-called Class A pan evaporation (mm) in the 24 hours to 9am
#	* Too little data.
# Sunshine - The number of hours of bright sunshine in the day.
#	* Too little data.
# Location - the name of the city located in Australia.
#	* We are determining Will it rain in Australia? So we don't need locations.
# RISK_MM - predicted amount of next day rain in mm.
#	* This could leak prediction data to our model, so drop it.
# Date - The date of observation
#	* Useless data.
weatherData <- subset(weatherData, select = -c(Date, RISK_MM, Location, Sunshine, Cloud9am, Cloud3pm, Evaporation) )

head(weatherData) # Preview cleaned data

### Remove rows containing null (empty) values
weatherData <-weatherData[complete.cases(weatherData),]

### Map "yes" -> "1", "no" -> "0"
mapping<- c("no"=0,"yes"=1)
weatherData$RainToday <- mapping[weatherData$RainToday]
weatherData$RainTomorrow <- mapping[weatherData$RainTomorrow]

head(weatherData) # Preview mapped data

### Create dummy variables
weatherData <- dummy.data.frame(weatherData, names = c("WindGustDir","WindDir3pm","WindDir9am") , sep = ".")

head(weatherData) # Preview data with dummy variables

#### ++++++++++++++++++++++++++++++
#### Data preprocess
#### ++++++++++++++++++++++++++++++

### Standardize data - make all data between 0 and 1
weatherData <- as.data.frame(apply(weatherData, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))

y = weatherData["RainTomorrow"]
x = subset(weatherData, select = -c(RainTomorrow) )

# Loading library

# Using random forest for variable selection
rfModel <-randomForest(y = weatherData[, 62], x = weatherData[,1:61])

rfModel <-randomForest(RainTomorrow ~ ., data = weatherData)

# Getting the list of important variables
importanceFeatureList <-importance(rfModel)

# Select columns by importance 
# Humidity3pm 4325.11484
# Pressure3pm 1319.30391
# WindGustSpeed 1197.12444
# Rainfall 1155.87331

xWithMostImportantFeatures <- subset(x, select = c(Humidity3pm, Pressure3pm, WindGustSpeed, Rainfall) )

#### ++++++++++++++++++++++++++++++
#### SVM
#### ++++++++++++++++++++++++++++++
