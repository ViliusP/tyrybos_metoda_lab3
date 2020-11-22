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
library(e1071)
library(caTools)
library(caret)
library(knitr)

### Set seed
set.seed(80085)


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

#y = weatherData["RainTomorrow"]
#x = subset(weatherData, select = -c(RainTomorrow) )

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

### weather data with most important features
finalWeatherData <- subset(weatherData, select = c(Humidity3pm, Pressure3pm, WindGustSpeed, Rainfall, RainTomorrow) )

### Split data to training and test sets
split = sample.split(finalWeatherData$RainTomorrow, SplitRatio = 0.75)
training_set = subset(finalWeatherData, split == TRUE)
test_set = subset(finalWeatherData, split == FALSE)


#### ++++++++++++++++++++++++++++++
#### SVM
#### ++++++++++++++++++++++++++++++



# in creating the folds we specify the target feature (dependent variable) and # of folds
folds = createFolds(training_set$RainTomorrow, k = 10)
# in cv we are going to applying a created function to our 'folds'
cv = lapply(folds, function(x) { # start of function
  # in the next two lines we will separate the Training set into it's 10 pieces
  training_fold = training_set[-x, ] # training fold =  training set minus (-) it's sub test fold
  test_fold = training_set[x, ] # here we describe the test fold individually
  # now apply (train) the classifer on the training_fold
  classifier = svm(formula = RainTomorrow ~ .,
                   data = training_fold,
                   type = 'C-classification',
                   kernel = 'radial')
  # next step in the loop, we calculate the predictions and cm and we equate the accuracy
  # note we are training on training_fold and testing its accuracy on the test_fold
  y_pred = predict(classifier, newdata = test_fold[-5]) # <------------- Change number according to your features count
  cm = table(test_fold[, 5], y_pred) # <------------- Change number according to your features count
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})

knitr::include_graphics("CV.png")
accuracy = mean(as.numeric(cv))