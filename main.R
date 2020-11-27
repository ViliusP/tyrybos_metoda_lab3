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
library(keras)
library(deepviz)
library(magrittr)
library(tensorflow)
library(tidyverse)
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
columnNames <-colnam
es(weatherData)
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

weatherData = weatherData %>% select(-Date, -RISK_MM, -Location, -Sunshine, -Cloud9am, -Cloud3pm, -Evaporation)
head(weatherData) # Preview cleaned data

### Remove rows containing null (empty) values
weatherData = weatherData %>% na.exclude()


head(weatherData) # Preview mapped data

### Create dummy variables

weatherData <- weatherData %>% 
  mutate_at(vars(WindGustDir, WindDir9am, WindDir3pm, RainToday, RainTomorrow), as.factor)



weatherData <- dummy.data.frame(weatherData, names = c("WindGustDir","WindDir3pm","WindDir9am") , sep = ".")

head(weatherData) # Preview data with dummy variables


### Map "yes" -> "1", "no" -> "0" and scale
weatherData = weatherData %>%
  mutate_at(vars(RainTomorrow), ~ as.numeric(.) - 1) %>%
  mutate_at(vars(RainToday), ~ as.numeric(.) - 1) %>%
  mutate_at(vars(-RainTomorrow, -RainToday), ~ as.vector(scale(.)))
head(weatherData) # Preview data scaled


#### ++++++++++++++++++++++++++++++
#### Data preprocess
#### ++++++++++++++++++++++++++++++

#y = weatherData["RainTomorrow"]
#x = subset(weatherData, select = -c(RainTomorrow) )

# Using random forest for variable selection
# use this one
rfModel <-randomForest(y = weatherData[, 62], x = weatherData[,1:61])

# or this one
rfModel <-randomForest(RainTomorrow ~ ., data = weatherData)

# Getting the list of important variables
importanceFeatureList <-importance(rfModel)

# Select columns by importance 
# Humidity3pm 4325.11484
# Pressure3pm 1319.30391
# WindGustSpeed 1197.12444
# Rainfall 1155.87331

### weather data with most important features
finalWeatherData <- weatherData %>%select(
      RainTomorrow,
      RainToday,
      Rainfall,
      Humidity3pm,
      MaxTemp,
      Pressure3pm) %>%
  mutate_at(vars(-RainTomorrow, -RainToday), ~ as.vector(scale(.)))
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
  y_pred = predict(classifier, newdata = test_fold[-ncol(training_set)]) # <------------- Change number according to your features count
  cm = table(test_fold[, ncol(training_set)], y_pred) # <------------- Change number according to your features count
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})

accuracy = mean(as.numeric(cv))



#### ++++++++++++++++++++++++++++++
#### Neural network (Keras)
#### +++++++++++++++++++++++++++++
x_train = training_set %>% select(-RainTomorrow)
y_train = training_set$RainTomorrow

x_test = test_set %>% select(-RainTomorrow)
y_test = test_set$RainTomorrow


# Initialize a sequential model
nn_model <- keras_model_sequential() 

# Add layers to the model
nn_model %>% 
  layer_dense(units = 10, input_shape = ncol(x_train), name="input") %>% 
  layer_activation_leaky_relu() %>% 
  
  layer_dense(units = 64) %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 128) %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 64) %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 10) %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 1, activation = "sigmoid")



# Print a summary of a model
summary(nn_model)

# Get model configuration
get_config(nn_model)

# Get layer configuration
get_layer(nn_model, index = 1)

# List the model's layers
nn_model$layers

# List the input tensors
nn_model$inputs

# List the output tensors
nn_model$outputs


optim = optimizer_rmsprop(lr = 0.0008)
nn_model %>% compile(loss = "binary_crossentropy",
                  optimizer = optim,
                  metrics = c("accuracy"))

nn_model %>% fit(
  x = as.matrix(x_train),
  y = as.matrix(y_train),
  epochs = 10,
  batch_size = 64,
  validation_split = 0.3
)


model = keras_model_sequential()

# Bulding the architecture of the network
model %>%
  layer_dense(units = 10,
              input_shape = ncol(x_train),
              name = "inputs") %>%
  layer_activation_leaky_relu() %>%
  
  layer_dense(units = 128) %>%
  layer_activation_leaky_relu() %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 128) %>%
  layer_activation_leaky_relu() %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 256) %>%
  layer_activation_leaky_relu() %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 126) %>%
  layer_activation_leaky_relu() %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 64) %>%
  layer_activation_leaky_relu() %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 32) %>%
  layer_activation_leaky_relu() %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 10) %>%
  layer_activation_leaky_relu() %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 1, activation = "sigmoid")


optim = optimizer_rmsprop(lr = 0.0008)
model %>% compile(loss = "binary_crossentropy",
                  optimizer = optim,
                  metrics = c("accuracy"))

model %>% fit(
  x = as.matrix(x_train),
  y = as.matrix(y_train),
  epochs = 10,
  batch_size = 64,
  validation_split = 0.5
)
