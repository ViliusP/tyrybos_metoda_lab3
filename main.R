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
library(RColorBrewer)
library('randomForest')
library(e1071)
library(caTools)
library(knitr)
library(keras)
library(deepviz)
library(magrittr)
library(rpart)
library(rpart.plot)
#library(tensorflow) # Tensforflow conflicts with randomForest
#library(tidyverse)
library(ggplot2)
library(tictoc)
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

write.xlsx(
  importanceFeatureList,
  RESULT_FILE_PATH,
  sheetName = "importance",
  col.names = TRUE, row.names = TRUE, 
  append = TRUE,
)



# Select columns by importance 

### weather data with most important features
finalWeatherData <- weatherData %>%select(
      RainTomorrow,
      Rainfall,
      Humidity3pm,
      Temp3pm,
      WindGustSpeed,
      Pressure3pm) %>%
  mutate_at(vars(RainTomorrow), as.factor)


### Data vizualization
corrplot(cor(finalWeatherData), type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

ggplot(weatherData, aes(y = Rainfall, x = RainTomorrow)) +
  geom_point()

ggplot(weatherData, aes(y = Humidity3pm, x = RainTomorrow)) +
  geom_point()

ggplot(weatherData, aes(y = Temp3pm, x = RainTomorrow)) +
  geom_point()

ggplot(weatherData, aes(y = WindGustSpeed, x = RainTomorrow)) +
  geom_point()

ggplot(weatherData, aes(y = Pressure3pm, x = RainTomorrow)) +
  geom_point()


### Split data to training and test sets
split = sample.split(finalWeatherData$RainTomorrow, SplitRatio = 0.75)
training_set = subset(finalWeatherData, split == TRUE)
test_set = subset(finalWeatherData, split == FALSE)

meta_check_split = sample.split(finalWeatherData$RainTomorrow, SplitRatio = 0.05)
meta_check_set = subset(finalWeatherData, meta_check_split == TRUE)

#### ++++++++++++++++++++++++++++++
#### SVM
#### ++++++++++++++++++++++++++++++

svm_radial <- tune.svm(RainTomorrow ~ .,
            data = meta_check_set, 
           type = "C-classification", 
           kernel = "radial", degree = 2, cost = 10^(1:3), 
           gamma = c(0.1, 1, 100), coef0 = c(0.1, 1, 10),  tunecontrol = tune.control(cross=5))

print(svm_radial$performances)
write.xlsx(
  svm_radial$performances,
  RESULT_FILE_PATH,
  sheetName = "svm_radial",
  col.names = TRUE, row.names = TRUE, 
  append = TRUE,
)
svm_poly <- tune.svm(RainTomorrow ~ .,
                       data = meta_check_set, 
                       type = "C-classification", 
                       kernel = "polynomial", degree = 3, cost = 10^(1:3), 
                       gamma = c(0.1, 1, 100), coef0 = c(0.1, 1, 10),  tunecontrol = tune.control(cross=5))

print(svm_poly$performances)
#### ++++++++++++++++++++++++++++++
#### Random forest classifier
#### +++++++++++++++++++++++++++++

rf_m4_n30 <- tune.randomForest(RainTomorrow ~ .,
                  data = meta_check_set, tunecontrol = tune.control(cross=10), maxnodes=4, ntree=30)

rf_m25_n30 <- tune.randomForest(RainTomorrow ~ .,
                  data = meta_check_set, tunecontrol = tune.control(cross=10), maxnodes=25, ntree=30)

rf_node500 <- tune.randomForest(RainTomorrow ~ .,
                        data = meta_check_set, tunecontrol = tune.control(cross=10), nodesize = 500)

rf_node2500 <- tune.randomForest(RainTomorrow ~ .,
                        data = meta_check_set, tunecontrol = tune.control(cross=10), nodesize = 2500)


rf <- tune.randomForest(RainTomorrow ~ .,
                  data = meta_check_set, tunecontrol = tune.control(cross=10))

control <- trainControl(method="repeatedcv", number=5, repeats=1, search="grid")
tunegrid <- expand.grid(.mtry=c(1:5))
metric <- "Accuracy"
rf_gridsearch <- train(RainTomorrow ~ ., data=meta_check_set, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

control <- trainControl(method="repeatedcv", number=5, repeats=1, search="random")
rf_randomsearch <- train(RainTomorrow ~ ., data=meta_check_set, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_randomsearch)
plot(rf_randomsearch)


plot(rf_default)



#### ++++++++++++++++++++++++++++++
#### Decision Tree
#### +++++++++++++++++++++++++++++


metric <- "Accuracy"
rpart_classifier <-train(
  RainTomorrow ~ ., data=training_set, method = "rpart",
  metric=metric,
  trControl = trainControl("cv", number = 10),
  tuneLength = 50
)
print(rpart_classifier)
plot(rpart_classifier)


write.xlsx(
  rpart_classifier$results,
  RESULT_FILE_PATH,
  sheetName = "rpart",
  col.names = TRUE, row.names = TRUE, 
  append = TRUE,
)

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
#get_config(nn_model)

# Get layer configuration
#get_layer(nn_model, index = 1)

# List the model's layers
#nn_model$layers

# List the input tensors
#nn_model$inputs

# List the output tensors
#nn_model$outputs


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

### NN2


# Initialize a sequential model
nn_model2 <- keras_model_sequential() 

# Add layers to the model
nn_model2 %>% 
  layer_dense(units = 10, input_shape = ncol(x_train), name="input") %>% 
  layer_activation_leaky_relu() %>% 
  
  layer_dense(units = 64) %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.75) %>%
  
  layer_dense(units = 128) %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.75) %>%
  
  layer_dense(units = 64) %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.75) %>%
  
  layer_dense(units = 10) %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.75) %>%
  
  layer_dense(units = 1, activation = "sigmoid")

optim = optimizer_rmsprop(lr = 0.0008)
nn_model2 %>% compile(loss = "binary_crossentropy",
                     optimizer = optim,
                     metrics = c("accuracy"))

nn_model2 %>% fit(
  x = as.matrix(x_train),
  y = as.matrix(y_train),
  epochs = 10,
  batch_size = 64,
  validation_split = 0.3
)



### NN3


# Initialize a sequential model
nn_model3 <- keras_model_sequential() 

# Add layers to the model
nn_model3 %>% 
  layer_dense(units = 10, input_shape = ncol(x_train), name="input") %>% 
  layer_activation_leaky_relu() %>% 
  
  layer_dense(units = 64) %>%
  layer_activation_leaky_relu() %>%

  layer_dense(units = 128) %>%
  layer_activation_leaky_relu() %>%

  layer_dense(units = 64) %>%
  layer_activation_leaky_relu() %>%

  layer_dense(units = 10) %>%
  layer_activation_leaky_relu() %>%

  layer_dense(units = 1, activation = "sigmoid")

optim = optimizer_rmsprop(lr = 0.0008)
nn_model3 %>% compile(loss = "binary_crossentropy",
                      optimizer = optim,
                      metrics = c("accuracy"))

nn_model3 %>% fit(
  x = as.matrix(x_train),
  y = as.matrix(y_train),
  epochs = 10,
  batch_size = 64,
  validation_split = 0.3
)


### NN4
# Initialize a sequential model
nn_model4 <- keras_model_sequential() 

# Add layers to the model
nn_model4 %>% 
  layer_dense(units = 10, input_shape = ncol(x_train), name="input") %>% 
  layer_activation_leaky_relu() %>% 
  
  layer_dense(units = 32) %>%
  layer_activation_leaky_relu() %>%
  
  layer_dense(units = 64) %>%
  layer_activation_leaky_relu() %>%
  
  layer_dense(units = 32) %>%
  layer_activation_leaky_relu() %>%
  
  layer_dense(units = 10) %>%
  layer_activation_leaky_relu() %>%
  
  layer_dense(units = 1, activation = "sigmoid")

optim = optimizer_rmsprop(lr = 0.0008)
nn_model4 %>% compile(loss = "binary_crossentropy",
                      optimizer = optim,
                      metrics = c("accuracy"))

nn_model4 %>% fit(
  x = as.matrix(x_train),
  y = as.matrix(y_train),
  epochs = 10,
  batch_size = 64,
  validation_split = 0.3
)


### NN5
# Initialize a sequential model
nn_model5 <- keras_model_sequential() 

# Add layers to the model
nn_model5 %>% 
  layer_dense(units = 10, input_shape = ncol(x_train), name="input") %>% 
  layer_activation_leaky_relu() %>% 
  
  layer_dense(units = 32) %>%
  layer_activation_leaky_relu() %>%
  
  layer_dense(units = 64) %>%
  layer_activation_leaky_relu() %>%
  
  layer_dense(units = 32) %>%
  layer_activation_leaky_relu() %>%
  
  layer_dense(units = 10) %>%
  layer_activation_leaky_relu() %>%
  
  layer_dense(units = 1, activation = "softmax")

optim = optimizer_rmsprop(lr = 0.0008)
nn_model5 %>% compile(loss = "binary_crossentropy",
                      optimizer = optim,
                      metrics = c("accuracy"))

nn_model5 %>% fit(
  x = as.matrix(x_train),
  y = as.matrix(y_train),
  epochs = 10,
  batch_size = 64,
  validation_split = 0.3
)

### NN6
# Initialize a sequential model
nn_model6 <- keras_model_sequential() 

# Add layers to the model
nn_model6 %>% 
  layer_dense(units = 10, input_shape = ncol(x_train), name="input") %>% 
  layer_activation_leaky_relu() %>% 
  
  layer_dense(units = 10) %>%
  layer_activation_leaky_relu() %>%
  
  layer_dense(units = 10) %>%
  layer_activation_leaky_relu() %>%
  
  layer_dense(units = 1, activation = "sigmoid")

optim = optimizer_rmsprop(lr = 0.0008)
nn_model6 %>% compile(loss = "binary_crossentropy",
                      optimizer = optim,
                      metrics = c("accuracy"))

nn_model6 %>% fit(
  x = as.matrix(x_train),
  y = as.matrix(y_train),
  epochs = 10,
  batch_size = 64,
  validation_split = 0.3
)
#############################
########### Final classifying
#############################

### Neuron network
# Initialize a sequential model
nn_model_final <- keras_model_sequential() 

# Add layers to the model
nn_model_final %>% 
  layer_dense(units = 10, input_shape = ncol(x_train), name="input") %>% 
  layer_activation_leaky_relu() %>% 
  
  layer_dense(units = 64) %>%
  layer_activation_leaky_relu() %>%
  
  layer_dense(units = 128) %>%
  layer_activation_leaky_relu() %>%
  
  layer_dense(units = 64) %>%
  layer_activation_leaky_relu() %>%
  
  layer_dense(units = 10) %>%
  layer_activation_leaky_relu() %>%
  
  layer_dense(units = 1, activation = "sigmoid")

optim = optimizer_rmsprop(lr = 0.0008)
nn_model_final %>% compile(loss = "binary_crossentropy",
                      optimizer = optim,
                      metrics = c("accuracy"))

tic("neural_network")
nn_model_final %>% fit(
  x = as.matrix(x_train),
  y = as.matrix(y_train),
  epochs = 20,
  batch_size = 64,
  validation_split = 0.3
)
toc()

score <- nn_model_final %>% predict_classes( x = as.matrix(x_test))
confusionMatrix(table(as.matrix(y_test), score))
### SVM

tic("svm")
svm(x_train, y = y_train, kernel="radial", cost =10, gamma = 0.1, coef0=0.1)
toc()     
      
### Decision Tree
levels(y_train) = c(0, 1)
levels(y_test) = c(0, 1)

tic("decision_tree")
dc_final <- rpart(RainTomorrow ~ ., data=training_set,  control=rpart.control(cp=8.88e-05), method="class")
toc()
prediction_dc <-predict(dc_final, x_test, method="class")
confusionMatrix(prediction_dc, y_test)

### Random forest

tic("rf")
rf_final <-randomForest(y = as.factor(y_train), x = x_train, mtry=1, do.trace=10)
toc()
prediction <-predict(rf_final, newdata=x_test)
confusionMatrix(prediction, as.factor(y_test))
print(rf_final)
