### Clear console
cat("\014")  

### Libraries
library(dplyr)



### Load data
weatherData <- read.csv(file = './weatherAUS.csv')
### Preview data
head(weatherData)

### Summary of weather data
summary <- summary(weatherData)

emptyRowCount <- colnames(weatherData)
emptyRowCount <- cbind(emptyRowCount, data.frame(count=character()))

for ( col in 1:ncol(weatherData))
{
  columnEmptyRowsCount[col, 2] <- sum(is.na(weatherData[,col]))
  print(sum(is.na(weatherData[,col])))
}