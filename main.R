### Clear console
cat("\014")  

### Constants
RESULT_FILE_PATH <- "./results.xlsx"

### Initialize libraries
library(xlsx)


### Load data
weatherData <- read.csv(file = './weatherAUS.csv')

### Preview data
head(weatherData)

### Summary of weather data
summary <- summary(weatherData)
summary

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
  append = FALSE,
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
# RISK_MM - amount of next day rain in mm.
#	* This could leak prediction data to our model, so drop it.
# Date - The date of observation
#	* Redundant data.
weatherData <- subset(weatherData, select = -c(Date, RISK_MM, Location, Sunshine, Cloud9am, Cloud3pm, Evaporation) )

head(weatherData) # Preview cleaned data

### Remove rows containing null (empty) values
ok <- complete.cases(weatherData)

