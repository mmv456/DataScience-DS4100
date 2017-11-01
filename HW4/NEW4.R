library(openxlsx) # for importing file
library(lubridate) # for date manipulation
library(stringr) # for string manipulation



setwd("D:/Mahitha/DataScience/HW4")



farmersmarkets <- read.xlsx("2013FarmersMarkets.xlsx", startRow = 3)

cleanDatesDF <- farmersmarkets[which(!is.na(farmersmarkets["Season1Date"])),]
farmersmarkets <- cleanDatesDF

# Assumptions
# 1) If a record (start date, end date, or both) is missing, it will be ignored and remain NA.
# 2) Make the following assumptions about seasons:
#    - Winter = December, January, February  
#    - Spring = March, April, May
#    - Summer = June, July, August
#    - Fall = September, October, November
#    - Half-year = Any time period between 180 days and 300 days
#    - Full-year = Any time period at 300 or more days
#    - The half-way date of each time period was used to determine if it was Winter, Spring, Summer or Fall


# Split the Season1Date Column into 2 columns
threeColumns <- as.data.frame(str_split_fixed(farmersmarkets$Season1Date, " ", n=3)) # split into 3 columns
colnames(threeColumns) <- c("first_date", "grammar", "second_date")
threeColumns$first_date <- as.character(threeColumns$first_date) # standardize all dates as characters for now
threeColumns$second_date <- as.character(threeColumns$second_date) # standardize all dates as characters for now


# Create scale helper function
scale <- data.frame(month = c("January","February","March",
                             "April","May","June","July",
                             "August","September","October","November",
                             "December"),
                   
                   date = c("01/01/2012", "02/01/2012",
                            "03/01/2012", "04/01/2012",
                            "05/01/2012", "06/01/2012",
                            "07/01/2012", "08/01/2012",
                            "09/01/2012", "10/01/2012",
                            "11/01/2012", "12/01/2012"))

scale$date <- as.Date(scale$date, format = "%m/%d/%Y") # classify as date

# Get all dates into the same format
dateConversion <- function(x){
if(!str_detect(x, '/') == TRUE){ # if the observation doesn't contain '/'...
  data <- as.Date(scale$date[match(x,scale$month)]) #...use the scale helper table and classify it as a date
} 
  else{
    data <- as.Date(x, format = "%m/%d/%Y") # otherwise turn it into a date object
  }
  return(as.Date(data))
}



# Make some new variables in the original data frame
farmersmarkets$first_date <- as.Date(sapply(threeColumns$first_date, dateConversion),origin = "1970-01-01") # add the cleaned start dates back to the main data frame
farmersmarkets$second_date <- as.Date(sapply(threeColumns$second_date, dateConversion),origin = "1970-01-01") # add the cleaned end dates back to the main data frame
farmersmarkets$time_diff <- farmersmarkets$second_date - farmersmarkets$first_date # find the total length of the time period in days
farmersmarkets$half_way <- farmersmarkets$time_diff/2 + farmersmarkets$first_date # find the midway point (used to determine seasons)



# Create helper function to determine date
getSeason <- function(DATES) {
  WI <- as.Date("2012-12-01", format = "%Y-%m-%d") # Winter start date
  SP <- as.Date("2012-3-01",  format = "%Y-%m-%d") # Spring start date
  SU <- as.Date("2012-6-01",  format = "%Y-%m-%d") # Summer start date
  FA <- as.Date("2012-9-01",  format = "%Y-%m-%d") # Fall start date
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WI | d <  SP, "Winter", 
          ifelse (d >=  SP & d < SU, "Spring",
                  ifelse (d >= SU & d < FA, "Summer", "Fall")))
}





# For loop to apply the functions 
for(i in 1:nrow(farmersmarkets)){ # for each row in the farmersmarkets data frame
  if(is.na(farmersmarkets$time_diff[i])){ # if the record is NA...
    farmersmarkets$period[i] <- NA # assign the period NA as well
  } 
  else {
    if(farmersmarkets$time_diff[i] <= 180){  # if time period was less than 180 days...
      farmersmarkets$period[i] <- getSeason(farmersmarkets$half_way[i]) #...we should consider it a season: Winter, Spring, Summer or Fall based on the date at the half way point of the time interval
    } 
    else if ((farmersmarkets$time_diff[i] <= 300) ==TRUE){ # if it's greater than 180, but less than 300...
      farmersmarkets$period[i] <- "Half-Year" #...we'll consider it a "Half-Year" 
    } 
    else {
      farmersmarkets$period[i] <- "Full-Year" # for anything else greater than 300 days, we'll consider it a full year, the reasoning being it wouldn't be appropriate to call, for example, a 10-month long farmer's market a "half year" one. 
    }
  }
  i <- i + 1 # keep the for loop going
}



# farmersmarkets$period contains all of the season categories:
farmersmarkets$period <- as.factor(farmersmarkets$period)

print(farmersmarkets$period)

