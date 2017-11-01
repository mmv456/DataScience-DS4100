#################################
#        Mahitha Valluru        #
#        DS4100                 #
#        Prof. Schedlbauer      #
#        Homework 3             #
#################################


require(lubridate)  # great package for manipulating dates

# set working directory
setwd("D:/Mahitha/DataScience/HW3")



# Now, I have to load the csv file into a dataframe. I'll do this by creating a function.
# Also, I think it's best to clean up the FlightDate and FlightYear fields, which I'll do using lubridate.
loadDF <- function() {
  if (!exists("birdstrikes")) {
    birdstrikes <- read.csv("Bird\ Strikes.csv")
    
    # cleans up FlightDate column
    birdstrikes$FlightDate <- as.Date(birdstrikes$FlightDate, format = "%d/%m/%Y")
    # creates a new column called FlightYear, just shows the year of incident
    birdstrikes$FlightYear <- year(as.Date(birdstrikes$FlightDate, format = "%d/%m/%Y"))
  }
}

# Loads the data.
loadDF()


# Gets the year of the greatest number of strikes 
# Args: none
# Returns: an number representing the year
mostStrikesInaYear <- function() {
  # create a table using birdstrikes but only showing the FlightYear column
  tableYear = table(birdstrikes$FlightYear)
  
  # sort this table in DECREASING order (most strikes in a year first)
  sortedTableYear = sort(tableYear, decreasing = TRUE)
  
  # get the name of the FIRST element in this sorted table, which is the year with the most bird strikes
  getName = names(sortedTableYear[1])
  
  #return getName
  return(getName)
}
#-------------------------
# Return value: 2010
#-------------------------



# Creates a datafram with the year and number of brid strikes
# Args: none
# Returns: A dataframe
strikesByYear <- function() {
  #create a table using birdstrikes but only showing the FlightYear column
  tableYear = table(birdstrikes$FlightYear)
  
  # create dataframe
  yearDF = as.data.frame(tableYear)
  
  return(yearDF)
}


# Returns a dataframe that calculates the number of bird strikes per airline, not including military or business
# Args: none
# Returns: a dataframe
strikesByAirline <- function() {
  # create a table using birdstrikes but only showing the Airline Operator column
  tableAirlineOperator = as.data.frame(table(birdstrikes$Aircraft..Airline.Operator))
  
  names(tableAirlineOperator)[1] = "Airline Operator"
  
  # now exclude the business and military operators from tableAirlineOperator
  tableAirlineOperator = tableAirlineOperator[which(tableAirlineOperator["Airline Operator"] != "MILITARY" &
                                                      tableAirlineOperator["Airline Operator"] != "BUSINESS"),]
  
  # create a dataframe
  AirlineStrikes = as.data.frame(tableAirlineOperator)
  
  return(AirlineStrikes)
}




# Returns the airline with the most bird strikes
# Args: AirlineStrikes (a dataframe with the airline and the number of bird strikes for each)
# Returns: an airline (a string)
mostStrikes <- function(AirlineStrikes) {
  removeUnknown <- AirlineStrikes[which(AirlineStrikes["Airline Operator"] != "UNKNOWN"),]
  
  result <- removeUnknown[which(removeUnknown["Freq"] == max(removeUnknown["Freq"])),1]
  
  return(result)
}
#-------------------------------------
# Return Value: UNITED AIRLINES
#-------------------------------------


# NOW, TO FIND THE RUNTIME ----------------------------------------


# we have to create copies of the dataframe
CopyAirlineStrikes <- AirlineStrikes

TwoAirlineStrikes <- rbind(AirlineStrikes, CopyAirlineStrikes)

FourAirlineStrikes <- rbind(TwoAirlineStrikes, TwoAirlineStrikes)

EightAirlineStrikes <- rbind(FourAirlineStrikes, FourAirlineStrikes)

SixteenAirlineStrikes <- rbind(EightAirlineStrikes, EightAirlineStrikes)

TwentyAirlineStrikes <- rbind(SixteenAirlineStrikes, FourAirlineStrikes) # we need this dataframe

FortyAirlineStrikes <- rbind(TwentyAirlineStrikes, TwentyAirlineStrikes) # we need this dataframe


# Find times for the data
Start1 <- Sys.time()
mostStrikes(AirlineStrikes)
End1 <- Sys.time()

Start20 <- Sys.time()
mostStrikes(TwentyAirlineStrikes)
End20 <- Sys.time()

Start40 <- Sys.time()
mostStrikes(FortyAirlineStrikes)
End40 <- Sys.time()


# Calculate runtime
Runtime1 <- End1 - Start1
Runtime20 <- End20 - Start20
RUntime40 <- End40 - Start40


# Get the number of elements in each dataset using nrow()
Observations1 <- nrow(AirlineStrikes)
Observations20 <- nrow(TwentyAirlineStrikes)
Observations40 <- nrow(FortyAirlineStrikes)


# Create a dataframe with the data calculated
TestDF <- data.frame(Observations = c(Observations1, Observations20, Observations40),
                     Time = c(Runtime1, Runtime20, RUntime40))

# With the data created, plot it
plot(TestDF$Observations, TestDF$Time, type = "o",
     xlab = "Elements", ylab = "Time", main = "mostStrikes Complexity")


# As can be seen by the chart created, the function can be described as O(n) runtime,
# as when the number of elements increases, the runtime increases linearly as well.









