#################################
#        Mahitha Valluru        #
#        DS4100                 #
#        Prof. Schedlbauer      #
#        Homework 1             #
#################################

library(XML)

# Set a working directory
setwd("C:/Mahitha/DataScience/HW1")


# Creates the data frame if it does not already exist
loadDF <- function() {
  if (!exists("airlinedelays")) {
    airlinedelays<-read.table("AirlineDelays.txt", header=TRUE, sep=",")
  }
}

# Loads the data
loadDF()



# Finds and returns the total number of delays of the carriers
# passed to the functions
# Args: The carrier name in the form of a string
# Returns: The number of delays for the given carrier
TotalDelaysByCarriers<-function(Carrier) {
  
  # boolean value, makes sure to check for the given Carrier only
  isCarrierSame <- airlinedelays[,3] == Carrier   
  
  # boolean value that makes sure the number of minutes delayed is
  # greater than 0
  isDepartureDelayed <- airlinedelays[,6] > 0
  isArrivalDelayed <- airlinedelays[,7] > 0
  
  # counts the number of delays for the carrier
  listDelays <- which((isDepartureDelayed | isArrivalDelayed) & isCarrierSame)
  
  return(length(listDelays))
  
}


# Finds and returns the total number of departure delays 
# for a particular airport
# Args: The origin name in the form of a string
# Returns: The number of departure delays for the given origin
TotalDepartureDelaysByOrigin<-function(Origin) {
  
  # boolean value, makes sure to check for the given origin only
  isOriginSame <- airlinedelays[,4] == Origin
  
  # boolean value that makes sure the number of minutes delayed is
  # greater than 0
  isDepartureDelayed <- airlinedelays[,6] > 0
  
  # counts the number of delays for the origin
  listDelays <- which(isDepartureDelayed & isOriginSame)
  
  return(length(listDelays))
}



# Calculates and returns the average arrival delay in minutes for a carrier
# flying into the specified destination airpport
# Args: The Carrier and Destination in the form of Strings
# Returns: The average number of minutes all flights from the given
#          carrier going to the given destination have been delayed
# NOTE: The tests for this function passed, but took ~30 seconds to process
AvgDelay<-function(Carrier, Dest) {
  
  # variable to keep track of the total number of minutes of all
  # delays of flights from the given carrier going to the given
  # destination
  totalMinutes <- 0
  
  # variable to keep track of all flights from the given carrier
  # going to the given destination
  numberFlights <- 0 
  
  for(i in 1:dim(airlinedelays)[1]) {         # A for loop is the best way to go about this
    
    
    # checks to see if the carrier and destination are the same, and also if the number of minutes late is 
    # positive and not NA
    if(airlinedelays$CARRIER[i] == Carrier    # I tried an if statement for the first time. It does take up a bit
       & airlinedelays$DEST[i] == Dest        # of time to process, but I believe this is the best way to go about
       & airlinedelays$ARR_DELAY[i] > 0       # calculating the totalMinutes and numberFlights variables
       & is.na(airlinedelays$ARR_DELAY[i]) == FALSE) {  
      
      # adds the number of minutes late
      totalMinutes <- totalMinutes + airlinedelays$ARR_DELAY[i]    
      
      # keeps count of the number of delays 
      numberFlights <- numberFlights + 1 
      
      
    }
    
  }
  
  # divides the total number of minutes late by the total number of flights
  average <- totalMinutes/numberFlights
  
  # rounds the average to 2 decimal places, as followed by the test cases given to
  # us by homework 1
  return(round(average), digits = 2) 
  
}





