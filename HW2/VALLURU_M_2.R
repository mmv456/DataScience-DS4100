#################################
#        Mahitha Valluru        #
#        DS4100                 #
#        Prof. Schedlbauer      #
#        Homework 2             #
#################################

library(rpart)
library(knitr)
library(ggplot2) # library for scatterplot
#require(Hmisc)
library(tibble) # for creating the random values for the training set
#library(data.table)
require(dplyr)

# Set a working directory
setwd("D:/Mahitha/DataScience/HW2")

# Creates the data frame if it does not already exist
loadDF <- function() {
  if (!exists("customerdata")) {
    customerdata<-read.csv("customertxndata.csv")
  }
}

# Loads the data
loadDF()


# Computes the total number of cases the customerdata dataframe has
# Args: None
# Returns: a number consisting of how many elements there are in the customerdata dataframe
TotalNumberOfCases <- function() {
  return(length(customerdata$Visits))
}
# Return value: 22,800

# Calculates the average number of visits there are 
# Args: None
# Returns: a number indicating the mean number of visits to the place
MeanNumberofVisits <- function() {
  totalNumberOfVisits <- sum(customerdata$Visits)
  return(totalNumberOfVisits/TotalNumberOfCases())
}
# Return value: 12.48649

# Calculates the median amount of revenue the totoal number of people have who shop here
# Args: None
# Returns: A number with the median revenue
MedianRevenue <- function() {
  medianValue <- median(customerdata$Revenue, na.rm = TRUE)
  return(medianValue)           # returns the value of 11,409 - which isn't the middle vale (11,400) - is this ignoring all 0s?
}
# Return value: 344.6516

# Give us the maximimum number of transactions one person made out of all the customers in the data set
# Args: None
# Returns: A number
MaximumNumberOfTransactions <- function() {
  maxNumber <- max(customerdata$Transactions, na.rm = TRUE)
  return(maxNumber)
}
# Return value:2

# Give us the minimum number of transactions one person made out of all the customers in the data set
# Args: None
# Returns: A number
MinimumNumberOfTransactions <- function() {
  minNumber <- min(customerdata$Transactions, na.rm = TRUE)
  return(minNumber)
}
# Return value: 0

# Give us the operating system used the most out of everyone in the data set
# Args: None
# Returns: Either "Android" or "iOS"
MostCommonOperatingSystem <- function() {
  android <- length(which(customerdata$OS == "Android"))
  ios <- length(which(customerdata$OS == "iOS"))
  
  if (android > ios) { return("Android") }
  else { return("iOS") }
}
# Return value: "Android"


# Creates a scatterplot between the operating system and revenue
# Args: None
# Returns: A scatterplot
CreateScatterplot <- function() {
  operatingSystem <- customerdata$OS
  revenue <- customerdata$Revenue
  ggplot(customerdata, aes(x=operatingSystem, y=revenue)) + geom_point(shape=1)
}







# To Impute the missing data -------------------------------------------------------------------------
customerdata.new <- customerdata   # creating a new data frame to impute missing values

# We will impute transactions based on revenue; if the revenue is below a certain value, we will
# impute with the mean of the transactions with people below that revenue amount
mean.low <- mean(customerdata.new$Transactions[customerdata.new$Revenue < 1000], na.rm = T)
mean.high <- mean(customerdata.new$Transactions[customerdata.new$Revenue >= 1000], na.rm = T)

# This is imputing transactions based on how much revenue they make.
customerdata.new[customerdata.new$Revenue < 1000 & is.na(customerdata.new$Transactions),]$Transactions <- round(mean.low)
customerdata.new[customerdata.new$Revenue >= 1000 & is.na(customerdata.new$Transactions),]$Transactions <- round(mean.high)

# Imputing gender based on the mean value of gender, which is male
customerdata.new$Gender[is.na(customerdata.new$Gender)] <- "Male"

# Adding these values on to the original dataframe
new <- lm(Revenue ~ Visits + Transactions + OS + Gender , data = customerdata.new)

customerdata <- customerdata.new

#-----------------------------------------------------------------------


# Split data into two different training sets -------------------------

odd.training <- customerdata[c(TRUE, FALSE),]
even.validation <- customerdata[c(FALSE, TRUE),]

# --------------------------------------------------------------------------------


random.training <- customerdata[sample(1:nrow(customerdata), TotalNumberOfCases()/2),]
notrandom.validation <- even.validation

# ---------------------------------------------------------------------------------------------------------

# given the dataframe, calculate the mean revenue 
GetMeanRevenue <- function(Dataframe) {
  sumRevenue <- sum(Dataframe$Revenue)
  totalNumber <- length(Dataframe$Visits)
  
  return(sumRevenue/totalNumber)
}



# odd.training = 449.6105
# even.validation = 460.26
# random.training = 455.95
# notrandom.validation = 460.26


# As you can see, the mean number of revenue is approximately the same for all four data frames. The range is
# about $11, and as an alaysis, splitting the datasets by odd and even or by random and notrandom yield us the 
# same results.

# -------------------------------------------------------------------------------------------------------




# tibble is a great package for creating training and validation sets, especially if you would like random values.
# Basically, you will need to split the data into certain indices, and then do:
# training = data[indexes,]
# and for validation, use all the indices not used in training
# validation = data[-indexes,]







