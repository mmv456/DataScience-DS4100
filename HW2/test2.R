###Make sure the below packages are installed on your R environment, else first install them 
### using install.packages("packageName")  and then knit this Rmd file

library(rpart)
library(knitr)
library(stargazer)
library(ggplot2)

opts_chunk$set(results = 'asis', comment = NA)

data.train <- read.csv("customertxndata.csv")

data.train.avg <- data.train
mean.android <- mean(data.train.avg$Transactions[data.train.avg$OS == "Android"], na.rm = T)
mean.ios <- mean(data.train.avg$Transactions[data.train.avg$OS == "iOS"], na.rm = T)

data.train.avg[data.train.avg$OS == "Android" & is.na(data.train.avg$Transactions),]$Transactions <- round(mean.android)
data.train.avg[data.train.avg$OS == "iOS" & is.na(data.train.avg$Transactions),]$Transactions <- round(mean.ios)
data.train.avg$Gender[is.na(data.train.avg$Gender)] <- "Male"

avg <- lm(Revenue ~ Visits + Transactions + OS + Gender , data = data.train.avg)
