##Assignment 3 with the dataset
train <- read.csv("./mlcourse/flight_delays_train.csv", header = TRUE)
test <- read.csv("./mlcourse/flight_delays_test.csv", header = TRUE)
summary(train)

#Trees 
library(tidyverse)
library(rattle)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
attach(train)

train$Month <- as.numeric(train$Month)
train$Origin <- as.numeric(train$Origin)
train$Dest <- as.numeric(train$Dest)

#FullRR with all covariates
rf <- randomForest(dep_delayed_15min ~.,data = train, mtry = 8, importance = TRUE)
predict.rf <- predict(rf.1, newdata = test)
rf

rf.1 <- randomForest(dep_delayed_15min ~.-Origin - Dest,data = train, mtry = 8,
                     importance = TRUE)
rf.1

rf.2 <- randomForest(dep_delayed_15min ~.-Origin - Dest,data = train,
                     importance = TRUE)
rf.2

test <- test%>%filter(!(UniqueCarrier == "9E"))
test$UniqueCarrier[test$UniqueCarrier == "9E"] <- NA
test <- na.omit(test)
levels(test$UniqueCarrier)
