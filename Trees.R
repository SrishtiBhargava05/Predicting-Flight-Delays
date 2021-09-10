#Assignement Three
DF <- read.csv("FullData", header = TRUE)
library("tidyverse")
DF <- DF%>%select(-X)

#Converting to factor
DF$Month <- as.factor(DF$Month)
DF$DayOfWeek <- as.factor(DF$DayOfWeek)
DF$DayofMonth<- as.factor(DF$DayofMonth)
DF$DepHour <- as.factor(DF$DepHour)
DF$DelayTime <- as.factor(DF$DelayTime)

#Dividing the data into train and test
set.seed(1714155)
samp <- sample(nrow(DF), 0.6 * nrow(DF))
train <- DF[samp, ]
test <- DF[-samp, ]
write.csv(train, "Train.csv")
write.csv(test, "Test.csv")
summary(train$DelayTime)/nrow(train)

library(gbm)
library(caret)

#Gradient boosting 
#Model 1: with ntrees 200 
Delayed.Boost <-  gbm(DelayTime ~ DepHour + Distance+ UniqueCarrier +
                        Month + DayofMonth+ DayOfWeek, distribution = "adaboost", 
                      data = train,  n.trees = 100, interaction.depth = 5, 
                      cv.folds = 3, shrinkage = 0.1, class.stratify.cv = TRUE)

summary(Delayed.Boost, xlim = range(DF$Distance))
gbm.perf(Delayed.Boost)
preds <- predict.gbm(Delayed.Boost, newdata = test, n.trees = 100, type = "response")
summary(preds)

#Model 2: ntrees 200, interaction depth 4 
Delayed.Boost.1 <- gbm(DelayTime ~ DepHour + Distance+ U+
                         Month + DayofMonth+ DayOfWeek, distribution = "adaboost", 
                       data = train, n.trees = 1000, interaction.depth = 4)
summary(Delayed.Boost.1)
gbm.perf(Delayed.Boost.2)
Delayed.Boost.2$train.error[5000]
summary(DF$DelayTime)

#Model 3:
Delayed.Boost.2<- gbm(DelayTime ~DepHour + Distance+ UniqueCarrier +
                        Month + DayofMonth+ DayOfWeek, distribution = "adaboost", 
                      data = train, n.trees = 5000)
summary(Delayed.Boost.2)

#Model 4 
Delayed.Boost.3 <- gbm(DelayTime ~., distribution = "adaboost", data = train,
                       n.trees = 400, interaction.depth = 8)
summary(Delayed.Boost.3)
gbm.perf(Delayed.Boost.3)
preds <- predict(Delayed.Boost.3, newdata = test, n.trees = 366, type = "response")
table(preds, test$DelayTime)


#Traing Sample 
set.seed(1714155)
down_train <- downSample(x = train[, -ncol(train)],
                         y = train$DelayTime)
summary(down_train$Class)
down_train <- read.csv("~/Documents/Knowledge Discovery and Data Mining/down_train.csv",
                       header = TRUE)
down_train <- down_train%>%select(-X)%>%mutate(Class = as.factor(Class),
                                               Month = as.factor(Month),
                                               DayofMonth = as.factor(DayofMonth),
                                               DayOfWeek = as.factor(DayOfWeek),
                                               DepHour = as.factor(DepHour))

gbmFit2 <- train(Class ~ Month + Origin + Dest+ UniqueCarrier +
                   DepHour, data = down_train, method = "gbm", 
                 trControl = fitControl, verbose = FALSE)
gbmFit2
preds1<- predict.train(gbmFit2, newdata = test)
truth <- test$DelayTime
x <- table(preds1, truth)
table <- caret::confusionMatrix(x, positive = "1")


#GBM with caret 
fitControl <- trainControl(method = "repeatedcv", number = 2)


#Without altering paramters 
gbmFit2 <- train(DelayTime ~ Month+DayofMonth+DayOfWeek+Dest+Origin 
                 + DepHour, data = train, method = "gbm", trControl = fitControl,
                 verbose = FALSE, tuneGrid = gbmGrid)

summary(gbmFit2$finalModel)
plot(gbmFit1)
gbmFit2
gbmGrid <-  expand.grid(interaction.depth = 6, 
                        n.trees = 400, 
                        shrinkage = 0.1, n.minobsinnode = 10)

gbmFit <- train(DelayTime ~ Month+DayofMonth+DayOfWeek+UniqueCarrier+Distance 
                 + DepHour, data = train, method = "gbm", trControl = fitControl,
                 verbose = FALSE, tuneGrid = gbmGrid)
gbmFit2


gbmFit1 <- train(DelayTime ~ Month+DayofMonth+DayOfWeek+UniqueCarrier+Distance 
                 + DepHour, data = train, method = "gbm",trControl = fitControl,
                 verbose = FALSE)


#Model Assessment
preds<- predict.train(gbmFit1, newdata = test)
truth <- test$DelayTime
x <- table(preds, truth)
x
caret::precision(x, reference = truth, relevant = "1")
caret::recall(x, reference = truth, relevant = "1")
caret::F_meas(x, reference = truth, relevant = "1")

summary(DF$DelayTime)
tab <- freqTable4<- Dat%>%group_by(DepHour)%>%summarise(Mean = mean(DepDelay))

ggplot(freqTable4, aes(x=DepHour, y= Mean, fill = DepHour )) +
  geom_bar(stat="identity")+ theme(legend.position = "none")


citation()
