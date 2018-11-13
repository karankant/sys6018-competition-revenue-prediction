library(caret)
library(readr)
library(tidyverse)
library(data.table)
train<-read_csv('data/train_clean.csv')
test<-read_csv('data/test_clean_newdataset.csv')

#add day of week, hour and mo
train["day_of_week"] <- wday(train$date)
train["hour"] <- hour(as.POSIXct(train$visitStartTime, origin="1970-01-01"))
train["month"] <- month(train$date)

#removing NA values and replacing with 0
train$transactionRevenue[is.na(train$transactionRevenue)]=0
#subsetting with values where transaction value is greater than 0 
train.subset<-train[train$transactionRevenue>0,]
#subsetting for a certain set of predictors with high p values
train.subset2<-train.subset[c('day_of_week','hour','month','pageviews','isMobile','transactionRevenue')]
#replace NA values with 0 or unknown 
train.subset2[is.na(train.subset2$pageviews)]=0
train.subset2[is.na(train.subset2)]<-'Unknown'
train$pageviews<-as.numeric(train$pageviews)

#create model
train.lm<-lm(transactionRevenue~.,data=train.subset2)
summary(train.lm)

train_control <- trainControl(method="cv", number=10)


# fix the parameters of the algorithm
grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
# train the model
model <- train(Species~., data=iris, trControl=train_control, method="nb", tuneGrid=grid)
# summarize results
print(model)
#setup test set
test["date_new"] <- as.POSIXct(test$date, origin="1970-01-01")
test["day_of_week"] <- wday(test$date_new)
test["hour"] <- hour(as.POSIXct(test$visitStartTime, origin="1970-01-01"))
test["month"] <- month(test$date_new)
test.subset<-test[c('day_of_week','hour','month','pageviews','browser','isMobile')]
#test.subset[is.na(test.subset$pageviews)]<-0
test.subset[is.na(test.subset)]<-'Unknown'
y1 <- predict(train.lm, newdata = test.subset)
y1[y1<0] <- 1

#prepare submission and get log trasnformations
submission <- as.data.frame(test$fullVisitorId)
submission['PredictedLogRevenue'] <- log(y1)
names(submission) <- c("fullVisitorId","PredictedLogRevenue")
submission$PredictedLogRevenue[submission$PredictedLogRevenue == 0] <- mean(submission$PredictedLogRevenue)


#get mean for duplicate Id
by_fullVisitorId <- group_by(submission,fullVisitorId)
PredictedLogRevenue_means <- summarise(by_fullVisitorId, meanRev = mean(PredictedLogRevenue))
names(PredictedLogRevenue_means) <- c("fullVisitorId","PredictedLogRevenue")
submission <- PredictedLogRevenue_means
submission$PredictedLogRevenue[is.na(submission$PredictedLogRevenue)] <- mean(submission$PredictedLogRevenue, na.rm = TRUE)

write.table(submission, file = "submission.csv",row.names=F,col.names=c("fullVisitorId","PredictedLogRevenue"), sep=",")
