#install.packages("lspline")
library(lspline)
library(tidyverse)
library(jsonlite)
library(dplyr)
library(magrittr)
library(lubridate)
library(data.table)
library(boot)

#setwd("D:/DM competitions/sys6018-competition-revenue-prediction/New/")

#---------------------------------------------------------------
# TRAIN SET
#---------------------------------------------------------------

# loading data
train <- read_csv("train_clean.csv")

# extracting new variables from the dataset

train["day_of_week"] <- wday(train$date)
train["hour"] <- hour(as.POSIXct(train$visitStartTime, origin="1970-01-01"))
train["month"] <- month(train$date)

cols = c('date','campaign','source','day_of_week','hour','month','medium','deviceCategory','continent','subContinent','isMobile','pageviews', 'day_of_week', 'transactionRevenue')

# subsetting data for relevant columns
sample_train = train[,cols]

# filling in the missing values
sample_train$month[is.na(sample_train$month)] <- 0
sample_train$hour[is.na(sample_train$hour)] <- 25
sample_train$day_of_week[is.na(sample_train$day_of_week)] <- 0

sample_train$date[is.na(sample_train$date)] <- "1970-01-01"

sample_train$campaign[is.na(sample_train$campaign)] <- 'Unknown'
sample_train$source[is.na(sample_train$source)] <- 'Unknown'

sample_train$pageviews[is.na(sample_train$pageviews)] <- 0
sample_train$transactionRevenue[is.na(sample_train$transactionRevenue)] <- 0

# converting variables into factor if needed
sample_train$month <- as.factor(sample_train$month)
sample_train$day_of_week <- as.factor(sample_train$day_of_week)
sample_train$hour <- as.factor(sample_train$hour)
sample_train$medium <- as.factor(sample_train$medium)
sample_train$deviceCategory <- as.factor(sample_train$deviceCategory)
sample_train$continent <- as.factor(sample_train$continent)
sample_train$subContinent <- as.factor(sample_train$subContinent)
sample_train$isMobile <- as.factor(sample_train$isMobile)
sample_train$source <- as.factor(sample_train$source)
sample_train$campaign <- as.factor(sample_train$campaign)

# ---------------------------------------------------------
# TEST
# ---------------------------------------------------------

test <- read_csv("test_clean_newdataset.csv")

# extracting new variables from the dataset

test["date_new"] <- as.POSIXct(test$date, origin="1970-01-01")
test["day_of_week"] <- wday(test$date_new)
test["hour"] <- hour(as.POSIXct(test$visitStartTime, origin="1970-01-01"))
test["month"] <- month(test$date_new)
test["date"] <- as.Date(test$date_new)

cols = c('date','campaign','source','day_of_week','hour','month','medium','deviceCategory','continent','subContinent','isMobile','pageviews', 'day_of_week', 'transactionRevenue')

# subsetting data for relevant columns
sample_test = test[,cols]

# filling in the missing values
sample_test$month[is.na(sample_test$month)] <- 0
sample_test$hour[is.na(sample_test$hour)] <- 25
sample_test$day_of_week[is.na(sample_test$day_of_week)] <- 0
sample_test$date[is.na(sample_test$date)] <- "1970-01-01"
sample_test$campaign[is.na(sample_test$campaign)] <- 'Unknown'
sample_test$source[is.na(sample_test$source)] <- 'Unknown'
sample_test$pageviews[is.na(sample_test$pageviews)] <- 0
sample_test$transactionRevenue[is.na(sample_test$transactionRevenue)] <- 0

# converting variables into factor if needed
sample_test$month <- as.factor(sample_test$month)
sample_test$day_of_week <- as.factor(sample_test$day_of_week)
sample_test$hour <- as.factor(sample_test$hour)
sample_test$medium <- as.factor(sample_test$medium)
sample_test$deviceCategory <- as.factor(sample_test$deviceCategory)
sample_test$continent <- as.factor(sample_test$continent)
sample_test$subContinent <- as.factor(sample_test$subContinent)
sample_test$isMobile <- as.factor(sample_test$isMobile)
sample_test$source <- as.factor(sample_test$source)
sample_test$campaign <- as.factor(sample_test$campaign)

#--------------------------------------------------------
# SPLINES
#--------------------------------------------------------

levels(sample_train$campaign) <- union(levels(sample_train$campaign), levels(sample_test$campaign))
levels(sample_train$source) <- union(levels(sample_train$source), levels(sample_test$source))

# Knots at n equal intervals
spline.n1 <- lm(transactionRevenue ~ date+campaign+source+day_of_week+hour+month+
                 medium+deviceCategory+subContinent+isMobile+continent+elspline(pageviews, 2),
               data=sample_train)

spline.n2 <- lm(transactionRevenue ~ date+campaign+source+day_of_week+hour+month+
                  medium+deviceCategory+subContinent+isMobile+continent+elspline(pageviews, 4),
                data=sample_train)

spline.n3 <- lm(transactionRevenue ~ date+campaign+source+day_of_week+hour+month+
                  medium+deviceCategory+subContinent+isMobile+continent+elspline(pageviews, 6),
                data=sample_train)

spline.n4 <- lm(transactionRevenue ~ date+campaign+source+day_of_week+hour+month+
                  medium+deviceCategory+subContinent+isMobile+continent+elspline(pageviews, 8),
                data=sample_train)

#cross validating
cv.error1 = cv.glm(sample_train ,spline.n1 ,K=5)$delta[1]
cv.error2 = cv.glm(sample_train ,spline.n2 ,K=5)$delta[1]
cv.error3 = cv.glm(sample_train ,spline.n3 ,K=5)$delta[1]
cv.error4 = cv.glm(sample_train ,spline.n4 ,K=5)$delta[1]

# picking out the best model

# Adding missing variable levels
spline.n3$xlevels[["campaign"]] <- union(spline.n3$xlevels[["campaign"]], levels(sample_test$campaign))
spline.n3$xlevels[["source"]] <- union(spline.n3$xlevels[["source"]], levels(sample_test$source))

# predicting
y1 <- predict(spline.n3, newdata = sample_test)

fullVisitorId <- test$fullVisitorId
submission = data.frame(fullVisitorId, y1)

#putting zero where values are negative
submission$y1[submission$y1<0] <- 0

#grouping by fullvisitorId
submission = submission  %>% group_by(fullVisitorId)  %>% summarise(PredictedLogRevenue = sum(y1))
#adding 1 to it
submission$PredictedLogRevenue <- submission$PredictedLogRevenue + 1
#taking log
submission$PredictedLogRevenue <- log(submission$PredictedLogRevenue)
head(submission)

write.table(submission, file = "submission_spline1.csv", row.names=F, col.names=c("fullVisitorId","PredictedLogRevenue"), sep=",")

#--------------------------------------------------------
#--------------------------------------------------------

# Knots at quantiles of x
spline.q1 <- lm(transactionRevenue ~ date+campaign+source+day_of_week+hour+month+medium
                 +deviceCategory+subContinent+isMobile+continent+qlspline(pageviews, 2),
               data=sample_train)

spline.q2 <- lm(transactionRevenue ~ date+campaign+source+day_of_week+hour+month+medium
               +deviceCategory+subContinent+isMobile+continent+qlspline(pageviews, 4),
               data=sample_train)

spline.q3 <- lm(transactionRevenue ~ date+campaign+source+day_of_week+hour+month+medium
               +deviceCategory+subContinent+isMobile+continent+qlspline(pageviews, 6),
               data=sample_train)

spline.q4 <- lm(transactionRevenue ~ date+campaign+source+day_of_week+hour+month+medium
               +deviceCategory+subContinent+isMobile+continent+qlspline(pageviews, 8),
               data=sample_train)

#cross validating
cv.error1 = cv.glm(sample_train ,spline.q1 ,K=5)$delta[1]
cv.error2 = cv.glm(sample_train ,spline.q2 ,K=5)$delta[1]
cv.error3 = cv.glm(sample_train ,spline.q3 ,K=5)$delta[1]
cv.error4 = cv.glm(sample_train ,spline.q4 ,K=5)$delta[1]

# picking out the best model

# Adding missing variable levels
spline.q2$xlevels[["campaign"]] <- union(spline.q2$xlevels[["campaign"]], levels(sample_test$campaign))
spline.q2$xlevels[["source"]] <- union(spline.q2$xlevels[["source"]], levels(sample_test$source))

# predicting
y2 <- predict(spline.q2, newdata = sample_test)

fullVisitorId <- test$fullVisitorId
submission2 = data.frame(fullVisitorId, y2)

#putting zero where values are negative
submission2$y2[submission2$y2<0] <- 0

#grouping by fullvisitorId
submission2 = submission2  %>% group_by(fullVisitorId)  %>% summarise(PredictedLogRevenue = sum(y2))
#adding 1 to it
submission2$PredictedLogRevenue <- submission2$PredictedLogRevenue + 1
#taking log
submission2$PredictedLogRevenue <- log(submission2$PredictedLogRevenue)
head(submission2)

write.table(submission2, file = "submission_spline2.csv", row.names=F, col.names=c("fullVisitorId","PredictedLogRevenue"), sep=",")

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------