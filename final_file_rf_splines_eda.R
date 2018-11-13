library(tidyverse)
library(jsonlite)
library(dplyr)
library(stringr)
library(feather)
library(data.table)
library(magrittr)
library(lubridate)
library(reshape2)
library(naniar)
library(ggplot2)
library(UpSetR)
library(scales)
library(caret)
options(warn=-1)
library(caTools)
library(Metrics)
library(h2o)
library(randomForest)
library(scales)
library(lspline)
library(tidyverse)
library(jsonlite)
library(dplyr)
library(magrittr)
library(lubridate)
library(data.table)
library(boot)


# DATA CLEANING PART 1 (for train set) -----------------------------------------------------------
# Initially, we worked with the old dataset and we got the cleaned train set and the cleaned test set.
# However, in the middle of competition, the entire new data was provided, 
# and the professor said we could still use the old train set to build a model. 
# However, for submission purpose on Kaggle,we needed to use "new" test set from "new dataset"
# By the end, here we cleaned "both" train/test data(from old dataset).
# But we won't use "cleaned test set" created by the below code.

# load data
train.data <- read.csv("train.csv",colClasses=c(fullVisitorId = 'character'))
test.data <- read.csv("test.csv",colClasses=c(fullVisitorId = 'character'))

#combine data for cleaning
train_test.data <-rbind(train.data, test.data)

#JSON columns are "device", "geoNetwork", "totals", "trafficSource"
temp_device <- paste("[", paste(train_test.data$device, collapse = ","), "]") %>% fromJSON(flatten = T)
temp_geoNetwork <- paste("[", paste(train_test.data$geoNetwork, collapse = ","), "]") %>% fromJSON(flatten = T)
temp_totals <- paste("[", paste(train_test.data$totals, collapse = ","), "]") %>% fromJSON(flatten = T)
temp_trafficSource <- paste("[", paste(train_test.data$trafficSource, collapse = ","), "]") %>% fromJSON(flatten = T)

#combine dataset and drop device, geoNetwork, totals and trafficSource
train_test_clean.data <- cbind(train_test.data,temp_device,temp_geoNetwork,temp_totals,temp_trafficSource)
train_test_clean.data <- train_test_clean.data[,-c(3,5,8,9)]

#cleaning NUll-like values
is_na_val <- function(x) x %in% c("not available in demo dataset", "(not provided)",
                                  "(not set)", "<NA>", "unknown.unknown",  "(none)")

train_test_clean.data %<>% mutate_all(funs(ifelse(is_na_val(.), NA, .)))

#change datatype
train_test_clean.data %<>%
  mutate(date = ymd(date),
         pageviews = as.integer(pageviews),
         bounces = as.integer(bounces),
         newVisits = as.integer(newVisits),
         transactionRevenue = as.numeric(transactionRevenue))

#Remove consant variables
fea_uniq_values <- sapply(train_test_clean.data, n_distinct)
(fea_del <- names(fea_uniq_values[fea_uniq_values == 1]))
train_test_clean.data %<>% select(-one_of(fea_del))

#remove NA on TranscationRevenue
train.data$transactionRevenue[is.na(train.data$transactionRevenue)] <- 0
train.data$transactionRevenue

#save cleaned dataset 
# (this code is needed since we don't need to run the above. We can simply load the cleaned csv files)
write.csv(train_test_clean.data[1:903653,], "train_clean.csv")
write.csv(train_test_clean.data[903654:1708337,], "test_clean.csv")

# cleaning is done and split dataset.
# (this code is needed since we don't need to run the above. We can simply load the cleaned csv files)

train.data <- train_test_clean.data[1:903653,]
test.data <- train_test_clean.data[903654:1708337,]

#load new data
# (this code is needed since we don't need to run the above. We can simply load the cleaned csv files)
train.data <- read.csv("train_clean.csv",colClasses=c(fullVisitorId = 'character'))
train.data <- train.data[,-c(1)]
test.data <- read.csv("test_clean.csv",colClasses=c(fullVisitorId = 'character'))
test.data <- test.data[,-c(1)]

# DATA CLEANING PART 2 (for test set) -----------------------------------
# Since the test set from "new dataset" is different from the old one. 
# We needed another way to clean this test set. 
# we won't use "hits" "socialEngagementType" columns since they don't contain useful information 
# and computationally it would take a lot to flatten
col_types <- cols(
  channelGrouping = col_character(),
  customDimensions = col_character(),
  date = col_datetime(),
  device = col_character(),
  fullVisitorId = col_character(),
  geoNetwork = col_character(),
  hits = col_skip(),
  socialEngagementType = col_skip(), 
  totals = col_character(),
  trafficSource = col_character(),
  visitId = col_integer(), 
  visitNumber = col_integer(),
  visitStartTime = col_integer() 
)

# Convert array/dictionary string to JSON format
unsnake <- . %>%
  str_replace_all(c("\\[\\]" = "[{}]", 
                    "^\\[|\\]$" = "", 
                    "(\\[|\\{|, |: |', )'" = "\\1\"", 
                    "'(\\]|\\}|: |, )" = '\"\\1')) 

separate_json <- . %>%
  str_replace_all(c("\"[A-Za-z]+\": \"not available in demo dataset\"(, )?" = "",
                    ", \\}" = "}")) %>% 
  paste(collapse = ",") %>% paste("[", ., "]") %>% 
  fromJSON(., flatten = TRUE)

NMAX = Inf

#clean test_vs.csv
test.v2 <- 
  bind_rows(
    read_csv("test_v2.csv",  col_types = col_types, n_max = NMAX) %>% mutate(test = T)
  ) %>%
  bind_cols(separate_json(.$device))        %>% select(-device) %>%
  bind_cols(separate_json(.$geoNetwork))    %>% select(-geoNetwork) %>%
  bind_cols(separate_json(.$totals))        %>% select(-totals) %>%
  bind_cols(separate_json(.$trafficSource)) %>% select(-trafficSource) %>%
  bind_cols(separate_json(unsnake(.$customDimensions))) %>% select(-customDimensions)

# Remove "visits" and "adwordsClickInfo.gclId" since they don't have useful information 
# also to save computation time.
test.v2$visits <- NULL 
test.v2$adwordsClickInfo.gclId <- NULL 

# Identify types
test.v2 <-
  test.v2 %>%
  mutate_at(vars(hits:transactions, index), as.integer) %>%
  mutate(
    visitStartTime = lubridate::as_datetime(visitStartTime),
    transactionRevenue = as.numeric(transactionRevenue), # Target
    totalTransactionRevenue = as.numeric(transactionRevenue)
  )

format(object.size(test.v2), units = "auto")

#cleaning NUll-like values
is_na_val <- function(x) x %in% c("not available in demo dataset", "(not provided)",
                                  "(not set)", "<NA>", "unknown.unknown",  "(none)")

test.v2 %<>% mutate_all(funs(ifelse(is_na_val(.), NA, .)))
test <- test.v2

# cleaning is done above.The below code is needed when we use cleaned dataset
train <- read.csv("train_clean.csv",colClasses=c(fullVisitorId = 'character'))
test <- read.csv("test_clean_newdataset.csv",colClasses=c(fullVisitorId = 'character'))
train['transactionRevenue'] = as.numeric(unlist(train['transactionRevenue']))

# DATA EXPLORATION :checking the portion of missing values on each column-----
train.temp.data %>% summarise_all(funs(sum(is.na(.))/n()*100)) %>% 
  gather(key="feature", value="missing_pct") %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity", fill="steelblue")+
  labs(y = "missing %", x = "features") +
  coord_flip() +
  theme_minimal()

# DATA EXPLORATION: create a new column for analysis
train["day_of_week"] <- wday(train$date)
test["day_of_week"] <- wday(test$date)

# ---------------------------------------------------------------------------
# Exploratory Data Analysis
# ---------------------------------------------------------------------------


#---------------------------------------------------------------
# TRAIN SET
#---------------------------------------------------------------

# loading clean train data
train <- read_csv("train_clean.csv")

# extracting new variables from the dataset

train["day_of_week"] <- wday(train$date)
train["hour"] <- hour(as.POSIXct(train$visitStartTime, origin="1970-01-01"))
train["month"] <- month(train$date)

cols = c('date','campaign','source','day_of_week','hour','month','medium','deviceCategory','continent','subContinent','isMobile','pageviews', 'day_of_week', 'transactionRevenue')

# subsetting data for relevant columns
sample_train = train[,cols]
dim(train)

# 903653 rows with 41 columns
n_distinct(train$sessionId)

# The number of unique sessionId's comes close to the total number of observations (903,653), but is 898 short.

# ---------------------------------------------------------------------------

# Missing value plots to see which variable is missing
options(repr.plot.height=4)
NAcol <- which(colSums(is.na(train)) > 0)
NAcount <- sort(colSums(sapply(train[NAcol], is.na)), decreasing = TRUE)
NADF <- data.frame(variable=names(NAcount), missing=NAcount)
NADF$PctMissing <- round(((NADF$missing/nrow(train))*100),1)
NADF %>%
  ggplot(aes(x=reorder(variable, PctMissing), y=PctMissing)) +
  geom_bar(stat='identity', fill='blue') + coord_flip(y=c(0,110)) +
  labs(x="", y="Percent missing") +
  geom_text(aes(label=paste0(NADF$PctMissing, "%"), hjust=-0.1))


# ---------------------------------------------------------------------------

#setting missing values to zero
train$transactionRevenue[is.na(train$transactionRevenue)] <- 0

y <- train$transactionRevenue #saving original values in a vector
train$transactionRevenue <- train$transactionRevenue/1000000


train %>% filter(transactionRevenue >0) %>% summarize('number of transactions'=n(), 'total revenues train set'=sum(transactionRevenue))

# # A tibble: 1 x 2
# `number of transactions` `total revenues train set`
# <int>                      <dbl>
#   1                    11194                   1255790.

# ---------------------------------------------------------------------------

range(train %>% select(transactionRevenue) %>% filter(transactionRevenue !=0))
train %>% filter(transactionRevenue>=1000) %>% summarize('number of transactions with at least 1000 USD revenues'=n(), 'sum revenues of transactions with at least 1000 USD revenues'=sum(transactionRevenue))

# range of transaction values
# [1]    0.01 2110.80

options(repr.plot.height=4)
train %>% filter(transactionRevenue >0 & transactionRevenue<1000) %>%
  ggplot(aes(x=transactionRevenue)) +
  geom_histogram(fill="blue", binwidth=10) +
  scale_x_continuous(breaks= seq(0, 1000, by=100), labels = comma)

# We see that most transaction values lie in the range of 0-100$

# ---------------------------------------------------------------------------

range(train$date)
# range of the training set is from 1st October 2016 to 1st october 2017

# ---------------------------------------------------------------------------

# Transaction by time of day, day of week, month

ggplot(train, aes(x=day_of_week, y=transactionRevenue)) + geom_bar(stat="identity")

# Here we see that day 3 (refering to Tuesday) has the most no of transaction values when it comes to sales

ggplot(train, aes(x=month, y=transactionRevenue)) + geom_bar(stat="identity")

# Here we see that the month 8 (refering to August) has most no of transaction values when it comes to sales

ggplot(train, aes(x=hour, y=transactionRevenue)) + geom_bar(stat="identity")

# Here we see that the afternoon hours (refering to 13,14) have most no of transaction values when it comes to sales

# -----------------------------------------------------------------------------------------------

# Session frequency

library(grid)
library(gridExtra)

options(repr.plot.height=6)
d1 <- train %>% group_by(date) %>% summarise(dailySessions = n()) %>%
  ggplot(aes(x=date, y=dailySessions)) + geom_line(col='blue') +
  scale_y_continuous(labels=comma) + geom_smooth(col='red') +
  labs(x="", y="Sessions per Day") + scale_x_date(date_breaks = "1 month", date_labels = "%b %d")
d2 <- train %>% group_by(date) %>% summarise(dailyRevenue = sum(transactionRevenue)) %>%
  ggplot(aes(x=date, y=dailyRevenue)) + geom_line(col='blue') +
  scale_y_continuous(labels=comma) + geom_smooth(col='red') +
  labs(x="", y="Daily Revenues (USD)") + scale_x_date(date_breaks = "1 month", date_labels = "%b %d")
grid.arrange(d1,d2)


# Here we see a time series plot of sessions per day and daily revenues
# We notice a consistent marginal decline as time progresses
# We notice a quadratic low - peak -  low (Aug, Nov, Jan) when it comes to sessions per day which stablizes over time

# -----------------------------------------------------------------------------------------------

# PageViews
#install.packages("repr")
#install.packages("ggrepel")

options(repr.plot.height=4)

p1 <- train %>% filter(!is.na(train$pageviews) & pageviews <=28) %>% 
  ggplot(aes(x=pageviews)) +
  geom_histogram(fill='blue', binwidth=1) +
  scale_y_continuous(breaks=seq(0, 500000, by=100000), label=comma) +
  scale_x_continuous(breaks=seq(0, 28, by=5)) +
  coord_cartesian(x=c(0,28))

p2 <- train %>% filter(!is.na(train$pageviews) & pageviews <=28) %>% group_by(pageviews) %>%
  ggplot(aes(x=pageviews, y=transactionRevenue)) +
  geom_bar(stat='summary', fun.y = "sum", fill='blue') +
  scale_x_continuous(breaks=seq(0, 28, by=5)) +
  coord_cartesian(x=c(0,28)) + labs(y="sum of revenues")
grid.arrange(p1, p2)

#sessions with more than 28 pageviews all have frequencies of less than 1,000. Since these are hardly visible, these are being excluded
#excluding 100 pageview NAs

# We see that pageviews arounfd 15 fetch the most amount of revenue
# We also see that majority of the users have only 1 page view (and don't fetch any revenue)

#-------------------------------------------
# defining functions for plotting
#-------------------------------------------


#-------------------------------------------
plotSessions <- function(dataframe, factorVariable, topN=10) {
  var_col <- enquo(factorVariable)
  dataframe %>% count(!!var_col) %>% top_n(topN, wt=n) %>%
    ggplot(aes_(x=var_col, y=~n, fill=var_col)) +
    geom_bar(stat='identity')+
    scale_y_continuous(labels=comma)+
    labs(x="", y="number of sessions")+
    theme(legend.position="none")
}

#also creating a function to plot transactionRevenue for a factorvariable
plotRevenue <- function(dataframe, factorVariable, topN=10) {
  var_col <- enquo(factorVariable)
  dataframe %>% group_by(!!var_col) %>% summarize(rev=sum(transactionRevenue)) %>% filter(rev>0) %>% top_n(topN, wt=rev) %>% ungroup() %>%
    ggplot(aes_(x=var_col, y=~rev, fill=var_col)) +
    geom_bar(stat='identity')+
    scale_y_continuous(labels=comma)+
    labs(x="", y="Revenues (USD)")+
    theme(legend.position="none")
}

# -----------------------------------------------------------------------------------------------
# Channel Grouping

options(repr.plot.height=6)

#adding reordering of x manually
sessionOrder <- train %>% count(channelGrouping) %>% top_n(10, wt=n) %>% arrange(desc(n))
sessionOrder <- sessionOrder$channelGrouping

c1 <- plotSessions(train, channelGrouping) + scale_x_discrete(limits=sessionOrder)
c2 <- plotRevenue(train, channelGrouping) + scale_x_discrete(limits=sessionOrder)
grid.arrange(c1, c2)

# Here we see that channelgrouping 8 (Organic search) has the most amount of sessions

# Here we see that channelgrouping 7 (Referral) has the most amount of revenue generated

# -----------------------------------------------------------------------------------------------


# RandomForest ------------------------------------------------------------
# out of 37 variables, the following variables won't be used because there are too many missing values
# The below is general comments on variable selection
# "channelGrouping"               
# "date" --discarded, but we will use "day_of_week" which was extracte from this.                        
# "fullVisitorId" -This is just a ID.                 
# "sessionId" -This is just a ID.                      
# "visitId" -This is just a ID.                         
# "visitNumber"                   
# "visitStartTime"                
# "browser" -It is belived this is not a useful info to predict Y.                     
# "operatingSystem" -It is belived this is not a useful info to predict Y.               
# "isMobile" -used in the RF model                  
# "deviceCategory" -used in the RF model                
# "continent" -used in the RF model                     
# "subContinent" -- used in the RF model                 
# "country"                       
# "region"                        
# "metro"                         
# "city"                          
# "networkDomain" -It is belived this is not a useful info to predict Y.                
# "hits" -discarded since it is belived it doesn't contain useful info.                         
# "pageviews" -used in the RF model                    
# "bounces"                       
# "newVisits"                     
# "transactionRevenue" -This is Y            
# "campaign"                      
# "source" -It is belived this is not a useful info to predict Y.                         
# "medium" - used in the RF model                      
# "keyword"                       
# "isTrueDirect"  -Missing value is more than 65%                  
# "referralPath" -Missing value is more than 85%               
# "adContent" -Missing value is more than 85%                     
# "campaignCode" -Missing value is more than 85%                 
# "adwordsClickInfo.page" -Missing value is more than 85%         
# "adwordsClickInfo.slot" -Missing value is more than 85%         
# "adwordsClickInfo.gclId" -Missing value is more than 85%         
# "adwordsClickInfo.adNetworkType" -Missing value is more than 85% 
# "adwordsClickInfo.isVideoAd" -Missing value is more than 85%   


# FT - Model 1 ------------------------------------------------------------
#Train-Test Split 
cols = c( 'day_of_week','medium','deviceCategory','continent','subContinent','isMobile','pageviews','transactionRevenue')
sample_train = train[,cols]
sample_train$pageviews[is.na(sample_train$pageviews)] <- 0
sample_train$transactionRevenue[is.na(sample_train$totals.transactionRevenue)] <- 0
y.col <- 'transactionRevenue'
x.cols <- setdiff(names(sample_train), y.col)
dim(sample_train)

# We split train set for CV. 
set.seed(1) 
sample = sample.split(sample_train$transactionRevenue, SplitRatio = .8)
sub_train = subset(sample_train, sample == TRUE)
sub_test  = subset(sample_train, sample == FALSE)

#Modeling
# After trying different parameters on same model, there wouldn't be much difference if we 
# go beyond the given values.(ntrees = 50,max_depth=50,nfolds = 10)
h2o.init(nthreads=-1,max_mem_size='10G')
sub_train = as.h2o(sub_train)
sub_test = as.h2o(sub_test)
RF_model = h2o.randomForest(x=x.cols,
                            y=y.col,
                            ntrees = 50,
                            mtries = -1,
                            max_depth=50,
                            nfolds = 10,
                            training_frame=sub_train,
                            validation_frame=sub_test,
                            seed=1)
# RF_model will give you summary of RF model with Cross validation information on test(originally derived from train set)
RF_model
#
cols = c('day_of_week','medium','deviceCategory','continent','subContinent','isMobile','pageviews')
sample_test = test[,cols]
sample_test = as.h2o(sample_test)
preds = as.data.frame(h2o.predict(RF_model,sample_test))
fullVisitorId <- test$fullVisitorId
submission = data.frame(fullVisitorId, preds)
submission = submission  %>% group_by(fullVisitorId)  %>% summarise(PredictedLogRevenue = sum(predict))
submission$PredictedLogRevenue <- submission$PredictedLogRevenue + 1

# converting transactionRevenue to Log
submission$PredictedLogRevenue <- log(submission$PredictedLogRevenue)

write.table(submission, file = "submission_rf_model1.csv", row.names=F, col.names=c("fullVisitorId","PredictedLogRevenue"), sep=",")


# FT - Model 2  -----------------------------------------------------------
#Train-Test Split 
cols = c( 'day_of_week','keyword','deviceCategory','visitNumber','subContinent','isMobile','pageviews','transactionRevenue')
sample_train = train[,cols]
sample_train$pageviews[is.na(sample_train$pageviews)] <- 0
sample_train$transactionRevenue[is.na(sample_train$totals.transactionRevenue)] <- 0
y.col <- 'transactionRevenue'
x.cols <- setdiff(names(sample_train), y.col)
dim(sample_train)

# We split train set for CV. 
set.seed(1) 
sample = sample.split(sample_train$transactionRevenue, SplitRatio = .8)
sub_train = subset(sample_train, sample == TRUE)
sub_test  = subset(sample_train, sample == FALSE)

#Modeling
# After trying different parameters on same model, there wouldn't be much difference if we 
# go beyond the given values.(ntrees = 50,max_depth=50,nfolds = 10)
h2o.init(nthreads=-1,max_mem_size='10G')
sub_train = as.h2o(sub_train)
sub_test = as.h2o(sub_test)
RF_model = h2o.randomForest(x=x.cols,
                            y=y.col,
                            ntrees = 50,
                            mtries = -1,
                            max_depth=50,
                            nfolds = 10,
                            training_frame=sub_train,
                            validation_frame=sub_test,
                            seed=1)
# RF_model will give you summary of RF model with Cross validation information on test(originally derived from train set)
RF_model
#
cols = c('day_of_week','keyword','deviceCategory','visitNumber','subContinent','isMobile','pageviews')
sample_test = test[,cols]
sample_test = as.h2o(sample_test)
preds = as.data.frame(h2o.predict(RF_model,sample_test))
fullVisitorId <- test$fullVisitorId
submission = data.frame(fullVisitorId, preds)
submission = submission  %>% group_by(fullVisitorId)  %>% summarise(PredictedLogRevenue = sum(predict))
submission$PredictedLogRevenue <- submission$PredictedLogRevenue + 1

# converting transactionRevenue to Log
submission$PredictedLogRevenue <- log(submission$PredictedLogRevenue)

write.table(submission, file = "submission_rf_model2.csv", row.names=F, col.names=c("fullVisitorId","PredictedLogRevenue"), sep=",")

# Random Forest Conclusion ------------------------------------------------
# We checked different FT models. After we checked MSE of each model, we belived the 2nd model 
# would provide a lower prediction. Accordingly the final model for FT would be FT2. 


# ---------------------------------------
# SPLINES
# -----------------------------------------------------------------------------------------

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