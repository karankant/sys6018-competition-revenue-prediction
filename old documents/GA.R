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



