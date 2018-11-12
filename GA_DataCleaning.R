library(tidyverse)
library(jsonlite)
library(stringr)
library(feather)
library(data.table)

col_types <- cols(
  channelGrouping = col_character(),
  customDimensions = col_character(),
  date = col_datetime(), # Parses YYYYMMDD
  device = col_character(),
  fullVisitorId = col_character(),
  geoNetwork = col_character(),
  hits = col_skip(), # MASSIVE amount of data!
  #sessionId = col_character(), # not present in v2 comp; not used anwyay
  socialEngagementType = col_skip(), # Skip as always "Not Socially Engaged"
  totals = col_character(),
  trafficSource = col_character(),
  visitId = col_integer(), # visitId & visitStartTime look identical in all but 5000 cases
  visitNumber = col_integer(),
  visitStartTime = col_integer() # Convert to POSIXlt later,
)

# Convert Python array/dictionary string to JSON format
unsnake <- . %>%
  str_replace_all(c("\\[\\]" = "[{}]", # empty element must contain dictionary
                    "^\\[|\\]$" = "", # remove initial and final brackets
                    "(\\[|\\{|, |: |', )'" = "\\1\"", # open single- to double-quote (on key or value)
                    "'(\\]|\\}|: |, )" = '\"\\1')) # close quote

separate_json <- . %>%
  str_replace_all(c("\"[A-Za-z]+\": \"not available in demo dataset\"(, )?" = "",
                    ", \\}" = "}")) %>% # if last property in list was removed
  paste(collapse = ",") %>% paste("[", ., "]") %>% # As fromJSON() isn't vectorised
  fromJSON(., flatten = TRUE)

NMAX = Inf


# Cleaning Test Set -------------------------------------------------------

df <- 
  bind_rows(
    # read_csv("xxa", col_types = col_types, n_max = NMAX) %>% mutate(test = F),
    read_csv("test_v2.csv",  col_types = col_types, n_max = NMAX) %>% mutate(test = T)
  ) %>%
  bind_cols(separate_json(.$device))        %>% select(-device) %>%
  bind_cols(separate_json(.$geoNetwork))    %>% select(-geoNetwork) %>%
  bind_cols(separate_json(.$totals))        %>% select(-totals) %>%
  bind_cols(separate_json(.$trafficSource)) %>% select(-trafficSource) %>%
  bind_cols(separate_json(unsnake(.$customDimensions))) %>% select(-customDimensions)

# Remove junk to save a bit of time on loading
df$visits <- NULL # [2] constant column, no information
df$adwordsClickInfo.gclId <- NULL # [3,4] useless hash, no obvious information
#sessionId # [2] "unique" sessionId (some duplicates), not present in v2 comp
#campaignCode # [2] only one row with value, none present in test set (may not be true in v2 comp)

# Identify types
df <-
  df %>%
  mutate_at(vars(hits:transactions, index), as.integer) %>%
  mutate(
    visitStartTime = lubridate::as_datetime(visitStartTime),
    transactionRevenue = as.numeric(transactionRevenue), # Target
    totalTransactionRevenue = as.numeric(transactionRevenue)
  )

format(object.size(df), units = "auto")

#cleaning NUll-like values(https://www.kaggle.com/kailex/r-eda-for-gstore-glm-keras-xgb/code)
is_na_val <- function(x) x %in% c("not available in demo dataset", "(not provided)",
                                  "(not set)", "<NA>", "unknown.unknown",  "(none)")

df %<>% mutate_all(funs(ifelse(is_na_val(.), NA, .)))

read_csv("xaa_1.csv", col_types = col_types, n_max = NMAX) %>% mutate(test = F)
# Cleaning train Set -------------------------------------------------------
df2 <- 
  bind_rows(
    read_csv("xaa_1.csv", col_types = col_types, n_max = NMAX) %>% mutate(test = F),
    read_csv("test_v2.csv",  col_types = col_types, n_max = NMAX) %>% mutate(test = T)
  ) %>%
  bind_cols(separate_json(.$device))        %>% select(-device) %>%
  bind_cols(separate_json(.$geoNetwork))    %>% select(-geoNetwork) %>%
  bind_cols(separate_json(.$totals))        %>% select(-totals) %>%
  bind_cols(separate_json(.$trafficSource)) %>% select(-trafficSource) %>%
  bind_cols(separate_json(unsnake(.$customDimensions))) %>% select(-customDimensions)

# Remove junk to save a bit of time on loading
df2$visits <- NULL # [2] constant column, no information
df2$adwordsClickInfo.gclId <- NULL # [3,4] useless hash, no obvious information
#sessionId # [2] "unique" sessionId (some duplicates), not present in v2 comp
#campaignCode # [2] only one row with value, none present in test set (may not be true in v2 comp)

# Identify types
df2 <-
  df2 %>%
  mutate_at(vars(hits:transactions, index), as.integer) %>%
  mutate(
    visitStartTime = lubridate::as_datetime(visitStartTime),
    transactionRevenue = as.numeric(transactionRevenue), # Target
    totalTransactionRevenue = as.numeric(transactionRevenue)
  )

format(object.size(df2), units = "auto")

#cleaning NUll-like values(https://www.kaggle.com/kailex/r-eda-for-gstore-glm-keras-xgb/code)
is_na_val <- function(x) x %in% c("not available in demo dataset", "(not provided)",
                                  "(not set)", "<NA>", "unknown.unknown",  "(none)")

df2 %<>% mutate_all(funs(ifelse(is_na_val(.), NA, .)))


test <- df
train <- df2 
# RandomForest ------------------------------------------------------------
#LK : we should modify the below code. 

#converting transactionRevenue to Log
train['totals.totalTransactionRevenue'] = as.numeric(unlist(train['totals.totalTransactionRevenue']))

#Train-Test Split 
cols = c('trafficSource.medium','device.deviceCategory','geoNetwork.continent','geoNetwork.subContinent','device.isMobile','totals.pageviews','totals.totalTransactionRevenue')
sample_train = train[,cols]
sample_train$totals.pageviews[is.na(sample_train$totals.pageviews)] <- 0
sample_train$totals.totalTransactionRevenue[is.na(sample_train$totals.totalTransactionRevenue)] <- 0
y.col <- 'totals.totalTransactionRevenue'
x.cols <- setdiff(names(sample_train), y.col)
dim(sample_train)
#
set.seed(123562) 
sample = sample.split(sample_train$totals.totalTransactionRevenue, SplitRatio = .8)

sub_train = subset(sample_train, sample == TRUE)
sub_test  = subset(sample_train, sample == FALSE)

paste('training set rows: ',dim(sub_train)[1])
paste('test set rows: ',dim(sub_test)[1])

#Modeling
h2o.init(nthreads=-1,max_mem_size='10G')
sub_train = as.h2o(sub_train)
sub_test = as.h2o(sub_test)
RF_model = h2o.randomForest(x=x.cols,
                            y=y.col,
                            ntrees = 60,
                            mtries = -1,
                            max_depth=7,
                            nfolds = 50,
                            training_frame=sub_train,
                            validation_frame=sub_test,
                            seed=123523
)


#Submission
setnames(test, "medium", "trafficSource.medium")
setnames(test, "deviceCategory", "device.deviceCategory")
setnames(test, "continent", "geoNetwork.continent")
setnames(test, "subContinent", "geoNetwork.subContinent")
setnames(test, "isMobile", "device.isMobile")
setnames(test, "pageviews", "totals.pageviews")

#
cols = c('trafficSource.medium','device.deviceCategory','geoNetwork.continent','geoNetwork.subContinent','device.isMobile','totals.pageviews')
sample_test = test[,cols]
sample_test = as.h2o(sample_test)
preds = as.data.frame(h2o.predict(RF_model,sample_test))
fullVisitorId <- test$fullVisitorId
submission = data.frame(fullVisitorId, preds)
submission = submission  %>% group_by(fullVisitorId)  %>% summarise(PredictedLogRevenue = sum(predict))
submission$PredictedLogRevenue <- submission$PredictedLogRevenue + 1
submission$PredictedLogRevenue <- log(submission$PredictedLogRevenue)
head(submission)

write.table(submission, file = "submission_rf.csv", row.names=F, col.names=c("fullVisitorId","PredictedLogRevenue"), sep=",")

