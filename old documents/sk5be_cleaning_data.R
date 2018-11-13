library(tidyverse)
library(jsonlite)
library(dplyr)
library(magrittr)
library(lubridate)
library(data.table)
# DATA CLEANING -----------------------------------------------------------
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

#cleaning NUll-like values(https://www.kaggle.com/kailex/r-eda-for-gstore-glm-keras-xgb/code)
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

#save cleaned dataset
write.csv(train_test_clean.data[1:903653,], "train_clean.csv")
write.csv(train_test_clean.data[903654:1708337,], "test_clean.csv")

# cleaning is done and split dataset.
train.data <- train_test_clean.data[1:903653,]
test.data <- train_test_clean.data[903654:1708337,]

# #load new data
train.data <- read.csv("train_clean.csv",colClasses=c(fullVisitorId = 'character'))
test.data <- read.csv("test_clean.csv",colClasses=c(fullVisitorId = 'character'))
# train.data <- fread("train_clean.csv")
# test.data <- fread("test_clean.csv")

# useful ------------------------------------------------------------------
train.temp.data %>% summarise_all(funs(sum(is.na(.))/n()*100)) %>% 
  gather(key="feature", value="missing_pct") %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity", fill="steelblue")+
  labs(y = "missing %", x = "features") +
  coord_flip() +
  theme_minimal()
