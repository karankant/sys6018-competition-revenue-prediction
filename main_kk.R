library(h2o)
library(caret)
library(lme4)
library(ggalluvial)
library(xgboost)
library(jsonlite)
library(lubridate)
library(knitr)
library(Rmisc)
library(scales)
library(countrycode)
library(highcharter)
library(glmnet)
library(keras)
library(forecast)
library(zoo)
library(magrittr)
library(tidyverse)


train<-read_csv('data/train.csv')
test<-read_csv('data/test.csv')

flatten_json <- . %>% 
  str_c(., collapse = ",") %>% 
  str_c("[", ., "]") %>% 
  fromJSON(flatten = T)

parse <- train %>% 
  bind_cols(flatten_json(train$device)) %>%
  bind_cols(flatten_json(train$geoNetwork)) %>% 
  bind_cols(flatten_json(train$trafficSource)) %>% 
  bind_cols(flatten_json(train$totals)) %>% 
  select(-device, -geoNetwork, -trafficSource, -totals)

