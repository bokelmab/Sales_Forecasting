## libraries
library(data.table)
library(tidyr) ## create parameter grid
library(dplyr)
library(xgboost)
library(Metrics) ## calculate errors
library(caret) ## cross validation
library(foreach) ## parallel computation
library(doSNOW) ## parallel computation
library(magrittr)
library(lubridate)

## functions for data processing and predictive modeling
source('Code/feature_engineering.R') ## feature engineering (takes 30 minuates)
source('Code/parameter_tuning.R') ## parameter tuning
source('Code/model_evaluation.R') ## model evaluation

## read training data
## data.all is the data set after feature engineering
data.tr <- fread("data/data.all.csv", nrows = 2756003)

## set features
features <- c('adFlag', 'availability', 'competitorPrice', 'price', 'revenue', 'av_rate_pid', 'clicks_pid', 'seq3',
              'seq4', 'seq5', 'seq6', 'seq7', 'seq8')  ## list of features 


## 2) parameter tuning for extreme gradient boosting --------------------------------------------
grid.xgboost <- crossing(max_depth = 2:10, eta = (1:7) / 10, nrounds = 20) ## parameter grid
idx.best.parameters <- tune_parameters(data.tr, grid.xgboost, features)


## 3) model evaluation --------------------------------------------------------------------------
evaluate_model(data, grid.xgboost[idx.best.parameters, ], features) ## evaluate model with best tuning parameters


## 4) build and evaluate model -------------------------------------------------------------------

## build model
data.tr.xgb <- model.matrix(revenue ~ .-1, data.tr %>% subset(select = features)) 
param <- grid.xgboost[idx.best.parameters, ]
model <- xgboost(data.tr.xgb, label = data.tr$revenue, max_depth = param$max_depth, 
                 eta = param$eta, nthread = 2, nrounds = param$nrounds, 
                 colsample_bytree = 1, eval_metric = 'rmse', verbose = 0)
rm(data.tr, data.tr.xgb)

## load test set
data.ts <- fread("data/data.all.csv", nrows = 1210767, skip = 2756004)
names(data.ts) <- fread("data/data.all.csv", nrows = 1) %>% names

##evaluate
data.ts.xgb <- model.matrix(revenue~.-1, data.ts %>% subset(select = features)) 
rm(data.ts) ## remove test set
solution <- fread('data/realclass.csv')

predicted.revenue <- predict(model, newdata = data.ts.xgb) ## predict probability of return
(solution$revenue - predicted.revenue)^2 %>% sum %>% sqrt

## variable importance
importance_matrix <- xgb.importance(colnames(data.ts.xgb), model = model)
xgb.plot.importance(importance_matrix)

