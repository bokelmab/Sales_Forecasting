## read data
train <- fread('data/train.csv', nrows = )

## divide into training and test set
data_small.tr <- data_small[-folds[[i]],]
data_small.ts <- data_small[folds[[i]],]

## convert data_small for use of xgboost
options(na.action = 'na.pass')
data_small.tr.xgb <- model.matrix(return~.-1, data_small.tr %>% subset(select = p.features)) 
data_small.ts.xgb <- model.matrix(return~.-1, data_small.ts %>% subset(select = p.features)) 