## a function which returns the index of the best tuning parameters
tune_parameters <- function(p.data, p.grid, p.features){
  
  ## to shorten run time only 10% of the data is used
  idx.include <- createDataPartition(p.data$revenue, p = 0.01)
  data_small <- p.data[idx.include[[1]], ]
  rm(p.data)
  
  start.time <- Sys.time() ## calculate run time 
  
  ## cross validation to choose best set of parameters
  folds <- createFolds(data_small$revenue, k = 10) ## 10-fold cross validation
  cv.results.ts <- data.frame(matrix(rep(0, 10 * nrow(p.grid)), ncol = 10)) ## results test set
  cv.results.tr <- data.frame(matrix(rep(0, 10 * nrow(p.grid)), ncol = 10)) ## results training set
  
  for(i in 1:10){ ## folds
    
    ## divide into training and test set
    data_small.tr <- data_small[-folds[[i]],]
    data_small.ts <- data_small[folds[[i]],]
    
    ## convert data_small for use of xgboost
    options(na.action = 'na.pass')
    data_small.tr.xgb <- model.matrix(revenue~.-1, data_small.tr %>% subset(select = p.features)) 
    data_small.ts.xgb <- model.matrix(revenue~.-1, data_small.ts %>% subset(select = p.features)) 
    
    ## settings for parallel computing
    cl<-makeCluster(2) #change the 2 to your number of CPU cores
    registerDoSNOW(cl)
    
    cv.results <- foreach(j = 1:nrow(p.grid), .packages = c('dplyr', 'xgboost','Metrics'), .combine = rbind) %dopar% { ## paramters
      
      model <- xgboost(data_small.tr.xgb, label = data_small.tr$revenue, max_depth = p.grid$max_depth[j], eta = p.grid$eta[j], nthread = 2, 
                       nrounds = p.grid$nrounds[i], colsample_bytree = 1, eval_metric = 'rmse', verbose = 0)
      
      predicted.revenue <- predict(model, newdata = data_small.ts.xgb) ## predict probability of return
      
      c(model$evaluation_log$train_rmse[p.grid$nrounds[i]],rmse(data_small.ts$revenue, predicted.revenue)) ## results accuracy
    }
    
    stopCluster(cl) ## stop parallel computing
    
    cv.results.tr[, i] <- cv.results[, 1]
    cv.results.ts[, i] <- cv.results[, 2]
    
  }
  
  ## choose best set of parameters 
  average.rmse <- apply(cv.results.ts, 1, mean)
  index.best.parameters <- which(average.rmse == max(average.rmse))
  
  ## check for overfitting
  print('In-Sample rmse')
  apply(cv.results.tr, 1, mean)[index.best.parameters] %>% print
  print('----------------------------')
  print('Out-Of-Sample rmse')
  apply(cv.results.ts, 1, mean)[index.best.parameters] %>% print
  
  return(index.best.parameters)
}