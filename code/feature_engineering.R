## libraries
library(data.table)
library(dplyr)
library(magrittr)

## read data
train <- fread('data/train.csv')
class <- fread('data/class.csv')

## merge train and test
class[, c('revenue', 'click', 'order', 'basket') := NA]
class$revenue %<>% as.numeric
class$click %<>% as.integer
class$basket %<>% as.integer
class$order %<>% as.integer

setcolorder(class, names(train))

data.all <- rbind(train, class)
data.all$train <- rep(c(1, 0), c(nrow(train), nrow(class)))


### product-specific features ---------------------------------
data.all$av_rate_pid <- data.all %$% ave(revenue, pid, FUN = function(x) mean(x, na.rm = T)) ## average revenue per product

data.all$clicks_pid <- data.all %$% ave(revenue, pid, FUN = length) ## total clicks per product

### is product part of an ordered sequence of pid's?
## This is a trick about this data set.

for(j in 3:8){ ## mark all ordered sequences of length j 
  
  ## ordered sequence of length j
  check.order <- data.all$pid[-((nrow(data.all) - (j - 2)) : nrow(data.all))]
  for(i in 1:(j - 1)){
    if(i < (j - 1)){
      check.order <- cbind(check.order, data.all$pid[-c(1:i, (nrow(data.all) - (j - 2) + i) : nrow(data.all))])
    }else{
      check.order <- cbind(check.order, data.all$pid[-(1:i)])
    }
    
  }
  check.order %<>% data.table
  seq.order <- apply(check.order, 1, order) %>% t %>% as.data.table
  is.ordered <- apply(seq.order, 1, function(x) all(x == 1:j))
  is.ordered <- c(is.ordered, rep(F, (j - 1)))
  seq <- ifelse(is.ordered, 1, 0) ## mark first element of sequence
  
  
  ## mark the rest of the elements
  idx.seq <- which(seq == 1)
  for(i in 1:(j - 1)){
    seq[idx.seq + i] <- 1
  }
  data.all[, paste0('seq', j) := seq] ## create new variable in data
  
}

fwrite(data.all, 'data/data.all.csv')


rm(list = ls())

