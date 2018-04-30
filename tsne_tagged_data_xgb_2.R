.rs.restartR()

setwd("/mapr/projects/Rishabh_Work/Allstate/prepdata/")

train_data = readRDS("trainX.rds")
test_data = readRDS("testX.rds")

event = readRDS("event.RDS")

library(Metrics)

evalerror = function(preds, data_matrix) {
  labels = getinfo(data_matrix, "label")
  err = as.numeric(mae(exp(labels),exp(preds)))
  return(list(metric = "rmse", value = err))
}

fairobj <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  con <- 0.7
  x <- preds-labels
  grad <- con*x / (abs(x)+con)
  hess <- con^2 / (abs(x)+con)^2
  return(list(grad = grad, hess = hess))
}

# temp = xgb.cv(objective = fairobj, data=data_matrix, nthread=5,
#               eta = 0.001, min_child_weight = 50,subsample = 0.7,nfold=5,
#               colsample_bytree = 0.7, max_depth = 10, gamma = 0.02,alpha = 1,
#               eval_metric=evalerror,nrounds = 50000, verbose = 10, maximize = FALSE,
#               early.stop.round = 75,base_score = 7.74)

library(cvTools)
library(xgboost)

logevent = log(event+200)

k = 10

folds = cvFolds(nrow(train_data), K = k)                                                                      

test_predict = matrix(rep(0,nrow(test_data)),nrow(test_data),1)

for(i in 1:k){
  train = train_data[folds$subsets[folds$which != i], ] 
  train_y = logevent[folds$subsets[folds$which != i]]
  
  set.seed(2016)
  
  base = median(train_y)
  
  train_matrix = xgb.DMatrix(data = as.matrix(train),label = as.matrix(train_y))
  
  bst = xgb.train(data = train_matrix, obj = fairobj,
                  eta = 0.01, min_child_weight = 100,subsample = 0.7,
                  colsample_bytree = 0.7,max_depth = 12,gamma = 0,alpha = 0,
                  eval_metric=evalerror,nrounds = 2325, verbose = 10, maximize = FALSE,
                  base_score = base)
  
  exp_predict = predict(bst,as.matrix(test_data))
  
  print(mean(exp(exp_predict)-200))
  
  test_predict = test_predict + (exp(exp_predict)-200)
}

test_predict = test_predict/10

write.csv(test_predict,"test_10_cv.csv")
