rm(list=ls(all=T))
require(xgboost)
require(randomForest)
require(data.table)
options(scipen=999)
set.seed(1004)
require(Metrics)

setwd("/mapr/projects/Rishabh_Work/Allstate/prepdata/")

trainX = readRDS("trainX.rds")
testX = readRDS("testX.rds")

event = readRDS("event.RDS")

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

logevent = log(event+200)

data_matrix = xgb.DMatrix(data = as.matrix(trainX),label = as.matrix(logevent))

watchlist = list(train=data_matrix)

bst = xgb.train(data = data_matrix, obj = fairobj, watchlist = watchlist,
                eta = 0.01, min_child_weight = 100,subsample = 0.7,
                colsample_bytree = 0.7,max_depth = 12,gamma = 0,alpha = 0,
                feval=evalerror,nrounds = floor(2325/0.8), verbose = 10, maximize = FALSE,
                base_score = 7.747411)

# Make prediction
pred = predict(bst,as.matrix(testX))

pred = exp(pred) - 200

saveRDS(pred,"pred_tsne.RDS")

tmpC = 1:20

tmpL = nrow(trainX)

bagged_pred = matrix(rep(0,nrow(testX)),nrow(testX),1)

trind = 1:nrow(trainX)

for (z in tmpC) {
  print(z)
  tmpS1 = sample(trind,size=tmpL,replace=T)
  tmpS2 = setdiff(trind,tmpS1)
  
  tmpX2 = trainX[tmpS2,]; print(dim(tmpX2))
  tmpY2 = logevent[tmpS2]
  
  ############# CV done on XGB for 100 tree RF but data was 0.8 times hence increasing #########
  
  cst = randomForest(x = tmpX2, y = tmpY2, replace=F, ntree=125, do.trace=T, mtry=16)
  
  tmpX1 = trainX[tmpS1,]; print(dim(tmpX1))
  tmpY1 = logevent[tmpS1]
  
  tmpX2 = predict(cst, tmpX1)
  tmpX3 = predict(cst, testX)
  
  base.score = median(tmpY1)
  
  ndata = xgb.DMatrix(data = as.matrix(cbind(tmpX1,tmpX2)),label = as.matrix(tmpY1))
  
  watchlist = list(train=ndata)
  
  ########## Ntree calculated using simple validation sample ############
  
  bst = xgb.train(obj = fairobj,  eta = 0.01, min_child_weight = 10,subsample = 0.8,
            data = ndata, column_subsample = 0.6, gamma = 0.05, verbose = 10,
            alpha = 0.5, nrounds=2154, max_depth=8,watchlist=watchlist,nthread=5,
            base_score = base.score, feval = evalerror, maximize=F) 
  
  # Make prediction
  pred0 = predict(bst,as.matrix(cbind(testX,tmpX3)))
  
  pred0 = exp(pred0) - 200; print(mean(pred0))
  
  bagged_pred = cbind(bagged_pred,pred0)
}

total_pred = cbind(bagged_pred,pred)

saveRDS(total_pred,"total_pred.rds")

total_pred1 = total_pred[,-1]

avg_pred = apply(total_pred1,1,sum)
avg_pred1 = avg_pred/21
avg_pred1 = as.data.frame(avg_pred1)

write.csv(avg_pred1,"meta_bagged_prediction_6.csv")

double_bag_avg = apply(bagged_pred,1,mean)

new_avg = cbind(double_bag_avg,pred)

new_pred = apply(new_avg,1,mean)

write.csv(new_pred,"meta_bagged_prediction_7.csv")

write.csv(total_pred,"all_predictions_append.csv")
