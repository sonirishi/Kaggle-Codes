setwd("/mapr/projects/Rishabh_Work/Allstate/prepdata")

full_data = readRDS("full_data_2.rds")

train_data = full_data[seq(1,188318),]
test_data = full_data[188319:nrow(full_data),]

vars_r = setdiff(colnames(train_data),c("id","loss","logloss"))

train_data = as.data.frame(train_data); test_data = as.data.frame(test_data)

library(xgboost)
library(Metrics)

# data_matrix = xgb.DMatrix(data=as.matrix(train_data[,vars_r]),label=as.matrix(train_data$loss))

evalerror = function(preds, data_matrix) {
  labels = getinfo(data_matrix, "label")
  err = as.numeric(mae(labels,preds))
  return(list(metric = "rmse", value = err))
}

library(rBayesianOptimization)

# cv_folds = rBayesianOptimization::KFold(train_data$loss, nfolds = 5,stratified = TRUE, seed = 0)

xgb_cv_bayes = function(max.depth,min_child_weight) {
  cv = xgb.cv(params = list(booster = "gbtree", eta = 0.1,
                            max_depth = max.depth,
                            min_child_weight = min_child_weight,
                            subsample = 0.7, colsample_bytree = 0.5,
                            lambda = 1, alpha = 1, gamma = 0.02,
                            objective = "reg:linear",
                            eval_metric = "rmse"),
              data = as.matrix(train_data[,vars_r]), label=as.matrix(train_data$loss),
              nround = 5000, nfold = 5, prediction = TRUE, showsd = TRUE,
              early.stop.round = 25, maximize = FALSE, verbose = 10, base_score = 2859.758)
  list(Score = cv$dt[, min(test.rmse.mean)],Pred = cv$pred)
}

set.seed(1234)

opt_hyp = BayesianOptimization(xgb_cv_bayes,bounds=list(max.depth=c(6L,8L),min_child_weight=c(1,5)),
                               init_grid_dt = NULL,init_points=1,n_iter=25,acq="ei",eps=0,
                               verbose = 10)