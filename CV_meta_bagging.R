library(dplyr)
library(data.table)
library(randomForest)
library(xgboost)

setwd("/mapr/projects/Rishabh_Work/Allstate/prepdata/")

tmpX=readRDS("tmpX.rds"); xgb_val = readRDS("xgb_val.rds")

vars = readRDS("vars.rds")

tdata = xgb.DMatrix(data = as.matrix(tmpX[,c(vars,"pred_weak")]),label = as.matrix(tmpX$logloss))

vdata = xgb.DMatrix(data = as.matrix(xgb_val[,c(vars,"pred_weak")]),label = as.matrix(xgb_val$logloss))

watchlist <- list(validation=vdata,train=tdata) 

base.score = median(tmpX$logloss)

bst = xgb.train(obj = fairobj,  eta = 0.5, min_child_weight = 10,subsample = 0.8,
                data = tdata, column_subsample = 0.6, gamma = 0.05, verbose = 10,
                alpha = 0.5, nrounds=10000, max_depth=8, early.stop.round=2,watchlist=watchlist,
                base_score = base.score, feval = evalerror, maximize=F) 