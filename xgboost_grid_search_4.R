GridSearch = expand.grid(subsample = c(0.75), 
                                colsample_bytree = c(0.7), eta = c(0.01),
                                max.depth = c(10,12),
                                gamma = c(1.2,1),
                                ntree = 2000,
                                alpha = c(0.001), min_child_weight=c(15,20))

#Build a xgb.DMatrix object
data_matrix = xgb.DMatrix(data = as.matrix(full_data_fin[,vars_r]), label = as.matrix(full_data_fin[,"relevance"]))

df_gridsearch_results = apply(GridSearch, 1, function(hyper_param){
  
  #Extract Parameters to test
  currentSubsampleRate = hyper_param[["subsample"]]
  currentColsampleRate = hyper_param[["colsample_bytree"]]
  currenteta = hyper_param[["eta"]]
  currentdepth = hyper_param[["max.depth"]]
  currentntree = hyper_param[["ntree"]]
  currentleafsize = hyper_param[["min_child_weight"]]
  currentgamma = hyper_param[["gamma"]]
  currentalpha = hyper_param[["alpha"]]
  
  xgboost_model_cv = xgb.cv(data =  data_matrix, nrounds = currentntree, nfold = 5, showsd = TRUE, seed=1234,
                           metrics = "rmse", verbose = TRUE, "eval_metric" = "rmse", "early.stop.round" = 5,
                           "objective" = "reg:linear", "max.depth" = currentdepth, "eta" = currenteta,                               
                           "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate,
                           "min_child_weight" = currentleafsize,"nthread" = 10, alpha = currentalpha,
                           gamma = currentgamma)
  
  model_scores = as.data.frame(xgboost_model_cv)
  #Save rmse of the last iteration
  rmse_test = tail(model_scores$test.rmse.mean, 1)
  rmse_train = tail(model_scores$train.rmse.mean, 1)
  ntree = nrow(model_scores)
  
  return(c(rmse_train,rmse_test,currentSubsampleRate,currentColsampleRate,
           currenteta,ntree,currentdepth,currentleafsize,currentgamma,currentalpha))
  
})

df_gridsearch_results1 = as.data.frame(t(df_gridsearch_results))

colnames(df_gridsearch_results1) = c("Train_Err","Test_Err","Subsample","Colsample","ETA",
                                         "TreeCount","Depth","MinChildWt","Gamma","Alpha")

df_gridsearch_results1 = df_gridsearch_results1 %>% arrange(Test_Err)

saveRDS(df_gridsearch_results1,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/hyper_param_search_results5.rds")