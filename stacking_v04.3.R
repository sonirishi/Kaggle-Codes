library(cvTools)     
library(rJava)
options(java.parameters = "-Xmx2g" )
library(extraTrees)
k = 5 

folds = cvFolds(nrow(full_data), K = k)                                                                      

new_data = as.data.frame(matrix(rep(0,nrow(full_data)),nrow(full_data),1))
test_data_n = as.data.frame(matrix(rep(0,nrow(test_data)),nrow(test_data),1))

train_fin_data = as.data.frame(matrix(rep(0,nrow(full_data)),nrow(full_data),1))
test_fin_data = as.data.frame(matrix(rep(0,nrow(test_data)),nrow(test_data),1))

boost_1 = function(df,vars,y,test){
  model_create = extraTrees(df[,vars], 
                            df[,y],
                            ntree=800,
                            nodesize = 2,
                            evenCuts = FALSE,
                            numThreads = 2,
                            subsetSizes = NULL,
                            na.action = "stop")
  
  prediction = (predict(model_create,validate[,vars]))
  prediction_test = (predict(model_create,test[,vars]))
  
  return(list(prediction,prediction_test))
}

boost_2 = function(df,vars,y,validate,test){
  model_create = extraTrees(df[,vars], 
                            df[,y],
                            ntree=1100,
                            nodesize = 15,
                            evenCuts = FALSE,
                            numThreads = 2,
                            subsetSizes = 3,
                            na.action = "stop")
  
  prediction = (predict(model_create,validate[,vars]))
  prediction_test = (predict(model_create,test[,vars]))
  
  return(list(prediction,prediction_test))
  
}

boost_3 = function(df,vars,y,validate,test){
  model_create = xgboost(objective = "reg:linear", 
                         eta =  0.02,
                         min_child_weight = 4,
                         subsample = .7,
                         colsample_bytree = .65,
                         max_depth = 12,
                         nthread = 10,
                         gamma = 1.5,
                         alpha = 0.001,
                         eval_metric = "rmse",
                         data = as.matrix(df[,vars]),
                         label = as.matrix(df[,y]),                
                         nrounds = 1200, 
                         verbose = 1, 
                         maximize = FALSE)
  
  prediction = (predict(model_create,as.matrix(validate[,vars])))
  prediction_test = (predict(model_create,as.matrix(test[,vars])))
  
  return(list(prediction,prediction_test))
}

boost_4 = function(df,vars,y,validate,test){
  
  model_create = xgboost(objective = "reg:linear", 
                         eta =  0.1,
                         min_child_weight = 1,
                         subsample = .75,
                         colsample_bytree = .75,
                         max_depth = 10,
                         nthread = 10,
                         gamma = 0.6,
                         eval_metric = "rmse",
                         data = as.matrix(df[,vars]),
                         label = as.matrix(df[,y]),                
                         nrounds = 350, 
                         verbose = 1, 
                         maximize = FALSE)
  
  prediction = (predict(model_create,as.matrix(validate[,vars])))
  prediction_test = (predict(model_create,as.matrix(test[,vars])))
  
  return(list(prediction,prediction_test))
}

stack_model_list = c(boost_1,boost_2,boost_3,boost_4)

names(stack_model_list) = c("boost_1","boost_2","boost_3","boost_4")

list_remove = c("id","search_left","ind_string_dist_bit_NA","ind_ratio_dist_NA","product_uid",
                "relevance","value","search_term_stem_rem","product_title","product_title_stem",
                "desc_stem","value_stem_rem","search_term","search_term_stem","ind_string_dist_NA",
                "product_description_stem")

vars_r = setdiff(colnames(full_data),list_remove)

var_list = list()

var_list[[1]] = c("jaro_ratio","srt_jaro","avgjaro_match","sdjaro_match","medjaro_match","jaro_match_minbi",
                  "avgjaro_match_bi","sdjaro_match_bi","medjaro_match_bi","avgjaro_match_bi_att",
                  "sdjaro_match_bi_att","noun_jaro_match_ui","noun_jaro_match_maxui","noun_jaro_match_bi",
                  "noun_jaro_match_maxbi","max_jaro_match","max_jaro_match_bi","alt_jaro_match_ui",
                  "alt_jaro_match_bi","alt_jaro_match_maxbi","no.num_jaro_match_ui","no.num_jaro_match_maxui",
                  "no.num_jaro_match_bi","maxnoun_jaro_match_ui","maxnoun_jaro_match_bi","avgalt_jaro_match_ui",
                  "maxalt_jaro_match_ui","avgalt_jaro_match_bi","maxalt_jaro_match_bi","minno.num_jaro_match_ui",
                  "maxno.num_jaro_match_ui","avgno.num_jaro_match_bi","minno.num_jaro_match_bi",
                  "avgalt.no.num_jaro_match_ui","maxalt.no.num_jaro_match_ui","avgalt.no.num_jaro_match_bi",
                  "maxalt.no.num_jaro_match_bi","avgalt.no.num_attr_jaro_match_ui",
                  "minalt.no.num_attr_jaro_match_ui","maxalt.no.num_attr_jaro_match_ui",
                  "avgalt.no.num_attr_jaro_match_maxui","minalt.no.num_attr_jaro_match_maxui",
                  "avgalt.no.num_attr_jaro_match_bi","minalt.no.num_attr_jaro_match_bi",
                  "maxalt.no.num_attr_jaro_match_bi","avgalt.no.num_attr_jaro_match_maxbi",
                  "minalt.no.num_attr_jaro_match_maxbi","sqrt_jaro_match_bi","sqrt_noun_jaro_match_ui","jaro_col")

var_list[[2]] = vars_r

var_list[[3]] = c("jacc_ratio","jacc_ratio_desc","jacc_match_desc","ratio_jaccdesc_matchdesc","sdjacc_match",
                  "avgjacc_match_attr","sdjacc_match_attr","medjacc_match_attr","jacc_match_minbi",
                  "avgjacc_match_bi","sdjacc_match_bi","medjacc_match_bi","avgjacc_match_bi_att",
                  "sdjacc_match_bi_att","medjacc_match_bi_att","noun_jacc_match_ui","noun_jacc_match_bi",
                  "noun_jacc_match_maxbi","min_jacc_uni_bi","max_jacc_match","max_jacc_match_bi",
                  "alt_jacc_match_ui","alt_jacc_match_bi","no.num_jacc_match_ui","no.num_jacc_match_maxui",
                  "no.num_jacc_match_bi","alt.no.num_jacc_match_ui","avgnoun_jacc_match_ui","minnoun_jacc_match_ui",
                  "maxnoun_jacc_match_ui","avgnoun_jacc_match_bi","minnoun_jacc_match_bi","maxnoun_jacc_match_bi",
                  "avgalt_jacc_match_ui","maxalt_jacc_match_ui","avgalt_jacc_match_bi","maxalt_jacc_match_bi",
                  "avgno.num_jacc_match_ui","minno.num_jacc_match_ui","maxno.num_jacc_match_ui",
                  "minno.num_jacc_match_bi","avgalt.no.num_jacc_match_ui","maxalt.no.num_jacc_match_ui",
                  "avgalt.no.num_jacc_match_bi","avgalt.no.num_attr_jacc_match_ui",
                  "minalt.no.num_attr_jacc_match_ui","maxalt.no.num_attr_jacc_match_ui","avgratio_noun_norm_jacc",
                  "minratio_noun_norm_jacc","maxratio_noun_norm_jacc","avgalt.no.num_attr_jacc_match_maxui",
                  "minalt.no.num_attr_jacc_match_maxui","avgalt.no.num_attr_jacc_match_bi",
                  "maxalt.no.num_attr_jacc_match_bi","avgalt.no.num_attr_jacc_match_maxbi",
                  "maxalt.no.num_attr_jacc_match_maxbi","sqrt_jacc_match_bi","sqrt_jacc_ratio","jacc_mat")

var_list[[4]] = c("cos_ratio","cos_ratio_desc","cos_match_desc","exp_cos_match","exp_cos_ratio_desc","avgcos_match",
                  "sdcos_match","medcos_match","avgcos_match_attr","sdcos_match_attr","medcos_match_attr",
                  "cos_match_bi","cos_match_minbi","avgcos_match_bi","sdcos_match_bi","medcos_match_bi",
                  "avgcos_match_bi_att","sdcos_match_bi_att","noun_cos_match_ui","noun_cos_match_maxui",
                  "noun_cos_match_bi","noun_cos_match_maxbi","max_cos_match","alt_cos_match_ui","alt_cos_match_bi",
                  "alt_cos_match_maxbi","no.num_cos_match_ui","no.num_cos_match_maxui","no.num_cos_match_bi",
                  "maxnoun_cos_match_ui","avgnoun_cos_match_bi","minnoun_cos_match_bi","maxnoun_cos_match_bi",
                  "avgalt_cos_match_ui","maxalt_cos_match_ui","avgalt_cos_match_bi","maxalt_cos_match_bi",
                  "avgno.num_cos_match_ui","minno.num_cos_match_ui","maxno.num_cos_match_ui",
                  "avgno.num_cos_match_bi","minno.num_cos_match_bi","maxno.num_cos_match_bi",
                  "avgalt.no.num_cos_match_ui","maxalt.no.num_cos_match_ui","avgalt.no.num_cos_match_bi",
                  "maxalt.no.num_cos_match_bi","avgalt.no.num_attr_cos_match_ui","minalt.no.num_attr_cos_match_ui",
                  "maxalt.no.num_attr_cos_match_ui","avgalt.no.num_attr_cos_match_maxui",
                  "minalt.no.num_attr_cos_match_maxui","avgalt.no.num_attr_cos_match_bi",
                  "minalt.no.num_attr_cos_match_bi","maxalt.no.num_attr_cos_match_bi",
                  "avgalt.no.num_attr_cos_match_maxbi","sqrt_cos_match_bi")

for(j in 1:length(stack_model_list)){
  for(i in 1:k){
    train = full_data[folds$subsets[folds$which != i], ] 
    validation = full_data[folds$subsets[folds$which == i], ]
    model_create1 = do.call(names(stack_model_list)[j],list(train,var_list[[j]],"relevance",validation,test_data))
    new_data[folds$subsets[folds$which == i],1] = model_create1[[1]]
    test_data_n[,i+1] = model_create1[[2]]
  }
  test_predict_mean = rowMeans(test_data_n[,c(2,3,4,5,6)])
  train_fin_data = cbind(train_fin_data,new_data)
  test_fin_data = cbind(test_fin_data,test_predict_mean)
}

train_fin_data = train_fin_data[,-1]
test_fin_data = test_fin_data[,-1]

colnames(train_fin_data) = c("model1","model2","model3","model4")
colnames(test_fin_data) = c("model1","model2","model3","model4")

saveRDS(train_fin_data,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_stack5.rds")

saveRDS(test_fin_data,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_stack5.rds")

################# XGB on top Model ####################

train_fin_data$relevance = full_data$relevance

train_stack1 = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_stack1.rds")

test_stack1 = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_stack1.rds")

train_stack = as.data.frame(cbind(train_stack1,train_fin_data))
test_stack = as.data.frame(cbind(test_stack1,test_fin_data))

colnames(train_stack) = c("model1", "model2","model3","model4","model5","model6","model7","model8","relevance")
colnames(test_stack) = c("model1", "model2","model3","model4","model5","model6","model7","model8")

vars_stack = colnames(test_stack)

xgb_stacked = xgb.cv(objective = "reg:linear", 
                       eta =  0.01,
                       nfold = 5,
                       min_child_weight = 20,
                       subsample = .75,
                       colsample_bytree = .7,
                       max_depth = 8,
                       nthread = 10,
                       gamma = 1.5,
                       alpha = 0.001,
                       seed = 1,
                       eval_metric = "rmse",
                       data = as.matrix(train_stack[,vars_stack]),
                       label = as.matrix(train_stack[,"relevance"]),                
                       nrounds = 1400, 
                       verbose = 1, 
                       maximize = FALSE)

xgb_subm = xgboost(objective = "reg:linear", 
                     eta =  0.01,
                     nfold = 5,
                     min_child_weight = 20,
                     subsample = .75,
                     colsample_bytree = .7,
                     max_depth = 8,
                     nthread = 10,
                     gamma = 1.5,
                     alpha = 0.001,
                     seed = 1,
                     eval_metric = "rmse",
                     data = as.matrix(train_stack[,vars_stack]),
                     label = as.matrix(train_stack[,"relevance"]),                
                     nrounds = 588, 
                     verbose = 1, 
                     maximize = FALSE)

test_results = predict(stack_lm,test_stack)

submit_boost = cbind(test_data[,"id"],test_results)

colnames(submit_boost) = c("id","relevance")

write.csv(submit_boost,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/stack_submission2.csv")

################# Linear Model (XGB overfit) ####################

indexes = sample(1:nrow(train_stack), size=0.7*nrow(train_stack))

stack_lm = lm(relevance~.,data = train_stack[indexes,!(names(train_stack) %in% c("model2","model5","model11","model15"))])

test_lm = predict(stack_lm,train_stack[-indexes,!(names(train_stack) %in% c("model2","model5","model11","model15"))])

test_lm = as.data.frame(cbind(train_fin_data[-indexes,"relevance"],test_lm))

colnames(test_lm) = c("relevance","predict")

rmse_valid = sqrt(sum((test_lm$relevance - test_lm$predict)^2)/nrow(test_lm))

###################################################################################

train1 = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_stack1.rds")

test1 = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_stack1.rds")

train2 = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_stack2.rds")

test2 = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_stack2.rds")

train3 = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_stack3.rds")

test3 = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_stack3.rds")

train_fin_data = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_stack3.rds")

test_fin_data = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_stack3.rds")

train_fin_data$relevance = full_data$relevance

train_stack = as.data.frame(cbind(train1,train2,train3,train_fin_data))
test_stack = as.data.frame(cbind(test1,test2,test3,test_fin_data))

colnames(train_stack) = c("model1", "model2","model3","model4","model5","model6","model7","model8","model9",
                          "model10","model11","model12","model13","model14","model15","model16","relevance")
colnames(test_stack) = c("model1", "model2","model3","model4","model5","model6","model7","model8","model9",
                         "model10","model11","model12","model13","model14","model15","model16")

stack_lm = lm(relevance~.,data = train_stack[,!(names(train_stack) %in% c("model2","model5","model11","model15"))])

test_results = predict(stack_lm,test_stack)

submit_boost = cbind(test_data[,"id"],test_results)

colnames(submit_boost) = c("id","relevance")

write.csv(submit_boost,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/stack_submission8.csv")

#####################################################################

stack_lm = lm(relevance~.,data = train_fin_data)

test_results = predict(stack_lm,test_fin_data)

submit_boost = cbind(test_data[,"id"],test_results)

colnames(submit_boost) = c("id","relevance")

write.csv(submit_boost,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/stack_submission6.csv")

