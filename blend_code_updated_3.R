rm(list=ls())

model_results = read.csv("C:\\Users\\rsoni106\\Documents\\Work\\Methodology Work\\Kaggle\\HomeDepot\\submissions\\Final_subm\\blend_input.csv")

model_results_1 = as.matrix(model_results)

trans_orig_model = solve(t(model_results_1)%*%model_results_1)

# matrix_square = function(matrix) {
#   for (i in 1:nrow(matrix))
#     for (j in 1:ncol(matrix))
#       matrix[[i, j]] = matrix[[i, j]]*matrix[[i, j]]
#     return(matrix)
# }

trans_orig_model_sq = (model_results_1)^2

colsum_trans_orig_model_sq = colSums(trans_orig_model_sq)

### Formula for overall is n(2+2E1^2-E2^2) ######

rmse_0 = nrow(model_results_1)*(2 + 2*1.47868^2- 0.65570^2)  ###RMSE of 0 submission 1: 1.47868, 2: 0.65570

rmse_all = c(0.46047,0.46054)   # RMSE of individual models

rmse_all = as.matrix(rmse_all)
rmse_all = t(rmse_all)
rmse_all = nrow(model_results_1)*rmse_all*rmse_all

merge_all = 0.5*(colsum_trans_orig_model_sq+rmse_0-rmse_all)

weight_matrix = merge_all%*%trans_orig_model
weight_matrix = t(weight_matrix)

final_predict = model_results_1%*%weight_matrix

write.csv(final_predict,"C:\\Users\\rsoni106\\Documents\\Work\\Methodology Work\\Kaggle\\HomeDepot\\submissions\\Final_subm\\blend_out_18.csv")





