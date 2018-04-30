rm(list=ls(all=T))

setwd("C:/Users/rsoni106/Documents/Work/Methodology Work/Kaggle/Completed/Allstate/prepared_data")

library(data.table)
library(dplyr)
library(Hmisc)

train_data = fread("train_data_prep1.csv")

test_data = fread("test_data_prep1.csv")

catcols =  colnames(train_data)[grep("cat",colnames(train_data))]

contcols =  colnames(train_data)[grep("cont",colnames(train_data))]

train_data[,V1:=NULL]; test_data[,V1:=NULL]

library(caret)
library(moments)

for(i in contcols){
  temp = caret::BoxCoxTrans(train_data[,get(i)])
  train_data[,paste0(i,"bc") := unlist(predict(temp,get(i)))]
  test_data[,paste0(i,"bc") := unlist(predict(temp,get(i)))]
}

train_data[,contcols:=NULL,with=F]; test_data[,contcols:=NULL,with=F]

for(i in c("cat80","cat79","cat101","cat12","cat81","cat87","cat100","cat10","cat1")){
  train_data[,paste0(i,"mean"):= mean(logloss),get(i)]
  train_data[,paste0(i,"std"):= sd(logloss),get(i)]
  train_data[,paste0(i,"skew"):= moments::skewness(logloss),get(i)]
  temp = train_data[,mean(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"mean"))
  test_data = test_data %>% inner_join(temp,by=i)
  temp = train_data[,sd(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"std"))
  test_data = test_data %>% inner_join(temp,by=i)
  temp = train_data[,moments::skewness(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"skew"))
  test_data = test_data %>% inner_join(temp,by=i)
}

test_data = as.data.table(test_data)

colnames(test_data) = gsub(".y","",colnames(test_data))
colnames(test_data) = gsub(".x","",colnames(test_data))

train_data[which(is.na(train_data$cat101std)),cat101std:=0]
train_data[which(is.na(train_data$cat101skew)),cat101skew:=0]
test_data[which(is.na(test_data$cat101std)),cat101std:=0]
test_data[which(is.na(test_data$cat101skew)),cat101skew:=0]

contcols =  colnames(train_data)[grep("cont",colnames(train_data))]

for(i in seq(1,length(contcols)-1)){
  for(j in seq(i+1,length(contcols))){
    train_data = train_data[,paste0(contcols[i],"_",contcols[j],"rat"):=
                                  as.numeric((get(contcols[i])+3)/(get(contcols[j])+3))]
    test_data = test_data[,paste0(contcols[i],"_",contcols[j],"rat"):=
                            as.numeric((get(contcols[i])+3)/(get(contcols[j])+3))]
  }
}

rat_vars = colnames(train_data)[grep("rat",colnames(train_data))]

for(i in rat_vars){
  temp = caret::BoxCoxTrans(train_data[,get(i)])
  train_data[,paste0(i,"bc") := unlist(predict(temp,get(i)))]
  test_data[,paste0(i,"bc") := unlist(predict(temp,get(i)))]
}

rat_new = colnames(train_data)[grep("rat",colnames(train_data))]

temp = matrix(rep(0,182),182,1)

j=0
for(i in rat_new){
  j=j+1
  temp[j] = cor(train_data[,get(i)],train_data$logloss)
}

temp = as.data.frame(temp); rownames(temp)=rat_new; temp$field = rownames(temp)

temp = temp %>% mutate(abscor=abs(V1)) %>% arrange(desc(abscor))

train_data = train_data[,-rat_vars,with=F]; test_data = test_data[,-rat_vars,with=F]

for(i in c("cat80","cat79","cat101","cat12","cat81","cat87","cat100","cat10","cat1")){
  train_data[,paste0(i,"median"):= median(logloss),get(i)]
  temp = train_data[,median(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"median"))
  test_data = test_data %>% inner_join(temp,by=i)
}

test_data = as.data.table(test_data)

colnames(test_data) = gsub(".y","",colnames(test_data))
colnames(test_data) = gsub(".x","",colnames(test_data))

train_data[which(is.na(train_data$cat101std)),cat101std:=0]
test_data[which(is.na(test_data$cat101skew)),cat101skew:=0]

saveRDS(train_data,"train_data_3.rds"); saveRDS(test_data,"test_data_3.rds")

############## Round 2 #####################

train_data = readRDS("train_data_3.rds"); test_data = readRDS("test_data_3.rds")

for(i in c("cat80","cat79","cat101","cat12","cat81","cat87","cat100","cat10","cat1")){
  train_data[,paste0(i,"90per"):= quantile(logloss,0.9),get(i)]
  temp = train_data[,quantile(logloss,0.9),get(i)]; 
  colnames(temp)=c(i,paste0(i,"90per"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"90_10per"):= quantile(logloss,0.9)-quantile(logloss,0.1),get(i)]
  temp = train_data[,quantile(logloss,0.9)-quantile(logloss,0.1),get(i)]; 
  colnames(temp)=c(i,paste0(i,"90_10per"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"75_25per"):= quantile(logloss,0.75)-quantile(logloss,0.25),get(i)]
  temp = train_data[,quantile(logloss,0.75)-quantile(logloss,0.25),get(i)]; 
  colnames(temp)=c(i,paste0(i,"75_25per"))
  test_data = test_data %>% left_join(temp,by=i)
}

test_data = as.data.table(test_data)

colnames(test_data) = gsub(".y","",colnames(test_data))
colnames(test_data) = gsub(".x","",colnames(test_data))

inter = c("cat80","cat79","cat101","cat12")

for(i in seq(1,length(inter)-1)){
  for(j in seq(i+1,length(inter))){
    train_data = train_data[,paste0(inter[i],"_",inter[j],"inter"):= 
                              interaction(get(inter[i]),get(inter[j]))]
    test_data = test_data[,paste0(inter[i],"_",inter[j],"inter"):= 
                            interaction(get(inter[i]),get(inter[j]))]
  }
}

inter_1 = colnames(train_data)[grep("inter",colnames(train_data))] #cat columns

for(i in inter_1){
  train_data[,paste0(i,"mean"):= mean(logloss),get(i)]
  train_data[,paste0(i,"std"):= sd(logloss),get(i)]
  train_data[,paste0(i,"skew"):= moments::skewness(logloss),get(i)]
  temp = train_data[,mean(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"mean"))
  test_data = test_data %>% left_join(temp,by=i)
  temp = train_data[,sd(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"std"))
  test_data = test_data %>% left_join(temp,by=i)
  temp = train_data[,moments::skewness(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"skew"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"median"):= median(logloss),get(i)]
  temp = train_data[,median(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"median"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"90per"):= quantile(logloss,0.9),get(i)]
  temp = train_data[,quantile(logloss,0.9),get(i)]; 
  colnames(temp)=c(i,paste0(i,"90per"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"90_10per"):= quantile(logloss,0.9)-quantile(logloss,0.1),get(i)]
  temp = train_data[,quantile(logloss,0.9)-quantile(logloss,0.1),get(i)]; 
  colnames(temp)=c(i,paste0(i,"90_10per"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"75_25per"):= quantile(logloss,0.75)-quantile(logloss,0.25),get(i)]
  temp = train_data[,quantile(logloss,0.75)-quantile(logloss,0.25),get(i)]; 
  colnames(temp)=c(i,paste0(i,"75_25per"))
  test_data = test_data %>% left_join(temp,by=i)
}

test_data = as.data.table(test_data)

for(i in c("cat80_cat101interstd","cat80_cat101interskew","cat79_cat101interskew",
           "cat101_cat12interstd","cat101_cat12interskew","cat79_cat101interstd")){
  train_data[which(is.na(train_data[,get(i)])),(i):=0]
  test_data[which(is.na(test_data[,get(i)])),(i):=0]
}

nacol = colnames(test_data)[colSums(is.na(test_data)) > 0]

for(i in nacol){
  test_data[which(is.na(test_data[,get(i)])),(i):=0]
}
### Remove earlier interaction######
train_data[,seq(288,335):=NULL,with=F]; test_data[,seq(286,333):=NULL,with=F]

colnames(test_data) = gsub(".y","",colnames(test_data))
colnames(test_data) = gsub(".x","",colnames(test_data))

saveRDS(train_data,"train_data_4.rds"); saveRDS(test_data,"test_data_4.rds")

#############################################################

### Interaction of cat 80 with all 

train_data = readRDS("train_data_4.rds"); test_data = readRDS("test_data_4.rds")

inter = c("cat80","cat103","cat114","cat53","cat79","cat112","cat12") 

#train_data = as.data.table(train_data1)
catcols = paste0("cat",seq(1:116))

list_inter = setdiff(catcols,inter)

for(i in seq(1,length(inter))){
  for(j in seq(1,length(list_inter))){
    train_data = train_data[,paste0(inter[i],"_",list_inter[j],"inter"):= 
                              interaction(get(inter[i]),get(list_inter[j]))]
    test_data = test_data[,paste0(inter[i],"_",list_inter[j],"inter"):= 
                            interaction(get(inter[i]),get(list_inter[j]))]
  }
}

coln = c()
t=0
for(i in seq(1,length(inter))){
  for(j in seq(1,length(list_inter))){
    b = paste0(inter[i],"_",list_inter[j],"inter")
    coln = c(coln,b)
    t=t+1
  }
}

for(i in coln){
  train_data[,paste0(i,"mean"):= mean(logloss),get(i)]
  temp = train_data[,mean(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"mean"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"std"):= sd(logloss),get(i)]
  temp = train_data[,sd(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"std"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"median"):= median(logloss),get(i)]
  temp = train_data[,median(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"median"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"90per"):= quantile(logloss,0.9),get(i)]
  temp = train_data[,quantile(logloss,0.9),get(i)]
  colnames(temp)=c(i,paste0(i,"90per"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"75per"):= quantile(logloss,0.75),get(i)]
  temp = train_data[,quantile(logloss,0.75),get(i)]; 
  colnames(temp)=c(i,paste0(i,"75per"))
  test_data = test_data %>% left_join(temp,by=i) 
}

test_data = as.data.table(test_data)

saveRDS(train_data,"train_data_5.rds"); saveRDS(test_data,"test_data_5.rds")

cats = c("cat80_cat1intermean","cat80_cat1inter75per","cat80_cat2inter90per","cat80_cat3intermedian","cat80_cat4interstd","cat80_cat5intermean","cat80_cat5inter75per","cat80_cat6inter90per","cat80_cat7intermedian","cat80_cat8interstd","cat80_cat9intermean","cat80_cat9inter75per","cat80_cat10inter90per","cat80_cat11intermedian","cat80_cat1interstd","cat80_cat2intermean","cat80_cat2inter75per","cat80_cat3inter90per","cat80_cat4intermedian","cat80_cat5interstd","cat80_cat6intermean","cat80_cat6inter75per","cat80_cat7inter90per","cat80_cat8intermedian","cat80_cat9interstd","cat80_cat10intermean","cat80_cat10inter75per","cat80_cat11inter90per","cat80_cat1intermedian","cat80_cat2interstd","cat80_cat3intermean","cat80_cat3inter75per","cat80_cat4inter90per","cat80_cat5intermedian","cat80_cat6interstd","cat80_cat7intermean","cat80_cat7inter75per","cat80_cat8inter90per","cat80_cat9intermedian","cat80_cat10interstd","cat80_cat11intermean","cat80_cat11inter75per","cat80_cat1inter90per","cat80_cat2intermedian","cat80_cat3interstd","cat80_cat4intermean","cat80_cat4inter75per","cat80_cat5inter90per","cat80_cat6intermedian","cat80_cat7interstd","cat80_cat8intermean","cat80_cat8inter75per","cat80_cat9inter90per","cat80_cat10intermedian","cat80_cat11interstd")

train_data[,cats:=NULL,with=F]; test_data[,cats:=NULL,with=F]

################## Round 3 #########################

train_data = readRDS("train_data_5.rds");

nacol = colnames(train_data)[colSums(is.na(train_data)) > 0]; train_data[,nacol:=NULL,with=F]

importance_2 = read.csv("importance_matrix_2.csv");

cols_rem = importance_2$Feature[301:nrow(importance_2)]; cols_rem = as.character(cols_rem)

train_data[,cols_rem:=NULL,with=F]

test_data = readRDS("test_data_5.rds"); test_data[,cols_rem:=NULL,with=F]; test_data[,nacol:=NULL,with=F]

temp = sapply(colnames(train_data),function(x){ifelse(str_sub(x,-5,-1)=="inter",0,1)})

rm_inter_vars = colnames(train_data)[which(temp==0)]

train_data[,rm_inter_vars:=NULL,with=F]; test_data[,rm_inter_vars:=NULL,with=F]

na_test = colnames(test_data)[colSums(is.na(test_data)) > 0]

for(i in na_test){
  test_data[which(is.na(test_data[,get(i)])),(i):=0]
}

####################### Round 4 ############################

var_run3 = fread("var_run3.csv")

catcols_orig = paste0("cat",seq(1:116))
cont_cols_bc = paste0("cont",seq(1:14),"bc")

final_col_list1 = base::union(base::union(catcols_orig,cont_cols_bc),var_run3$vars_run3)

remove_list = setdiff(colnames(test_data),c(final_col_list1,"id"))

train_data[,remove_list:=NULL,with=F]
test_data[,remove_list:=NULL,with=F]

saveRDS(train_data,"train_data_6.rds"); saveRDS(test_data,"test_data_6.rds")

######### 3 level Interaction using last top variables #########

train_data[,cat80_cat101inter:= interaction(cat80,cat101)]
test_data[,cat80_cat101inter:= interaction(cat80,cat101)]

train_data[,cat114_cat100inter:= interaction(cat114,cat100)]
test_data[,cat114_cat100inter:= interaction(cat114,cat100)]

train_data[,cat112_cat116inter:= interaction(cat112,cat116)]
test_data[,cat112_cat116inter:= interaction(cat112,cat116)]

train_data[,cat79_cat116inter:= interaction(cat79,cat116)]
test_data[,cat79_cat116inter:= interaction(cat79,cat116)]

train_data[,cat103_cat111inter:= interaction(cat103,cat111)]
test_data[,cat103_cat111inter:= interaction(cat103,cat111)]

train_data[,cat114_cat101inter:= interaction(cat114,cat101)]
test_data[,cat114_cat110inter:= interaction(cat114,cat101)]

train_data[,cat80_cat81inter:= interaction(cat80,cat81)]
test_data[,cat80_cat81inter:= interaction(cat80,cat81)]

train_data[,cat53_cat100inter:= interaction(cat53,cat100)]
test_data[,cat53_cat100inter:= interaction(cat53,cat100)]

train_data[,cat112_cat100inter:= interaction(cat112,cat100)]
test_data[,cat112_cat100inter:= interaction(cat112,cat100)]

train_data[,cat79_cat109inter:= interaction(cat79,cat109)]
test_data[,cat79_cat109inter:= interaction(cat79,cat109)]

alloc.col(train_data, 100000); alloc.col(test_data, 100000)

cat_blast = setdiff(catcols_orig,c("cat80","cat101"))

for(i in c("cat80_cat101inter")){
  for(j in seq(1,length(cat_blast))){
    train_data = train_data[,paste0(i,"_",cat_blast[j],"inter"):= 
                              interaction(get(i),get(cat_blast[j]))]
    test_data = test_data[,paste0(i,"_",cat_blast[j],"inter"):= 
                            interaction(get(i),get(cat_blast[j]))]
  }
}

cat_blast1 = setdiff(catcols_orig,c("cat114","cat101"))

for(i in c("cat114_cat100inter")){
  for(j in seq(1,length(cat_blast1))){
    train_data = train_data[,paste0(i,"_",cat_blast1[j],"inter"):= 
                              interaction(get(i),get(cat_blast1[j]))]
    test_data = test_data[,paste0(i,"_",cat_blast1[j],"inter"):= 
                            interaction(get(i),get(cat_blast1[j]))]
  }
}

for(i in colnames(train_data)[438:551]){
  train_data[,paste0(i,"mean"):= mean(logloss),get(i)]
  temp = train_data[,mean(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"mean"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"median"):= median(logloss),get(i)]
  temp = train_data[,median(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"median"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"90per"):= quantile(logloss,0.9),get(i)]
  temp = train_data[,quantile(logloss,0.9),get(i)]
  colnames(temp)=c(i,paste0(i,"90per"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"75per"):= quantile(logloss,0.75),get(i)]
  temp = train_data[,quantile(logloss,0.75),get(i)]; 
  colnames(temp)=c(i,paste0(i,"75per"))
  test_data = test_data %>% left_join(temp,by=i) 
}

for(i in colnames(train_data)[552:665]){
  train_data[,paste0(i,"mean"):= mean(logloss),get(i)]
  temp = train_data[,mean(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"mean"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"median"):= median(logloss),get(i)]
  temp = train_data[,median(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"median"))
  test_data = test_data %>% left_join(temp,by=i)
}

cat_blast2 = c("cat80","cat101","cat81","cat100","cat53","cat57","cat53","cat12")

test_data = as.data.table(test_data)

for(i in c("cat112_cat116inter","cat79_cat116inter","cat103_cat111inter")){
  for(j in seq(1,length(cat_blast2))){
    train_data = train_data[,paste0(i,"_",cat_blast2[j],"inter"):= 
                              interaction(get(i),get(cat_blast2[j]))]
    test_data = test_data[,paste0(i,"_",cat_blast2[j],"inter"):= 
                            interaction(get(i),get(cat_blast2[j]))]
  }
}

for(i in colnames(train_data)[1350:1370]){
  train_data[,paste0(i,"mean"):= mean(logloss),get(i)]
  temp = train_data[,mean(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"mean"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"median"):= median(logloss),get(i)]
  temp = train_data[,median(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"median"))
  test_data = test_data %>% left_join(temp,by=i)
}

test_data = as.data.table(test_data)

cat_blast3 = c("cat12","cat72","cat113","cat111")

for(i in c("cat80_cat81inter","cat53_cat100inter",
           "cat112_cat100inter","cat79_cat109inter")){
  for(j in seq(1,length(cat_blast3))){
    train_data = train_data[,paste0(i,"_",cat_blast3[j],"inter"):= 
                              interaction(get(i),get(cat_blast3[j]))]
    test_data = test_data[,paste0(i,"_",cat_blast3[j],"inter"):= 
                            interaction(get(i),get(cat_blast3[j]))]
  }
}

for(i in colnames(train_data)[1413:1428]){
  train_data[,paste0(i,"mean"):= mean(logloss),get(i)]
  temp = train_data[,mean(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"mean"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"median"):= median(logloss),get(i)]
  temp = train_data[,median(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"median"))
  test_data = test_data %>% left_join(temp,by=i)
}

test_data = as.data.table(test_data)

na_test = colnames(test_data)[colSums(is.na(test_data)) > 0]

for(i in na_test){
  test_data[which(is.na(test_data[,get(i)])),(i):=0]
}

saveRDS(train_data,"train_data_7.rds"); saveRDS(test_data,"test_data_7.rds")

importance_4 = read.csv("importance_matrix_4_nc.csv")

col_rem_4 = union(union(importance_4$Feature,catcols_orig),cont_cols_bc)

rem_4 = setdiff(colnames(test_data),c(col_rem_4,"id"))

train_data[,rem_4:= NULL,with=F]; test_data[,rem_4:=NULL,with=F]

temp = sapply(importance_4$Feature,function(x){ifelse(str_match(x,"cont") > 0,0,1)})

cont_vars = importance_4$Feature[which(temp==0)]; cont_vars = as.character(cont_vars)

train_data$cont2bc1 = log(train_data$cont2bc + 0.0009)
test_data$cont2bc1 = log(test_data$cont2bc + 0.0009)

train_data$cont14bc1 = exp(train_data$cont14bc)
test_data$cont14bc1 = exp(test_data$cont14bc)

train_data$cont5bc_cont14bcratbc1 = (train_data$cont5bc_cont14bcratbc)^6
test_data$cont5bc_cont14bcratbc1 = (test_data$cont5bc_cont14bcratbc)^6

train_data$cont2bc_cont14bcratbc1 = (train_data$cont2bc_cont14bcratbc)^9
test_data$cont2bc_cont14bcratbc1 = (test_data$cont2bc_cont14bcratbc)^9

train_data$cont4bc_cont14bcratbc1 = (train_data$cont4bc_cont14bcratbc)^2
test_data$cont4bc_cont14bcratbc1 = (test_data$cont4bc_cont14bcratbc)^2

train_data$cont7cont2bc1sum = (train_data$cont7bc+train_data$cont2bc1)^2
test_data$cont7cont2bc1sum = (test_data$cont7bc+test_data$cont2bc1)^2

train_data$cont8cont2bc1sumsq = (train_data$cont8bc+train_data$cont2bc1)^2
test_data$cont8cont2bc1sumsq = (test_data$cont8bc+test_data$cont2bc1)^2

train_data$cont3cont2bc1log = log(train_data$cont3bc+train_data$cont2bc1+15)
test_data$cont3cont2bc1log = log(test_data$cont3bc+test_data$cont2bc1+15)

saveRDS(train_data,"train_data_8.rds"); saveRDS(test_data,"test_data_8.rds")

###################### Back to square one ##########################

train_data = readRDS("train_data_3.rds"); test_data = readRDS("test_data_3.rds")

train_data[,cat101std:=NULL]
test_data[,cat101std:=NULL]

rem = c("cat80median","cat79median","cat81median","cat1median","cat101median",
        "cat87median","cat12median","cat100median","cat10median","cat80mean","cat80std","cat79skew","cat12std",
        "cat81skew","cat100mean","cat10std","cat1skew","cat80skew","cat101mean","cat12skew","cat87mean","cat100std",
        "cat10skew","cat79mean","cat101skew","cat81mean","cat87std","cat100skew","cat1mean","cat79std",
        "cat12mean","cat81std","cat87skew","cat10mean","cat1std")

train_data[,rem:=NULL,with=F]
test_data[,rem:=NULL,with=F]

saveRDS(train_data,"train_data_8.1.rds"); saveRDS(test_data,"test_data_8.1.rds")

for(i in catcols_n){
  train_data[,paste0(i,"mean"):= mean(logloss),get(i)]
  temp = train_data[,mean(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"mean"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"median"):= median(logloss),get(i)]
  temp = train_data[,median(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"median"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"90per"):= quantile(logloss,0.9),get(i)]
  temp = train_data[,quantile(logloss,0.9),get(i)]; 
  colnames(temp)=c(i,paste0(i,"90per"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"75per"):= quantile(logloss,0.75),get(i)]
  temp = train_data[,quantile(logloss,0.75),get(i)]; 
  colnames(temp)=c(i,paste0(i,"75per"))
  test_data = test_data %>% left_join(temp,by=i)
}

test_data = as.data.table(test_data)

colnames(test_data) = gsub(".y","",colnames(test_data))
colnames(test_data) = gsub(".x","",colnames(test_data))

nacol2 = union(colnames(test_data)[colSums(is.na(test_data)) > 0],
               colnames(train_data)[colSums(is.na(train_data)) > 0])

train_data[,nacol2:=NULL,with=F]
test_data[,nacol2:=NULL,with=F]

inter = c("cat80","cat79","cat12","cat81","cat1","cat103","cat100","cat10","cat111","cat114","cat53",
          "cat2","cat87","cat57","cat72","cat5") 

#train_data = as.data.table(train_data1)
for(i in seq(1,length(inter)-1)){
  for(j in seq(i+1,length(inter))){
    train_data = train_data[,paste0(inter[i],"_",inter[j],"inter"):= 
                              interaction(get(inter[i]),get(inter[j]))]
    test_data = test_data[,paste0(inter[i],"_",inter[j],"inter"):= 
                            interaction(get(inter[i]),get(inter[j]))]
  }
}

inter_1 = colnames(train_data)[grep("inter",colnames(train_data))] #cat columns

for(i in inter_1){
  train_data[,paste0(i,"mean"):= mean(logloss),get(i)]
  temp = train_data[,mean(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"mean"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"median"):= median(logloss),get(i)]
  temp = train_data[,median(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"median"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"90per"):= quantile(logloss,0.9),get(i)]
  temp = train_data[,quantile(logloss,0.9),get(i)]; 
  colnames(temp)=c(i,paste0(i,"90per"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"75per"):= quantile(logloss,0.75),get(i)]
  temp = train_data[,quantile(logloss,0.75),get(i)]; 
  colnames(temp)=c(i,paste0(i,"75per"))
  test_data = test_data %>% left_join(temp,by=i)
}

test_data = as.data.table(test_data)

nacol3 = union(colnames(test_data)[colSums(is.na(test_data)) > 0],
               colnames(train_data)[colSums(is.na(train_data)) > 0])

train_data[,nacol3:=NULL,with=F]
test_data[,nacol3:=NULL,with=F]

for(i in catcols_n){
  train_data[,paste0(i,"max"):= max(logloss),get(i)]
  temp = train_data[,max(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"max"))
  test_data = test_data %>% left_join(temp,by=i)
  
  train_data[,paste0(i,"min"):= min(logloss),get(i)]
  temp = train_data[,min(logloss),get(i)]; colnames(temp)=c(i,paste0(i,"min"))
  test_data = test_data %>% left_join(temp,by=i)
}

nacol4 = union(colnames(test_data)[colSums(is.na(test_data)) > 0],
               colnames(train_data)[colSums(is.na(train_data)) > 0])

test_data = as.data.table(test_data)

train_data[,nacol4:=NULL,with=F]
test_data[,nacol4:=NULL,with=F]

temp = sapply(colnames(train_data),function(x){ifelse(str_sub(x,-5,-1)=="inter",0,1)})

rm_interact = colnames(train_data)[which(temp==0)]

train_data[,rm_interact:=NULL,with=F]
test_data[,rm_interact:=NULL,with=F]

#saveRDS(train_data,"train_data_9.rds"); saveRDS(test_data,"test_data_9.rds")

####################################################################################

temp = sapply(colnames(train_data),function(x){ifelse(str_sub(x,-5,-1)=="range",0,1)})

rm_range = colnames(train_data)[which(temp==0)]

train_data[,rm_range:=NULL,with=F]
test_data[,rm_range:=NULL,with=F]

saveRDS(train_data,"train_data_9.rds"); saveRDS(test_data,"test_data_9.rds")

train = fread("train_data_prep1.csv")

test = fread("test_data_prep1.csv")

train_data = train_data %>% left_join(train[,c("id","cont2"),with=F],by="id",copy=T)
test_data = test_data %>% left_join(test[,c("id","cont2"),with=F],by="id",copy=T)

train_data = as.data.table(train_data)
test_data = as.data.table(test_data)

train_data$logloss = log(train_data$loss+200)

train_data$cont2bc1 = log(train_data$cont2bc)
test_data$cont2bc1 = log(test_data$cont2bc)

train_data[,cont2_meanlogloss:= mean(logloss),cont2]
temp = train_data[,mean(logloss),cont2]; colnames(temp)=c("cont2","cont2_meanlogloss")
test_data = test_data %>% left_join(temp,by="cont2")

train_data$cont14bc1 = exp(train_data$cont14bc)
test_data$cont14bc1 = exp(test_data$cont14bc)

train_data$cont6_7diff = exp(train_data$cont6bc+train_data$cont7bc)
test_data$cont6_7diff = exp(test_data$cont6bc+test_data$cont7bc)

train_data$cont7_14diff = exp(train_data$cont7bc+train_data$cont14bc)
test_data$cont7_14diff = exp(test_data$cont7bc+test_data$cont14bc)

train_data$cont6_14diff = exp(train_data$cont6bc+train_data$cont7bc)
test_data$cont6_14diff = exp(test_data$cont6bc+test_data$cont7bc)

saveRDS(train_data,"train_data_9.1.rds"); saveRDS(test_data,"test_data_9.1.rds")

################ Remove unnecessary variables, create some more and 5 fold bagging ########

train_data = readRDS("train_data_9.1.rds")

test_data = readRDS("test_data_9.1.rds")
importance_matrix = fread("importance_matrix_5.csv")

col_stay = importance_matrix$Feature
colrem = setdiff(colnames(train_data),c(col_stay,"id","loss","logloss"))

test_data = as.data.table(test_data)

train_data[,colrem:=NULL,with=F]
test_data[,colrem:=NULL,with=F]

train_data[,cat80_12_100_53:= cat80_cat12intermean*cat100_cat53intermedian]
test_data[,cat80_12_100_53:= cat80_cat12intermean*cat100_cat53intermedian]

train_data[,cat80_12_79_57:= cat80_cat12intermean*cat79_cat57intermean]
test_data[,cat80_12_79_57:= cat80_cat12intermean*cat79_cat57intermean]

train_data[,cat80_12_81_111:= cat80_cat12intermean*cat81_cat111intermean]
test_data[,cat80_12_81_111:= cat80_cat12intermean*cat81_cat111intermean]

train_data[,cat81_111_100_53:= cat81_cat111intermean*cat100_cat53intermedian]
test_data[,cat81_111_100_53:= cat81_cat111intermean*cat100_cat53intermedian]

train_data[,cat103_10_100_53:= cat103_cat10intermean*cat100_cat53intermedian]
test_data[,cat103_10_100_53:= cat103_cat10intermean*cat100_cat53intermedian]

saveRDS(train_data,"train_data_10.rds"); saveRDS(test_data,"test_data_10.rds")

#######################################################################################

##interaction cat 112 and dates etc

train_data=readRDS("train_data_10.rds"); test_data=readRDS("test_data_10.rds")

train_data[,cat80cont2_inter:=interaction(cat80,as.character(cont2bc))]
test_data[,cat80cont2_inter:=interaction(cat80,as.character(cont2bc))]

train_data[,cont3char:= as.character(cont3bc)]
test_data[,cont3char:= as.character(cont3bc)]

train_data[,cat12cont2_inter:=interaction(cat12,as.character(cont2bc))]
test_data[,cat12cont2_inter:=interaction(cat12,as.character(cont2bc))]

train_data[,cat81cont2_inter:=interaction(cat81,as.character(cont2bc))]
test_data[,cat81cont2_inter:=interaction(cat81,as.character(cont2bc))]

train_data[,cat111cont2_inter:=interaction(cat111,as.character(cont2bc))]
test_data[,cat111cont2_inter:=interaction(cat111,as.character(cont2bc))]

train_data[,cat53cont2_inter:=interaction(cat53,as.character(cont2bc))]
test_data[,cat53cont2_inter:=interaction(cat53,as.character(cont2bc))]

train_data[,cat100cont2_inter:=interaction(cat100,as.character(cont2bc))]
test_data[,cat100cont2_inter:=interaction(cat100,as.character(cont2bc))]

train_data[,cat103cont2_inter:=interaction(cat103,as.character(cont2bc))]
test_data[,cat103cont2_inter:=interaction(cat103,as.character(cont2bc))]

tp =train_data[,list(mean(logloss),.N),cat103cont2_inter]
tp = tp %>% arrange(desc(V1))

saveRDS(train_data,"train_data_10.1.rds"); saveRDS(test_data,"test_data_10.1.rds")

################## Again #####################

train_data = readRDS("train_data_10.1.rds")

test_data = readRDS("test_data_10.1.rds")

library(splines)

library(mgcv)

gamcols = colnames(train_data)[grep("bc",colnames(train_data))]

for(col in gamcols){
  gam_model1 = gam(substitute(logloss ~ s(col,bs="gp"),list(col=as.name(col))),data = train_data)
  gam_model2 = gam(substitute(logloss ~ s(col,bs="cr"),list(col=as.name(col))),data = train_data)
  gam_model3 = gam(substitute(logloss ~ s(col,bs="ds"),list(col=as.name(col))),data = train_data)
  cor1 = cor(gam_model1$fitted.values,train_data$logloss)
  cor2 = cor(gam_model2$fitted.values,train_data$logloss)
  cor3 = cor(gam_model3$fitted.values,train_data$logloss)
  if(abs(cor1) > abs(cor2) & abs(cor1) > abs(cor3)){
    train_data[,paste0(col,"gam"):=gam_model1$fitted.values]
    gam_pred = predict.gam(gam_model1,test_data,type="response")
    test_data[,paste0(col,"gam"):= gam_pred]
  } else if (abs(cor2) > abs(cor1) & abs(cor2) > abs(cor3)){
    train_data[,paste0(col,"gam"):=gam_model2$fitted.values]
    gam_pred = predict.gam(gam_model2,test_data,type="response")
    test_data[,paste0(col,"gam"):= gam_pred]
  } else{
    train_data[,paste0(col,"gam"):=gam_model3$fitted.values]
    gam_pred = predict.gam(gam_model3,test_data,type="response")
    test_data[,paste0(col,"gam"):= gam_pred]
  }
}

test = test[,c(gamcols,"id"),with=F]
train = train[,c(gamcols,"id"),with=F]

train_data = train_data %>% left_join(train,by="id")
test_data = test_data %>% left_join(test,by="id")

saveRDS(train_data,"train_data_12.rds"); saveRDS(test_data,"test_data_12.rds")

#########################################################################

train_data = readRDS("train_data_12.rds")
test_data = readRDS("test_data_12.rds")

# train_data = as.data.table(train_data); test_data = as.data.table(test_data)
# 
# rem_gam = read.csv("remove_gam_vars.csv")
# 
# rem = as.character(rem_gam$Feature)
# 
# train_data[,rem:=NULL,with=F]
# test_data[,rem:=NULL,with=F]

contcols = colnames(train_data)[setdiff(grep("cont",colnames(train_data)),grep("inter",colnames(train_data)))]

contcols = setdiff(contcols,"cont3char")

ir.pca = prcomp(train_data[,contcols,with=F], center = TRUE, scale. = TRUE) #keep 108 PCA

predict_test = predict(ir.pca,test_data[,contcols,with=F])

train_data[,contcols:=NULL,with=F]
test_data[,contcols:=NULL,with=F]

train_data = cbind(as.data.frame(train_data),as.data.frame(ir.pca$x[,1:108]))
test_data = cbind(as.data.frame(test_data),as.data.frame(predict_test[,1:108]))

saveRDS(train_data,"train_data_12.1.rds"); saveRDS(test_data,"test_data_12.1.rds")

######################## PCA sucks, gam on 3 only #############

train_data = readRDS("train_data_10.1.rds")
test_data = readRDS("test_data_10.1.rds")

gamcols = c("cont14bc","cont7bc","cont12bc")

train1 = train_data %>% sample_frac(0.7)
val1 = train_data %>% anti_join(train1,by="id")

library(mgcv)

i=0
error = matrix(rep(0,9),3,3)
for(col in gamcols){
  i = i+1
  gam_model1 = gam(substitute(logloss ~ s(col,bs="gp",k=-1),list(col=as.name(col))),data = train1)
  gam_model2 = gam(substitute(logloss ~ s(col,bs="cr",k=-1),list(col=as.name(col))),data = train1)
  gam_model3 = gam(substitute(logloss ~ s(col,bs="ds",k=-1),list(col=as.name(col))),data = train1)
  gam_val1 = predict.gam(gam_model1,val1,type="response")
  gam_val2 = predict.gam(gam_model2,val1,"response")
  gam_val3 = predict.gam(gam_model3,val1,"response")
  error[i,1] = sum(abs(val1$logloss-gam_val1))/nrow(val1)
  error[i,2] = sum(abs(val1$logloss-gam_val2))/nrow(val1)
  error[i,3] = sum(abs(val1$logloss-gam_val3))/nrow(val1)
}

######### Loess Fit

# train1 = train_data %>% sample_frac(0.7)
# val1 = train_data %>% anti_join(train1,by="id")
# 
# error = matrix(rep(0,6),6,1)
# i=0
# for(sp in seq(0.5,1,0.1)){
#   i=i+1
#   loess_fit = loess(logloss~cont14bc,data = train1,span=sp,control=loess.control(surface="direct"))
#   loess_val = predict(loess_fit,val1)
#   error[i] = sum(abs(val1$logloss-loess_val))/nrow(val1)
# }

gam_model1 = gam(logloss ~ s(cont14bc,bs="ds",k=-1),data = train_data)
gam_model2 = gam(logloss ~ s(cont7bc,bs="gp",k=-1),data = train_data)
gam_model3 = gam(logloss ~ s(cont12bc,bs="cr",k=-1),data = train_data)

train_data[,cont14bcgam:=gam_model1$fitted.values]
gam_pred = predict.gam(gam_model1,test_data,type="response")
test_data[,cont14bcgam:= gam_pred]

train_data[,cont7bcgam:=gam_model2$fitted.values]
gam_pred = predict.gam(gam_model2,test_data,type="response")
test_data[,cont7bcgam:= gam_pred]

train_data[,cont12bcgam:=gam_model3$fitted.values]
gam_pred = predict.gam(gam_model3,test_data,type="response")
test_data[,cont12bcgam:= gam_pred]

##############################################

train_data = train_data %>% left_join(traina[,c(diff1,"id"),with=F],by="id")

test_data = test_data %>% left_join(testa[,c(diff1,"id"),with=F],by="id")

saveRDS(train_data,"train_data_13.rds"); saveRDS(test_data,"test_data_13.rds")

train_data = readRDS("train_data_13.rds")
test_data = readRDS("test_data_13.rds")

train_data = as.data.table(train_data)
test_data = as.data.table(test_data)

gam = colnames(train_data)[grep("gam",colnames(train_data))]

train_data[,gam:=NULL,with=F]
test_data[,gam:=NULL,with=F]

################################################

rem_again = fread("remove_again.csv")

train_earlier = fread("train_data_prep1.csv")
test_earlier = fread("test_data_prep1.csv")

train_data[,unlist(rem_again$rem):=NULL,with=F]
test_data[,unlist(rem_again$rem):=NULL,with=F]

cols = setdiff(colnames(train_earlier),colnames(train_data))

train_data = train_data %>% left_join(train_earlier[,c(cols,"id"),with=F], by="id")
test_data = test_data %>% left_join(test_earlier[,c(cols,"id"),with=F], by="id")

saveRDS(train_data,"train_data_14.rds"); saveRDS(test_data,"test_data_14.rds")

################### Start Again ###################

train_data = readRDS("train_data_14.rds")
test_data = readRDS("test_data_14.rds")

train_data[,cat112_cont7mean:=mean(cont7),cat112]
test_data[,cat112_cont7mean:=mean(cont7),cat112]

train_data[,cat112_cont14mean:=mean(cont14),cat112]
test_data[,cat112_cont14mean:=mean(cont14),cat112]

train_data[,cat112_cont2mean:=mean(cont2),cat112]
test_data[,cat112_cont2mean:=mean(cont2),cat112]

train_data[,cat112_cont7std:=sd(cont7),cat112]
test_data[,cat112_cont7std:=sd(cont7),cat112]

train_data[,cat112_cont14std:=sd(cont14),cat112]
test_data[,cat112_cont14std:=sd(cont14),cat112]

train_data[,cat112_cont2std:=sd(cont2),cat112]
test_data[,cat112_cont2std:=sd(cont2),cat112]

train_data[,cat112_cont7skw:=moments::skewness(cont7),cat112]
test_data[,cat112_cont7skw:=moments::skewness(cont7),cat112]

train_data[,cat112_cont14skw:=moments::skewness(cont14),cat112]
test_data[,cat112_cont14skw:=moments::skewness(cont14),cat112]

train_data[,cat112_cont2skw:=moments::skewness(cont2),cat112]
test_data[,cat112_cont2skw:=moments::skewness(cont2),cat112]

train_data[,cat112_cont7rg:=max(cont7)-min(cont7),cat112]
test_data[,cat112_cont7rg:=max(cont7)-min(cont7),cat112]

train_data[,cat112_cont14rg:=max(cont14)-min(cont14),cat112]
test_data[,cat112_cont14rg:=max(cont14)-min(cont14),cat112]

train_data[,cat112_cont2rg:=max(cont2)-min(cont2),cat112]
test_data[,cat112_cont2rg:=max(cont2)-min(cont2),cat112]

saveRDS(train_data,"train_data_14.1.rds"); saveRDS(test_data,"test_data_14.1.rds")

######################

train_data = readRDS("train_data_14.1.rds"); 
test_data = readRDS("test_data_14.1.rds")

train_data[,cat114_57_79_57:=cat114_cat57intermean*cat79_cat57intermean]
test_data[,cat114_57_79_57:=cat114_cat57intermean*cat79_cat57intermean]

train_data[,cat80_12_81_111_112:= cat80_12_81_111*cat112mean]
test_data[,cat80_12_81_111_112:= cat80_12_81_111*cat112mean]

train_data[,cat80_12_81_111_14cont:= cat80_12_81_111*cont14bc]
test_data[,cat80_12_81_111_14cont:= cat80_12_81_111*cont14bc]

train_data[,cat80_12_81_111_7cont:= cat80_12_81_111*cont7bc]
test_data[,cat80_12_81_111_7cont:= cat80_12_81_111*cont7bc]

train_data[,cat80_12_81_111_101:= cat80_12_81_111*cat101mean]
test_data[,cat80_12_81_111_101:= cat80_12_81_111*cat101mean]

saveRDS(train_data,"train_data_14.2.rds"); saveRDS(test_data,"test_data_14.2.rds")

train_data = readRDS("train_data_14.2.rds") 
test_data = readRDS("test_data_14.2.rds")

train_data = train_data %>% mutate(loss1 = ifelse(loss >= 10000,10000,loss))

vars_r = setdiff(colnames(train_data),c("id","logloss","loss","loss1"))

catcols_n = colnames(train_data)[sapply(train_data,class) %in% c("character","factor")]

train_test = rbind(train_data[,c("id",vars_r,"loss1"),with=F],test_data,fill=T)

for (f in catcols_n) {
  levels = unique(train_test[[f]])
  train_test[[f]] = factor(train_test[[f]], levels=levels, ordered=T)
}

train_data = train_test[1:nrow(train_data)]
test_data = train_test[nrow(train_data)+1:nrow(test_data)]
rm(train_test)

test_data[,loss1:=NULL]

glm_model = glm(loss1 ~ ., data = train_data[,c(vars_r,"loss1"),with=F], 
                family = Gamma(link = "log"),maxit = 100000)

########################################

train_data = readRDS("train_data_14.2.rds") 
test_data = readRDS("test_data_14.2.rds")

cols = c("cat112_cont7mean","cat112_cont14mean","cat112_cont14std","cat112_cont2skw","cat112_cont2mean",
         "cat112_cont2std","cat112_cont7rg","cat112_cont7std","cat112_cont7skw","cat112_cont14rg",
         "cat112_cont14skw","cat112_cont2rg")

numcols = colnames(train_data)[sapply(train_data,class) %in% c("integer","numeric")]

corrl = as.data.frame(matrix(rep(0,267),267,1))
j=0
for(i in numcols){
  j=j+1
  corrl[j,1] = cor(train_data[,i,with=F],train_data$logloss)
  corrl[j,2] = i
}

corrl = corrl[-c(1,2,3),]
corrl = corrl %>% arrange(desc(abs(V1)))

train_data[,cols:=NULL,with=F]
test_data[,cols:=NULL,with=F]

train_data[,cat80_12_81_111:=NULL]; test_data[,cat80_12_81_111:=NULL]
train_data[,cat80_cat100intermedian:=NULL]; test_data[,cat80_cat100intermedian:=NULL]

train_data[,cat80_cat100inter90per:=NULL]; test_data[,cat80_cat100inter90per:=NULL]

train_data[,cat80_cat12intermedian:=NULL]; test_data[,cat80_cat12intermedian:=NULL]

train_data[,cat79_cat87inter75per:=NULL]; test_data[,cat79_cat87inter75per:=NULL]

train_data[,cat79_cat87inter90per:=NULL]; test_data[,cat79_cat87inter90per:=NULL]

train_data[,cat79_cat72intermedian:=NULL]; test_data[,cat79_cat72intermedian:=NULL]

train_data[,cat79_cat103intermedian:=NULL]; test_data[,cat79_cat103intermedian:=NULL]

train_data[,cat79_cat57intermedian:=NULL]; test_data[,cat79_cat57intermedian:=NULL]

train_data[,cat79_cat57inter90per:=NULL]; test_data[,cat79_cat57inter90per:=NULL]

train_data[,cat100_cat72inter75per:=NULL]; test_data[,cat100_cat72inter75per:=NULL]

train_data[,cat100_cat2inter75per:=NULL]; test_data[,cat100_cat2inter75per:=NULL]

train_data[,cat100_cat2inter90per:=NULL]; test_data[,cat100_cat2inter90per:=NULL]

train_data[,cat1_cat100inter90per:=NULL]; test_data[,cat1_cat100inter90per:=NULL]

train_data[,cat103_cat10intermedian:=NULL]; test_data[,cat103_cat10intermedian:=NULL]

train_data[,cat114_cat2intermedian:=NULL]; test_data[,cat114_cat2intermedian:=NULL]

train_data[,cat114_cat2inter90per:=NULL]; test_data[,cat114_cat2inter90per:=NULL]

train_data[,cat114_cat2inter75per:=NULL]; test_data[,cat114_cat2inter75per:=NULL]

train_data[,cat103_cat2inter90per:=NULL]; test_data[,cat103_cat2inter90per:=NULL]

train_data[,cat114_cat57intermedian:=NULL]; test_data[,cat114_cat57intermedian:=NULL]

train_data[,cat81_cat111intermedian:=NULL]; test_data[,cat81_cat111intermedian:=NULL]

train_data[,cat81_cat111inter75per:=NULL]; test_data[,cat81_cat111inter75per:=NULL]

train_data[,cat1_cat103intermedian:=NULL]; test_data[,cat1_cat103intermedian:=NULL]

train_data[,cat1_cat103inter90per:=NULL]; test_data[,cat1_cat103inter90per:=NULL]

train_data[,cat103_cat72inter75per:=NULL]; test_data[,cat103_cat72inter75per:=NULL]

train_data[,cat111_cat57inter75per:=NULL]; test_data[,cat111_cat57inter75per:=NULL]

train_data[,cat111_cat57intermedian:=NULL]; test_data[,cat111_cat57intermedian:=NULL]

train_data[,cat103_cat53intermedian:=NULL]; test_data[,cat103_cat53intermedian:=NULL]

train_data[,cat103_cat53inter75per:=NULL]; test_data[,cat103_cat53inter75per:=NULL]

saveRDS(train_data,"train_data_15.rds"); saveRDS(test_data,"test_data_15.rds")

###################### Re Start ###################

rm(list=ls(all=T))

setwd("C:/Users/rsoni106/Documents/Work/Methodology Work/Kaggle/Completed/Allstate/prepared_data/new_data/")

library(data.table)
library(dplyr)

train_data = fread("C:\\Users\\rsoni106\\Documents\\Work\\Methodology Work\\Kaggle\\Completed\\Allstate\\train.csv")
test_data = fread("C:\\Users\\rsoni106\\Documents\\Work\\Methodology Work\\Kaggle\\Completed\\Allstate\\test.csv")

catcols = paste0("cat",seq(1,116),collapse=NULL)

for(x in catcols){
  label_table = train_data[,.N,get(x)]
  colnames(label_table) = c("var","count")
  label_table = label_table %>% mutate(ind = ifelse(count < 10,1,0))
  categories = label_table$var[label_table$ind==1]
  train_data[,(x):=ifelse(get(x) %in% categories,"rare",get(x))]
}

for(x in catcols){
  label_table = test_data[,.N,get(x)]
  colnames(label_table) = c("var","count")
  label_table = label_table %>% mutate(ind = ifelse(count < 10,1,0))
  categories = label_table$var[label_table$ind==1]
  test_data[,(x):=ifelse(get(x) %in% categories,"rare",get(x))]
}

saveRDS(train_data,"train_data_1.rds"); saveRDS(test_data,"test_data_1.rds")

full_data = rbind(train_data,test_data,fill=T)

count_func = function(df,x){
  val = df[!is.na(df$loss),.N,get(x)]
  colnames(val) = c(x,"count")
  temp = df %>% left_join(val,by=x)
  print(length(which(is.na(temp[,ncol(temp)]))))
  temp[is.na(temp$count),"count"] = 0
  return(temp$count)
}

for(i in catcols){
  temp = count_func(full_data,i)
  full_data = cbind(full_data,temp)
  colnames(full_data)[ncol(full_data)] = paste0(i,"cnt")
}

count_func_2 = function(df,x,y){
  val = df[!is.na(df$loss),.N,list(get(x),get(y))]
  colnames(val) = c(x,y,"count")
  temp = df %>% left_join(val,by=c(x,y))
  print(length(which(is.na(temp$count))))
  temp[is.na(temp$count),"count"] = 0
  return(temp$count)
}

temp = count_func_2(full_data,"cat80","cat12")
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_12cnt"

temp = count_func_2(full_data,"cat80","cat79")
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_79cnt"

temp = count_func_2(full_data,"cat80","cat101")
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_101cnt"

temp = count_func_2(full_data,"cat80","cat81")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_81cnt"

temp = count_func_2(full_data,"cat80","cat87")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_87cnt"

temp = count_func_2(full_data,"cat80","cat100")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_100cnt"

temp = count_func_2(full_data,"cat80","cat10")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_10cnt"

temp = count_func_2(full_data,"cat79","cat101")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat79_101cnt"

temp = count_func_2(full_data,"cat79","cat12")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat79_12cnt"

temp = count_func_2(full_data,"cat79","cat81")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat79_81cnt"

temp = count_func_2(full_data,"cat79","cat87")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat79_87cnt"

temp = count_func_2(full_data,"cat101","cat12")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat101_12cnt"

temp = count_func_2(full_data,"cat101","cat81")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat101_81cnt"

temp = count_func_2(full_data,"cat101","cat87")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat101_87cnt"

temp = count_func_2(full_data,"cat101","cat100")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat101_100cnt"

temp = count_func_2(full_data,"cat81","cat111")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat81_111cnt"

temp = count_func_2(full_data,"cat79","cat57")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat79_57cnt"

temp = count_func_2(full_data,"cat53","cat100")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat53_100cnt"

temp = count_func_2(full_data,"cat103","cat10")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat103_10cnt"

temp = count_func_2(full_data,"cat114","cat2")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat114_2cnt"

temp = count_func_2(full_data,"cat103","cat53")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat103_53cnt"

temp = count_func_2(full_data,"cat100","cat2")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat100_2cnt"

full_data$cont2copy = full_data$cont2

full_data$cont2 = ifelse(full_data$cont2 %in% c(0.001503,0.001966,0.002571,0.001149),0,full_data$cont2)

count_func_3 = function(df,x,y,z){
  val = df[!is.na(df$loss),.N,list(get(x),get(y),get(z))]
  colnames(val) = c(x,y,z,"count")
  temp = df %>% left_join(val,by=c(x,y,z))
  print(length(which(is.na(temp[,ncol(temp)]))))
  temp[is.na(temp$count),"count"] = 0
  return(temp$count)
}

temp = count_func_3(full_data,"cat80","cat12","cat81")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_12_81cnt"

temp = count_func_3(full_data,"cat80","cat12","cat111")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_12_111cnt"

temp = count_func_3(full_data,"cat80","cat12","cat79")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_12_79cnt"

temp = count_func_3(full_data,"cat80","cat12","cat57")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_12_57cnt"

temp = count_func_3(full_data,"cat80","cat12","cat100")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_12_100cnt"

temp = count_func_3(full_data,"cat80","cat12","cat53")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_12_53cnt"

temp = count_func_3(full_data,"cat103","cat10","cat100")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat103_10_100cnt"

temp = count_func_3(full_data,"cat103","cat10","cat53")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat103_10_53cnt"

temp = count_func_3(full_data,"cat81","cat111","cat100")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat81_111_100cnt"

temp = count_func_3(full_data,"cat81","cat111","cat53")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat81_111_53cnt"

count_func_4 = function(df,x,y,z,a){
  val = df[!is.na(df$loss),.N,list(get(x),get(y),get(z),get(a))]
  colnames(val) = c(x,y,z,a,"count")
  temp = df %>% left_join(val,by=c(x,y,z,a))
  print(length(which(is.na(temp[,ncol(temp)]))))
  temp[is.na(temp$count),"count"] = 0
  return(temp$count)
}

temp = count_func_4(full_data,"cat80","cat12","cat81","cat111")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_12_81_111cnt"

temp = count_func_4(full_data,"cat80","cat12","cat79","cat57")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_12_79_57cnt"

temp = count_func_4(full_data,"cat80","cat12","cat100","cat53")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_12_100_53cnt"

temp = count_func_4(full_data,"cat81","cat111","cat100","cat53")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat81_111_100_53cnt"

saveRDS(full_data,"full_data_1.rds")

########## Adding a noise element of 5% +/- in the risk variable ###########

loss_append = function(df,x,y){
  val = df[!is.na(loss),.(sum(loss),.N),list(get(x),get(y))] 
  colnames(val) = c(x,y,"loss1","cnt")
  temp = df %>% left_join(val,by=c(x,y));temp = as.data.table(temp)
  temp = temp[,c("loss","loss1","cnt"),with=F]
  temp[!is.na(loss) & cnt>1,loss2:=(loss1-loss)/(cnt-1)]
  temp[!is.na(loss) & cnt==1,loss2:=loss/cnt]
  noise_tr = 1 + (runif(nrow(train_data))-0.5)*.1
  noise_te = rep(1,nrow(test_data))
  noise = c(noise_tr,noise_te)
  temp[is.na(loss),loss2:=loss1/cnt]
  temp$loss2 = temp$loss2*noise
  return(temp$loss2)
}

temp = loss_append(full_data,"cat80","cat12")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_12_mloss"

temp = loss_append(full_data,"cat81","cat111")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat81_111_mloss"

temp = loss_append(full_data,"cat79","cat57")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat79_57_mloss"

temp = loss_append(full_data,"cat100","cat53")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat100_53_mloss"

temp = loss_append(full_data,"cat103","cat10")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat103_10_mloss"

temp = loss_append(full_data,"cat103","cat53")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat103_53_mloss"

temp = loss_append(full_data,"cat114","cat2")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat114_2_mloss"

temp = loss_append(full_data,"cat110","cat2")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat110_2_mloss"

temp = loss_append(full_data,"cat111","cat57")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat111_57_mloss"

temp = loss_append(full_data,"cat79","cat87")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat79_87_mloss"

temp = loss_append(full_data,"cat111","cat72")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat111_72_mloss"

temp = loss_append(full_data,"cat103","cat1")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat103_1_mloss"

temp = loss_append(full_data,"cat80","cat112")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_112_mloss"

temp = loss_append(full_data,"cat79","cat112")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat79_112_mloss"

temp = loss_append(full_data,"cat101","cat112")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat101_112_mloss"

temp = loss_append(full_data,"cat12","cat112")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat12_112_mloss"

temp = loss_append(full_data,"cat81","cat112")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat81_112_mloss"

temp = count_func(full_data,"cont2")
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cont2cnt"

temp = count_func_2(full_data,"cat80","cont2")
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_c2cnt"

temp = count_func_2(full_data,"cat79","cont2")
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat79_c2cnt"

temp = loss_append(full_data,"cat80","cont2")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_c2_mloss"

temp = loss_append(full_data,"cat79","cont2")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat79_c2_mloss"

saveRDS(full_data,"full_data_1.rds")

############## Adding a +/- 5% perturbation to the risk variable #########

loss_append_sing = function(df,x){
  val = df[!is.na(loss),.(sum(loss),.N),get(x)] 
  colnames(val) = c(x,"loss1","cnt")
  temp = df %>% left_join(val,by=c(x));temp = as.data.table(temp)
  temp = temp[,c("loss","loss1","cnt"),with=F]
  temp[!is.na(loss) & cnt>1,loss2:=(loss1-loss)/(cnt-1)]
  temp[!is.na(loss) & cnt==1,loss2:=loss/cnt]
  temp[is.na(loss),loss2:=loss1/cnt]
  noise_tr = 1 + (runif(nrow(train_data))-0.5)*.1
  noise_te = rep(1,nrow(test_data))
  noise = c(noise_tr,noise_te)
  temp$loss2 = temp$loss2*noise
  return(temp$loss2)
}

for(i in catcols){
  temp = loss_append_sing(full_data,i)
  temp[which(is.na(temp))] = 0
  full_data = cbind(full_data,temp)
  colnames(full_data)[ncol(full_data)] = paste0(i,"mloss")
}

# train_data = full_data[seq(1,nrow(train_data)),]
# test_data = full_data[seq(nrow(train_data)+1,nrow(full_data)),]
# 
# train_data[,catcols:=NULL,with=F]
# test_data[,catcols:=NULL,with=F]

contcols = paste0("cont",seq(1,14))

for(i in contcols){
  temp = caret::BoxCoxTrans(train_data[,get(i)])
  train_data[,paste0(i,"bc") := unlist(predict(temp,get(i)))]
  test_data[,paste0(i,"bc") := unlist(predict(temp,get(i)))]
}

temp = caret::BoxCoxTrans(train_data[,cont2copy])
train_data[,cont2copybc := unlist(predict(temp,cont2copy))]
test_data[,cont2copybc := unlist(predict(temp,cont2copy))]

corr = matrix(rep(0,14),14,1)
j=0
for(i in contcols){
  j=j+1
  corr[j] = cor(train_data[,get(i)],train_data[,paste0(i,"bc"),with=F])
}

train_data[,contcols:=NULL,with=F]
test_data[,contcols:=NULL,with=F]

corr = as.data.frame(matrix(rep(0,14*10),14,10))

contcols_bc = colnames(train_data)[grep("bc",colnames(train_data))]

j=0
for(i in contcols_bc){
  j=j+1
  corr[j,1] = cor(train_data[,get(i)],train_data$loss)
  corr[j,2] = cor(log(train_data[,get(i)]+10),train_data$loss)
  corr[j,3] = cor(exp(train_data[,get(i)]),train_data$loss)
  corr[j,4] = cor(asin(sqrt(train_data[,get(i)])),train_data$loss)
  corr[j,5] = cor(sin(train_data[,get(i)]),train_data$loss)
  corr[j,6] = cor((train_data[,get(i)])^2,train_data$loss)
  corr[j,7] = cor((train_data[,get(i)])^3,train_data$loss)
  corr[j,8] = cor(asin(train_data[,get(i)]),train_data$loss)
  corr[j,9] = cor((train_data[,get(i)])^2+(train_data[,get(i)]),train_data$loss)
  corr[j,10] = cor((train_data[,get(i)])^3+(train_data[,get(i)])^2+train_data[,get(i)],train_data$loss)
}

colnames(corr) = c("com","log","exp","asin_sqt","sin","sq","cube","asin","sq_c","cube_c")

train_data$cont13_tran = train_data$cont13bc^3+train_data$cont13bc^2+train_data$cont13bc
test_data$cont13_tran = test_data$cont13bc^3+test_data$cont13bc^2+test_data$cont13bc

train_data$cont14_tran = train_data$cont14bc^3+train_data$cont14bc^2+train_data$cont14bc
test_data$cont14_tran = test_data$cont14bc^3+test_data$cont14bc^2+test_data$cont14bc

newcont = contcols_bc = colnames(train_data)[grep("cont",colnames(train_data))]

corr_1 = as.data.frame(matrix(rep(0,16*16),16,16))

for(i in seq(1,length(newcont)-1)){
  for(j in seq(i+1,length(newcont))){
    temp = cor(train_data$loss,exp(train_data[,get(newcont[i])]/train_data[,get(newcont[j])]))
    corr_1[i,j] = temp; corr_1[j,i] = temp
  }
}

train_data$cont3_7exp = exp(train_data$cont7bc/train_data$cont3bc)
test_data$cont3_7exp = exp(test_data$cont7bc/test_data$cont3bc)

train_data$cont3_12exp = exp(train_data$cont3bc/train_data$cont12bc)
test_data$cont3_12exp = exp(test_data$cont3bc/test_data$cont12bc)

train_data$cont2_12exp = exp(train_data$cont2bc/train_data$cont12bc)
test_data$cont2_12exp = exp(test_data$cont2bc/test_data$cont12bc)

colnames(corr_1) = newcont; rownames(corr_1) = newcont

train_data[,cont14bc:=NULL]; test_data[,cont14bc:=NULL]  ## cont14 high corr

corr_2 = as.data.frame(matrix(rep(0,16*16),16,16))

for(i in seq(1,length(newcont)-1)){
  for(j in seq(i+1,length(newcont))){
    corr_2[i,j] = cor(train_data$loss,log(train_data[,get(newcont[i])]+train_data[,get(newcont[j])]+10))
    corr_2[j,i] = cor(train_data$loss,log(train_data[,get(newcont[j])]+train_data[,get(newcont[i])]+10))
  }
}

colnames(corr_2) = newcont; rownames(corr_2) = newcont

train_data$cont2_3log = log(train_data$cont2bc+train_data$cont3bc+10)
test_data$cont2_3log = log(test_data$cont2bc+test_data$cont3bc+10)

train_data$cont2_11exp = exp(train_data$cont2bc/train_data$cont11bc)
test_data$cont2_11exp = exp(test_data$cont2bc/test_data$cont11bc)

corr_3 = as.data.frame(matrix(rep(0,16*16),16,16))

for(i in seq(1,length(newcont)-1)){
  for(j in seq(i+1,length(newcont))){
    corr_3[i,j] = cor(train_data$loss,exp((train_data[,get(newcont[i])]+train_data[,get(newcont[j])]+10)^2))
    corr_3[j,i] = cor(train_data$loss,exp((train_data[,get(newcont[j])]+train_data[,get(newcont[i])]+10)^2))
  }
}

colnames(corr_3) = newcont; rownames(corr_3) = newcont

full_data = rbind(train_data,test_data)

saveRDS(full_data,"full_data_2.rds")

test_data[,loss:=NULL]

saveRDS(train_data,"train_data_fin.rds")
saveRDS(test_data,"test_data_fin.rds")

##########################################################
setwd("C:/Users/rsoni106/Documents/Work/Methodology Work/Kaggle/Completed/Allstate/prepared_data/new_data/")

library(data.table)
library(dplyr)

contcols = paste0("cont",seq(1,14)); contcols = setdiff(contcols,"cont2")

train_data = readRDS("train_data_fin.rds")
test_data = readRDS("test_data_fin.rds")

library(mgcv)

gam_model = gam(loss ~ s(cont1,bs="cr"),data = train_data)

dev.new(width=5, height=4)

plot.gam(gam_model,se=F)

gam_model = gam(loss ~ s(cont3,bs="cr"),data = train_data)

dev.new(width=5, height=4)

plot.gam(gam_model,se=F)

gam_model = gam(loss ~ s(cont4,bs="cr"),data = train_data)

dev.new(width=5, height=4)

plot.gam(gam_model,se=F)

gam_model = gam(loss ~ s(cont5,bs="cr"),data = train_data)

dev.new(width=5, height=4)

plot.gam(gam_model,se=F)

gam_model = gam(loss ~ s(cont6,bs="cr"),data = train_data)

dev.new(width=5, height=4)

plot.gam(gam_model,se=F)

gam_model = gam(loss ~ s(cont7,bs="cr"),data = train_data)

dev.new(width=5, height=4)

plot.gam(gam_model,se=F)

gam_model = gam(loss ~ s(cont8,bs="cr"),data = train_data)

dev.new(width=5, height=4)

plot.gam(gam_model,se=F)

gam_model = gam(loss ~ s(cont9,bs="cr"),data = train_data)

dev.new(width=5, height=4)

plot.gam(gam_model,se=F)

gam_model = gam(loss ~ s(cont10,bs="cr"),data = train_data)

dev.new(width=5, height=4)

plot.gam(gam_model,se=F)

gam_model = gam(loss ~ s(cont11,bs="cr"),data = train_data)

dev.new(width=5, height=4)

plot.gam(gam_model,se=F)

gam_model = gam(loss ~ s(cont12,bs="cr"),data = train_data)

dev.new(width=5, height=4)

plot.gam(gam_model,se=F)

gam_model = gam(loss ~ s(cont13,bs="cr"),data = train_data)

dev.new(width=5, height=4)

plot.gam(gam_model,se=F)

gam_model = gam(loss ~ s(cont14,bs="cr"),data = train_data)

dev.new(width=5, height=4)

plot.gam(gam_model,se=F)

################# Continuous to categorical using spline fit ###############

train_data$cont1_cat = ifelse(train_data$cont1<=0.22,"1",
                              ifelse(train_data$cont1<=0.38,"2",
                                     ifelse(train_data$cont1<=0.65,"3",
                                            ifelse(train_data$cont1<=0.78,"4",
                                                   ifelse(train_data$cont1<=0.86,"5","6")))))

test_data$cont1_cat = ifelse(test_data$cont1<=0.22,"1",
                              ifelse(test_data$cont1<=0.38,"2",
                                     ifelse(test_data$cont1<=0.65,"3",
                                            ifelse(test_data$cont1<=0.78,"4",
                                                   ifelse(test_data$cont1<=0.86,"5","6")))))

train_data$cont3_cat = ifelse(train_data$cont3<=0.1,"1","2")
                              
test_data$cont3_cat = ifelse(test_data$cont3<=0.1,"1","2")
                             

train_data$cont4_cat = ifelse(train_data$cont4<=0.25,"1",
                              ifelse(train_data$cont4<=0.38,"2",
                                     ifelse(train_data$cont4<=0.45,"3",
                                            ifelse(train_data$cont4<=0.64,"4",
                                                   ifelse(train_data$cont4<=0.8,"5","6")))))

test_data$cont4_cat = ifelse(test_data$cont4<=0.25,"1",
                             ifelse(test_data$cont4<=0.38,"2",
                                    ifelse(test_data$cont4<=0.45,"3",
                                           ifelse(test_data$cont4<=0.64,"4",
                                                  ifelse(test_data$cont4<=0.8,"5","6")))))

train_data$cont5_cat = ifelse(train_data$cont5<=0.4,"1",
                              ifelse(train_data$cont5<=0.54,"2",
                                     ifelse(train_data$cont5<=0.74,"3",
                                            ifelse(train_data$cont5<=0.9,"4","5"))))

test_data$cont5_cat = ifelse(test_data$cont5<=0.4,"1",
                              ifelse(test_data$cont5<=0.54,"2",
                                     ifelse(test_data$cont5<=0.74,"3",
                                            ifelse(test_data$cont5<=0.9,"4","5"))))

train_data$cont6_cat = ifelse(train_data$cont6<=0.5,"1",
                              ifelse(train_data$cont6<=0.62,"2","3"))

test_data$cont6_cat = ifelse(test_data$cont6<=0.5,"1",
                              ifelse(test_data$cont6<=0.62,"2","3"))

train_data$cont8_cat = ifelse(train_data$cont8<=0.75,"1",
                              ifelse(train_data$cont8<=0.9,"2","3"))

test_data$cont8_cat = ifelse(test_data$cont8<=0.75,"1",
                              ifelse(test_data$cont8<=0.9,"2","3"))

train_data$cont9_cat = ifelse(train_data$cont9<=0.3,"1",
                              ifelse(train_data$cont9<=0.55,"2",
                                     ifelse(train_data$cont9<=0.62,"3",
                                            ifelse(train_data$cont9<=0.78,"4",
                                                   ifelse(train_data$cont9<=0.9,"5","6")))))

test_data$cont9_cat = ifelse(test_data$cont9<=0.3,"1",
                              ifelse(test_data$cont9<=0.55,"2",
                                     ifelse(test_data$cont9<=0.62,"3",
                                            ifelse(test_data$cont9<=0.78,"4",
                                                   ifelse(test_data$cont9<=0.9,"5","6")))))

train_data$cont10_cat = ifelse(train_data$cont10<=0.18,"1",
                              ifelse(train_data$cont10<=0.6,"2",
                                     ifelse(train_data$cont10<=0.75,"3","4")))

test_data$cont10_cat = ifelse(test_data$cont10<=0.18,"1",
                               ifelse(test_data$cont10<=0.6,"2",
                                      ifelse(test_data$cont10<=0.75,"3","4")))

train_data$cont13_cat = ifelse(train_data$cont13<=0.25,"1",
                               ifelse(train_data$cont13<=0.38,"2",
                                      ifelse(train_data$cont13<=0.5,"3",
                                             ifelse(train_data$cont13<=0.78,"4","5"))))

test_data$cont13_cat = ifelse(test_data$cont13<=0.25,"1",
                               ifelse(test_data$cont13<=0.38,"2",
                                      ifelse(test_data$cont13<=0.5,"3",
                                             ifelse(test_data$cont13<=0.78,"4","5"))))

train_data$cont14_cat = ifelse(train_data$cont14<=0.25,"1",
                               ifelse(train_data$cont14<=0.32,"2",
                                      ifelse(train_data$cont14<=0.5,"3",
                                             ifelse(train_data$cont14<=0.78,"4","5"))))

test_data$cont14_cat = ifelse(test_data$cont14<=0.25,"1",
                              ifelse(test_data$cont14<=0.32,"2",
                                     ifelse(test_data$cont14<=0.5,"3",
                                            ifelse(test_data$cont14<=0.78,"4","5"))))

train_data$loss95 = ifelse(train_data$loss >= quantile(train_data$loss,.95), 
                         quantile(train_data$loss,.95),train_data$loss)

train_data$logloss = log(train_data$loss+200)

train_data$loss80 = ifelse(train_data$loss >= quantile(train_data$loss,.80), 
                           quantile(train_data$loss,.80),train_data$loss)

train_data$loss80 = ifelse(train_data$loss80 <= quantile(train_data$loss,.01), 
                           quantile(train_data$loss,.01),train_data$loss80)

saveRDS(train_data,"train_data_fin1.rds")
saveRDS(test_data,"test_data_fin1.rds")

loss_append_sing = function(df,x){
  val = df[!is.na(loss),.(sum(loss),.N),get(x)] 
  colnames(val) = c(x,"loss1","cnt")
  temp = df %>% left_join(val,by=c(x));temp = as.data.table(temp)
  temp = temp[,c("loss","loss1","cnt"),with=F]
  temp[!is.na(loss) & cnt>1,loss2:=(loss1-loss)/(cnt-1)]
  temp[!is.na(loss) & cnt==1,loss2:=loss/cnt]
  temp[is.na(loss),loss2:=loss1/cnt]
  noise_tr = 1 + (runif(nrow(train_data))-0.5)*.1
  noise_te = rep(1,nrow(test_data))
  noise = c(noise_tr,noise_te)
  temp$loss2 = temp$loss2*noise
  return(temp$loss2)
}

cols_new = c("cont1_cat","cont3_cat","cont8_cat","cont14_cat","cont4_cat","cont9_cat","cont5_cat",
             "cont10_cat","cont6_cat","cont13_cat")

full_data = rbind(train_data,test_data,fill=T)

for(i in cols_new){
  temp = loss_append_sing(full_data,i)
  print(sum(is.na(temp)))
  temp[which(is.na(temp))] = 0
  full_data = cbind(full_data,temp)
  colnames(full_data)[ncol(full_data)] = paste0(i,"mloss")
}

loss_append = function(df,x,y){
  val = df[!is.na(loss),.(sum(loss),.N),list(get(x),get(y))] 
  colnames(val) = c(x,y,"loss1","cnt")
  temp = df %>% left_join(val,by=c(x,y));temp = as.data.table(temp)
  temp = temp[,c("loss","loss1","cnt"),with=F]
  temp[!is.na(loss) & cnt>1,loss2:=(loss1-loss)/(cnt-1)]
  temp[!is.na(loss) & cnt==1,loss2:=loss/cnt]
  noise_tr = 1 + (runif(nrow(train_data))-0.5)*.1
  noise_te = rep(1,nrow(test_data))
  noise = c(noise_tr,noise_te)
  temp[is.na(loss),loss2:=loss1/cnt]
  temp$loss2 = temp$loss2*noise
  return(temp$loss2)
}

temp = loss_append(full_data,"cat80","cont14_cat")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_14c_mloss"

temp = loss_append(full_data,"cat80","cont13_cat")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat80_13c_mloss"

temp = loss_append(full_data,"cat79","cont13_cat")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat79_13c_mloss"

temp = loss_append(full_data,"cat79","cont14_cat")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat79_14c_mloss"

temp = loss_append(full_data,"cat12","cont14_cat")
sum(is.na(temp))
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat12_14c_mloss"

temp = loss_append(full_data,"cat12","cont13_cat")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat12_13c_mloss"

temp = loss_append(full_data,"cat111","cont13_cat")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat111_13c_mloss"

temp = loss_append(full_data,"cat111","cont14_cat")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat111_14c_mloss"

temp = loss_append(full_data,"cat57","cont13_cat")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat57_13c_mloss"

temp = loss_append(full_data,"cat57","cont14_cat")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat57_14c_mloss"

temp = loss_append(full_data,"cat112","cont13_cat")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat112_13c_mloss"

temp = loss_append(full_data,"cat112","cont1_cat")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat112_1c_mloss"

temp = loss_append(full_data,"cat112","cont3_cat")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat112_3c_mloss"

temp = loss_append(full_data,"cat112","cont4_cat")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat112_4c_mloss"

temp = loss_append(full_data,"cat112","cont6_cat")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat112_6c_mloss"

temp = loss_append(full_data,"cat112","cont8_cat")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat112_8c_mloss"

temp = loss_append(full_data,"cat112","cont9_cat")
sum(is.na(temp))
temp[which(is.na(temp))] = 0
full_data = cbind(full_data,temp)
colnames(full_data)[ncol(full_data)] = "cat112_9c_mloss"

saveRDS(full_data,"full_data_fin2.rds")

train_data = full_data[seq(1,188318),]
test_data = full_data[seq(188319,nrow(full_data)),]

test_data[,loss95:=NULL]; test_data[,logloss:=NULL]; test_data[,loss80:=NULL]; test_data[,loss:=NULL]

saveRDS(train_data,"train_data_fin2.rds"); saveRDS(test_data,"test_data_fin2.rds")

train_data$cont7_1exp = exp((train_data$cont7)/(train_data$cont1+1))
test_data$cont7_1exp = exp((test_data$cont7)/(test_data$cont1+1))

train_data$cont12_10exp = exp((train_data$cont12-2)/(train_data$cont10-3))
test_data$cont12_10exp = exp((test_data$cont12-2)/(test_data$cont10-3))

train_data$cont2_14sqrt = sqrt((train_data$cont2)/(train_data$cont14))
test_data$cont2_14sqrt = sqrt((test_data$cont2)/(test_data$cont14))

train_data$cont3_2_sq = train_data$cont3^2+train_data$cont2^2
test_data$cont3_2_sq = test_data$cont3^2+test_data$cont2^2

train_data$cont7_2_sq = train_data$cont7^2+train_data$cont2^2
test_data$cont7_2_sq = test_data$cont7^2+test_data$cont2^2

train_data$cont14_2exp = exp(train_data$cont14+train_data$cont2)
test_data$cont14_2exp = exp(test_data$cont14+test_data$cont2)

train_data$cont12_2_sq = (train_data$cont12^2+train_data$cont2^2)
test_data$cont12_2_sq = (test_data$cont12^2+test_data$cont2^2)

train_data$cont10_2_sq = sqrt(train_data$cont10^2+train_data$cont2^2)
test_data$cont10_2_sq = sqrt(test_data$cont10^2+test_data$cont2^2)

############## Keep only important vars and try again #############

options(scipen=999)

train_data = readRDS("train_data_fin2.rds")
test_data = readRDS("test_data_fin2.rds")

# keep_vars = read.csv("keep_var_only.csv")
# 
# keep_vars1 = as.character(unlist(as.list(keep_vars$keep_var)))
# 
# train_data = train_data[,c(keep_vars1,"loss","loss80","loss95","logloss"),with=F]
# test_data = test_data[,keep_vars1,with=F]

col_single = c("cat1cnt","cat2cnt","cat3cnt","cat7cnt","cat11cnt","cat15cnt","cat19cnt","cat23cnt",
               "cat27cnt","cat31cnt","cat35cnt","cat39cnt","cat43cnt","cat47cnt","cat51cnt","cat55cnt",
               "cat59cnt","cat63cnt","cat67cnt","cat71cnt","cat75cnt","cat79cnt","cat83cnt","cat87cnt",
               "cat91cnt","cat95cnt","cat99cnt","cat103cnt","cat107cnt","cat111cnt","cat115cnt","cat4cnt",
               "cat8cnt","cat12cnt","cat16cnt","cat20cnt","cat24cnt","cat28cnt","cat32cnt","cat36cnt",
               "cat40cnt","cat44cnt","cat48cnt","cat52cnt","cat56cnt","cat60cnt","cat64cnt","cat68cnt",
               "cat72cnt","cat76cnt","cat80cnt","cat84cnt","cat88cnt","cat92cnt","cat96cnt","cat100cnt",
               "cat104cnt","cat108cnt","cat112cnt","cat116cnt","cat5cnt","cat9cnt","cat13cnt","cat17cnt",
               "cat21cnt","cat25cnt","cat29cnt","cat33cnt","cat37cnt","cat41cnt","cat45cnt","cat49cnt",
               "cat53cnt","cat57cnt","cat61cnt","cat65cnt","cat69cnt","cat73cnt","cat77cnt","cat81cnt",
               "cat85cnt","cat89cnt","cat93cnt","cat97cnt","cat101cnt","cat105cnt","cat109cnt","cat113cnt",
               "cat6cnt","cat10cnt","cat14cnt","cat18cnt","cat22cnt","cat26cnt","cat30cnt","cat34cnt",
               "cat38cnt","cat42cnt","cat46cnt","cat50cnt","cat54cnt","cat58cnt","cat62cnt","cat66cnt",
               "cat70cnt","cat74cnt","cat78cnt","cat82cnt","cat86cnt","cat90cnt","cat94cnt","cat98cnt",
               "cat102cnt","cat106cnt","cat110cnt","cat114cnt")

train_data[,col_single:=NULL,with=F]
test_data[,col_single:=NULL,with=F]

col_single_2 = c("cat1mloss","cat2mloss","cat3mloss","cat7mloss","cat11mloss","cat15mloss","cat19mloss",
                 "cat23mloss","cat27mloss","cat31mloss","cat35mloss","cat39mloss","cat43mloss",
                 "cat47mloss","cat51mloss","cat55mloss","cat59mloss","cat63mloss","cat67mloss",
                 "cat71mloss","cat75mloss","cat79mloss","cat83mloss","cat87mloss","cat91mloss",
                 "cat95mloss","cat99mloss","cat103mloss","cat107mloss","cat111mloss","cat115mloss",
                 "cat4mloss","cat8mloss","cat12mloss","cat16mloss","cat20mloss","cat24mloss","cat28mloss",
                 "cat32mloss","cat36mloss","cat40mloss","cat44mloss","cat48mloss","cat52mloss",
                 "cat56mloss","cat60mloss","cat64mloss","cat68mloss","cat72mloss","cat76mloss",
                 "cat80mloss","cat84mloss","cat88mloss","cat92mloss","cat96mloss","cat100mloss",
                 "cat104mloss","cat108mloss","cat112mloss","cat116mloss","cat5mloss","cat9mloss",
                 "cat13mloss","cat17mloss","cat21mloss","cat25mloss","cat29mloss","cat33mloss",
                 "cat37mloss","cat41mloss","cat45mloss","cat49mloss","cat53mloss","cat57mloss",
                 "cat61mloss","cat65mloss","cat69mloss","cat73mloss","cat77mloss","cat81mloss",
                 "cat85mloss","cat89mloss","cat93mloss","cat97mloss","cat101mloss","cat105mloss",
                 "cat109mloss","cat113mloss","cat6mloss","cat10mloss","cat14mloss","cat18mloss",
                 "cat22mloss","cat26mloss","cat30mloss","cat34mloss","cat38mloss","cat42mloss",
                 "cat46mloss","cat50mloss","cat54mloss","cat58mloss","cat62mloss","cat66mloss",
                 "cat70mloss","cat74mloss","cat78mloss","cat82mloss","cat86mloss","cat90mloss",
                 "cat94mloss","cat98mloss","cat102mloss","cat106mloss","cat110mloss","cat114mloss")

train_data[,col_single_2:=NULL,with=F]
test_data[,col_single_2:=NULL,with=F]

saveRDS(train_data,"train_data_fin3.rds")
saveRDS(test_data,"test_data_fin3.rds")

train_data$cat80_12_100_53mloss = train_data$cat80_12_mloss*train_data$cat100_53_mloss
test_data$cat80_12_100_53mloss = test_data$cat80_12_mloss*test_data$cat100_53_mloss

train_data$cat80_12_79_57mloss = train_data$cat80_12_mloss*train_data$cat79_57_mloss
test_data$cat80_12_79_57mloss = test_data$cat80_12_mloss*test_data$cat79_57_mloss

train_data$cat100_53_79_57mloss = train_data$cat100_53_mloss*train_data$cat79_57_mloss
test_data$cat100_53_79_57mloss = test_data$cat100_53_mloss*test_data$cat79_57_mloss

train_data$cat80_12_79_c2mloss = train_data$cat80_12_mloss*train_data$cat79_c2_mloss
test_data$cat80_12_79_c2mloss = test_data$cat80_12_mloss*test_data$cat79_c2_mloss

train_data$cat80_12_114_2_mloss = train_data$cat80_12_mloss*train_data$cat114_2_mloss
test_data$cat80_12_114_2_mloss = test_data$cat80_12_mloss*test_data$cat114_2_mloss

train_data$cat80_12_114_2_mloss = train_data$cat80_12_mloss*train_data$cat114_2_mloss
test_data$cat80_12_114_2_mloss = test_data$cat80_12_mloss*test_data$cat114_2_mloss

train_data$cat80_12_79_87_mloss = train_data$cat80_12_mloss*train_data$cat79_87_mloss
test_data$cat80_12_79_87_mloss = test_data$cat80_12_mloss*test_data$cat79_87_mloss

train_data$cat80_12_103_1_mloss = train_data$cat80_12_mloss*train_data$cat103_1_mloss
test_data$cat80_12_103_1_mloss = test_data$cat80_12_mloss*test_data$cat103_1_mloss

train_data$cat80_12_103_1_mloss = train_data$cat80_12_mloss*train_data$cat103_1_mloss
test_data$cat80_12_103_1_mloss = test_data$cat80_12_mloss*test_data$cat103_1_mloss

train_data$cat79_57_114_2mloss = train_data$cat79_57_mloss*train_data$cat114_2_mloss
test_data$cat79_57_114_2mloss = test_data$cat79_57_mloss*test_data$cat114_2_mloss

train_data = readRDS("train_data_fin3.rds")
test_data = readRDS("test_data_fin3.rds")

train_data$lcat103_10_53cnt = log(train_data$cat103_10_53cnt)
test_data$lcat103_10_53cnt = log(test_data$cat103_10_53cnt)

train_data$lcat81_111_100_53cnt = log(train_data$cat81_111_100_53cnt)
test_data$lcat81_111_100_53cnt = log(test_data$cat81_111_100_53cnt)

train_data$lcat80_12_103_1_mloss = log(train_data$cat80_12_103_1_mloss+1)
test_data$lcat80_12_103_1_mloss = log(test_data$cat80_12_103_1_mloss+1)

train_data$lcat80_12_79_c2mloss = log(train_data$cat80_12_79_c2mloss+1)
test_data$lcat80_12_79_c2mloss = log(test_data$cat80_12_79_c2mloss+1)

train_data$lcat80_12_100_53mloss = log(train_data$cat80_12_100_53mloss+1)
test_data$lcat80_12_100_53mloss = log(test_data$cat80_12_100_53mloss+1)

dev.new(5,4)

panel.smoother = function(x,y,fill.color) {
  fill = fill.color
  panel.xyplot(x,y,col=fill)
  panel.lmline(x,y)
  latticeExtra::panel.lmlineq(x,y,r.squared =TRUE,col.text='blue',col.line="blue")
}

xyplot(logloss~sqrt(cat100_53_mloss+1),data=train_data, panel=panel.smoother, fill.color="green")

###############################################################

train_data$cat103_10_53cnt = train$cat103_10_53cnt
test_data$cat103_10_53cnt = test$cat103_10_53cnt

train_data$cat81_111_100_53cnt = train$cat81_111_100_53cnt
test_data$cat81_111_100_53cnt = test$cat81_111_100_53cnt

train_data[,c("lcat81_111_100_53cnt","lcat103_10_53cnt"):=NULL,with=F]
test_data[,c("lcat81_111_100_53cnt","lcat103_10_53cnt"):=NULL,with=F]

saveRDS(train_data,"train_data_fin4.rds")
saveRDS(test_data,"test_data_fin4.rds")

