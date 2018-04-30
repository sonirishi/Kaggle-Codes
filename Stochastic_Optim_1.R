rm(list=ls(all=T))
library(lpSolveAPI)
library(dplyr)
library(data.table)
library(triangle)
options(scipen=20, digits=4)

setwd("/mapr/projects/Rishabh_Work/PracticeCodes")

final_candidate_bag_list = readRDS("final_candidate_bag_list.rds")

final_candidate_bag_mat = as.matrix(final_candidate_bag_list)

smwt_ball = function(n) pmax(0, 1 + rnorm(n=n, mean=1, sd=0.3))

smwt_bike = function(n) pmax(0, rnorm(n=n, mean=20, sd=10))

smwt_block = function(n) if(n == 0) return(numeric(0)) else return(rtriangle(n=n, a=5, c=10, b=20))

smwt_book = function(n) rchisq(n=n, df=2)

smwt_coal = function(n) 47 * rbeta(n=n, shape1=0.5, shape2=0.5)

smwt_doll = function(n) rgamma(n=n, shape=5, rate=1)

smwt_gloves = function(n) runif(n=n, min=0, max=1) + (runif(n=n, min=0, max=1) < .3) * 3

smwt_horse = function(n) pmax(0, rnorm(n=n, mean=5, sd=2))

smwt_train = function(n) pmax(0, rnorm(n=n, mean=10, sd=5))

iter = 100000

for(j in 1:200){
  expected_wt = matrix(rep(0,nrow(final_candidate_bag_list)),nrow(final_candidate_bag_list),1)
  horse_weight = smwt_horse(iter)
  ball_weight = smwt_ball(iter)
  bike_weight = smwt_bike(iter)
  train_weight = smwt_train(iter)
  coal_weight = smwt_coal(iter)
  book_weight = smwt_book(iter)
  doll_weight = smwt_doll(iter)
  blocks_weight = smwt_block(iter)
  gloves_weight = smwt_gloves(iter)
  for(i in 1:nrow(final_candidate_bag_mat)){
    bag = final_candidate_bag_mat[i,]
    if (bag[1] != 0){
      horse_sample = sample(1:length(horse_weight),bag[1])
      wt_horse = sum(horse_weight[horse_sample])
      horse_weight = horse_weight[-horse_sample]
    } else{
      wt_horse = 0
    }
    if (bag[2] != 0){
      ball_sample = sample(1:length(ball_weight),bag[2])
      wt_ball = sum(ball_weight[ball_sample])
      ball_weight = ball_weight[-ball_sample]
    } else{
      wt_ball = 0
    }
    if (bag[3] != 0){
      bike_sample = sample(1:length(bike_weight),bag[3])
      wt_bike = sum(bike_weight[bike_sample])
      bike_weight = bike_weight[-bike_sample]
    } else{
      wt_bike = 0
    }
    if (bag[4] != 0){
      train_sample = sample(1:length(train_weight),bag[4])
      wt_train = sum(train_weight[train_sample])
      train_weight = train_weight[-train_sample]
    } else{
      wt_train = 0
    }
    if (bag[5] != 0){
      coal_sample = sample(1:length(coal_weight),bag[5])
      wt_coal = sum(coal_weight[coal_sample])
      coal_weight = coal_weight[-coal_sample]
    } else{
      wt_coal = 0
    }
    if (bag[6] != 0){
      book_sample = sample(1:length(book_weight),bag[6])
      wt_book = sum(book_weight[book_sample])
      book_weight = book_weight[-book_sample]
    } else{
      wt_book = 0
    }
    if (bag[7] != 0){
      doll_sample = sample(1:length(doll_weight),bag[7])
      wt_doll = sum(doll_weight[doll_sample])
      doll_weight = doll_weight[-doll_sample]
    } else{
      wt_doll = 0
    }
    if (bag[8] != 0){
      blocks_sample = sample(1:length(blocks_weight),bag[8])
      wt_blocks = sum(blocks_weight[blocks_sample])
      blocks_weight = blocks_weight[-blocks_sample]
    } else{
      wt_blocks = 0
    }
    if (bag[9] != 0){
      gloves_sample = sample(1:length(gloves_weight),bag[9])
      wt_gloves = sum(gloves_weight[gloves_sample])
      gloves_weight = gloves_weight[-gloves_sample]
    } else{
      wt_gloves = 0
    }
    expected_wt[i] = sum(wt_horse,wt_ball,wt_bike,wt_train,wt_coal,wt_book,wt_doll,wt_blocks,wt_gloves)
    print(paste0("Bags completed ",i," & completed iterations out of 15 ",j))
  }
  expected_wt = as.numeric(expected_wt)
  final_candidate_bag_list = cbind(final_candidate_bag_list,expected_wt)
  colnames(final_candidate_bag_list)[ncol(final_candidate_bag_list)] = paste0("expected_wt_iter",j)
  rm(horse_weight);rm(book_weight);rm(bike_weight);rm(train_weight);rm(ball_weight)
  rm(blocks_weight);rm(gloves_weight);rm(coal_weight);rm(doll_weight)
}

saveRDS(final_candidate_bag_list,"final_candidate_bag_list_wt.rds")

expected_wt = apply(as.matrix(final_candidate_bag_list[,10:ncol(final_candidate_bag_list)]),1,mean)

expected_sd = apply(as.matrix(final_candidate_bag_list[,10:ncol(final_candidate_bag_list)]),1,sd)

final_candidate_bag_list = cbind(final_candidate_bag_list,expected_wt,expected_sd)

colcount = ncol(final_candidate_bag_list)

final_bag = final_candidate_bag_list[final_candidate_bag_list$expected_wt<50,c(1:9,colcount-1,colcount)]

#################### LP solver built solution #################

item_count = data.table(GiftId=c("horse","ball","bike","train","coal","book","doll","blocks","gloves"),
                        count=c(1000,1100,500,1000,166,1200,1000,1000,200))

optim_prob = make.lp(nrow=10,ncol=nrow(final_bag))

set.type(optim_prob, columns=seq_len(nrow(final_bag)), type = "integer")

set.constr.type(optim_prob, c(rep("<=", 10)))
set.rhs(optim_prob, c(item_count$count, 1000))

final_bag_mat = as.matrix(final_bag)

for(i in seq(nrow(final_bag))){
  coeffs = c(final_bag_mat[i,1:9],1)
  set.column(optim_prob,i,coeffs)
}

set.objfn(optim_prob, -final_bag$expected_wt+3*final_bag$expected_sd)

solve(optim_prob)

get.objective(optim_prob)
get.variables(optim_prob)

final_bag1 = cbind(final_bag,as.numeric(get.variables(optim_prob)))

colnames(final_bag1)[ncol(final_bag1)] = "bagcount"

final_bag1 = final_bag1 %>% arrange(desc(bagcount))


################ Expected Score basis the simulations ####################

final_candidate_bag_list = readRDS("final_candidate_bag_list_wt.rds")

trunc_func = function(x){
  if (x > 50){
    x = 0
  } else{
    x = x
  }
}

for(i in 10:ncol(final_candidate_bag_list)){
  final_candidate_bag_list[,i] = sapply(final_candidate_bag_list[,i],trunc_func)
}

expected_wt = apply(as.matrix(final_candidate_bag_list[,10:ncol(final_candidate_bag_list)]),1,mean)

#expected_sd = apply(as.matrix(final_candidate_bag_list[,10:ncol(final_candidate_bag_list)]),1,sd)

final_candidate_bag_list = cbind(final_candidate_bag_list,expected_wt)

colcount = ncol(final_candidate_bag_list)

final_bag = final_candidate_bag_list[final_candidate_bag_list$expected_wt<50,c(1:9,colcount)]

item_count = data.table(GiftId=c("horse","ball","bike","train","coal","book","doll","blocks","gloves"),
                        count=c(1000,1100,500,1000,166,1200,1000,1000,200))

optim_prob = make.lp(nrow=10,ncol=nrow(final_bag))

set.type(optim_prob, columns=seq_len(nrow(final_bag)), type = "integer")

set.constr.type(optim_prob, c(rep("<=", 10)))
set.rhs(optim_prob, c(item_count$count, 1000))

final_bag_mat = as.matrix(final_bag)

for(i in seq(nrow(final_bag))){
  coeffs = c(final_bag_mat[i,1:9],1)
  set.column(optim_prob,i,coeffs)
}

set.objfn(optim_prob, -final_bag$expected_wt)

solve(optim_prob)

get.objective(optim_prob)
get.variables(optim_prob)

final_bag1 = cbind(final_bag,as.numeric(get.variables(optim_prob)))

colnames(final_bag1)[ncol(final_bag1)] = "bagcount"

final_bag1 = final_bag1 %>% arrange(desc(bagcount))
