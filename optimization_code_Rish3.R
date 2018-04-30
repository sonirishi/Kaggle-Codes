rm(list=ls(all=T))
library(lpSolveAPI)
library(dplyr)
library(data.table)
library(triangle)
options(scipen=20, digits=4)
setwd("C:/Users/rsoni106/Documents/Work/Methodology Work/Kaggle/Completed/Santa")

gift_data = fread("gifts.csv")

library(stringr)

gift_data[,gift_new:=str_sub(str_trim(GiftId),1,str_locate(str_trim(GiftId),"_")[,1]-1)]

total_prod = table(gift_data$gift_new)

smwt_ball = function(n) pmax(0, 1 + rnorm(n=n, mean=1, sd=0.3))

smwt_bike = function(n) pmax(0, rnorm(n=n, mean=20, sd=10))

smwt_block = function(n) if(n == 0) return(numeric(0)) else return(rtriangle(n=n, a=5, c=10, b=20))

smwt_book = function(n) rchisq(n=n, df=2)

smwt_coal = function(n) 47 * rbeta(n=n, shape1=0.5, shape2=0.5)

smwt_doll = function(n) rgamma(n=n, shape=5, rate=1)

smwt_gloves = function(n) runif(n=n, min=0, max=1) + (runif(n=n, min=0, max=1) < .3) * 3

smwt_horse = function(n) pmax(0, rnorm(n=n, mean=5, sd=2))

smwt_train = function(n) pmax(0, rnorm(n=n, mean=10, sd=5))

iter = 10000

wt_matrix = as.data.frame(matrix(rep(0,9*iter),iter,9))
colnames(wt_matrix) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

wt_matrix[,2] = wt_ball(iter)
wt_matrix[,3] = wt_bike(iter)
wt_matrix[,8] = wt_block(iter)
wt_matrix[,6] = wt_book(iter)
wt_matrix[,5] = wt_coal(iter)
wt_matrix[,7] = wt_doll(iter)
wt_matrix[,9] = wt_gloves(iter)
wt_matrix[,1] = wt_horse(iter)
wt_matrix[,4] = wt_train(iter)

wt_simulate = as.data.frame(matrix(rep(0,36),4,9))

colnames(wt_simulate) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

for (i in 1:ncol(wt_simulate)){
  wt_simulate[1,i] = mean(wt_matrix[,i])
  wt_simulate[2,i] = sd(wt_matrix[,i])
  wt_simulate[3,i] = max(wt_matrix[,i])
  wt_simulate[4,i] = min(wt_matrix[,i])
}

rownames(wt_simulate) = c("mean","sd","max","min")

generate.sd = 50/wt_matrix

item_count = data.table(GiftId=c("horse","ball","bike","train","coal","book","doll","blocks","gloves"),
                        count=c(1000,1100,500,1000,166,1200,1000,1000,200))

set.seed(123)

generate_gifts = function(){
  random_generator = runif(10,0,1)
}

bags_all = as.data.frame(matrix(rep(0,1000000),100000,10))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts() 
}

map_gift = function(random_generator){
  bag_generator = matrix(rep(0,9),1,9)
  if (random_generator <= 0.13955){
    bag_generator = "Horse"
  } else if (random_generator > 0.13955 & random_generator <= 0.29305){
    bag_generator = "Ball"
  } else if (random_generator > 0.29305 & random_generator <= 0.36282){
    bag_generator = "Bike"
  } else if (random_generator > 0.36282 & random_generator <= 0.50237){
    bag_generator = "Train"
  } else if (random_generator > 0.50237 & random_generator <= 0.52553){
    bag_generator = "Coal"
  } else if (random_generator > 0.52553 & random_generator <= 0.69299){
    bag_generator = "Book"
  } else if (random_generator > 0.69299 & random_generator <= 0.83254){
    bag_generator = "Doll"
  } else if (random_generator > 0.83254 & random_generator <= 0.97209){
    bag_generator = "Blocks"
  } else {
    bag_generator = "Gloves"
  }
  return(bag_generator)
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_10.rds")

################################9 object Bags###########################

generate_gifts = function(n){
  random_generator = runif(n,0,1)
}

bags_all = as.data.frame(matrix(rep(0,900000),100000,9))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts(9) 
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_9.rds")

#####################8 candidate bags ##############

bags_all = as.data.frame(matrix(rep(0,800000),100000,8))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts(8) 
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_8.rds")

############################################################

bags_all = as.data.frame(matrix(rep(0,700000),100000,7))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts(7) 
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_7.rds")

############################################################

bags_all = as.data.frame(matrix(rep(0,600000),100000,6))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts(6) 
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_6.rds")

############################################################

bags_all = as.data.frame(matrix(rep(0,500000),100000,5))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts(5) 
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_5.rds")

set.seed(678)

bags_all = as.data.frame(matrix(rep(0,500000),100000,5))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts(5) 
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_5_1.rds")

############################################################

bags_all = as.data.frame(matrix(rep(0,400000),100000,4))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts(4) 
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_4.rds")

set.seed(4578)

bags_all = as.data.frame(matrix(rep(0,400000),100000,4))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts(4) 
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_4_1.rds")

############################################################

bags_all = as.data.frame(matrix(rep(0,300000),100000,3))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts(3) 
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_3.rds")

set.seed(9876)

bags_all = as.data.frame(matrix(rep(0,300000),100000,3))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts(3) 
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_3_1.rds")


########################One more 10 item bag###########################

bags_all = as.data.frame(matrix(rep(0,1000000),100000,10))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts(10) 
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_10_2.rds")

#################9 bags second round###############
generate_gifts = function(n){
  random_generator = runif(n,0,1)
}

bags_all = as.data.frame(matrix(rep(0,900000),100000,9))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts(9) 
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_9_2.rds")

set.seed(98809)

bags_all = as.data.frame(matrix(rep(0,900000),100000,9))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts(9) 
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_9_3.rds")

#######################################################

bags_all = as.data.frame(matrix(rep(0,800000),100000,8))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts(8) 
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_8_2.rds")

bags_all = as.data.frame(matrix(rep(0,800000),100000,8))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts(8) 
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_8_3.rds")

###############################7 bags once more###########

bags_all = as.data.frame(matrix(rep(0,700000),100000,7))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts(7) 
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_7_2.rds")

set.seed(1988)
bags_all = as.data.frame(matrix(rep(0,700000),100000,7))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts(7) 
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_7_3.rds")

set.seed(3412)
bags_all = as.data.frame(matrix(rep(0,800000),100000,8))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts(8) 
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_8_4.rds")

set.seed(1996)
bags_all = as.data.frame(matrix(rep(0,700000),100000,7))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts(7) 
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_7_4.rds")

set.seed(4567)

bags_all = as.data.frame(matrix(rep(0,900000),100000,9))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts(9) 
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_9_4.rds")

set.seed(1965)

bags_all = as.data.frame(matrix(rep(0,1000000),100000,10))

for(i in 1:nrow(bags_all)){
  bags_all[i,] = generate_gifts(10) 
}

bags_all1 = as.data.frame(matrix(rep(0,100000),100000,1))

for(i in 1:ncol(bags_all)){
  temp = lapply(bags_all[,i],map_gift)
  bags_all1 = cbind(bags_all1,unlist(temp))
}

candidate_bags = as.data.frame(matrix(rep(0,900000),100000,9))
colnames(candidate_bags) = c("Horse","Ball","Bike","Train","Coal","Book","Doll","Blocks","Gloves")

bags_all1 = bags_all1[,-1]
bags_all1 = t(bags_all1)

for(i in 1:nrow(candidate_bags)){
  tt = as.data.frame(table(bags_all1[,i]))
  candidate_bags[i,] = tt$Freq[match(colnames(candidate_bags),tt$Var1)]
}

saveRDS(candidate_bags,"candidate_bags_10_3.rds")
################## Finally all bags created #####################

candidate_bags_10 = readRDS("candidate_bags_10.rds")
candidate_bags_9 = readRDS("candidate_bags_9.rds")
candidate_bags_8 = readRDS("candidate_bags_8.rds")
candidate_bags_7 = readRDS("candidate_bags_7.rds")
candidate_bags_6 = readRDS("candidate_bags_6.rds")
candidate_bags_5 = readRDS("candidate_bags_5.rds")
candidate_bags_4 = readRDS("candidate_bags_4.rds")
candidate_bags_7_2 = readRDS("candidate_bags_7_2.rds")
candidate_bags_8_2 = readRDS("candidate_bags_8_2.rds")
candidate_bags_9_2 = readRDS("candidate_bags_9_2.rds")
candidate_bags_10_2 = readRDS("candidate_bags_10_2.rds")
candidate_bags_8_3 = readRDS("candidate_bags_8_3.rds")
candidate_bags_7_3 = readRDS("candidate_bags_7_3.rds")
candidate_bags_9_3 = readRDS("candidate_bags_9_3.rds")
candidate_bags_10_3 = readRDS("candidate_bags_10_3.rds")
candidate_bags_8_4 = readRDS("candidate_bags_8_4.rds")
candidate_bags_7_4 = readRDS("candidate_bags_7_4.rds")
candidate_bags_9_4 = readRDS("candidate_bags_9_4.rds")
candidate_bags_3_1 = readRDS("candidate_bags_3_1.rds")
candidate_bags_4_1 = readRDS("candidate_bags_4_1.rds")
candidate_bags_5_1 = readRDS("candidate_bags_5_1.rds")

candidate_bag_fin = rbind(candidate_bags_10,candidate_bags_9,candidate_bags_8,candidate_bags_7,
                          candidate_bags_6,candidate_bags_5,candidate_bags_4,candidate_bags_7_2,
                          candidate_bags_8_2,candidate_bags_9_2,candidate_bags_10_2,candidate_bags_8_3,
                          candidate_bags_8_4,candidate_bags_7_4,candidate_bags_9_4,candidate_bags_7_3,
                          candidate_bags_9_3,candidate_bags_10_3,candidate_bags_3_1,candidate_bags_4_1,
                          candidate_bags_5_1)

saveRDS(candidate_bag_fin,"candidate_bag_fin.rds")

###########################################################################################

na_rem = function(x){
  x[which(is.na(x))] = 0 
  return(x)
}

for(i in 1:ncol(candidate_bag_fin)){
  candidate_bag_fin[,i] = sapply(candidate_bag_fin[,i], na_rem)
}

candidate_bag_fin = unique(candidate_bag_fin)

final_candidate_bag_list = candidate_bag_fin

saveRDS(final_candidate_bag_list,"final_candidate_bag_list.rds")

###################Optimization Problem###############

final_candidate_bag_list = readRDS("final_candidate_bag_list.rds")

final_candidate_bag_list$total = apply(as.matrix(final_candidate_bag_list),1,sum)

table(final_candidate_bag_list$total); final_candidate_bag_list$total=NULL

bag_transpose = t(final_candidate_bag_list); bag_transpose = as.matrix(bag_transpose)

wt_simulate = as.matrix(wt_simulate)

mean = wt_simulate[1,]%*%bag_transpose
std = wt_simulate[2,]%*%bag_transpose

final_candidate_bag_list$mean = t(mean)
final_candidate_bag_list$sd = t(std)

all_final_candidate_bag_list = final_candidate_bag_list[which(final_candidate_bag_list$mean <= 50),]

all_final_candidate_bag_list = all_final_candidate_bag_list[sample(nrow(all_final_candidate_bag_list)),]

final_candidate_bag_list = head(all_final_candidate_bag_list,5000)

optim_prob = make.lp(nrow=10,ncol=nrow(final_candidate_bag_list))

set.type(optim_prob, columns=seq_len(nrow(final_candidate_bag_list)), type = "integer")

set.constr.type(optim_prob, c(rep("<=", 10)))
set.rhs(optim_prob, c(item_count$count, 1000))

final_candidate_bag_list_mat = as.matrix(final_candidate_bag_list)

for(i in seq(nrow(final_candidate_bag_list))){
  coeffs = c(final_candidate_bag_list_mat[i,1:9],1)
  set.column(optim_prob,i,coeffs)
}

set.objfn(optim_prob, -final_candidate_bag_list$mean + final_candidate_bag_list$sd)

solve(optim_prob)

get.objective(optim_prob)
get.variables(optim_prob)

final_candidate_bag_list = cbind(final_candidate_bag_list,get.variables(optim_prob))

colnames(final_candidate_bag_list)[12] = "bag_count"

final_candidate_bag_list$mean = as.numeric(final_candidate_bag_list$mean)
final_candidate_bag_list$sd = as.numeric(final_candidate_bag_list$sd)

final_candidate_bag_list = final_candidate_bag_list %>% arrange(desc(bag_count))

################# New way of giving weights #####################

final_candidate_bag_list = readRDS("final_candidate_bag_list.rds")

iter = 100000

final_candidate_bag_mat = as.matrix(final_candidate_bag_list)

for(j in 1:15){
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
}

saveRDS(final_candidate_bag_list,"final_candidate_bag_list_wt.rds")

expected_wt = apply(as.matrix(final_candidate_bag_list[,10:24]),1,mean)

expected_sd = apply(as.matrix(final_candidate_bag_list[,10:24]),1,sd)

final_candidate_bag_list = cbind(final_candidate_bag_list,expected_wt,expected_sd)

final_bag = final_candidate_bag_list[final_candidate_bag_list$expected_wt<50,c(1:9,25,26)]

#################### LP solver built solution #################

optim_prob = make.lp(nrow=10,ncol=nrow(final_bag))

set.type(optim_prob, columns=seq_len(nrow(final_bag)), type = "integer")

set.constr.type(optim_prob, c(rep("<=", 10)))
set.rhs(optim_prob, c(item_count$count, 1000))

final_bag_mat = as.matrix(final_bag)

for(i in seq(nrow(final_bag))){
  coeffs = c(final_bag_mat[i,1:9],1)
  set.column(optim_prob,i,coeffs)
}

set.objfn(optim_prob, -final_bag$expected_wt + 0.3*final_bag$expected_sd)

solve(optim_prob)

get.objective(optim_prob)
get.variables(optim_prob)

final_bag1 = cbind(final_bag,as.numeric(get.variables(optim_prob)))

colnames(final_bag1)[ncol(final_bag1)] = "bagcount"

final_bag1 = final_bag1 %>% arrange(desc(bagcount))
