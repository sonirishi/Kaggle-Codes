# -*- coding: utf-8 -*-
"""
Created on Wed Oct 12 12:07:09 2016

@author: rsoni106
"""

import pandas as pd
import numpy as np
import dplython as dp
import os
import matplotlib.pyplot as plt
import seaborn as sb

data_prep = "C:/Users/rsoni106/Documents/Work/Methodology Work/Kaggle/Completed/Allstate/prepared_data"

raw_data = "C:/Users/rsoni106/Documents/Work/Methodology Work/Kaggle/Completed/Allstate"

train_data = pd.read_csv(raw_data+"/train.csv")

count_null = train_data.isnull().sum()

varnames = pd.DataFrame(train_data.columns)

catcols = [x for x in train_data.columns if 'cat' in x]

contcols = [x for x in train_data.columns if 'cont' in x]

def fun_cat(x):
    return {'length': len(x.unique())}

unique_cat = train_data[catcols].apply(fun_cat) 

summary = train_data[contcols].describe()

def quantile_func(df):
    return(df.quantile([0.01,0.05,0.1,0.9,0.99]))

quantile_cont = quantile_func(train_data[contcols])

summary_cont = pd.concat((summary,quantile_cont),axis=0)

temp = train_data.groupby("cat99",as_index = False).agg({"loss1":np.mean})

################## Continuous Variable with Loss #############################

from scipy.stats import chisquare

train_data["logloss"] = np.log(train_data["loss"])

train_data["cat1"].value_counts().plot(kind='bar') ## histogram   

train_data[["loss"]].hist(bins=60)

train_data[["loss"]].apply(lambda x: np.log(x)).hist(bins=60)

sb.lmplot(x="cont5",y="loss",data=train_data)  ### Linear Plots between dependent and covariates

cap = float(train_data[["loss"]].quantile(0.999))
low = float(train_data[["loss"]].quantile(0.001))
   
train_data[["loss1"]] = train_data[["loss"]].applymap(lambda x: 2*cap if x > 2*cap else x)

train_data[["loss1"]] = train_data[["loss1"]].applymap(lambda x: low/2 if x < low/2 else x)

loss_1 = pd.qcut(train_data["logloss"],10)

temp = pd.crosstab(loss_1,train_data["cat100"])

#############################################################################

import scipy.stats.stats as st

high_card_cols = ["cat" + str(i) for i in range(73,117)]

for i in range(len(high_card_cols)):
    temp = train_data.groupby(high_card_cols[i],as_index=False).agg({'id':lambda x: x.count(),'logloss': 
        {"mean_loss": lambda x: x.mean(), "std_loss": lambda x: x.std(), "skew_loss": lambda x: st.skew(x,bias=False)}})
    temp = pd.DataFrame(temp.to_records())
    del temp["index"]
    temp.columns = [high_card_cols[i],"std_loss","skew_loss","mean_loss","Count"]
    print(temp)

x = high_card_cols[40]
temp = train_data.groupby(x,as_index=False).agg({'id':lambda x: x.count(),'logloss': 
        {"mean_loss": lambda x: x.mean(), "std_loss": lambda x: x.std(), "skew_loss": lambda x: st.skew(x,bias=False)}})
temp = pd.DataFrame(temp.to_records())
del temp["index"]
temp.columns = [x,"std_loss","skew_loss","mean_loss","Count"]
print(temp)

low_card_cols = ["cat" + str(i) for i in range(1,73)]

for i in range(len(low_card_cols)):
    temp = train_data.groupby(low_card_cols[i],as_index=False).agg({'id':lambda x: x.count(),'logloss': 
        {"mean_loss": lambda x: x.mean(), "std_loss": lambda x: x.std(), "skew_loss": lambda x: st.skew(x,bias=False)}})
    temp = pd.DataFrame(temp.to_records())
    del temp["index"]
    temp.columns = [low_card_cols[i],"std_loss","skew_loss","mean_loss","Count"]
    print(temp)

test_data = pd.read_csv(raw_data+"/test.csv")

for i in range(len(low_card_cols)):
    temp = test_data.groupby(low_card_cols[i],as_index=False).agg({'id':lambda x: x.count()})
    temp = pd.DataFrame(temp.to_records())
    del temp["index"]
    temp.columns = [low_card_cols[i],"Count"]
    print(temp)

high_card_cols = ["cat" + str(i) for i in range(73,117)]
    
x = high_card_cols[40]
temp = test_data.groupby(x,as_index=False).agg({'id':lambda x: x.count()})
temp = pd.DataFrame(temp.to_records())
del temp["index"]
temp.columns = [x,"Count"]
print(temp)

################### Categorical data in test not train combining ###################

train_data["cat92"] = train_data["cat92"].map(lambda x: "A" if x == "G" else x)

test_data["cat92"] = test_data["cat92"].map(lambda x: "A" if x == "G" else x)

train_data["cat96"] = train_data["cat96"].map(lambda x: "E" if x == "H" else x)

test_data["cat96"] = test_data["cat96"].map(lambda x: "E" if x == "H" else x)

train_data["cat99"] = train_data["cat99"].map(lambda x: "P" if x == "U" else x)

test_data["cat99"] = test_data["cat99"].map(lambda x: "P" if x == "U" else x)

train_data["cat103"] = train_data["cat103"].map(lambda x: "A" if x == "M" else x)

test_data["cat103"] = test_data["cat103"].map(lambda x: "A" if x == "M" else x)

train_data["cat106"] = train_data["cat106"].map(lambda x: "G" if x == "Q" else x)

test_data["cat106"] = test_data["cat106"].map(lambda x: "G" if x == "Q" else x)

train_data["cat109"] = train_data["cat109"].map(lambda x: "BI" if x == "AD" else x)

test_data["cat109"] = test_data["cat109"].map(lambda x: "BI" if x == "AD" else x)

train_data["cat110"] = train_data["cat110"].map(lambda x: "CL" if x in ("BH","CA","EN") else x)

test_data["cat110"] = test_data["cat110"].map(lambda x: "CL" if x in ("BH","CA","EN") else x)

train_data["cat111"] = train_data["cat111"].map(lambda x: "A" if x == "L" else x)

test_data["cat111"] = test_data["cat111"].map(lambda x: "A" if x == "L" else x)

train_data["cat113"] = train_data["cat113"].map(lambda x: "BM" if x in ("R","AA") else x)

test_data["cat113"] = test_data["cat113"].map(lambda x: "BM" if x in ("R","AA") else x)

train_data["cat116"] = train_data["cat116"].map(lambda x: "HK" if x in ("A","AI","AQ","BE","BH","BJ","BN","BR","DB","EM","ER","ET","EX","FY","HS","IS","IW","JS","KO","LP","LS","MX","N") else x)

test_data["cat116"] = test_data["cat116"].map(lambda x: "HK" if x in ("A","AI","AQ","BE","BH","BJ","BN","BR","DB","EM","ER","ET","EX","FY","HS","IS","IW","JS","KO","LP","LS","MX","N") else x)

####################### Data ALl #############################

prep_data = "C:/Users/rsoni106/Documents/Work/Methodology Work/Kaggle/Completed/Allstate/prepared_data"

train_data.to_csv(prep_data+"/train_data_prep1.csv")

test_data.to_csv(prep_data+"/test_data_prep1.csv")

##################### Model ##################################
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.grid_search import GridSearchCV

train_data_2 = pd.read_csv(data_prep + "/train_data_2.csv");

train_data_2 = train_data_2.drop('Unnamed: 0',axis=1)

vars_model = [x for x in train_data_2.columns if x not in ("id","loss","logloss")

gbm_model_1 =  GradientBoostingClassifier(learning_rate = 0.1, n_estimators = 200, max_depth = 8, min_samples_leaf = 15,
                                        verbose = 10, subsample = 0.75, max_features = 0.5,random_state=1234)
                                            
gbm_model_1.fit(train_data_2.loc[:,vars_model], np.ravel(train_data_2.loc[:,"logloss"]))

########################################################################

from pylightgbm.models import GBMRegressor
from sklearn.cross_validation import train_test_split

data_loc = "C:/Users/rsoni106/Documents/Work/Methodology Work/Kaggle/Completed/Allstate/prepared_data/new_data"

train_data = pd.read_csv(data_loc+"/train_data_py.csv")

event = pd.read_csv(data_loc+"/event_py.csv")

test_data = pd.read_csv(data_loc+"/test_data_py.csv")

vars_model = [x for x in train.columns if x not in ("id","logloss")] # this creates a tuple

gbm = GBMRegressor(num_iterations=int(2558/0.9), learning_rate=0.01, num_leaves=200, min_data_in_leaf=8,
                   feature_fraction=0.3, bagging_fraction=0.8, bagging_freq=100, verbose=True, application='regression',
                   metric='l2',num_threads=2, exec_path = )

gbm.fit(train_data,event)




from heapq import nlargest
from operator import itemgetter

temp=0

for k,v in val.items():
    if temp == 10:
        break
    print(k,v)
    temp+=1