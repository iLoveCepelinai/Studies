# 3 praktikos kontrolinis
# Matas Amšiejus
# DM2
# 2022 05 31


# moduliai, klasės

import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import MinMaxScaler
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import recall_score
from sklearn.metrics import confusion_matrix
from sklearn.metrics import classification_report
from sklearn.metrics import roc_auc_score



# 0. duomenys
path = "D:/Users/User/Desktop/Studijos/trecias kursas/Machine learning/Kontroliniai prakt/kontr/bank-full.csv"
df = pd.read_csv(path)

# kategorinių kintamųjų perkodavimas (supaprastintas)
label_encoding = {
    "y":{"no":0,"yes":1},
    "poutcome":{"unknown":0,"failure":0,"other":0,"success":1},
    "month":{"jan":0,"feb":1,"mar":2,"apr":3,"may":4,"jun":5,"jul":6,"aug":7,"sep":8,"oct":9,"nov":10,"dec":11},
    "contact":{"unknown":0,"cellular":1,"telephone":1},
    "loan":{"no":0,"yes":1},
    "housing":{"no":0,"yes":1},
    "default":{"no":0,"yes":1},
    "education":{"tertiary":1,"secondary":0,"unknown":0,"primary":0},
    "marital":{"married":0,"single":1,"divorced":1},   
    "job":{"management":0,"technician":1,"entrepreneur":2,"blue-collar":3,"unknown":4,"retired":5,"admin.":6,"services":7,"self-employed":8,"unemployed":9,"housemaid":10,"student":11}
}


df = df.replace(label_encoding)
df = df.drop("job", axis=1)

# kiekybinių kintamųjų transformavimas
num_features=['age', 'balance', 'day', 'duration','campaign', 'pdays', 'previous']

scaler = MinMaxScaler(feature_range=(0, 1))

df[num_features] = scaler.fit_transform(df[num_features]) 

# X, y
X, y = df.drop('y',axis=1).values , df['y'].values
X.shape, y.shape

# skaidymas į train-val ir test
X_train_val, X_test, y_train_val, y_test = train_test_split(X, y, test_size=0.2, random_state=0, stratify=y)


#####################
# 1.
#####################
# Parametru gardele:
param = [0.01, 0.1, 1, 10, 100, 1000, 10000]

X_train, X_val, y_train, y_val = train_test_split(X_train_val, y_train_val, test_size=0.2, random_state=0, stratify=y_train_val)


for C in param:
    # Modelio kurimas ir mokymas
    h_lr = LogisticRegression(C = C, max_iter = 10000)
    h_lr.fit(X_train,y_train)

    # Modelio prognoze
    y_pred = h_lr.predict(X_val)
    
    # Klasifikavimo matrica
    cm = confusion_matrix(y_val, y_pred)
    TN = cm[0][0]
    FN = cm[1][0]
    FP = cm[0][1]
    TP = cm[1][1]
    n = np.sum(cm)

    # a)
    TNR = TN / (TN + FP)

    # b)
    #recall = recall_score(y_true=y_val,y_pred=y_pred)
    recall = TP / (TP + FN)
    GM = (recall * TNR)**0.5

    print("Kai C = {}, TNR = {}, GM = {};".format(C, TNR, GM))

# Matome, kad pagal TNR didelio skirtumo niekur nera (su didesniais C minimaliai krenta), taciau pagal GM
# galime teigti, kad geriausiai klasifikuoja klasifikatoriai su parametrais >1 (nors realiai 1 labai nesiskiria irgi). Tai bet 
# koks klasifikatorius su C>=1 butu neblogas pasirinkimas.
# Jei rinktis atskirai, tai a) C = 0,01, b) C=[100,1000,10000] (sakykime imame 100)


#####################
# 2.
#####################
# a, b)
# a modelio kurimas ir mokymas
lr_a = LogisticRegression(C = 0.01, max_iter = 10000)
lr_a.fit(X_train_val,y_train_val)

# Modelio prognoze
y_pred_a = lr_a.predict(X_test)

print(classification_report(y_test, y_pred_a))
# Teigiama prognostine verte, jautrumas ir f1 matai sumazeja, kai pakeiciama i kategorija 1 (sudare sutarti)

##################
# b modelio kurimas ir mokymas
lr_b = LogisticRegression(C = 100, max_iter = 10000)
lr_b.fit(X_train_val,y_train_val)

# Modelio prognoze
y_pred_b = lr_b.predict(X_test)

print(classification_report(y_test, y_pred_b))
# Teigiama prognostine verte, jautrumas ir f1 matai sumazeja, kai pakeiciama i kategorija 1 (sudare sutarti)

# c)
# Renkantis pagal f1 mata rinkciausi b modeli (vidutiniai f1 geresni)
a_prob = lr_a.predict_proba(X_test)[:,1]
b_prob = lr_b.predict_proba(X_test)[:,1]

roc_auc_score(y_test, a_prob)
roc_auc_score(y_test, b_prob)
# Pagal AUC ROC irgi rinkciausi b klasifikatoriu


#####################
# 3.
#####################
# a)
y_train_val_n = 1 - y_train_val

X_train_n, X_val_n, y_train_n, y_val_n = train_test_split(X_train_val, y_train_val_n, test_size=0.2, random_state=0, stratify=y_train_val_n)


for C in param:
    # Modelio kurimas ir mokymas
    h_lr = LogisticRegression(C = C, max_iter = 10000)
    h_lr.fit(X_train_n,y_train_n)

    # Modelio prognoze
    y_pred_n = h_lr.predict(X_val_n)
    
    # Klasifikavimo matrica
    cm = confusion_matrix(y_val_n, y_pred_n)
    TN = cm[0][0]
    FN = cm[1][0]
    FP = cm[0][1]
    TP = cm[1][1]
    n = np.sum(cm)

    # a)
    TNR = TN / (TN + FP)

    # b)
    #recall = recall_score(y_true=y_val,y_pred=y_pred)
    recall = TP / (TP + FN)
    GM = (recall * TNR)**0.5

    print("Kai C = {}, TNR = {}, GM = {};".format(C, TNR, GM))

# Panasu, kad sutampa