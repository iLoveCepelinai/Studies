# 1 praktikos kontrolinis
# Matas Amšiejus
# DM2
# 2022 05 03

# paketai, klasės
import pandas as pd
import numpy as np
import math
from sklearn.neighbors import KNeighborsClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.svm import LinearSVC
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import train_test_split
from sklearn.model_selection import LeaveOneOut


# Duomenu nuskaitymas
path = "D:/Users/User/Desktop/Studijos/trecias kursas/Machine learning/Kontroliniai prakt/amsiejus_dm2/Date_Fruit_Datasets.xlsx"
df = pd.read_excel(io=path,sheet_name="Date_Fruit_Datasets")

df.Class # klases

# a) logistines regresijos modelis

# Sukuriame y matrica
y = np.array(0 + (df.iloc[:,34]=="BERHI") + (df.iloc[:,34]=="DEGLET") + (df.iloc[:,34]=="DOKOL") + (df.iloc[:,34]=="IRAQI"))

# sukuriame X matricą
X = np.array(df.iloc[:,:34])

param = (0.05, 1, 20)

for C in param:
    h_lr = LogisticRegression(C = C)
    scores_lr = cross_val_score(h_lr, X, y, cv=10)
    print("LR modelio su parametru C = {} rizika: {}".format(C, 1-scores_lr.mean()))


    h_lr.fit(X,y)
    print("Logistinės regresijos hiperpl. koeficientas prie MAJOR_AXIS kintamojo: {}\n".format(h_lr.coef_[0][2]))

# Kadangi visu hipoteziu rizikos buvo vienodos ar labai panasios, spausdinau visu koeficientus prie MAJOR_AXIS
# nario is hiperplokstumu. Visi koeficientai irgi vienodi.





# b) SVM modelis

# naujas y
y_2 = np.array(0 +(df.iloc[:,34]=="DOKOL") + (df.iloc[:,34]=="IRAQI") + (df.iloc[:,34]=="ROTANA"))

# nauji parametrai SVM modeliui
param_2 = (math.log(2,10), math.log(10,10), math.log(50,10)) #aisku antras yra =1, bet paliksiu kaip salygoje

# pries tai reikia padalinti aibe i mokymo ir testavimo duomenu aibes:
X_train, X_test, y_train, y_test = train_test_split(X, y_2, random_state=419, test_size=0.1)

# patikriname, ar tirkai 10 % testavimui
y_test.size / y.size


for C in param_2:
    h_svm = LinearSVC(C=C)
    h_svm.fit(X_train, y_train) # mokymas

    print("SVM modelio su parametru C = {} rizika: {}".format(C, 1-h_svm.score(X_test, y_test)))

# Matome, kad maziausia rizika gaunasi imant C=log(2,10) arba log(50,10).

# Nustatykime klase konkreciai datulei (10 eilute) su pirmu variantu
h_svm = LinearSVC(C=math.log(2,10))
h_svm.fit(X_train, y_train) # mokymas

x_datule = X[9]
y_prediction = h_svm.predict([x_datule])
y_prediction 
# Panasu, kad si datule nera is klasiu DOKOL, IRAQI, ROTANA



# c) kNN
y_3 = np.array(0 + (df.iloc[:,34]=="BERHI") + (df.iloc[:,34]=="DOKOL") + (df.iloc[:,34]=="ROTANA") + (df.iloc[:,34]=="SOGAY"))

h_knn = KNeighborsClassifier(n_neighbors=2)
#h_knn.fit(X, y_3)

scores_knn = cross_val_score(h_knn, X, y_3, cv=y_3.size)
print(1-scores_lr.mean())

#man meta klaida, kad negali buti didesnis folds skaicius nei skaicius irasu kiekvienoje klaseje, idomu.

scores_knn = cross_val_score(h_knn, X, y_3, cv=LeaveOneOut())
print(1-scores_knn.mean())


# issiskiria nepersidengianciais testavimo foldais, taip apeinant visus duomenis kas viena