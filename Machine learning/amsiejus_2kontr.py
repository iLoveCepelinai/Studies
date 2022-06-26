# 2 praktikos kontrolinis
# Matas Amšiejus
# DM2
# 2022 05 17

# paketai, klasės
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import MinMaxScaler
import numpy as np
from sklearn.model_selection import GridSearchCV
from sklearn.svm import SVC
from sklearn.neighbors import KNeighborsClassifier
from sklearn.linear_model import LogisticRegression


# 0.1. duomenys
path = "D:/Users/User/Desktop/Studijos/trecias kursas/Machine learning/Kontroliniai prakt/kontr2/smoking.csv"
df = pd.read_csv(path)
df = df.replace({'gender':{'F':1,'M':0},'oral':{'Y':1,'N':0},'tartar':{'Y':1,'N':0}})
df.groupby("smoking").count()
df = df.groupby('smoking', group_keys=False).apply(pd.DataFrame.sample, frac=.05)
X, y = df.loc[:,df.columns!='smoking'].values, df['smoking'].values


# 0.2. duomenų skaidymas ir transformavimas
X_train, X_test, y_train, y_test = train_test_split(X,y, train_size=0.75, random_state=0)
scaler = MinMaxScaler().fit(X_train)
X_train_scaled = scaler.transform(X_train)
X_test_scaled = scaler.transform(X_test)

#--------------------------------------------------------------

# 1. Gardeles konstravimas

# Logistinio klasifikatoriaus gardele
Intercept = list((True, False)) # jei True, nebus 0, jei False - bus
ro = list(np.arange(0.1,1.1,0.1))
log_grid = {'C':[1], 'fit_intercept':Intercept, 'l1_ratio':ro, 'penalty': ['elasticnet']}

# Knn klasifikatoriaus gardele
N = list(np.arange(1,11))
knn_grid = {'n_neighbors':N}

# AVK klasifikatoriaus gardele
gamma = np.logspace(-3,3,num=7)
avk_grid = {'kernel':['rbf'], 'gamma':gamma}

#param_grid = [log_class,knn_class,avk_class]

#--------------------------------------------------------------
# Geriausiu parametru atranka logistinio klasifikatoriaus
grid_search_log = GridSearchCV(estimator=LogisticRegression(solver='saga'), param_grid=log_grid, cv=5)
grid_search_log.fit(X_train_scaled,y_train)

print("Geriausi parametrai: {}".format(grid_search_log.best_params_))
print("Geriausios logistines regresijos h rizika testinėje aibėje: {}".format(1-grid_search_log.score(X_test_scaled, y_test)))

# Geriausiu parametru atranka knn klasifikatoriaus
grid_search_knn = GridSearchCV(estimator=KNeighborsClassifier(), param_grid=knn_grid, cv=5)
grid_search_knn.fit(X_train_scaled,y_train)

print("Geriausi parametrai: {}".format(grid_search_knn.best_params_))
print("Geriausios knn h rizika testinėje aibėje: {}".format(1-grid_search_knn.score(X_test_scaled, y_test)))

# Geriausiu parametru atranka avk klasifikatoriaus
grid_search_avk = GridSearchCV(estimator=SVC(), param_grid=avk_grid, cv=5)
grid_search_avk.fit(X_train_scaled,y_train)

print("Geriausi parametrai: {}".format(grid_search_avk.best_params_))
print("Geriausios avk h rizika testinėje aibėje: {}".format(1-grid_search_avk.score(X_test_scaled, y_test)))

#--------------------------------------------------------------
# a)
# Taip, naudoja l1 reguliarizacija, nes parinktas l1_ratio parametras yra = 1, kas reiskia,
# kad reguliarizacijai bus naudojama tik l1 (jei butu 0, butu tik l2, o tame tarpe - kombinacija dvieju)

# b)
results = pd.DataFrame(grid_search_knn.cv_results_)
df=results[['mean_score_time']]
print("Vidutiniskas 10 kaimynu score laikas: {}".format(df.iloc[9]))
# 0.027598 sekundes



#--------------------------------------------------------------
# 2)
C_did = 10**20
log_grid2 = {'C':[1, C_did], 'fit_intercept':['False']}
grid_search_log2 = GridSearchCV(estimator=LogisticRegression(max_iter=10**6), param_grid=log_grid2, cv=5)
grid_search_log2.fit(X_train_scaled,y_train)



#--------------------------------------------------------------
# 3)
# funkcija K(x,x') skaičiavimui
def ekernel(xi, xj):
    xi = np.array(xi)
    xj = np.array(xj)
    xi_sq = np.power(xi,2)
    xj_sq = np.power(xj,2)
    y_norm = np.sqrt(xi_sq.sum())
    x_norm = np.sqrt(xj_sq.sum())
    xy = xi*xj
    xy = 2*xy.sum()
    k_val = 1/(1+(x_norm + xy + y_norm))   
    return np.array([k_val])

def kmatrix(X1,X2):
    m = len(X1)
    n = len(X2)
    kmat = np.ndarray(shape=(m,n))
    for i in range(m):
        for j in range(n):
            kmat[i,j]=ekernel(X1[i], X2[j])
    return kmat

# apskaičiuojame matricą K(xi,xj) modelio apmokymui
kmat_train = kmatrix(X1=X_train_scaled,X2=X_train_scaled)


k_clf = SVC(C=0.9, kernel='precomputed')

