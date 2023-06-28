# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
import dataiku
import pandas as pd, numpy as np
from dataiku import pandasutils as pdu
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
from numpy import array
from sklearn.preprocessing import MinMaxScaler
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_squared_error
from minisom import MiniSom
import matplotlib.pyplot as plt
from sklearn.cluster import KMeans
from numpy import array
from numpy import hstack
from sklearn.model_selection import train_test_split
import tensorflow as tf
import sklearn
from sklearn.metrics import silhouette_score
from scipy.stats import pearsonr, spearmanr
from sklearn.cluster import AgglomerativeClustering
import optuna
from sklearn.metrics import r2_score
import optuna
import math
from lightgbm import LGBMRegressor
import lightgbm as lgb
# Read recipe inputs
EAME_Corn_val2020_dta_SelEnv = dataiku.Dataset("EAME_Corn_val2020_dta_SelEnv")
dt = EAME_Corn_val2020_dta_SelEnv.get_dataframe()
dt = dt[dt['group']=='train']
dt = dt.sample(frac=0.99, random_state=8)
dt.index = range(dt.shape[0])
dt.shape

# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
dt.head(2)

# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
dt_y = dt[['YGSMN']]

# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
x_train0, x_valid0, y_train, y_valid = train_test_split(dt, dt_y, test_size=0.5, random_state= 437)
x_train, x_valid = x_train0.drop(['LINCD', 'ENV', 'YGSMN', 'FEMALE', 'MALE', 'year', 'group'],axis=1), x_valid0.drop(['LINCD', 'ENV', 'YGSMN', 'FEMALE', 'MALE', 'year', 'group'],axis=1)
x_train.index = x_train0.index = y_train.index = range(x_train.shape[0])
x_valid.index = x_valid0.index = y_valid.index = range(x_valid.shape[0])

# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
params = {'metric':'rmse',
             'objective':'regression',
              'learning_rate': 0.1,
                'max_depth': 6,
                'num_leaves': 20,
                'feature_fraction': 0.5,
                'subsample_for_bin': 1000,
               'reg_alpha':0.01,
                'boosting_type':'dart',  
              'n_jobs':2
              }

# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
def accuracy_Env(y_pred, y_observ, Env_dt):
    rs= []
    valA = pd.concat([y_pred, y_observ, Env_dt],axis=1)
    valA.columns=['observed','predicted','ENV']
    locs = list(valA['ENV'].unique())
    for loc in locs:
        dtsb = valA[valA['ENV']==loc]
        if dtsb.shape[0]>6:
            r1 = pearsonr(dtsb['observed'], dtsb['predicted'])[0]
            if math.isnan(r1):
                pass
            else:
                rs.append(r1)
    return sum(rs)/len(rs)
        
    
def train_evaluate(x_train, y_train, x_valid, y_valid, x_valid0, params):
    y_train.columns = y_valid.columns = ['y']
    train_data = lgb.Dataset(x_train, label=y_train)
    #valid_data = lgb.Dataset(x_valid, label=y_valid, reference=train_data)

    model = lgb.train(params, train_data)
    y_pred= model.predict(x_valid)
    r_ts= accuracy_Env(pd.DataFrame(y_pred), y_valid, x_valid0[['ENV']])
    
    print('*****####******')
    print('*****####******')
    print('***** testing accuacy ******')
    print(r_ts)
    print('*****####******')
    print('*****####******')
    return r_ts


def objective(trial=20):

    params = {
              'learning_rate': trial.suggest_loguniform('learning_rate', 0.005, 0.3),
              'max_depth': trial.suggest_int("max_depth", 8, 20, step=2),
              'num_leaves': trial.suggest_int("num_leaves", 20, 40, step=4),
              'feature_fraction': trial.suggest_float('feature_fraction', 0.2, 1, step=0.2),
              'subsample_for_bin': trial.suggest_int('subsample_for_bin', 500, 2000 , step=500),
              'reg_alpha': trial.suggest_loguniform('reg_alpha', 0.001, 0.3),
              'boosting_type': trial.suggest_categorical("boosting_type", ["goss", "dart"]),
              'n_jobs':2
              }


    accuracy= train_evaluate(x_train, y_train, x_valid, y_valid,  x_valid0, params)
    return accuracy

# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
para_opt = optuna.create_study(direction="maximize", sampler=optuna.samplers.TPESampler())
para_opt.optimize(objective, n_trials=100, n_jobs=8)
output = para_opt.trials_dataframe()

# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE

# Compute recipe outputs from inputs
# TODO: Replace this part by your actual code that computes the output, as a Pandas dataframe
# NB: DSS also supports other kinds of APIs for reading and writing data. Please see doc.

HyPerPara_Tun1_df = output


# Write recipe outputs
HyPerPara_Tun1 = dataiku.Dataset("HyPerPara_Tun1")
HyPerPara_Tun1.write_with_schema(HyPerPara_Tun1_df)
