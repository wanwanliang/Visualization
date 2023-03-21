# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
import dataiku
import pandas as pd, numpy as np
from dataiku import pandasutils as pdu
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from numpy import array
from sklearn.preprocessing import MinMaxScaler
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_squared_error
from minisom import MiniSom
import matplotlib.pyplot as plt
from sklearn.cluster import KMeans
from numpy import array
from numpy import hstack
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import Flatten
from keras.layers.convolutional import Conv1D
from keras.layers.convolutional import MaxPooling1D
from keras.layers.convolutional import AveragePooling1D
from keras.layers import BatchNormalization
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from keras.utils.vis_utils import plot_model
from keras.models import Model
from sklearn.manifold import TSNE
from sklearn.model_selection import train_test_split
import tensorflow as tf
from keras.layers import Dense, merge
from keras.layers import add
from keras.layers.core import Activation
import sklearn
from sklearn.metrics import silhouette_score
from tensorflow import keras
from keras.layers import Add
from scipy.stats import pearsonr
from sklearn.cluster import AgglomerativeClustering
import optuna
from sklearn.metrics import r2_score

# Read recipe inputs
NA_corn_env_prepare = dataiku.Dataset("NA_corn_env_Tr_Val_inte_fac")
dt = NA_corn_env_prepare.get_dataframe()
#dt = dt.sample(frac=1, random_state=369)
print(dt.shape)
dt.index = range(dt.shape[0])
years= list(dt['year'].unique())
print(years)
nms = ['ETRPlanting-VE',
 'ETRVE-V6',
 'ETRV6-VT',
 'ETRVT-R2',
 'ETRR2-R4',
 'ETRR4-R6',
 'ETRR6-Harvest','WaterDeficitPlanting-VE', 'WaterDeficitVE-V6', 'WaterDeficitV6-VT', 'WaterDeficitVT-R2', 'WaterDeficitR2-R4','WaterDeficitR4-R6','WaterDeficitR6-Harvest',
            'VaporPressureDeficitPlanting-VE', 'VaporPressureDeficitVE-V6', 'VaporPressureDeficitV6-VT', 'VaporPressureDeficitVT-R2', 'VaporPressureDeficitR2-R4',
             'VaporPressureDeficitR4-R6','VaporPressureDeficitR6-Harvest',
 'precsumPlanting-VE',  'precsumVE-V6', 'precsumV6-VT', 'precsumVT-R2', 'precsumR2-R4',
       'precsumR4-R6', 'precsumR6-Harvest',
 'rhavgPlanting-VE', 'rhavgVE-V6', 'rhavgV6-VT', 'rhavgVT-R2', 'rhavgR2-R4','rhavgR4-R6','rhavgR6-Harvest', 'rhminPlanting-VE','rhminVE-V6', 'rhminV6-VT',
       'rhminVT-R2', 'rhminR2-R4', 'rhminR4-R6', 'rhminR6-Harvest','HeatPlanting-VE','HeatVE-V6', 'HeatV6-VT', 'HeatVT-R2', 'HeatR2-R4', 'HeatR4-R6','HeatR6-Harvest','wsy_c']


dt_x = dt[nms]
dt_x = pd.get_dummies(dt_x)
nms2 = list(dt_x.columns)
dt_x = pd.DataFrame(StandardScaler().fit_transform(dt_x))
dt_x.columns = nms2
dt_y = dt['ygsmn']

from tensorflow import keras
from tensorflow.keras.optimizers import Adam
from tensorflow.keras.optimizers import SGD
from tensorflow.keras.optimizers import RMSprop
import math

def mlp_clustering3(dt_x, dt_y, dta, lr, n_layer, optimizer, n_neuron1, n_neuron2, batch_size, epochs, n_cluster=5):
    x_train, x_val, y_train, y_val = train_test_split(dt_x, dt_y, test_size=0.25)

    model = Sequential()
    model.add(Dense(n_neuron1, activation='relu', input_dim = dt_x.shape[1]))

    if n_layer>0:
        for n in range(n_layer):
            model.add(Dense(n_neuron1, activation='relu'))

    model.add(Dense(n_neuron2, activation='relu', name='layer3'))
    model.add(Dense(1, name='final'))
    model.compile(optimizer= eval(optimizer)(learning_rate=lr), loss='mse')

    my_callbacks = [
        tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=10)
    ]

    model.fit(x_train, y_train, validation_data=(x_val, y_val),epochs=epochs, batch_size=batch_size, verbose=0)

    fea_pred = model.predict(x_val)
    fea_pred = fea_pred.ravel()
    r2_ts= pearsonr(y_val.ravel(), fea_pred)[0]

    new_model = Model(inputs=model.input,
                  outputs=model.get_layer("layer3").output)
    output = new_model.predict(dt_x)

    clu = AgglomerativeClustering(n_clusters=n_cluster).fit(output).labels_
    dta['cluster'] = clu
    ts = dta.groupby(['cluster'])['inte_fac'].mean()
    ts = ts.reset_index().sort_values(['inte_fac'],ascending=True)
    ts['DH_MLP'] = range(ts.shape[0])
    ts['DH_MLP'] = ts['DH_MLP'] + 1
    dta2 = pd.merge(dta, ts[['cluster','DH_MLP']], on=['cluster'])

    id_stress = dta2[['trial_id','DH_MLP']]

    return id_stress, r2_ts

def fit_by_year(dt, lr, n_layer, optimizer, n_neuron1, n_neuron2, batch_size, epochs, nms=nms, year=2020):

    dt_x = dt[nms]
    dt_x = pd.get_dummies(dt_x)
    nms2 = list(dt_x.columns)
    dt_x = pd.DataFrame(StandardScaler().fit_transform(dt_x))
    dt_x.columns = nms2
    dt_y = dt['ygsmn']

    x_train, x_test = dt_x.loc[~(dt['year']==year),], dt_x.loc[(dt['year']==year),]
    y_train, y_test = dt_y.loc[~(dt['year']==year),], dt_y.loc[(dt['year']==year),]
    dt_sb = dt.loc[~(dt['year']==year),]
    dt_sb.index = x_train.index = y_train.index = range(dt_sb.shape[0])
    dt_test = dt.loc[(dt['year']==year),]
    dt_test.index = range(dt_test.shape[0])

    stress_df = dt_sb[['trial_id']]
    ts_df = []
    for i in range(6):
        re_df, r2 = mlp_clustering3(x_train, y_train, dt_sb,lr, n_layer, optimizer, n_neuron1, n_neuron2, batch_size, epochs, n_cluster=5)
        nm = 'MLP_DH_iter' + str(i+1)
        re_df.columns = ['trial_id',nm]

        stress_df = pd.merge(stress_df, re_df, on=['trial_id'])
        ts_df.append(r2)
        print(i)
    ts_df = pd.DataFrame(ts_df)
    stress_df2 = stress_df.copy()

    stress_df2['mlp'] = stress_df2.drop(['trial_id'],axis=1).mean(axis=1)
    stress_df2['mlp'] = stress_df2['mlp'].round(0)

    dt_str = pd.merge(stress_df2[['mlp','trial_id']], dt_sb, on=['trial_id'])
    x_var, y_var = dt_str[nms], dt_str[['mlp']]

    from sklearn.ensemble import RandomForestClassifier
    dp = 6
    ms = 1
    rf = RandomForestClassifier(max_depth=dp, n_estimators=100,min_samples_leaf=ms).fit(pd.get_dummies(x_var),y_var)
    dt_test['mlp'] = rf.predict(pd.get_dummies(dt_test[nms]))


    return dt_str[['trial_id','mlp','ygsmn','inte_fac']],  dt_test[['trial_id','mlp','ygsmn','inte_fac']]

def objective(trial=20):

    params = {
              'learning_rate': trial.suggest_loguniform('learning_rate', 0.0005, 0.008),
              'n_layer': trial.suggest_int('n_layer', 0,1),
              'optimizer': trial.suggest_categorical("optimizer", ["Adam", "RMSprop"]),
              'n_neuron1': trial.suggest_int("n_neuron1", 30, 50, step=10),
              'n_neuron2': trial.suggest_int("n_neuron2", 4, 10, step=2),
              'batch_size': trial.suggest_int("batch_size", 32,96, step = 32),
              'epochs' : trial.suggest_int("epochs",100, 200, step = 100)
              }
    lr = params['learning_rate']
    n_layer = params['n_layer']
    optimizer = params['optimizer']
    n_neuron1 = params['n_neuron1']
    n_neuron2 = params['n_neuron2']
    batch_size = params['batch_size']
    epochs = params['epochs']


    accuracy = build_model(lr, n_layer, optimizer, n_neuron1, n_neuron2, batch_size, epochs,years, dt_x, dt_y)
    return accuracy

def build_model(lr, n_layer, optimizer, n_neuron1, n_neuron2, batch_size, epochs,years, dt_x, dt_y):

    re_df = pd.DataFrame()
    for year in years:
        print(year)
        tr_dt, ts_dt = fit_by_year(dt, lr, n_layer, optimizer, n_neuron1, n_neuron2, batch_size, epochs, year= year, nms=nms)
        tr_cor = tr_dt[['mlp','ygsmn','inte_fac']].corr()['mlp'][1]
        ts_cor = ts_dt[['mlp','ygsmn','inte_fac']].corr()['mlp'][1]

        df1 = pd.DataFrame([year, tr_cor, ts_cor])
        re_df = pd.concat([re_df, df1.T], axis=0)
    re_df.columns=['year','tr_accuracy','ts_accuracy']
    val_score1 = re_df['ts_accuracy'].mean()*(-1)
    val_score2 = re_df['ts_accuracy'].median()*(-1)
    val_score = (val_score1 + val_score2)/2
    if math.isnan(val_score):
        val_score = 0
    return val_score

# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
import multiprocessing
ncpu = multiprocessing.cpu_count() - 2
ncpu = 6

para_opt = optuna.create_study(direction="maximize", sampler=optuna.samplers.TPESampler())
para_opt.optimize(objective, n_trials=20, n_jobs=ncpu)

output = para_opt.trials_dataframe()
output

# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
NA_corn_DH_stress_MLP_para_opt_df = output


# Write recipe outputs
NA_corn_DH_stress_MLP_para_opt = dataiku.Dataset("NA_corn_DH_stress_MLP_para_opt")
NA_corn_DH_stress_MLP_para_opt.write_with_schema(NA_corn_DH_stress_MLP_para_opt_df)
