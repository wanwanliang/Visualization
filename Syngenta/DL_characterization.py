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
NA_corn_env_DH_var_only = dataiku.Dataset("NA_corn_env_Tr_Val_inte_fac")
dt = NA_corn_env_DH_var_only.get_dataframe()
dt.index = range(dt.shape[0])
test_dt = dataiku.Dataset('NA_corn_test')
test_dt = test_dt.get_dataframe()

nms1 = ['tempmaxPlanting-VE_Stress_Heat',
 'tempmaxVE-V6_Stress_Heat',
 'tempmaxV6-VT_Stress_Heat',
 'tempmaxVT-R2_Stress_Heat',
 'tempmaxR2-R4_Stress_Heat',
 'tempmaxR4-R6_Stress_Heat',
 'tempmaxR6-Harvest_Stress_Heat',
 'tempavgPlanting-VE_Stress_Heat',
 'tempavgVE-V6_Stress_Heat',
 'tempavgV6-VT_Stress_Heat',
 'tempavgVT-R2_Stress_Heat',
 'tempavgR2-R4_Stress_Heat',
 'tempavgR4-R6_Stress_Heat',
 'tempavgR6-Harvest_Stress_Heat',
        'ETRPlanting-VE', 'ETRVE-V6', 'ETRV6-VT', 'ETRVT-R2','ETRR2-R4','ETRR4-R6',
 'ETRR6-Harvest','WaterDeficitPlanting-VE', 'WaterDeficitVE-V6', 'WaterDeficitV6-VT', 'WaterDeficitVT-R2', 'WaterDeficitR2-R4','WaterDeficitR4-R6','WaterDeficitR6-Harvest',
            'VaporPressureDeficitPlanting-VE', 'VaporPressureDeficitVE-V6', 'VaporPressureDeficitV6-VT', 'VaporPressureDeficitVT-R2', 'VaporPressureDeficitR2-R4',
             'VaporPressureDeficitR4-R6','VaporPressureDeficitR6-Harvest',
 'precsumPlanting-VE',  'precsumVE-V6', 'precsumV6-VT', 'precsumVT-R2', 'precsumR2-R4',
       'precsumR4-R6', 'precsumR6-Harvest',
 'rhavgPlanting-VE', 'rhavgVE-V6', 'rhavgV6-VT', 'rhavgVT-R2', 'rhavgR2-R4','rhavgR4-R6','rhavgR6-Harvest', 'rhminPlanting-VE','rhminVE-V6', 'rhminV6-VT',
       'rhminVT-R2', 'rhminR2-R4', 'rhminR4-R6', 'rhminR6-Harvest','HeatPlanting-VE','HeatVE-V6', 'HeatV6-VT', 'HeatVT-R2', 'HeatR2-R4', 'HeatR4-R6','HeatR6-Harvest','wsy_c']

dt_x = dt[nms1]
dt_x = pd.get_dummies(dt_x)
nms2 = list(dt_x.columns)
dt_x = pd.DataFrame(StandardScaler().fit_transform(dt_x))
dt_x.columns = nms2
dt_y = dt['ygsmn']


def mlp_clustering3(dt_x, dt_y, dta, n_cluster=5):
    x_train, x_test, y_train, y_test = train_test_split(dt_x, dt_y, test_size=0.05)

    model0 = Sequential()

    model0.add(Dense(40, activation='relu', name='layer1', input_dim = x_train.shape[1]))
    #model0.add(Dense(40, activation='relu', name='layer2'))
    model0.add(Dense(8, activation='relu', name='layer3'))
    model0.add(Dense(1, name='final'))

    model0.compile(optimizer= tf.keras.optimizers.RMSprop(lr=0.0006), loss='mse')

    my_callbacks = [
        tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=10),
    ]

    model0.fit(x_train, y_train, validation_data=(x_test, y_test),epochs=100, batch_size=128, verbose=0)
    fea_pred = model0.predict(x_test)
    fea_pred = fea_pred.ravel()
    r2_ts= pearsonr(y_test.ravel(), fea_pred)[0]

    new_model = Model(inputs=model0.input,
                  outputs=model0.get_layer("layer3").output)
    output = new_model.predict(dt_x)

    clu = AgglomerativeClustering(n_clusters=n_cluster).fit(output).labels_
    dta['cluster'] = clu
    ts = dta.groupby(['cluster'])['inte_fac'].mean()
    ts = ts.reset_index().sort_values(['inte_fac'],ascending=True)
    ts['DH_MLP'] = range(ts.shape[0])
    ts['DH_MLP'] = ts['DH_MLP'] + 1
    dta2 = pd.merge(dta, ts[['cluster','DH_MLP']], on=['cluster'])

    ts = dta.groupby(['cluster'])['ygsmn'].mean()
    ts = ts.reset_index().sort_values(['ygsmn'],ascending=False)
    ts['DH_MLP2'] = range(ts.shape[0])
    ts['DH_MLP2'] = ts['DH_MLP2'] + 1
    dta3 = pd.merge(dta2, ts[['cluster','DH_MLP2']], on=['cluster'])
    id_stress = dta3[['trial_id','DH_MLP','DH_MLP2']]
    id_stress = id_stress.sort_values(['trial_id'])

    return id_stress, r2_ts

# -------------------------------------------------------------------------------- NOTEBOOK-CELL: MARKDOWN
# ### use code below

# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
stress_df = dt[['trial_id']]
ts_df = []
for i in range(20):
    re_df, r2 = mlp_clustering3(dt_x, dt_y, dt, n_cluster=10)
    nm = 'MLP_DH_iter' + str(i+1)
    nm2 = 'MLP_DH2_iter' + str(i+1)
    re_df.columns = ['trial_id',nm, nm2]

    stress_df = pd.merge(stress_df, re_df, on=['trial_id'])
    ts_df.append(r2)
    print(i)
ts_df = pd.DataFrame(ts_df)
q1 = ts_df[0].quantile(0.1)


ts_df = pd.DataFrame(ts_df)
iters= list(ts_df.index[ts_df[0]<q1].to_numpy() + 1)
print(iters)
#stress_df2 = stress_df.drop(stress_df.columns[iters], axis=1)
stress_df2 = stress_df.copy()
print(stress_df2.shape)
stress_df2.head(2)


nms = list(stress_df2.columns)
nma = []
nmb = []
for nm in nms:
    if 'MLP_DH2' in nm:
        nmb.append(nm)
    elif 'trial_id' in nm:
        pass
    else:
        nma.append(nm)
print(nma)
print(nmb)


stress_df2['mode'] = stress_df2[nma].mode(axis=1)[0]
stress_df2['mean'] = stress_df2[nma].mean(axis=1)
stress_df2['mode'] = stress_df2['mode'].round(0)
stress_df2['mean'] = stress_df2['mean'].round(0)
stress_df2['mode2'] = stress_df2[nmb].mode(axis=1)[0]
stress_df2['mean2'] = stress_df2[nmb].mean(axis=1)
stress_df2['mode2'] = stress_df2['mode2'].round(0)
stress_df2['mean2'] = stress_df2['mean2'].round(0)
stress_df2['mean3'] = stress_df2.drop(['trial_id'],axis=1).mean(axis=1)
stress_df2['mean3'] = stress_df2['mean3'].round(0)

stress_df3 = pd.merge(stress_df2, dt[['trial_id','inte_fac','ygsmn']], on=['trial_id'])
print(stress_df3.groupby(['mean2'])['ygsmn','inte_fac'].describe().round(1))
print(stress_df3.groupby(['mean'])['ygsmn','inte_fac'].describe().round(1))

# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
dta = pd.merge(stress_df3.drop(['ygsmn','inte_fac'], axis=1), dt, on=['trial_id'])
dta[['ygsmn','inte_fac','mean','mean2','mean3']].corr().round(2)


# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
NA_corn_DH_MLP_output_df = dta


# Write recipe outputs
NA_corn_DH_MLP_output = dataiku.Dataset("NA_corn_DH_MLP_output")
NA_corn_DH_MLP_output.write_with_schema(NA_corn_DH_MLP_output_df)
