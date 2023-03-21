# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
import dataiku
import pandas as pd, numpy as np
from dataiku import pandasutils as pdu
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
from sklearn.metrics import mean_squared_error
from keras.utils.vis_utils import plot_model
from keras.models import Model
from sklearn.manifold import TSNE
from keras.layers import concatenate
from tensorflow.keras.optimizers import SGD
from tensorflow.keras.optimizers import Adam
from keras import metrics
import keras
import tensorflow as tf
from keras.layers import RepeatVector
from keras.layers import TimeDistributed
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import MinMaxScaler, StandardScaler
from keras.models import Model
from sklearn.cluster import AgglomerativeClustering
import optuna


# Read recipe inputs
NA_corn_env_DH_var_only = dataiku.Dataset("NA_corn_env_Tr_Val_inte_fac")
dt = NA_corn_env_DH_var_only.get_dataframe()

dt_seq = dt.drop(['wsy_c','ygsmn','trial_id','inte_fac','year'],  axis=1)
nms = list(dt_seq.columns)
dt_seq = pd.DataFrame(MinMaxScaler().fit_transform(dt_seq))
dt_seq.columns = nms
dt_seq.shape

dt_seq.columns

# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
periods = ['Planting-VE', 'VE-V6', 'V6-VT', 'VT-R2', 'R2-R4','R4-R6','R6-Harvest']

dt_nseq = pd.get_dummies(dt[['wsy_c']])
irrs= ["wsy_c_IRR_" + period for period in periods]
#lirrs = ["wsy_c_LIRR_" + period for period in periods]
rain = ["wsy_c_RAIN_" + period for period in periods]

for i in range(len(periods)):
    dt_nseq[irrs[i]] = dt_nseq[['wsy_c_IRR']]
# for i in range(len(periods)):
#     dt_nseq[lirrs[i]] = dt_nseq[['wsy_c_LIRR']]
for i in range(len(periods)):
    dt_nseq[rain[i]] = dt_nseq[['wsy_c_RAIN']]

dt_nseq.head(2)
dt_nseq = dt_nseq.drop(['wsy_c_IRR','wsy_c_RAIN'], axis=1)
dt_nseq.head(2)

# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
dt_seq = pd.concat([dt_seq,dt_nseq],axis=1)
dt_seq.shape

# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
np.set_printoptions(formatter={'float': '{: 0.3f}'.format})
dt3d = np.empty((dt_seq.shape[0],7,9))

for i in range(dt_seq.shape[0]):
    dt_ind = np.array(dt_seq.iloc[i,])
    dt_ind2 = dt_ind.reshape(9,7)
    dt_ind3 = dt_ind2.T
    dt3d[i,:,:] = dt_ind3

print(dt3d.shape)
dt_x = dt3d.copy()
dt_y = dt['ygsmn']

# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
def objective(trial=20):

    params = {
              'learning_rate': trial.suggest_loguniform('learning_rate', 1e-4, 0.05),
              'n_layer': trial.suggest_int('n_layer', 0,1, step= 1),
              'optimizer': trial.suggest_categorical("optimizer", ["Adam", "RMSprop"]),
              'n_neuron1': trial.suggest_int("n_neuron1", 30, 50, step=5),
              'n_neuron2': trial.suggest_int("n_neuron2", 5, 15, step=5),
              'batch_size': trial.suggest_int("batch_size", 16, 96, step = 16),
              'epochs' : trial.suggest_int("epochs",100, 500, step = 100)
              }
    accuracy = build_model(params, dt_x, dt_y)
    return accuracy


def build_model(params, dt_x, dt_y):

    val_r2= []
    for i in range(5):

        x_train, x_val, y_train, y_val = train_test_split(dt_x, dt_y, test_size=0.3, random_state=6658*i)
        model = Sequential()
        model.add(LSTM(params['n_neuron1'], return_sequences = True, input_shape=(x_train.shape[1], x_train.shape[2]), name='layer1'))

        if params['n_layer']>0:
            for n in range(params['n_layer']):
                model.add(LSTM(params['n_neuron1'], return_sequences = True))

        model.add(Flatten(name='layer5'))
        model.add(Dense(params['n_neuron2'], name='layer7'))

        model.add(Dense(1, name='final'))
        model.compile(optimizer= eval(params['optimizer'])(learning_rate=params['learning_rate']), loss='mse')
        model.fit(x_train, y_train, epochs=params['epochs'], batch_size=params['batch_size'], verbose=0)
        try:
            val_r2.append(r2_score(y_val.ravel(), model.predict(x_val).ravel()))
        except:
            val_r2.append(-1)
    val_r2 = pd.DataFrame(val_r2)
    val_score = val_r2[0].mean().round(2)
    return val_score

# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
import multiprocessing
import optuna
from sklearn.metrics import r2_score

from tensorflow import keras
from tensorflow.keras.optimizers import Adam
from tensorflow.keras.optimizers import SGD
from tensorflow.keras.optimizers import RMSprop
import math

# ncpu = multiprocessing.cpu_count() - 2
# ncpu = 2
# para_opt = optuna.create_study(direction="maximize", sampler=optuna.samplers.TPESampler())
# para_opt.optimize(objective, n_trials=100, n_jobs=ncpu)
# para_opt.best_params
# output = para_opt.trials_dataframe()

# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
# import multiprocessing
# import optuna
# from sklearn.metrics import r2_score


# ncpu = multiprocessing.cpu_count() - 2
# ncpu = 6
# para_opt = optuna.create_study(direction="maximize", sampler=optuna.samplers.TPESampler())
# para_opt.optimize(objective, n_trials=30, n_jobs=ncpu)
# para_opt.best_params
# output = para_opt.trials_dataframe()

# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
def rnn_clustering(dt_x, dt_y, dta, n_cluster=5):
    from scipy.stats import pearsonr
    x_train, x_val, y_train, y_val = train_test_split(dt_x, dt['ygsmn'], test_size=0.25, random_state=6658*i)
    model2 = Sequential()
    model2.add(LSTM(50, return_sequences = True, input_shape=(x_train.shape[1], x_train.shape[2]), name='layer1'))
    model2.add(LSTM(units = 6, return_sequences = False, name='layer7'))
    #model2.add(Flatten(name='layer5'))
    #model2.add(Dense(5, name='layer7'))
    model2.add(Dense(1, name='layer8'))
    model2.compile(optimizer= tf.keras.optimizers.Adam(lr=0.003), loss='mse')

    my_callbacks = [
        #tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=100),
        tf.keras.callbacks.ModelCheckpoint(filepath='./models/CNN_1D/model_best.h5', save_best_only=True, monitor='val_accuracy'),
        tf.keras.callbacks.ReduceLROnPlateau(monitor='val_loss', factor=0.2, patience=10, verbose=1, epsilon=1e-4, min_lr = 0.000001, mode='auto')
    ]

    history = model2.fit(x_train, y_train, epochs=200, batch_size=32, verbose=0, shuffle=False)
    fea_pred = model2.predict(x_val)
    fea_pred = fea_pred.ravel()
    r2_ts= pearsonr(y_val.ravel(), fea_pred)[0]


    new_model = Model(inputs=model2.input,
    outputs=model2.get_layer("layer7").output)

    output = new_model.predict(dt_x)

    clu = AgglomerativeClustering(n_clusters=n_cluster).fit(output).labels_
    dta['cluster'] = clu
    ts = dta.groupby(['cluster'])['inte_fac'].mean()
    ts = ts.reset_index().sort_values(['inte_fac'],ascending=True)
    ts['DH_RNN'] = range(ts.shape[0])
    ts['DH_RNN'] = ts['DH_RNN'] + 1
    dta2 = pd.merge(dta, ts[['cluster','DH_RNN']], on=['cluster'])

    ts = dta.groupby(['cluster'])['ygsmn'].mean()
    ts = ts.reset_index().sort_values(['ygsmn'],ascending=False)
    ts['DH_RNN2'] = range(ts.shape[0])
    ts['DH_RNN2'] = ts['DH_RNN2'] + 1
    dta3 = pd.merge(dta2, ts[['cluster','DH_RNN2']], on=['cluster'])
    id_stress = dta3[['trial_id','DH_RNN','DH_RNN2']]
    id_stress = id_stress.sort_values(['trial_id'])

    return id_stress, r2_ts

# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
stress_df = dt[['trial_id']]
ts_df = []
for i in range(20):
    re_df, r2 = rnn_clustering(dt_x, dt_y, dt, n_cluster=5)
    nm = 'RNN_DH_iter' + str(i+1)
    nm2 = 'RNN_DH2_iter' + str(i+1)
    re_df.columns = ['trial_id',nm, nm2]

    stress_df = pd.merge(stress_df, re_df, on=['trial_id'])
    ts_df.append(r2)
    print(i)
ts_df = pd.DataFrame(ts_df)
q1 = ts_df[0].quantile(0.1)


ts_df = pd.DataFrame(ts_df)
iters= list(ts_df.index[ts_df[0]<q1].to_numpy() + 1)
print(iters)
stress_df2 = stress_df.drop(stress_df.columns[iters], axis=1)
print(stress_df2.shape)
stress_df2.head(2)

nms = list(stress_df2.columns)
nma = []
nmb = []
for nm in nms:
    if 'RNN_DH2' in nm:
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

dta = pd.merge(stress_df3.drop(['ygsmn','inte_fac'], axis=1), dt, on=['trial_id'])
dta[['ygsmn','inte_fac','mean','mean2','mean3']].corr().round(2)

NA_corn_DH_rnn_output_df = dta


# Write recipe outputs
NA_corn_DH_rnn_output = dataiku.Dataset("NA_corn_DH_rnn_output")
NA_corn_DH_rnn_output.write_with_schema(NA_corn_DH_rnn_output_df)
