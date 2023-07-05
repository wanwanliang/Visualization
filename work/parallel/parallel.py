# -------------------------------------------------------------------------------- NOTEBOOK-CELL: CODE
import dataiku, joblib, pickle
import pandas as pd, numpy as np
from dataiku import pandasutils as pdu
import lightgbm as lgb
import shap
#from lgbm_utils import *
from dataiku import insights
from PIL import Image
from dataiku.core.managed_folder import Folder
import io
import matplotlib.pyplot as plt
from functools import partial
import multiprocessing
from multiprocessing import  Pool
import time

def get_loc(string):
    string2 = string.split(':')[0]
    return string2


def top_shaps(dt, year=2022, n_top=10, state='TN', be_bid='be_bid', out='list'):
    if year=='all':
        dt = dt.drop(['year'],axis=1)
        dtsb = dt.groupby(['state_province_code']).mean()
        dtsb = dtsb.reset_index()
    else:
        dtsb = dt.loc[dt['year']==year]
    if state=='all':
        dtsb = dtsb
    else:
        dtsb = dtsb.loc[dtsb['state_province_code']==state]
    x_var = dtsb.drop(['state_province_code'], axis=1).T
    x_var.columns=['shap']
    x_var = x_var.sort_values(['shap'])
    if out=='df':
        btms= pd.DataFrame(list(x_var.iloc[:n_top,].index),columns=[state])
    elif out=='list':
        btms= list(x_var.iloc[:n_top,].index)
    x_var = x_var.sort_values(['shap'], ascending=False)

    if out=='df':
        tops = pd.DataFrame(list(x_var.iloc[:n_top,].index),columns=[state])
    elif out=='list':
        tops= list(x_var.iloc[:n_top,].index)

    return tops, btms

def top_shaps2(dt,  n_top=10, state='TN', be_bid='be_bid', out='list'):

    dtsb = dt.loc[(dt['state_province_code']==state)&(dt['be_bid']==be_bid)]
    x_var = dtsb.drop(['state_province_code','be_bid'], axis=1).T
    x_var.columns=['shap']
    x_var = x_var.sort_values(['shap'])
    if out=='df':
        btms= pd.DataFrame(list(x_var.iloc[:n_top,].index),columns=[state])
    elif out=='list':
        btms= list(x_var.iloc[:n_top,].index)
    x_var = x_var.sort_values(['shap'], ascending=False)

    if out=='df':
        tops = pd.DataFrame(list(x_var.iloc[:n_top,].index),columns=[state])
    elif out=='list':
        tops= list(x_var.iloc[:n_top,].index)

    return tops, btms



def fig_out(nm='out.png',folder='shap_plots',path=''):
    nm = os.path.join(partition_path,nm)
    bs = io.BytesIO()
    plt.savefig(bs, format="png")
    folder = Folder(folder)
    folder.upload_stream(nm, bs.getvalue())
    plt.clf()

def shap_out_state(dt, model,state='MN', return_shap=False,path='',by_irr=True):

    explainer = shap.TreeExplainer(model)

    if state=='All':
        dtsb = dt
    else:
        dtsb = dt.loc[dt['state_province_code']==state]
    dtsb.index = range(dtsb.shape[0])

    if by_irr== False:
        X, y =  dtsb[model.feature_name_],  dtsb['YGHMN']

        shap_values_out = shap.TreeExplainer(model).shap_values(X)
        nm = 'SHAP_' + state +'.png'
        shap.summary_plot(shap_values_out, X, feature_names = X.columns,max_display=30, show=False)
        #shap.plots.beeswarm(shap_values_out)
        fig_out(nm=nm,folder='shap_plots',path=path)

        #print('ts1')
       # shap_interactions = shap.TreeExplainer(model).shap_interaction_values(X)
        #print('ts2')
       # nm = 'SHAP_interaction_' + state +'.png'
       # shap.summary_plot(shap_interactions,  X, feature_names = X.columns, show=False)
        #fig_out(nm=nm,folder='shap_plots',path=path)
    else:
        irrs = list(dtsb['irrigation'].unique())
        for irr in irrs:
            dtsb2 = dtsb.loc[dtsb['irrigation']==irr]
            dtsb2.index = range(dtsb2.shape[0])

            X, y =  dtsb2[model.feature_name_],  dtsb2['YGHMN']
            X["maturity_group"] = X["maturity_group"].round(0)
            X["maturity_group"] = pd.Categorical(X.maturity_group)

            shap_values_out = shap.TreeExplainer(model).shap_values(X)
            nm = 'SHAP_' + state+'_'+irr +'.png'
            shap.summary_plot(shap_values_out, X, feature_names = X.columns,max_display=30,show=False)
            fig_out(nm=nm,folder='shap_plots',path=path)

           # shap_interaction = shap.TreeExplainer(model).shap_interaction_values(X)
            #nm = 'SHAP_interaction_' + state +'.png'
            #shap.summary_plot(shap_interaction,  X, show=False)
            #fig_out(nm=nm,folder='shap_plots',path=path)

    if return_shap==True:
        if by_irr == True:
            X, y =  dtsb[model.feature_name_],  dtsb['YGHMN']
            X["maturity_group"] = X["maturity_group"].round(0)
            X["maturity_group"] = pd.Categorical(X.maturity_group)

            shap_values_out = shap.TreeExplainer(model).shap_values(X)
            nm = 'SHAP_' + state +'.png'
            shap.summary_plot(shap_values_out, X, feature_names = X.columns,max_display=30,show=False)
            fig_out(nm=nm,folder='shap_plots',path=path)


           # shap_interaction = shap.TreeExplainer(model).shap_interaction_values(X)
           # nm = 'SHAP_interaction_' + state +'.png'
           # shap.summary_plot(shap_interaction,  X, show=False)
           # fig_out(nm=nm,folder='shap_plots',path=path)

        return shap_values_out



def calculate_shap(i, estimator, n_count):
    #del dta
    print('Prcoess' + str(i) + 'started')
    n_process = dta.shape[0]//n_count
    if i< n_count:
        id1 = i*n_process
        id2 = (i+1)*n_process
    else:
        id1 = i*n_process
        id2 = dta.shape[0]

    #dt = dta[dta['state_province_code']==state]
    dt_sb = dta.iloc[id1:id2,]
    X = dt[estimator.feature_name_]
    dt_shap = shap.TreeExplainer(estimator).shap_values(X)
    dt_shap = pd.DataFrame(dt_shap)
    dt_shap.columns = X.columns
    dt_shap = dt_shap.round(4)

    dt_shap = pd.concat([dt_sb[['be_bid','state_province_code']],dt_shap], axis=1)
    return dt_shap

def calculate_shap(state):

    print('***** IMPORTANT NOTICE *******')
    print('Calculating shap in parallel!')
    print(dta.shape)
    print(estimator.feature_name_)
    dt_sb = dta[(dta['state_province_code']==state)]
    X =  dt_sb[estimator.feature_name_]

    print(X.shape)
    dt_shap = shap.TreeExplainer(estimator).shap_values(X)
    print('Batch on calcuting SHAP Done!')
    dt_shap = pd.DataFrame(dt_shap)
    dt_shap.columns = X.columns
    dt_shap = dt_shap.round(4)
    dt_shap.index= dt_sb.index
    print('Before concat!')
    dt_shap = pd.concat([dt_sb[['state_province_code','be_bid']],dt_shap], axis=1)
    print('Batch done!')
    return dt_shap

def init_worker(data, model):
    # declare scope of a new global variable
    global dta
    global estimator
    # store argument in the global variable for this process
    dta = data
    estimation = model

# Read recipe inputs
print('******************')
print('*****Reading data*************')
start_time = time.time()
lightgbm_model_folder = dataiku.Folder("R5SmCYGf")
folder_path = lightgbm_model_folder.get_path()
partition_path = lightgbm_model_folder.get_partition_folder(dataiku.dku_flow_variables["DKU_DST_analysis_year"])
estimator = joblib.load(folder_path+partition_path+'lightgbm_model.pkl')
rfecv_mask = np.loadtxt(folder_path+partition_path+'feature_mask.csv', dtype = "float64", delimiter = ",", usecols = (0))
rfecv_mask = (rfecv_mask != 0)

# load training data and aggregate, and Calculate shap #
train_data_lgbm = dataiku.Dataset("train_data_lgbm")
train_data_df = train_data_lgbm.get_dataframe()
#train_data_df = train_data_df[train_data_df['year']>2017]

train_data_shap_df = train_data_df.groupby(['state_province_code','be_bid']).mean().reset_index()
train_data_shap_df["maturity_group"] = train_data_shap_df["maturity_group"].round(0)
train_data_shap_df["maturity_group"] = pd.Categorical(train_data_shap_df.maturity_group)
#train_data_shap_df = train_data_shap_df.iloc[:50000,]
print('******************')
print('time duration on reading data: {}'.format(time.time()-start_time))
print('******************')


# Calculate shap -- original
print('******************')
print ('*********** Step 1 **********')
print('start to calculate shap')
print('******************')
start_time = time.time()
X, y = train_data_shap_df[estimator.feature_name_], train_data_shap_df['YGHMN']
dt_shap = shap.TreeExplainer(estimator).shap_values(X)
dt_shap = pd.DataFrame(dt_shap)
dt_shap.columns = X.columns
dt_shap = dt_shap.round(4)
dt_shap.index = train_data_shap_df.index
dt_shap = pd.concat([train_data_shap_df[['be_bid','state_province_code']],dt_shap], axis=1)
print('******************')
print ('*********** End of Step 1 **********')
print('time duration on calculating shap: {}'.format(time.time()-start_time))
print('******************')

#print('start to calculate shap')
#start_time = time.time()
# Calculate shap -- parallel
#states = list(train_data_shap_df['state_province_code'].unique())
#n_count = multiprocessing.cpu_count() - 2
#n_count = 3
#pool = Pool(initializer=init_worker, initargs=(train_data_shap_df,estimator), processes=n_count)
#shap_cal_parallel = partial(calculate_shap, estimator = estimator )
#results = pool.map(shap_cal_parallel, states)
#results = pool.map(calculate_shap, states)
#dt_shap = pd.concat(results, axis=0) if len(results) >0 else pd.DataFrame()
#print('time duration on calculating shap: {}'.format(time.time()-start_time))


## shap plot visualization ##
# get partition path
#output_folder = Folder('shap_plots')
#partition_path = output_folder.get_partition_folder(dataiku.dku_flow_variables["DKU_DST_analysis_year"])
# shap plot All
#shap_values_out = shap_out_state(train_data_shap_df, estimator,state='All', return_shap=True,by_irr=False)

# shap plot by state
#states= list(train_data_shap_df['state_province_code'].unique())
#for state in states:
#    shap_out_state(train_data_shap_df, estimator,state=state, path= partition_path,by_irr=False)


# Derive top and botom variables


def shap_by_id(i):

    if (i%50000 )==0:
        print('IMPTT NOTICE: ')
        print('Deriving top influencers by id!!!')
        #print("{} % done!".format(i*100/dta.shape[0]))
    state = dta['state_province_code'].iloc[i]
    be_bid = dta['be_bid'].iloc[i]

    top_df, btm_df = top_shaps2(dta,  n_top=5, state=state, be_bid=be_bid, out='list')

    dt_id_i = pd.DataFrame([state,be_bid,','.join(top_df), ','.join(btm_df)])
    dt_id_i = dt_id_i.T
    dt_id_i.columns= ['state_province_code','be_bid','positive influencers', 'negative influencers']



    #dt_id['positive influencers'].iloc[i] = ','.join(top_df)
    #dt_id['negative influencers'].iloc[i] = ','.join(btm_df)
    #dt_id_i = dt_id.iloc[[i]]
    return dt_id_i
def init_worker(data):
    # declare scope of a new global variable
    global dta
    # store argument in the global variable for this process
    dta = data



def shap_by_state_parallel_apply(state):
    dtsb = dta[dta['state_province_code']==state]
    dtsb['positive influencers'], dtsb['negative influencers']= zip(*dtsb.apply(lambda row: shap_by_id_apply(row), axis=1))
    return dtsb

def shap_by_id_apply(x_var):
    x_var = pd.DataFrame(x_var)
    x_var = x_var.drop(['state_province_code','be_bid','positive influencers','negative influencers'], axis=0)
    x_var.columns=['shap']
    x_var = x_var.sort_values(['shap'])
    btms= list(x_var.iloc[:5,].index)
    x_var = x_var.sort_values(['shap'], ascending=False)
    tops= list(x_var.iloc[:5,].index)
    return tops, btms   
print('******************')    
print ('*********** Step 2 **********')    
print('start to derive top shap variables')
print('******************')
n_count = 8

## *********** parallel method1 - parallel only *********** ##
#start_time = time.time()
#pool = Pool(processes=n_count)
#pool = Pool(initializer=init_worker, initargs=(dt_shap,), processes=n_count)
#results = pool.map(shap_by_id, range(dt_shap.shape[0]))
#results = pd.concat(results, axis=0) if len(results) >0 else pd.DataFrame(columns=['state_province_code','be_bid','positive influencers', 'negative influencers'])
#results = pd.merge(results,dt_shap,on=['state_province_code','be_bid'])
#print('*********** parallel method1 - parallel only ***********')
#print('time duration on getting top variables: {}'.format(time.time()-start_time))


states = list(dt_shap['state_province_code'].unique())
start_time = time.time()
## *********** parallel method2 parallel + apply ***********##
dt_shap['positive influencers']= 'NONE'
dt_shap['negative influencers']= 'NONE'
pool = Pool(initializer=init_worker, initargs=(dt_shap,), processes=n_count)
#dt_constant= partial(shap_by_id, dta = dta)
results = pool.map(shap_by_state_parallel_apply, states)
results = pd.concat(results, axis=0) if len(results) >0 else pd.DataFrame()
print('******************')
print ('*********** End of Step 2 **********')
print('time duration on getting top variables: {}'.format(time.time()-start_time))
print('******************')

# Write recipe outputs
trian_data_shap_agg = dataiku.Dataset("train_data_shap_by_state")
trian_data_shap_agg.write_with_schema(results)
