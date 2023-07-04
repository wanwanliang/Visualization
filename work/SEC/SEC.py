"""
Information about packages to be installed:
    environment python_test has the correct packages
    aiohttp==3.8.1
    asyncio==3.4.3
    nest-asyncio==1.5.5
    
    nest-asyncio allows loops to be run in a python recipe setting
        nest_asyncio.apply() must be in the python recipe before calling
        loop = asyncio.get_event_loop() and loop.run_until_complete(function)

    nest_asyncio and asyncio need to be imported in the python recipe.
"""

import asyncio
import aiohttp
import pandas as pd
import numpy as np
from sklearn.preprocessing import PolynomialFeatures
from sklearn.linear_model import LinearRegression as LR
import statsmodels.api as sma
from sklearn.preprocessing import MinMaxScaler
from sklearn.ensemble import RandomForestClassifier
from sklearn.cluster import AgglomerativeClustering
import pkg_resources
from minisom import MiniSom
from sklearn.metrics import silhouette_score
from sklearn.decomposition import PCA


"""
This function prepares an input dataframe for the cropfact pipeline. It assumes that the following columns are in the dataframe:
    'ap_data_sector','x_longitude','y_latitude','year' (necessary)
    'plant_date', 'harvest_date', 'GMSTP', 'irrigation' (not necessary, but helpful)
    
This function can do the following:
    Convert dates to the correct format (yyyy-MM-dd typically)
    Perform a first pass on the irrigation column to only leave 'IRR', 'LIRR', and 'TILE' options. 
        This code could be quickly adjusted to perform on a 'placement' column, though that has not been done yet
    Round the longitude and latitude columns to 6 digits (< 1 meter) and round the GMSTP column to 0 digits
        This is done to reduce the number of unique inputs. The number of digits to round is inputted as a dictionary
        Only perform rounding on columns in rounding_dict
    Extract only the columns necessary for crop fact 
    Drop duplicate rows in the dataframe
The to_dos list can be modified to remove any action, which is useful when merging the output of crop fact back with the original dataset
This function is intended to help users prepare their inputs for the rest of the pipeline, though can be avoided...

"""
def prepareDataFrameForCropFact(df, to_dos=['convert_date_format','convert_irr_format', 'extract_cols', 'perform_rounding', 'drop_duplicates'],
                                rounding_dict={'x_longitude' : 6, 'y_latitude' : 6, 'GMSTP' : 0}):
    df_out = df.copy()
    if 'convert_date_format' in to_dos:
        if 'plant_date' in df_out.columns:
            df_out['plant_date_as_date'] = df_out['plant_date'].dt.date.astype(str)
        if 'harvest_date' in df_out.columns:
            df_out['harvest_date_as_date'] = df_out['harvest_date'].dt.date.astype(str)

    if 'convert_irr_format' in to_dos and 'irrigation' in df_out.columns:
        # convert df['irrigation'] to 3 different inputs before doing unique
        # conversion to irr_stat and irr_trigger are done in a different function, this just removes 'DRY' and 'RAIN'
        df_out['irr_conv'] = df_out['irrigation']
        df_out['irr_conv'][(df_out['irr_conv'] != 'IRR') & \
                           (df_out['irr_conv'] != 'LIRR') & \
                           (df_out['irr_conv'] != 'TILE')] = 'NONE'

    # extract a small number of inputs from data frame
    
    input_cols = ['ap_data_sector','x_longitude','y_latitude','year'] # required inputs.
    round_cols = ['x_longitude','y_latitude']
    
    # append extra inputs if available
    if 'plant_date_as_date' in df_out.columns:
        input_cols.append('plant_date_as_date')
    elif 'plant_date' in df_out.columns: # this assumes that the date is in the correct format already....
        df_out.rename(columns={'plant_date' : 'plant_date_as_date'}) # rename so naming is consistent across inputs
        input_cols.append('plant_date_as_date')

    if 'harvest_date_as_date' in df_out.columns:
        input_cols.append('harvest_date_as_date')
    elif 'harvest_date' in df_out.columns: # assumes that the date is in the correct format already....
        df_out.rename(columns={'harvest_date' : 'harvest_date_as_date'}) # rename so naming is consistent across inputs
        input_cols.append('harvest_date_as_date')

    if 'irr_conv' in df_out.columns:
        input_cols.append('irr_conv')
    elif 'irrigation' in df_out.columns: # assumes that
        df_out.rename(columns={'irrigation' : 'irr_conv'}) # rename so naming is consistent across inputs
        input_cols.append('irr_conv')

    if 'GMSTP' in df_out.columns:
        input_cols.append('GMSTP')
        round_cols.append('GMSTP')

    if 'extract_cols' in to_dos:
        df_cropfact_input = df_out[input_cols]
    else:
        df_cropfact_input = df_out

    # round to reduce number of unique inputs, 6 decimal places for longitude/latitude is ~ < 1 meter
    if 'perform_rounding' in to_dos:
        for i in range(len(round_cols)):
            if round_cols[i] in rounding_dict:
                round_digs = rounding_dict[round_cols[i]]
                df_cropfact_input[round_cols[i]] = np.round(df_cropfact_input[round_cols[i]], round_digs)

    if 'drop_duplicates' in to_dos:
        df_cropfact_input = df_cropfact_input.drop_duplicates()

    return df_cropfact_input


""" 
This function generates a list of jsons from a dataframe. getCropFactJSON accepts a single set of parameters at a time, so this function loops through rows of a dataframe
This function is meant to be called after the dataframe is prepared and duplicates are removed (possibly via prepareDataFrameForCropFact)
df_uniq_input needs to have the following columns:
    x_longitude : longitude values
    y_latitude : latitude values
    plant_date_as_date : date (yyyy, or yyyy-MM-dd), could be None
    harvest_date_as_date : data in format similar to plant_date_as_date. If this is provided, GMSTP also must be provided
    ap_data_sector : data sector used to get crop name, for example 'CORN_NA_SUMMER'
    GMSTP : grain moisture at harvest. For this to be used, harvest_date_as_date needs to be in yyyy-MM-dd format.
df_uniq_input can also have the following inputs:
    irr_conv : irrigation and drainage information ('IRR','LIRR','TILE',None). Code converts this to irr_status, irr_trigger_threshold, and drainage status
    
returns a list of jsons, which can be used to get data from crop fact.
"""

def getCropFactJSONWrapper(df_uniq_input):
    country='GLOBAL' # global works, code doesn't allow for more specific country codes at this point
    jsons = []
    for index, row in df_uniq_input.iterrows():
        coord = [row['x_longitude'],row['y_latitude']]
        drain_stat="Absence"
        irr_stat="Absence"
        irr_trig_thresh=None
        grain_moist=None

        
        # get crop name
        crop = getCropFromDataSector(row['ap_data_sector'])
        
        
        # make sure grain mositure is within required range
        # if harvest date, then GMSTP is mandatory
       
        grain_moist = max(5,min(34.9,row['GMSTP']))
        if crop == 'Soybean':
            grain_moist = max(5,min(29.9,row['GMSTP']))
            
      #  if 'GMSTP' in row.index and row['GMSTP'] is not None and np.isnan(row['GMSTP']) == False:
       #     grain_moist = max(5,min(34.9,row['GMSTP']))

        # convert irr_conv to irr_stat and irr_trig_thresh, also drain_stat
        if 'irr_conv' in row.index:
            if row['irr_conv'] == 'IRR':
                irr_stat="Presence"
                irr_trig_thresh = 0.85
            elif row['irr_conv'] == 'LIRR':
                irr_stat="Presence"
                irr_trig_thresh = 0.65
            elif row['irr_conv'] == 'TILE':
                drain_stat="Presence"

        # dates need to have day, month year to use grain_moist as input 
       # if grain_moist is not None and (row['year'] is None or len(str(row['harvest_date_as_date'])) < 10):
       #     grain_moist = None
        
        if row['plant_date_as_date']=='NaT':
            plant_date = str(row['year'])
            grain_moist = None
        else:
            plant_date = row['plant_date_as_date']
        
        if row['harvest_date_as_date']=='NaT':
            harvest_date = str(row['year'])
            grain_moist = None
        else:
            harvest_date = row['harvest_date_as_date']
        


        # dates must be within specified ranges, these ranges differ for each crop
        # end dataframe will have resulting error
        jsons.append(getCropFactJSON(country=country, country_sub=None,season="1",
                                   coord=coord, crop=crop, crop_sub=None,plant_date= plant_date,
                                     harvest_date=harvest_date, grain_moisture=grain_moist,
                                     irrigation_status=irr_stat,irrigation_trigger=irr_trig_thresh,
                                    drainage_status=drain_stat))
    return jsons


"""
This function creates a json that will be used when calling cropfact. 
    country : country. 'US' for example, though 'GLOBAL' also works
    country_sub : sub division of county, like IL or NE for USA. Only for US, CA, FR
    coord : [longitude, latitude], longitude can be in [-180, 180], latitude [-90,90]
    crop : crop name. This can be automatically grabbed using getCropFromDataSector if the ap_data_sector is provided and in the predefined dictionary
    crop_sub: subdivision for crop, only available for NA Corn Grain and NA Soybean. Check CropFact API for more details
    season : growing season, "1" or "2" -- not really sure what this does, as providing "2" to cropfact throws an error
    plant_date : planting date. format: yyyy-MM-dd or yyyy. 
    harvest_date : harvest date. same format as plant_date
    graint_moisture: grain moisture at harvest. in percentage, [5-34.9] for corn, [5-29.9] for soybean. Harvest date must be provided as yyyy-MM-dd to use grain moisture as input
    irrigation_status : "Presence" or "Absence" of irrigation. 
    irrgation_trigger : Value of the ratio of the crop actual evapotranspiration to the crop potential evapotranspiration used to trigger irrigation events
        if IRR, 0.85. if LIRR, 0.65 apparently?
    irrigation_timestamp: timestamps of irrigation events, date format yyyy-MM-dd. 
    irrigation_value : amount of irrigation events (>=0)
    drainage_status : "Presence" or "Absence" of drainage tiles
    
    outputs information in the appropriately structured json dictionary style. This can be used to call cropfact
    
    This function does not apply constraints to the input, such as fixing the grain moisture limit, or preventing calls when the harvest date is not provided.
    The example (above) has some code to do this. I did not include that code in this function so that users would know/decide which constraints/modifications 
    are applied to their input data.
    
"""
def getCropFactJSON(country='GLOBAL', country_sub=None, coord=[-90,40], crop='Corn Grain', crop_sub=None,season="1",
                    plant_date='2020',harvest_date='2020',grain_moisture=None,
                    irrigation_status="Absence",irrigation_trigger=None, irrigation_timestamp=None, irrigation_value=None,
                   drainage_status="Absence"):

    # predefine scenario
    scenario = {}

    # set country and location info, put in scenario
    geo = {'type':'Point','coordinates':coord}
    cropping_area = {'country': country, 'countrySubDivision': country_sub, 'geometry' : geo}
    scenario['croppingArea'] = cropping_area

    # set crop info
    scenario['genotype'] = {'crop' : crop, 'cropSubDivision' : crop_sub}

    # set management info
    operations_list = [{"name" : "Planting", "timestamp" : plant_date},
                       {"name" : "Harvest", "timestamp" : harvest_date},
                       {"name" : "Irrigation", "status" : irrigation_status, "triggerThreshold" : irrigation_trigger, "timestamp" : irrigation_timestamp, "value" : irrigation_value}]

    equip_list = [{"name" : "DrainageSystem", "status" : drainage_status}]
    scenario['management'] = {'season' : season, 'operations': operations_list, 'equipments' : equip_list}

    # phenotype info.
    pheno = {}
    pheno['physiologicalTraits'] = [{"name" : "GrainMoistureContent", "timestamp" : "Harvest", "value" : grain_moisture}]
    scenario['phenotype'] = pheno

    json_body = {}
    json_body['scenario'] = scenario

    return json_body

"""
helper function to extract data from the structure cropfact returns. This function hides a lot of the logic used to extract relevant information.
getRecords calls this function a few times. See getRecords for usage examples
"""
def parseCropFactData(curr_dict={},crop_dict=None):
    for var in crop_dict:
        if 'value' in var:
            if var['value'] is not None and isinstance(var['value'],list) and len(var['value']) > 1:
                for i_var in range(len(var['value'])):
                    if var['name'] == 'Temperature' or var['name'] == 'RelativeHumidity' or var['code'] == 'vpdmax':
                        curr_dict[var['code']+var['timestamp'][i_var]] = var['value'][i_var]
                    else:
                        curr_dict[var['name']+var['timestamp'][i_var]] = var['value'][i_var]
            elif var['value'] is not None and isinstance(var['value'],list) and len(var['value']) == 1:
                curr_dict[var['name']] = var['value'][0]
            elif var['value'] is None:
                curr_dict[var['name']] = None
            else:
                curr_dict[var['name']] = var['value']
        else: # phenological stages
            curr_dict['PhenologicalStage'+var['name']] = var['timestamp']
    return curr_dict


"""
Function to convert ap_data_sector to appropriate crop name for crop fact. 
Fill in dictionary with ap_data_sector : crop name if a specific ap_data_sector is missing.
See crop fact api for more details about crop names
"""
def getCropFromDataSector(sector):
    # map ap data sector to crop name. Need to fill in ap data sectors

    # crop can be 'Corn Grain', 'Corn Silage', 'Soybean', 'Sunflower', 'Winter Wheat', 'Winter Barley'
    # map ap data sector to crop name
    crop_mapper = {'CORN_NA_SUMMER' : 'Corn Grain',
                  'SOY_NA_SUMMER' : 'Soybean',
                  'SUNFLOWER_EAME_SUMMER': 'Sunflower',
                  'CORNGRAIN_APAC_1': 'Corn Grain',
                  'CORN_EAME_SUMMER': 'Corn Grain',
                  'CORNGRAIN_EAME_SUMMER': 'Corn Grain',
                  'CORNSILAGE_EAME_SUMMER':'Corn Silage', # cropFact may not support Corn Silage yet
                  'WHEAT_EAME_WINTER': 'Winter Wheat',
                  'BARLEY_EAME_WINTER': 'Winter Barley',
                  'CORN_INDONESIA_WET': 'Corn Grain',
                  'CORN_INDONESIA_DRY': 'Corn Grain'}

    return crop_mapper[sector]

"""
This function calls crop fact. It is an async function, enabling multiple calls simultaneously.
getRecords calls this function repeatedly, it should probably not be called outside of getRecords

my_api : "https://cropfact.syngentaaws.org/services/cropphysiology"
my_header = {'x-api-key': key, 'Content-Type': 'application/json'}

"""
async def callCropFact(session,json, my_api, my_header):
    async with session.post(url=my_api, headers=my_header, json=json) as resp:
        return await resp.json()


"""
Call this function to get crop fact data for a list of jsons. Calls are made asynchronously, allowing for many calls at once.
Calls are made in blocks of 5000, as making more calls simultaneoulsy resulted in an error.
    --- THIS MAY BE OLD. batch_size controls this, and it might be that a maximum of 20 can be used. This wasn't the case when the code was developed

There is a maximum of 100,000 calls in a day for the same api-key



    jsons_all : list of jsons generated by repeated calls to getCropFactJSON
    my_api : "https://cropfact.syngentaaws.org/services/cropphysiology"
    my_header = {'x-api-key': key, 'Content-Type': 'application/json'}


returns dataframe with relevant information extracted from crop fact output
"""
async def getRecords(jsons_all, my_api, my_header, batch_size=20):
    # extract data from crop fact calls
    data = []

    #print("started calling crop fact")
    for i in range(0,len(jsons_all), batch_size):
        #if i%100 == 0:
        #    print(i/len(jsons_all))
        # split jsons_all
        if i+batch_size > len(jsons_all):
            jsons = jsons_all[i:]
        else:
            jsons = jsons_all[i:i+batch_size]

        # generate list of tasks
        async with aiohttp.ClientSession() as session:
            # check if we have a list of jsons or just a single one
            if isinstance(jsons, list):
                tasks = [callCropFact(session,json, my_api, my_header) for json in jsons]
            else:
                tasks = [callCropFact(session,jsons, my_api, my_header)]
            # asynchronously perform tasks, which are different calls to crop fact
            out = await asyncio.gather(*tasks)
        # done calling crop fact, now packaging outputs
        for i in range(len(out)):
            cd = out[i]
            if 'error' not in cd.keys():
                try: # mostly to prevent code from stopping if there is an error. Errors should be stored in output dataframe
                    data.append({})
                    # some meta information
                    data[-1]['error'] = None
                    data[-1]['country'] = cd['scenario']['croppingArea']['country']
                    data[-1]['countrySubDivision'] = cd['scenario']['croppingArea']['countrySubDivision']
                    data[-1]['x_longitude'] = cd['scenario']['croppingArea']['geometry']['coordinates'][0]
                    data[-1]['y_latitude'] = cd['scenario']['croppingArea']['geometry']['coordinates'][1]
                    data[-1]['crop'] = cd['scenario']['genotype']['crop']
                    
                    # get grain moisture content input, useful for merging later
                    phys_dict = jsons[i]['scenario']['phenotype']['physiologicalTraits']
                    for d in phys_dict:
                        if "name" in d.keys() and d["name"] == 'GrainMoistureContent':
                            data[-1]['GMSTP_input'] = d['value']
                    
                    for op in cd['scenario']['management']['operations']:
                        data[-1][op['name']] = op['timestamp']

                    for eq in cd['scenario']['management']['equipments']:
                        data[-1][eq['name']] = eq['status']

                    # phenological stages: get timestamp
                    data[-1] = parseCropFactData(curr_dict=data[-1].copy(), crop_dict=cd['scenario']['phenotype']['phenologicalStages'])

                    # physiological traits, split by stage if trait is defined for each stage
                    data[-1] = parseCropFactData(curr_dict=data[-1].copy(), crop_dict=cd['scenario']['phenotype']['physiologicalTraits'])

                    # environment variables -- Soil properties
                    data[-1] = parseCropFactData(curr_dict=data[-1].copy(), crop_dict=cd['scenario']['environment']['soilProperties'])

                    # environment vars -- growth conditions
                    data[-1] = parseCropFactData(curr_dict=data[-1].copy(), crop_dict = cd['scenario']['environment']['growthConditions'])

                    # environment vars -- stresses
                    data[-1] = parseCropFactData(curr_dict=data[-1].copy(), crop_dict = cd['scenario']['environment']['stresses'])
                except Exception as e:
                    if 'message' in cd:
                        data[-1]['error'] = cd['message']
                    else:
                        data[-1]['error'] = e
            else: # append error message, leave rest of dataframe empty
                data.append({})
                data[-1]['error'] = cd['error']['message']

    # make data frame from crop fact outputs
    df_cropfact = pd.DataFrame.from_dict(data,orient='columns')

    return df_cropfact


def environment_selection(new_data,  trait_column ='YGSMN_result_numeric_value_avg', unique_id='trial_id'):
    
    #### data preprocessing for environment variable selection
   
    new_data2 = new_data.dropna(subset=[trait_column,'Texture'])
    new_data2.index = range(new_data2.shape[0])
    nms = np.array(list(new_data2.columns))
    ar= np.where(nms == 'WaterUseEfficiency')
    idex= ar[0][0] + 1
    nm0 = nms[:idex]
    
    ### remove columns that are not environment variables
    dt_x = new_data2.drop(list(nm0), axis=1)
    dt_y = new_data2[[trait_column]]
    
    dt_x['wsy_c'] = new_data2['wsy_c']

    dta= dt_x.copy()
    dta[trait_column] = new_data2[trait_column]

    ### aggregate y/trait variable at unique environment level
    dta2 = dta.groupby(list(dt_x.columns)).mean()
    dta2 = dta2.reset_index()
    dt_x, dt_y = dta2.drop([trait_column], axis=1), dta2[[trait_column]]
    
    from scipy.stats import pearsonr
    from sklearn.preprocessing import MinMaxScaler
    from sklearn.model_selection import train_test_split
    from sklearn.linear_model import Lasso
    from sklearn.linear_model import LinearRegression

    dt_x = pd.get_dummies(dt_x)
    env_nms = list(dt_x.columns)
    dt_x = pd.DataFrame(MinMaxScaler().fit_transform(dt_x))
    dt_x.columns = env_nms

    r2_df = pd.DataFrame()
    paras= [0.00001, 0.00003, 0.00005, 0.0001,0.0003, 0.0005, 0.001, 0.003,0.005, 0.01, 0.03, 0.05, 0.1, 0.3, 0.5]
    var_imp_lasso = pd.DataFrame()

    for n in paras:

        rs = []
        lasso_coef_df = pd.DataFrame()
        for i in range(20):
            X_train, X_test, y_train, y_test = train_test_split(dt_x, dt_y, test_size=0.3, random_state=i*908*60+23)
            rf = Lasso(alpha= n)
            rf.fit(X_train, y_train)
            lasso_coef_df = pd.concat([lasso_coef_df, pd.DataFrame(rf.coef_)], axis=1)

            y_pred_train = rf.predict(X_train)
            y_pred_test = rf.predict(X_test)
            pred_y = rf.predict(dt_x)
            r2_test = pearsonr(y_test.values.ravel(), y_pred_test)[0]
            rs.append(r2_test)
        rs = pd.DataFrame(rs)
        r2_df = pd.concat([r2_df, rs], axis=1)

        las_imp = pd.DataFrame(lasso_coef_df.mean(axis=1))
        las_imp['feature'] = list(dt_x.columns)
        las_imp['feature_importance'] = abs(las_imp[0]).round(4)
        las_imp = las_imp[['feature_importance']]
        var_imp_lasso = pd.concat([var_imp_lasso, las_imp], axis=1)

    var_imp_lasso.columns = paras
    var_imp_lasso = var_imp_lasso.loc[:,var_imp_lasso.std(axis=0)>0]
    var_imp_lasso['feature'] = list(dt_x.columns)
    r2_df.columns = paras


    #### get best lamba parameter
    r2_df = pd.DataFrame()
    nvars = var_imp_lasso.shape[1] - 1
    for n in range(nvars):
        las_imp2 = pd.DataFrame(var_imp_lasso.iloc[:,n])
        las_imp2.columns = ['feature_importance']
        las_imp2['feature'] = var_imp_lasso['feature']
        las_imp2 = las_imp2.sort_values(['feature_importance'], ascending=False)
        las_imp3 = las_imp2.loc[las_imp2['feature_importance']>0,]

        nms = list(las_imp3['feature'])
        dt_x2 = dt_x[nms]

        rs = []
        for i in range(50):
            X_train, X_test, y_train, y_test = train_test_split(dt_x2, dt_y, test_size=0.3, random_state=i*800+23)
            rf = LinearRegression()
            rf.fit(X_train, y_train)

            y_pred_test = rf.predict(X_test)
            r2_test = pearsonr(y_test.values.ravel(), y_pred_test.ravel())[0]
            rs.append(r2_test)
        rs = pd.DataFrame(rs)
        rs= rs.round(2)

        r2_df = pd.concat([r2_df, rs], axis=1)

    r2_df.columns = list(var_imp_lasso.columns[:nvars])
    best = pd.DataFrame(r2_df.median(axis=0))

    best = pd.DataFrame(r2_df.median(axis=0))
    best_va = best[0].max()
    best2 = best.loc[best[0]==best_va]
    best_para= best2.index[best2.shape[0]-1]


    #### Get final weights for selected variables 
    lasso_coef_df_final = pd.DataFrame()
    for i in range(50):
        X_train, X_test, y_train, y_test = train_test_split(dt_x, dt_y, test_size=0.3, random_state=i*600+23)
        rf = Lasso(alpha=best_para)
        rf.fit(X_train, y_train)
        lasso_coef_df = pd.concat([lasso_coef_df, pd.DataFrame(rf.coef_)], axis=1)
    las_imp = pd.DataFrame(lasso_coef_df.mean(axis=1))
    las_imp['feature'] = list(dt_x.columns)
    las_imp['feature_importance'] = abs(las_imp[0]).round(3)
    las_imp2 = las_imp.loc[las_imp['feature_importance']>0]
    las_imp2.rename(columns={0:best_para}, inplace=True)

    #### get name of selected variables
    env_selected = las_imp2['feature']
    env_selected2 = []
    for env_nm in env_selected:
        if env_nm.startswith('Texture'):
            env_nm = 'Texture'
        if env_nm.startswith('wsy_c'):
            env_nm = 'wsy_c'
        env_selected2.append(env_nm)    
    env_selected2 = list(set(env_selected2))
    env_selected2.sort()


    #### get environment at user-defined unique_id level
    new_data_sel = new_data.loc[:,new_data.columns.isin(env_selected2)]
    new_data_sel[unique_id] = new_data[unique_id]

    if len(list(set(['Texture']) & set(env_selected2))) >0:
        if len(list(set(['wsy_c']) & set(env_selected2))) >0:
            new_data_sel2 = new_data_sel.groupby([unique_id,'wsy_c','Texture']).mean()
        else:
            new_data_sel2 = new_data_sel.groupby([unique_id,'Texture']).mean()
        new_data_sel2 = new_data_sel2.reset_index()
    else:
        if len(list(set(['wsy_c']) & set(env_selected2))) >0:
            new_data_sel2 = new_data_sel.groupby([unique_id,'wsy_c']).mean()
        else:
            new_data_sel2 = new_data_sel2.groupby([unique_id]).mean()
        new_data_sel2 = new_data_sel2.reset_index()
    
    return new_data_sel2, best_va

def DH_stress_pred(new_data, irrigation='irrigation', ap_data_sector='CORN_NA_SUMMER', stress='DH'):
    
    import warnings
    warnings.filterwarnings('ignore')

    max_yield= 200
    min_yield= 20
    significant_alpha= 0.05
    
    historic_data = load_corn_data(ap_data_sector= ap_data_sector)

    if stress=='DH':
        heat_stress= True
    else:
        heat_stress = False
        
    new_data['ID'] = range(new_data.shape[0])
    new_data['ID'] = 'ID_' + new_data['ID'].map(str)
    
        
    
    if irrigation=='NONE':
        new_data['wsy_c'] = 'RAIN'
    else:
        new_data['wsy_c'] = new_data[irrigation]
        new_data['wsy_c'].loc[new_data['wsy_c']=='DRY'] = 'RAIN'
    

    new_data_temp = new_data[['tempmaxPlanting-VE', 'tempmaxVE-V6', 'tempmaxV6-VT', 'tempmaxVT-R2', 'tempmaxR2-R4', 'tempmaxR4-R6', 'tempmaxR6-Harvest',
            'tempavgPlanting-VE', 'tempavgVE-V6', 'tempavgV6-VT', 'tempavgVT-R2', 'tempavgR2-R4', 'tempavgR4-R6', 'tempavgR6-Harvest','ID']]
    new_data_temp = new_data_temp.dropna()
    new_data_temp.index = range(new_data_temp.shape[0])
    new_data_id = new_data_temp[['ID']]
    historic_data['ygsmn'] = historic_data['adjusted_ygsmn']
    historic_data_temp = historic_data[['tempmaxPlanting-VE', 'tempmaxVE-V6', 'tempmaxV6-VT', 'tempmaxVT-R2', 'tempmaxR2-R4', 'tempmaxR4-R6', 'tempmaxR6-Harvest',
        'tempavgPlanting-VE', 'tempavgVE-V6', 'tempavgV6-VT', 'tempavgVT-R2', 'tempavgR2-R4', 'tempavgR4-R6', 'tempavgR6-Harvest','ygsmn']]


    new_stress_df = pd.DataFrame()
    n = historic_data_temp.shape[1] - 1
    for i in range(n):
        stress = stress_calc(i,historic_data_temp, new_data_temp, heat_stress=heat_stress)
        new_stress_df = pd.concat([new_stress_df, stress], axis=1)

    new_stress_df = 1 - new_stress_df
    new_stress_df.index = new_data_id.index
    dta = pd.concat([new_data_id, new_stress_df], axis=1)
    new_data = pd.merge(new_data, dta, on=['ID'], how='left') 
        
        
    nms = list(historic_data.drop(['adjusted_ygsmn','ygsmn','DH_stress5'],axis=1).columns)
    new_data_dh = new_data[nms]
    new_data_dh['ID'] = new_data['ID']
    new_data_dh  = new_data_dh.dropna()

    x, y = pd.get_dummies(historic_data[nms]), historic_data[['DH_stress5']]
    x_new = pd.get_dummies(new_data_dh[nms])

    set_diff = list(set(x.columns) - set((x_new.columns)))
    for i in set_diff:
        x_new[i] = 0

    rfm = RandomForestClassifier(n_estimators=128, max_depth=10, min_samples_split=2).fit(x,y)
    new_data_dh['DH_stress5'] = rfm.predict(x_new)

    dta = pd.merge(new_data, new_data_dh[['ID','DH_stress5']], on=['ID'], how='left')
    
    if str(irrigation) != 'wsy_c':
        dta = dta.drop(['ID', 'wsy_c'],axis=1)
        
    
    return dta
def get_cropFact_env(df,year= 'year', plant_date = 'plant_date', harvest_date= 'harvest_date', ap_data_sector='ap_data_sector',
                  longitude='x_longitude', latitude='y_latitude',irrigation='irrigation',gmstp='GMSTP_result_numeric_value_avg',
                    env_selection=False, trait_column ='YGSMN_result_numeric_value_avg', aggregate_by_ids= False, unique_id='trial_id',
                    pred_stress=True, stress_type='DH'):
    
    import warnings
    warnings.filterwarnings('ignore')

    df['year']  = pd.to_numeric(df[year], errors='coerce')
    df['plant_date2'] = pd.to_datetime(df[plant_date], errors='coerce')
    df['harvest_date2'] = pd.to_datetime( df[harvest_date], errors='coerce')
    df['plant_date_as_date'] = df['plant_date2'].dt.date.astype(str)
    df['harvest_date_as_date'] = df['harvest_date2'].dt.date.astype(str)
    
    if gmstp=='NONE':
         df['GMSTP'] = np.nan
    else:
        df['GMSTP'] = pd.to_numeric(df[gmstp], errors='coerce')
        df['GMSTP'] = df['GMSTP'].round(1)
        
    df[longitude] = pd.to_numeric(df[longitude], errors='coerce')
    df[latitude] = pd.to_numeric(df[latitude], errors='coerce')
    df[['x_longitude','y_latitude']] = df[[longitude,latitude]].round(4)
    
    if len(set([ap_data_sector])&set(list(df.columns)))>0:
        df['ap_data_sector'] = df[ap_data_sector]
    else: 
        df['ap_data_sector'] = ap_data_sector
    
    if irrigation=='NONE':
        df['irr_conv'] = 'DRY'
    else:
        df['irr_conv'] = df[irrigation]
        df['irr_conv'][(df['irr_conv'] != 'IRR') & (df['irr_conv'] != 'LIRR') & (df['irr_conv'] != 'TILE')] = 'NONE'
    df['Env_id'] = 'ID_' + df['year'].map(str) + df['plant_date_as_date'].map(str) + df['harvest_date_as_date'].map(str) +  df['x_longitude'].map(str) + df['y_latitude'].map(str) + df['irr_conv'] + df['ap_data_sector'] + df['GMSTP'].map(str) 

    df_cropfact_input = df[['Env_id','ap_data_sector','x_longitude','y_latitude','year','plant_date_as_date','harvest_date_as_date','irr_conv','GMSTP']]
    df_cropfact_input = df_cropfact_input.drop_duplicates()
    df_cropfact_input.index = range(df_cropfact_input.shape[0])
    
    import nest_asyncio
    nest_asyncio.apply()
    
    my_api = "https://cropfact.syngentaaws.org/services/cropphysiology"
    my_header = {'x-api-key': 'N1sgY9MO7O2vky35RBSLL20napGp3qRH6LWOAdKT', 'Content-Type': 'application/json'}
        
    jsons = getCropFactJSONWrapper(df_cropfact_input)
    loop = asyncio.get_event_loop()
    env_df = loop.run_until_complete(getRecords(jsons, my_api, my_header))
    
   
    env_df.index =  df_cropfact_input.index 
    nm2 = list(set(list(df.columns)) & set(list(env_df.columns)))
    
    env_df['Env_id'] = df_cropfact_input['Env_id']
    dta = pd.merge(df, env_df.drop(nm2, axis=1), on=['Env_id'])
    #dta = dta.dropna(subset=['Texture'])
    dta = dta.drop(['irr_conv','GMSTP_input','Planting','Harvest','Irrigation','plant_date2','harvest_date2','DrainageSystem','GMSTP','Env_id'], axis=1, errors='ignore')
        
    
    if env_selection== True:
        aggregate_by_ids = False
        
    if aggregate_by_ids== True:
        if irrigation =='NONE':
            dta = dta.groupby([unique_id, 'Texture']).mean() 
            dta = dta.reset_index()
        else:
            dta = dta.groupby([unique_id, irrigation, 'Texture']).mean() 
            dta = dta.reset_index()
       
    if env_selection==True:
        
        if irrigation=='NONE':
            dta['wsy_c'] = 'RAIN'
        else:
            dta['wsy_c'] = dta[irrigation]
            dta['wsy_c'].loc[dta['wsy_c']=='DRY'] = 'RAIN'
        
        
        sel_env_dt, validation_accuracy = environment_selection(dta,  trait_column = trait_column , unique_id=unique_id)
        
        if pred_stress==True:
            dta = DH_stress_pred(dta, irrigation='wsy_c', ap_data_sector=list(dta['ap_data_sector'].unique())[0], stress=stress_type)
    
        
        if str(irrigation)!='wsy_c':
            dta = dta.drop(['wsy_c'], axis=1)
        try:
            sel_env_dt.rename(columns={'wsy_c':irrigation}, inplace=True)    
        except Exception:
            pass    
        
        return dta, sel_env_dt, validation_accuracy
    else:
        if pred_stress==True:
            dta = DH_stress_pred(dta, irrigation=irrigation, ap_data_sector=list(dta['ap_data_sector'].unique())[0], stress=stress_type)
    
        return dta

def env_preprocessing(env):
    
    env2 = env.dropna(subset=['Texture'])
    nms = np.array(list(env2.columns))
    ar= np.where(nms == 'WaterUseEfficiency')
    idex= ar[0][0] + 1
    nm0 = nms[:idex]
    nm_remove = list(nm0) + ['SandContent', 'SiltContent', 'ClayContent', 'pHwater', 'CationExchangeCapacity', 'CoarseFragmentContent', 'CalciumCarbonateContent', 'OrganicMatterContent', 'BulkDensity', 'RootDepthConstraint', 'AvailableWaterAtFieldCapacity', 'Texture', 'HydraulicConductivityAtSaturation']
    env3 = env2.drop(nm_remove, axis=1)
    
    if 'trial_id' in list(env2.columns):
        env3[['trial_id','GrainYieldSolarPotential', 'GrainYieldPotential', 'GrainYieldAttainable']] = env2[['trial_id','GrainYieldSolarPotential', 'GrainYieldPotential', 'GrainYieldAttainable']]
    else:
        env3[['GrainYieldSolarPotential', 'GrainYieldPotential', 'GrainYieldAttainable']] = env2[['GrainYieldSolarPotential', 'GrainYieldPotential', 'GrainYieldAttainable']]
    return env3


def stress_calc(i, historic_data_temp, new_data_temp,  heat_stress=True, max_yield= 200, min_yield= 20, significant_alpha= 0.05):
    x, y =  pd.DataFrame(historic_data_temp.iloc[:,i]), pd.DataFrame(historic_data_temp['ygsmn'])
    x2 = pd.DataFrame(PolynomialFeatures(degree=2).fit_transform(x))
    x2 = x2.iloc[:,1:3]
    ft_nlm = sma.OLS(y,x2).fit()
    ft_lm = sma.OLS(y,x).fit()

    x_new =  pd.DataFrame(new_data_temp.iloc[:,i])
    x2_new = pd.DataFrame(PolynomialFeatures(degree=2).fit_transform(x_new))
    x2_new = x2_new.iloc[:,1:3]

    if heat_stress==True:
        nm2 = historic_data_temp.columns[i] + "_Stress_Heat"
    else:
        nm2 = historic_data_temp.columns[i] + "_Stress_Cold"


    if ((ft_nlm.pvalues[2]>significant_alpha)|(ft_nlm.params[2]>0)):

        if (ft_lm.pvalues[0] < significant_alpha):

            y_pred = pd.DataFrame(ft_lm.predict(x))
            y_pred.columns = ['pred_ygsmn']
            y_pred['pred_ygsmn'].loc[y_pred['pred_ygsmn']<=0] = min_yield
            y_pred['pred_ygsmn'].loc[y_pred['pred_ygsmn']> max_yield] =  max_yield

            y_pred_new = pd.DataFrame(ft_lm.predict(x_new))
            y_pred_new.columns = ['pred_ygsmn']
            y_pred_new['pred_ygsmn'].loc[y_pred_new['pred_ygsmn']<=0] = min_yield
            y_pred_new['pred_ygsmn'].loc[y_pred_new['pred_ygsmn']> max_yield] =  max_yield

            if (ft_lm.params[0]>0):
                if heat_stress==True:
                    pred_stress = pd.DataFrame(y*0)
                    pred_stress_new = pd.DataFrame(y_new*0)
                else:
                    y_pred[nm2] = 1/y_pred['pred_ygsmn']
                    y_pred_new[nm2] = 1/y_pred_new['pred_ygsmn']

                    sca =  MinMaxScaler().fit(y_pred[[nm2]])
                    pred_stress = sca.transform(y_pred[[nm2]])
                    pred_stress_new = sca.transform(y_pred_new[[nm2]])

            elif (ft_lm.params[0]<0):

                if heat_stress==True:
                    y_pred[nm2] = 1/y_pred['pred_ygsmn']
                    y_pred_new[nm2] = 1/y_pred_new['pred_ygsmn']

                    sca =  MinMaxScaler().fit(y_pred[[nm2]])
                    pred_stress = sca.transform(y_pred[[nm2]])
                    pred_stress_new = sca.transform(y_pred_new[[nm2]])

                else:
                    pred_stress = pd.DataFrame(y*0)
                    pred_stress_new = pd.DataFrame(y_new*0)
            else:
                pred_stress = pd.DataFrame(y*0)
                pred_stress_new = pd.DataFrame(y_new*0)

        else:
            pred_stress = pd.DataFrame(y*0)
            pred_stress_new = pd.DataFrame(y_new*0)

    else:
        y_pred = pd.DataFrame(ft_nlm.predict(x2))
        y_pred.columns = ['pred_ygsmn']
        y_pred['x_var'] = x.iloc[:,0]
        y_pred['pred_ygsmn'].loc[y_pred['pred_ygsmn']<=0] = min_yield
        y_pred['pred_ygsmn'].loc[y_pred['pred_ygsmn']> max_yield] =  max_yield
        x_var_turn = y_pred['x_var'][y_pred['pred_ygsmn'].idxmax()]

        y_pred_new = pd.DataFrame(ft_nlm.predict(x2_new))
        y_pred_new.columns = ['pred_ygsmn']
        y_pred_new['x_var'] = x_new.iloc[:,0]
        y_pred_new['pred_ygsmn'].loc[y_pred_new['pred_ygsmn']<=0] = min_yield
        y_pred_new['pred_ygsmn'].loc[y_pred_new['pred_ygsmn']> max_yield] =  max_yield

        if heat_stress==True:
            y_pred[nm2] = 1/y_pred['pred_ygsmn']
            y_pred_new[nm2] = 1/y_pred_new['pred_ygsmn']

            y_pred[nm2].loc[y_pred['x_var']<= x_var_turn] = min(y_pred[nm2])
            y_pred_new[nm2].loc[y_pred_new['x_var']<= x_var_turn] = min(y_pred[nm2])

            sca =  MinMaxScaler().fit(y_pred[[nm2]])
            pred_stress = sca.transform(y_pred[[nm2]])
            pred_stress_new = sca.transform(y_pred_new[[nm2]])


        else:
            y_pred[nm2] = 1/y_pred['pred_ygsmn']
            y_pred_new[nm2] = 1/y_pred_new['pred_ygsmn']

            y_pred[nm2].loc[y_pred['x_var']> x_var_turn] = min(y_pred[nm2])
            y_pred_new[nm2].loc[y_pred_new['x_var']> x_var_turn] = min(y_pred[nm2])

            sca =  MinMaxScaler().fit(y_pred[[nm2]])
            pred_stress = sca.transform(y_pred[[nm2]])
            pred_stress_new = sca.transform(y_pred_new[[nm2]])

    pred_stress = pd.DataFrame(pred_stress)
    pred_stress_new = pd.DataFrame(pred_stress_new)
    pred_stress.columns = pred_stress_new.columns = [nm2]

    return pred_stress_new


def stress_calc_new_data(historic_data, new_data,  heat_stress=True, max_yield= 200, min_yield= 20, significant_alpha= 0.05):

    new_data['ID'] = range(new_data.shape[0])
    new_data['ID'] = 'ID_' + new_data['ID'].map(str)
    new_data['irrigation'].loc[new_data['irrigation']=='DRY'] ='RAIN'
    new_data['wsy_c'] = new_data['irrigation']
    

    new_data_temp = new_data[['tempmaxPlanting-VE', 'tempmaxVE-V6', 'tempmaxV6-VT', 'tempmaxVT-R2', 'tempmaxR2-R4', 'tempmaxR4-R6', 'tempmaxR6-Harvest',
            'tempavgPlanting-VE', 'tempavgVE-V6', 'tempavgV6-VT', 'tempavgVT-R2', 'tempavgR2-R4', 'tempavgR4-R6', 'tempavgR6-Harvest','ID']]
    new_data_temp = new_data_temp.dropna()
    new_data_temp.index = range(new_data_temp.shape[0])
    new_data_id = new_data_temp[['ID']]
    historic_data['ygsmn'] = historic_data['adjusted_ygsmn']

    eame_data_temp = historic_data[['tempmaxPlanting-VE', 'tempmaxVE-V6', 'tempmaxV6-VT', 'tempmaxVT-R2', 'tempmaxR2-R4', 'tempmaxR4-R6', 'tempmaxR6-Harvest',
        'tempavgPlanting-VE', 'tempavgVE-V6', 'tempavgV6-VT', 'tempavgVT-R2', 'tempavgR2-R4', 'tempavgR4-R6', 'tempavgR6-Harvest','ygsmn']]


    new_stress_df = pd.DataFrame()
    n = eame_data_temp.shape[1] - 1

    for i in range(n):
        stress = stress_calc(i,eame_data_temp, new_data_temp, heat_stress=True)
        new_stress_df = pd.concat([new_stress_df, stress], axis=1)

    new_stress_df = 1 - new_stress_df

    new_stress_df.index = new_data_id.index
    dta = pd.concat([new_data_id, new_stress_df], axis=1)

    dta = pd.merge(new_data, dta, on=['ID'], how='left')
    dta = dta.drop_duplicates()

    return dta



def load_corn_data(ap_data_sector = 'CORN_EAME_SUMMER'):

    import pkg_resources

    if ap_data_sector == 'CORN_EAME_SUMMER':
        stream = pkg_resources.resource_stream(__name__, 'data/eame_corn.csv')
        
    if ap_data_sector == 'CORN_NA_SUMMER':
        stream = pkg_resources.resource_stream(__name__, 'data/na_corn.csv')
    
    return pd.read_csv(stream)

def load_test_data(ap_data_sector = 'CORN_EAME_SUMMER'):

    import pkg_resources
    
    if ap_data_sector == 'CORN_EAME_SUMMER':
        stream = pkg_resources.resource_stream(__name__, 'data/test_data_eame_corn.csv')
        
    if ap_data_sector == 'CORN_NA_SUMMER':
        stream = pkg_resources.resource_stream(__name__, 'data/test_data_na_corn.csv')

    if ap_data_sector == 'SUNFLOWER_EAME_SUMMER':
        stream = pkg_resources.resource_stream(__name__, 'data/test_data_eame_sunflower.csv')    
    
    return pd.read_csv(stream)


def env_preprocessing(env, unique_id='trial_id', return_y=True, trait_column='OIL_P', longitude ='longitude',latitude='latitude',
                      use_soil_properties=False, use_irrigation=False, irrigation='irrigation'):


    nms = np.array(list(env.columns))
    ar= np.where(nms == 'WaterUseEfficiency')
    idex= ar[0][0] + 1
    nm0 = nms[:idex]

    if use_soil_properties == False:
        nm_remove = list(nm0) + ['SandContent', 'SiltContent', 'ClayContent', 'pHwater', 'CationExchangeCapacity', 'CoarseFragmentContent', 'CalciumCarbonateContent', 'OrganicMatterContent', 'BulkDensity', 'RootDepthConstraint', 'AvailableWaterAtFieldCapacity', 'Texture', 'HydraulicConductivityAtSaturation']
        env2 = env.drop(nm_remove, axis=1)
    else:
        env2 = env.drop(nm0, axis=1)

    nms = list(env2.columns)
    remove = []
    for nm in nms:
        if '30DBP-Planting' in nm:
            remove.append(nm)
    env2 = env2.drop(remove, axis=1)

    add_columns = [longitude, latitude, 'GrainYieldPotential','GrainYieldSolarPotential', 'GrainYieldAttainable',unique_id]
    env2[add_columns] = env[add_columns]

    if trait_column != 'NONE':
        env2[trait_column] = env[trait_column]

    if use_irrigation==False:
        if use_soil_properties == True:
            env2 = env2.groupby([unique_id, 'Texture']).mean()
        else:
            env2 = env2.groupby([unique_id]).mean()
    else:
        env2['irrigation'] = env[irrigation]
        if use_soil_properties == True:
            env2 = env2.groupby([unique_id, 'Texture','irrigation']).mean()
        else:
            env2 = env2.groupby([unique_id, 'irrigation']).mean()

    env2 = env2.reset_index()
    env2.index = range(env2.shape[0])
    if return_y==True:
        env2 = env2.dropna(subset=[trait_column])
        env2.index = range(env2.shape[0])
        dt_y = env2[[trait_column]]
        return env2, dt_y
    else:
        return env2

def env_sel_weight(dta, unique_id='trial_id', select_variables=True, alpha= 0.05, trait_column = 'OIL_P', assign_weights=False,
                   longitude ='longitude',latitude='latitude', use_soil_properties=False, use_irrigation=False, irrigation='irrigation'):

    dta = dta.dropna(subset=['Texture'])
    dta.index= range(dta.shape[0])
    from sklearn.decomposition import PCA
    from sklearn.model_selection import train_test_split
    from sklearn.linear_model import LinearRegression as LR
    from scipy.stats import pearsonr
    from sklearn.preprocessing import MinMaxScaler

    if ((select_variables == True)|(assign_weights == True)):

        return_y = True
        dta, dt_y = env_preprocessing(dta, unique_id= unique_id, return_y=return_y, trait_column=trait_column, longitude =longitude,latitude=latitude,
                                   use_soil_properties= use_soil_properties, use_irrigation= use_irrigation, irrigation=irrigation)
        dt_x = dta.drop([unique_id, trait_column], axis=1)
        dt_x = pd.get_dummies(dt_x)
        dt_x = dt_x.loc[:,dt_x.std(axis=0)>0]
        dt_x2 = pd.DataFrame(MinMaxScaler().fit_transform(dt_x))
        
        try:
            pcaA = PCA(n_components=0.95).fit(dt_x2)
            dt_x3 = pd.DataFrame(pcaA.transform(dt_x2))
        except:
            pcaA = PCA(n_components= min(50, dt_x2.shape[0])).fit(dt_x2)
            dt_x3 = pd.DataFrame(pcaA.transform(dt_x2))

        r2A = []
        psA= []
        nvar = dt_x3.shape[1]

        for i in range(nvar):

            fac_x2 = pd.DataFrame(dt_x3.iloc[:,i])
            r2s = []
            ps = []
            for k in range(20):
                fea_tr, fea_ts, y_tr, y_ts = train_test_split(fac_x2, dt_y, test_size=0.3, random_state = k*1000)
                lrm = LR().fit(fea_tr, y_tr)
                lr_pred = lrm.predict(fea_ts)

                fea_pred= pd.DataFrame(lr_pred)
                fea_pred.index = y_ts.index
                fea_pred[trait_column]= y_ts
                r2=  pearsonr(lr_pred.ravel(), y_ts[trait_column])[0]
                r2s.append(r2)
                p=  pearsonr(lr_pred.ravel(), y_ts[trait_column])[1]
                ps.append(p)

            mn = pd.DataFrame(r2s).median()
            r2A.append(mn)
            pm = pd.DataFrame(ps).median()
            psA.append(pm)

        r2A = pd.DataFrame(r2A).round(4)
        psA = pd.DataFrame(psA).round(4)
        r2A.rename(columns={0:'weights'}, inplace=True)


        if select_variables == True:
            if (sum(psA[0]<=alpha) <= 5):
                psA2 = psA.sort_values([0])
                alpha = psA2[0].iloc[4]
            r2A['weights'].loc[psA[0]>alpha]  = 0
            
        if assign_weights == False:
            r2A['weights'].loc[r2A['weights']>0]  = 1

        for i in range(nvar):
            dt_x3.iloc[:,i] = dt_x3.iloc[:,i]*r2A['weights'][i]
        dt_x3 = dt_x3.loc[:,dt_x3.std(axis=0)>0]


    else:
        return_y = False
        dta = env_preprocessing(dta,unique_id=unique_id,return_y=return_y, trait_column=trait_column, longitude =longitude,latitude=latitude,
                                   use_irrigation= use_irrigation, irrigation=irrigation)
        if trait_column == 'NONE':
            dt_x3 = dta.drop([unique_id], axis=1)
        else:
            dt_x3 = dta.drop([unique_id, trait_column], axis=1)

    return  dta, dt_x3

def optimal_K(data, min_k= 3, max_k = 15):

    from kneed import DataGenerator, KneeLocator
    from sklearn.cluster import KMeans

    ks = range(1, max_k+1)
    sse = []
    for k in ks:
        km = KMeans(n_clusters=k, n_init=10, max_iter=500, init='k-means++')
        km = km.fit(data)
        sse.append(km.inertia_)

    kneedle =KneeLocator(ks, sse,  curve="convex", direction="decreasing")
    oc = kneedle.elbow

    if oc<= min_k:
        oc = min_k
    if oc>= max_k:
        oc = max_k

    return oc


def som_dimensions(n_cluster=15):

    diff = 20
    for d1 in range(n_cluster):
        dm1 = d1+1

        if n_cluster%dm1 == 0:
            dm2 = n_cluster/dm1
            diff0 = abs(dm2 - dm1)

            if diff0<diff:
                dm1_f = dm1

            diff = diff0
    dm2_f = int(n_cluster/dm1_f)
    return dm1_f, dm2_f

def som_clustering(data_x, n_cluster, data_all, trait_column = 'OIL_P', return_label=True):

    som_shape = som_dimensions(n_cluster)

    def som_fn(space):
        sig= space['sig']
        learning_rate = space['learning_rate']
        val = MiniSom(som_shape[0], som_shape[1], data_x.shape[1], sigma= sig, learning_rate = learning_rate).quantization_error(data_x)
        return{'loss':val, 'status':STATUS_OK}

    from hyperopt import Trials, STATUS_OK, fmin, tpe, hp
    space = {
        'sig': hp.uniform('sig', 0.05, 2.5),
        'learning_rate':hp.uniform('learning_rate', 0.001, 3)
    }



    trials = Trials()
    best= fmin(fn= som_fn, space=space, algo=tpe.suggest, max_evals=30, trials=trials)


    som = MiniSom(som_shape[0], som_shape[1], data_x.shape[1], sigma=best['sig'], neighborhood_function='gaussian',learning_rate=best['learning_rate'])
    som.pca_weights_init(data_x)

    max_iter = 1000
    q_error = []
    t_error = []

    for i in range(max_iter):
        rand_i = np.random.randint(len(data_x))
        som.update(data_x[rand_i], som.winner(data_x[rand_i]), i, max_iter)
        q_error.append(som.quantization_error(data_x))
        t_error.append(som.topographic_error(data_x))

    winner_coordinates = np.array([som.winner(x) for x in data_x]).T
    cluster_index = np.ravel_multi_index(winner_coordinates, som_shape)

    data_all['cluster_SOM'] = cluster_index

    if trait_column =='NONE':
        dta = data_all
    else:
        ts = data_all.groupby(['cluster_SOM'])[trait_column].mean()
        ts = pd.DataFrame(ts)
        ts = ts.reset_index()
        ts.sort_values([trait_column])
        ts = ts.sort_values([trait_column])
        nm = trait_column + '_Level_SOM'
        ts[nm] = range(ts.shape[0])
        ts[nm] = ts[nm] + 1

        dta = pd.merge(data_all, ts[['cluster_SOM',nm]], on=['cluster_SOM'])
        dta = dta.drop(['cluster_SOM'], axis=1)
        dta = dta.drop_duplicates()

    if return_label==True:
        return cluster_index
    else:
        return dta

def som_clustering_best(data_x, n_cluster, data_all, trait_column = 'OIL_P', rep = 2):

    clusters_df = pd.DataFrame()
    scores= []

    for i in range(rep):

        lb = som_clustering(data_x, n_cluster, data_all, trait_column =trait_column , return_label=True)
        if len(np.unique(lb))==1:
            score = -1
        else:
            score = silhouette_score(data_x, lb)

        clusters_df = pd.concat([clusters_df, pd.DataFrame(lb)], axis=1)
        scores.append(score)
        print('completion: ' + str((i+1)*100/rep) + '%')

    scores_df = pd.DataFrame(scores)
    ind = scores_df[0].argmax(axis=0)
    best_cluster = clusters_df.iloc[:,ind]

    data_all['EC_SOM'] = best_cluster
    if trait_column =='NONE':
        dta = data_all
    else:
        ts = data_all.groupby(['EC_SOM'])[trait_column].mean()
        ts = pd.DataFrame(ts)
        ts = ts.reset_index()
        ts.sort_values([trait_column])
        ts = ts.sort_values([trait_column])
        nm = trait_column + '_Level_EC'
        ts[nm] = range(ts.shape[0])
        ts[nm] = ts[nm] + 1

        dta = pd.merge(data_all, ts[['EC_SOM',nm]], on=['EC_SOM'])
        dta = dta.drop(['EC_SOM'], axis=1)
        dta.rename(columns={nm:'EC_SOM'}, inplace=True)
        dta = dta.drop_duplicates()
    return dta

def hierachical_clustering(data_x, n_cluster, data_all, trait_column = 'OIL_P'):
    hc = AgglomerativeClustering(n_clusters= n_cluster).fit(data_x).labels_
    data_all['EC_HC'] = hc

    if trait_column =='NONE':
        dta = data_all
    else:
        ts = data_all.groupby(['EC_HC'])[trait_column].mean()
        ts = pd.DataFrame(ts)
        ts = ts.reset_index()
        ts.sort_values([trait_column])
        ts = ts.sort_values([trait_column])
        nm = trait_column + '_Level_EC'
        ts[nm] = range(ts.shape[0])
        ts[nm] = ts[nm] + 1

        dta = pd.merge(data_all, ts[['EC_HC',nm]], on=['EC_HC'])
        dta = dta.drop(['EC_HC'], axis=1)
        dta.rename(columns={nm:'EC_HC'}, inplace=True)
        dta = dta.drop_duplicates()

    return dta

def distinct_cluster(dta, cluster1 = 'EC_HC', cluster2 = 'EC_SOM', min_number_per_cluster = 100):

    ts = pd.DataFrame(dta.groupby([cluster1, cluster2]).size())
    ts.rename(columns={0:'count'},inplace=True)
    ts = ts.reset_index()

    ts2= ts.groupby([cluster1])['count'].max()
    ts2 = ts2.reset_index()
    ts = pd.merge(ts, ts2, on=[cluster1,'count'])

    ts2= ts.groupby([cluster2])['count'].max()
    ts2 = ts2.reset_index()
    ts = pd.merge(ts, ts2, on=[cluster2,'count'])
    ts = ts.loc[ts['count']>=min_number_per_cluster]
    ts = ts.drop(['count'], axis=1)

    if ts.shape[0]>1:
        dtsm = pd.merge(dta, ts, on=[cluster1, cluster2])
        dtsm= dtsm.drop_duplicates()
        dtsm = dtsm.drop([cluster1], axis=1)

        ts = pd.DataFrame(dtsm[cluster2].unique())
        ts.columns = [cluster2]
        ts= ts.sort_values([cluster2])
        ts['EC_distinct'] = range(ts.shape[0])
        ts['EC_distinct'] = ts['EC_distinct'] + 1
        
        dtsm = pd.merge(dtsm, ts, on=[cluster2])
        dtsm = dtsm.drop_duplicates()
        dtsm = dtsm.drop([cluster2], axis=1)
        return dtsm
    else:
        return 'No distinct environment clusters, suggest to decrease min_number_per_cluster or use hierachical_clustering approache'

def env_charact(env_data, all_data,unique_id='trial_id', trait_column='OIL_P', rep=5, n_cluster = 15,  approach = 'distinct_clustering', min_number_per_cluster = 200, return_distinct= True, return_environment_variable= False):


    if n_cluster == 'NONE':
        n_cluster = optimal_K(env_data, min_k= 3, max_k = 15)

    if approach == 'hc_clustering':
        charact_results = hierachical_clustering(env_data, n_cluster, all_data, trait_column =trait_column)
        charact_results['EC_HC'] = 'EC' + charact_results['EC_HC'].map(str)
        
        if return_environment_variable == False:
            charact_results = charact_results[[unique_id, 'EC_HC']]


    elif approach == 'som_clustering':
        env_data = env_data.to_numpy()
        charact_results =  som_clustering_best(env_data, n_cluster, all_data, trait_column =trait_column, rep = rep)
        charact_results['EC_SOM'] = 'EC' + charact_results['EC_SOM'].map(str)
        
        if return_environment_variable == False:
            charact_results = charact_results[[unique_id, 'EC_SOM']]

    elif approach == 'distinct_clustering':
        charact_results1 = hierachical_clustering(env_data, n_cluster, all_data, trait_column =trait_column)
        charact_results1['EC_HC'] = 'EC' + charact_results1['EC_HC'].map(str)

        env_data = env_data.to_numpy()
        charact_results2 =  som_clustering_best(env_data, n_cluster, all_data, trait_column =trait_column, rep =rep)
        charact_results2['EC_SOM'] = 'EC' + charact_results2['EC_SOM'].map(str)

        all_results = pd.merge(charact_results1, charact_results2[[unique_id,'EC_SOM']], on=[unique_id])
        charact_results = distinct_cluster(all_results, cluster1 = 'EC_HC', cluster2 = 'EC_SOM', min_number_per_cluster = min_number_per_cluster)
        charact_results['EC_distinct'] = 'EC' + charact_results['EC_distinct'].map(str)
        all_results = pd.merge(all_results, charact_results[[unique_id, 'EC_distinct']], on=[unique_id],how='left')
         
        
        if return_environment_variable == False:
            all_results = all_results[[unique_id, 'EC_distinct','EC_HC','EC_SOM']]
            charact_results = charact_results[[unique_id, 'EC_distinct']]
    
        
    if  return_distinct== True:
        return charact_results
    elif ((return_distinct == 'Both')&(approach=='distinct_clustering')):
        return all_results, charact_results
    else:
        return charact_results


def env_clustering(tri_info, year='year', plant_date = 'planting_date', harvest_date= 'harvest_date', ap_data_sector='SUNFLOWER_EAME_SUMMER',
                   longitude='longitude', latitude='latitude', irrigation='NONE',gmstp='gmstp',  unique_id='trial_id',
                   select_variables=True,  alpha= 0.05, assign_weights=False, trait_column = 'OIL_P',
                   use_soil_properties=False, use_irrigation=False,
                   n_cluster = 15, approach = 'distinct_clustering', rep=5, min_number_per_cluster = 200, 
                   return_distinct= True, return_environment_variable= False):
    
    if ((approach == 'hc_clustering')| (approach == 'som_clustering') | (approach == 'distinct_clustering')):
        pass
    else:
        return 'clustering approach is not recognized'

    trial_env =  get_cropFact_env(tri_info, year=year, plant_date = plant_date, harvest_date= harvest_date,
                             ap_data_sector=ap_data_sector, longitude= longitude, latitude=latitude,
                             env_selection=False, irrigation=irrigation, gmstp=gmstp, unique_id='trial_id',pred_stress=False)

    dta, dt_x = env_sel_weight(trial_env,  unique_id=unique_id, select_variables=select_variables, alpha= alpha, trait_column = trait_column, assign_weights=assign_weights,
                               longitude =longitude,latitude=latitude, use_soil_properties=use_soil_properties, use_irrigation=use_irrigation, irrigation=irrigation)
        
    if ((approach == 'hc_clustering')|(approach =='som_clustering')):
        return_distinct = False
        
    if return_distinct =='Both':
        all_re, distinct_re = env_charact(dt_x, dta,unique_id= unique_id, trait_column =trait_column, rep=rep, n_cluster = n_cluster,  approach = approach, min_number_per_cluster = min_number_per_cluster, return_distinct= return_distinct, return_environment_variable= return_environment_variable)   
        return all_re, distinct_re
    
    else:
        clustering_re = env_charact(dt_x, dta, unique_id= unique_id, trait_column =trait_column, rep=rep,n_cluster = n_cluster,  approach = approach, min_number_per_cluster = min_number_per_cluster, return_distinct= return_distinct, return_environment_variable= return_environment_variable)    
        return clustering_re
