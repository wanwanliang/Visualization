Stress and Environment Characterization (SEC) python package is developed to automate environment extraction and stress/environment characterization to support GxE analysis. 
The package includes 4 functions: 1) extract environment variables from cropFact API, 
				  2) conduct environment variable selection (trait value such as YGSMN need to be provided), 
				  3) predict Drought_heat stress level for NA Corn and EAME Corn trials based on historic data, 
			          and 4) conduct environment characterization/clustering 


Prerequisite 
Python pacakges: 1) asyncio, aiohttp,  nest_asyncio, pandas, numpy, scikit-learn, statsmodels, MiniSom, kneed, hyperopt


use codes in https://git.syngentaaws.org/christopher.basten/gsgxe/-/tree/wanwan/python/SEC/test_code for more details on the paramters and how to excute main functions
