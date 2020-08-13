# easyXgboost
easyXgboost : An easy way to perform xgboost analysis.

## How To easyXgboost

This R package was designed for translational and research data. So, a standard format has been established to insure datasets compatiblity.
All dataset must contain these features : 
  - primary ID : a numeric feature with unique ID for each observation (=rows)
  - patient ID : a numeric feature
  - timepoint : a numeric feature (if not a longitudinal study, create a timepoint feature and fill with "0"). This feature is not used by easyXgboost 
  - stimulation : a categorical feature (if stimulation feature is not required, create a stimulation feature and fill with "unstim").
  - cohort : a numeric feature. This feature is not used by easyXgboost as cross validation is perform independently.
  - outcome : the outcome features that must be studied for the dataset
  - valid : a logical feature which specify the observations to include ("1") or to exclude ("0")
  

easyXgboost:::xgboost_initialize() : let you select study dataset (csv/xls format) and generates 2 configuration files (xgboost_metadata.csv, xgboost_grid_parameters.csv)


Now, you just have to complete both configuration files (/"project folder"/easyXgboost/attachments/).


easyXgboost:::run_analysis() : 
  - Import parameters : recall parameters saved in configuration files.
  - Prepare dataset : clean dataset, convert binary features into compatible format (levels : "1","2"), One-Hot-Encoding categorical features, impute data.
  - Xgboost analysis : perform multiple cross-validated xgboost analysis and autotuning of the model 
  - Xgboost explain : explain xgboost models
  - Report : print pdf report of xgboost models
