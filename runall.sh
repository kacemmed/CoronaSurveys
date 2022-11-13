#! bash

export MC_CORES=50

Rscript dates2microdata.R

Rscript micro2total.R

Rscript total2dummies.R

Rscript model_Xgboost_generation.R

Rscript dummies2agt.R


