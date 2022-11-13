setwd("/Volumes/Simo")
# library(caTools)
# library(glmnet)
library(xgboost)
library(caret)
library(dplyr)
library(parallel)
library(tidyverse)


#input_path = "/Volumes/Michelangelo/"

#microdata_path = paste0(input_path, "microdata/")

#total_path = paste0(microdata_path, "total/" )

#dummy_path = paste0(microdata_path, "dummies/" )
dummy_path = "./dummies/"

#xgb_path = paste0(microdata_path, "/models_XGB/")
xgb_path = "./models_XGB/"

create_model <- function(train, test) {
  cat("Unique",unique(train$positive),"\n")
  data = as.matrix(train %>% dplyr::select(-positive))
  # data = matrix(as.numeric(data), ncol = ncol(data))
  classifier_Xgboost = xgboost(data = data,
                               label = train$positive,
                               # eta = 0.1,
                               # max_depth = 15,
                               nround=15,
                               # subsample = 0.5,
                               # colsample_bytree = 0.5,
                               # seed = 1,
                               eval_metric = "logloss",
                               objective = "binary:logistic",
                               # num_class = 12,
                               # nthread = 3
  )
  
  return(classifier_Xgboost)
}


process_state = function(state) {
  cat("state:", state, "\n")
  
  # file_short_csv <- paste0(state,".csv")
  file_short_rds = paste0(state,".rds")
  file_input = paste0(dummy_path, file_short_rds)
  
  if (file.exists(file_input)){
    df = readRDS(file=file_input)
    cat("Total:", dim(df), state, "\n")
    
    df [is.na(df)] = 0
    
    golden = which(df$B10.1==1 & (df$B10c.1==1 | df$B10c.2==1))
    df_golden = df[golden,]
    
    df_golden$positive <- df_golden$B10c.1 
    
    df_golden = df_golden %>%
      dplyr::select(-c("EndDatetime", "fips", "state_code", "state", "B2_14_TEXT", "B2"), 
                    -contains(".NA"), -contains("B10")
                    #, -contains("V1"), -contains("V2")
      )
    
    cat("Total tested:", dim(df_golden), state, "\n")
    
    if (nrow(df_golden)>0 & length(unique(df_golden$positive))>1) {
      #df_golden = df_golden[complete.cases(df_golden),]
      train = df_golden
      test = df_golden
      classifier_Xgboost = create_model(train, test)
      
      file_short_model = paste0(state,".model")
      file_output = paste0(xgb_path, file_short_model)
      xgb.save(classifier_Xgboost, file_output)
    }
    
  }
}


#--------------------------------main

dir.create(xgb_path, showWarnings = F)

#Read all the files and extract the versions that we have in that folder
allFiles<-list.files(dummy_path, pattern="*", full.names=FALSE, recursive=FALSE)
states <- unique(str_extract(allFiles, '.*(?=\\.rds)'))


# load("col_min.R")
# cols_to_dummify <- intersect(cols_to_dummify, col_min)

kk = lapply(states, process_state)
#kk = mclapply(states, process_state)
