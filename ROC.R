setwd("/Volumes/Simo")
library(stringr)
library(dplyr)
library(parallel)
library(randomForest)
library(xgboost)
library(data.table)
library(e1071)
library(neuralnet)
library(ROCR)



comp_path = "./Comparaison/"




process_state<-function(state)
{
  state_path = paste0(comp_path, state, "/")
  
  cat("state:", state, "\n")

  
 
    
  dfTotal <- data.frame()
    
    for (i in 1:50){
      file_input_i = paste0(state_path, i, "/results.csv")
      df_i = read.csv(file=file_input_i, header = TRUE)
      dfTotal <- dplyr::bind_rows(dfTotal,df_i)
    }
  
      pred__RF = prediction(dfTotal$pos_XGB, dfTotal$positive)
      
      ROC = performance(pred__RF,"sens","fpr")
    
      plot(ROC, xlab="",col='red', main="courbes ROC")
      
      perf = performance(pred__RF,"auc")
      (AUC=round(unlist(perf@y.values),4) )
      cat("auc for", state, " = ", AUC)
      
      
      
      
      
      
    
    

}



dir.create(comp_path, showWarnings = F)


allFiles = list.files(dummy_path, pattern="*", full.names=FALSE, recursive=FALSE)
states = unique(str_extract(allFiles, '.*(?=\\.rds)'))
states = "California"

kk = lapply(states, process_state)
#kk = mclapply(states, process_state)
