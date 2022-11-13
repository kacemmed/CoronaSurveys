library(dplyr)
library(xgboost)
library(parallel)
library(stringr)


#input_path = "/Volumes/Michelangelo/"

#microdata_path = paste0(input_path, "microdata/")

#total_path = paste0(microdata_path, "total/" )

#dummy_path = paste0(microdata_path, "dummies/" )
dummy_path = "./dummies/"

#xgb_path = paste0(microdata_path, "/models_XGB/")
xgb_path = "./models_XGB/"

#xgb_pred_path = paste0(microdata_path, "XGB_pred/" )
xgb_pred_path = "./XGB_pred/"

dir.create(xgb_pred_path, showWarnings = F)


process_state = function(state)
{
  file_short_rds <- paste0(state,".rds")
  file_input <- paste0(dummy_path, file_short_rds)
  
  if (file.exists(file_input)){
    df <- readRDS(file=file_input)
    cat("Total:", dim(df), state, "\n")
    
    golden <- which(df$B10.1==1 & (df$B10c.1==1 | df$B10c.2==1))
    df_test1 <-df[-golden,]
    
     df_test1 <- df_test1 %>%
       dplyr::select(-c("EndDatetime", "fips", "state_code", "state", "B2_14_TEXT", "B2"), 
                     -contains(".NA"), -contains("B10")
                     #, -contains("V1"), -contains("V2")
       )
     
     
     df_test = as.matrix(df_test1)
     #df_test = matrix(as.numeric(df_test), ncol = ncol(df_test))

    
    file_short_model = paste0(xgb_path, state,".model")
    bst <- xgb.load(file_short_model)
    
    prob = predict(bst, df_test)
    prediction <- as.numeric(prob > 0.5)
    df$prob = rep(NA, nrow(df))
    df$pos_XGB = rep(NA, nrow(df))
    
    df[-golden,]$prob = prob
    df[-golden,]$pos_XGB = prediction
    
    df[golden,]$prob = df[golden,]$B10c.1
    df[golden,]$pos_XGB = df[golden,]$B10c.1
    
    df$date = as.Date(substr(df$EndDatetime, 1, 10))
    
    # for (d in dates) {
    #   dfd = df[which((as.Date(d) <= df$date) &  (df$date <= (as.Date(d)+7))),]
    #   pos = sum(df$pos_XGB ==1)
    #   T["states", d] = pos
    # }
    
    df <- df %>%
      dplyr::select(c("state", "date", "prob", "pos_XGB"))
    
    filename_rds <- paste0(xgb_pred_path, state, ".rds")
    saveRDS(df, file=filename_rds)
  }
}

  
  #Read all the files and extract the versions that we have in that folder
  allFiles<-list.files(dummy_path, pattern="*", full.names=FALSE, recursive=FALSE)
  states <- unique(str_extract(allFiles, '.*(?=\\.rds)'))
  # countries <- c("PT", "GR", "IN") #, "BR") # ***
  
  dir.create(xgb_pred_path, showWarnings = F)
  
  # load("col_min.R")
  # cols_to_dummify <- intersect(cols_to_dummify, col_min)
   
  kk = lapply(states, process_state)
  #kk = mclapply(states, process_state)
