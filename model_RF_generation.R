setwd("/Volumes/Simo")
# library(caTools)
# Ilbrary(g_mnet)
library(randomForest)
library(caret)
library(stringr)
library(dplyr)
library(parallel)

input_path = "./dummies/"
output_path = "./models_rf/"

create_model = function(train, test) 
{
  classifier_RF = randomForest(x = as.matrix(train %>% dplyr::select(-positive)),
                               y = train$positive,
                               ntree = 100,
                               #,importance = TRUE
  )

  return(classifier_RF)
}

process_state<-function(state)
{
  cat("state:", state, "\n")
  file_short_rds = paste0(state,".rds")
  file_input = paste0(input_path, file_short_rds)
  
  if (file.exists(file_input)){
    df = readRDS(file=file_input)
    cat("Total:", dim(df), state, "\n")
    
    df[is.na(df)] = 0
    
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
      #cat("Total tested after removing NAs:", dim(df_golden), state, "\n")
      train = df_golden
      test = df_golden
      classifier_RF = create_model(train, test)
      

      #file_output = paste0(output_path, file_short_rds)
      file_output = paste0("/Volumes/Simo/models_rf/", file_short_rds)
      saveRDS(classifier_RF,file_output)
    }
    
  }
}


dir.create(output_path, showWarnings = F)

#Read all the files and extract the versions that we have in that folder
allFiles<-list.files(input_path, pattern="*", full.names=FALSE, recursive=FALSE)
states <- unique(str_extract(allFiles, '.*(?=\\.rds)'))


# load("col_min.R")
# cols_to_dummify <- intersect(cols_to_dummify, col_min)

kk = lapply(states, process_state)
#kk = mclapply(states, process_state)

