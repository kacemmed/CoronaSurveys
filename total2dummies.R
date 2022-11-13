setwd("/Volumes/Simo")
library(dplyr)
library(nnet)
library(parallel)
library(stringr)

#start_date = "2020-04-01"
#end_date = "2022-06-30"

#input_path = "/Volumes/Michelangelo/"
input_path = "./"

#microdata_path = paste0(input_path, "microdata/")

#total_path = paste0(microdata_path, "total/" )
total_path = "./total/"

#dummy_path = paste0(microdata_path, "dummies/" )
dummy_path = "./dummies/"

dir.create(dummy_path, showWarnings = F)

source(paste0(input_path, "RemoveOutliers.R"))

cols_to_dummify = c("A1_1", "A1_2", "A1_3", "A1_4", "A1_5", 
                 "B10c", "V1", "V2", "D1", "D2", "C14", "C14a", "C14b", "C14c", "B13a", "B10")

Create_Dummies <- function(df, cols_to_dummify, state) {

  
  #Transform categorical variables into binaries, then remove the categorical columns(the original ones).
  for (i in cols_to_dummify){
    # cat("* Column:", i, iso2, "\n")
    # if(i %in% colnames(df) & (sum(is.na(df[[i]])) == 0) ){
    df[[i]][which(is.na(df[[i]]))] <- "NA"
    if(length(unique(df[[i]])) <= 21){ # In case the column takes many values
      dfcol<- class.ind(as.factor(df[[i]]))
      colnames(dfcol)<-paste0(i,".",colnames(dfcol))
      df <- cbind(df,dfcol)
      df[[i]]<-NULL
    } else {
      cat("*** Column has more than 10 values:", i, state, "\n")
    }
  }
  for (i in 1:20)
  {
    val = paste0(",",as.character(i),",")
    dfcol = grepl(val, paste0(",",df$B2,","), fixed = TRUE)
    df[paste0("B2.",as.character(i))] = as.numeric(dfcol)
    
  }
  return(df)
}

process_state <- function(state, cols_to_dummify)
{
  cat("Country:", state, "\n")
  
  filein_short <- paste0(state, ".rds")
  filein <- paste0(total_path, filein_short)
  if (file.exists(filein)){
    df <- readRDS(file=filein)
    cat("Total:", dim(df), state, "\n")
    
    cols_to_dummify <- intersect(cols_to_dummify, colnames(df))
    cat("Columns to dummify:", cols_to_dummify, state, "\n")
    
    try(df <- RemoveOutliersBeforeDummification(df), silent = F)
    cat("After first outlier removal:", dim(df),  state,"\n")
    
    df <- Create_Dummies(df, cols_to_dummify, state)
    cat("After create dummies:", dim(df), state, "\n")
    
    # try(df <-RemoveBinaryOutliers(df, cols_to_dummify), silent = F)
    # cat("After removing binary outliers:", dim(df), state, "\n")
    
    # filename_csv <- paste0(dummy_path, state, ".csv") # ***
    # fwrite(df, file=filename_csv) # ***
    
    filename_rds <- paste0(dummy_path, state, ".rds")
    saveRDS(df, file=filename_rds)
  }
}


cat("*** total2dummies\n")

#Read all the files and extract the versions that we have in that folder
allFiles<-list.files(total_path, pattern="*", full.names=FALSE, recursive=FALSE)
states <- unique(str_extract(allFiles, '.*(?=\\.rds)'))
# countries <- c("PT", "GR", "IN") #, "BR") # ***

dir.create(total_path, showWarnings = F)

# load("col_min.R")
# cols_to_dummify <- intersect(cols_to_dummify, col_min)

#kk = lapply(states, process_state, cols_to_dummify)
kk = mclapply(states, process_state, cols_to_dummify)

