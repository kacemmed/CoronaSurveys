setwd("/Volumes/Simo")
library(data.table)
library(dplyr)
library(parallel)

start_date = "2020-04-01"
#start_date = "2021-01-01"
end_date = "2022-06-30"

#input_path = "/Volumes/Michelangelo/"
input_path = "./cmu-dates/"
#microdata_path = paste0(input_path, "microdata/")
microdata_path = "./microdata/"
data_state = "https://raw.githubusercontent.com/kjhealy/fips-codes/master/county_fips_master.csv"



numeric_cols = c("fips", "A1_1", "A1_2", "A1_3", "A1_4", "A1_5", "A5_1", "A5_2", "A5_3", "A2", 
            "B2b", "B10c", "V1", "V2", "D1", "D2", "C14", "C14a", "C14b", "C14c", "B13a", "B10")

character_cols = c("EndDatetime", "B2", "B2_14_TEXT")

col_min <- c(numeric_cols, character_cols)

process_data = function(m)
{
  
  month = substr(m,1,7)
  df = read.csv(paste0(input_path,month , ".csv"), header = TRUE)
  #df <- fread(paste0(input_path,month , ".csv"), data.table = F, header=T)
  for (col in col_min) {
    if (!(col %in% colnames(df))){
      df[[col]] <- NA
    }
  }
  
  
  df <- df %>% 
    dplyr::select(all_of(col_min))
  
  
  # fread is messing up the column types, so I am forcing the class of those used
  for (c in character_cols) {
    df[[c]] <- as.character(df[[c]])
  }
  for (c in numeric_cols) {
    df[[c]] <- as.numeric(df[[c]])
  }
  
  dir.create(paste0(microdata_path, month,"/"), showWarnings = F)
  
  
  df <- df[!(is.na(df$fips)),]
  
  df$state_code = df$fips %/% 1000
  
  
  
  code_list <- unique(df$state_code)
  
  
  for (st in code_list) {
    # cat(a3b, " ")
    
    dfb = df[which(df$state_code == st),]
    
    state = table_states$state_name[which(table_states$state == st)][1]
    
    if (!is.na(state))
    {
      # state = "Outside USA"
    
    dfb$state = state
    
    cat(state, "\n")
    
    # country_file <- paste0(date_dir, iso2, ".csv") # ***
    # fwrite(dfb, file=country_file) #, row.names = FALSE) # ***
    
    state_file <- paste0(microdata_path, month, "/", state, ".rds")
    saveRDS(dfb, file=state_file)
    }
  }
  
  
}


dir.create(microdata_path, showWarnings = F)
table_states = fread(data_state, data.table = F)

table_states = table_states %>% 
  dplyr::select(c("state", "state_name"))



dates = seq(as.Date(start_date), as.Date(end_date), by="month")
kk = lapply(as.character(as.Date(dates)), process_data)
#kk = mclapply(as.character(as.Date(dates)), process_data)

