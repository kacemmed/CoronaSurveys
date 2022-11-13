setwd("/Volumes/Simo")
library(dplyr)
library(parallel)
library(data.table)

#input_path = "/Volumes/Michelangelo/"

#microdata_path = paste0(input_path, "microdata/")
microdata_path = "./microdata/"

#total_path = paste0(microdata_path, "total/" )
total_path = "./total/"

data_state = "https://raw.githubusercontent.com/kjhealy/fips-codes/master/county_fips_master.csv"



process_state <- function(state, dates)
{
  dfTotal <- data.frame()
  for (m in dates) {
    month = substr(m,1,7)
    filename <- paste0(microdata_path, month, "/", state, ".rds")
    if (file.exists(filename)) {
      df <- readRDS(file=filename)
      dfTotal <- dplyr::bind_rows(dfTotal,df)
    }
  }
  if (nrow(dfTotal) > 0 ) {
    cat("\n Total:", "dim:", dim(dfTotal), state, "\n")
    
    filename <- paste0(total_path, state, ".rds")
    saveRDS(dfTotal, file=filename)
  } else {
    cat("*** State with empty data:", dim(dfTotal), state, "\n")
  }
}



dir.create(total_path, showWarnings = F)
table_states = fread(data_state, data.table = F, header=T)

dates <- as.character(seq(as.Date(start_date), as.Date(end_date), by="month"))

#states = c(unique(table_states$state_name), "Outside USA")
states = unique(table_states$state_name)

#kk = lapply(states, process_state, dates)
kk = mclapply(states, process_state, dates)
