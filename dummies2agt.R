setwd("/Volumes/Simo")
library(dplyr)
library(xgboost)
library(stringr)
library(data.table)
library(parallel)
library(randomForest)
library(e1071)
library(neuralnet)

#input_path = "/Volumes/Michelangelo/"

#microdata_path = paste0(input_path, "microdata/")

#dummy_path = paste0(microdata_path, "dummies/" )
dummy_path = "./dummies/"

#xgb_path = paste0(microdata_path, "models_XGB/")
xgb_path = "./models_XGB/"

#aggreg_path = paste0(microdata_path, "aggregates/")
aggreg_path = "./aggregates/"

models_rf_path = "./models_rf/"

models_glm_path = "./models_glm/"

models_svm_path = "./models_svm/"

models_nn_path = "./models_nn/"


start_date = "2020-04-01"
end_date = "2022-06-30"


min_responses = 100
# values are aggregated at least by this number of days

cols_no_aggregate <- c("EndDatetime", "B2","fips", "state_code", "B2b")

cols_to_average <- c("A5_1", "A5_2", "A5_3")

# --------------------------

#smooth_param = 30

# reach <- 71 #53 # 53 is the global average reach, 71 is the Indian average reach
ci_level = 0.95
z = qnorm(ci_level+(1-ci_level)/2)

process_ratio = function(numerator, denominator){
  p_est = pmin(1, numerator/denominator)
  se = sqrt(p_est*(1-p_est))/sqrt(denominator)
  return(list(val=p_est, low=pmax(0,p_est-z*se), high=pmin(1,p_est+z*se), error=z*se, std=se))
}

process_cmu_file = function(dfdf, state) {  
  
  est = process_ratio(dfdf$infected, (dfdf$infected + dfdf$not_infected))
  dfdf$p_infected = est$val
  dfdf$p_infected_CI = est$error
  
  est <- process_ratio(dfdf$pos_RF, dfdf$count)
  dfdf$p_rf <- est$val
  dfdf$p_rf_CI <- est$error
  # 
  # est <- process_ratio(dfdf$pos_RF_symp, dfdf$count)
  # dfdf$p_rf_symp <- est$val
  # dfdf$p_rf_symp_CI <- est$error
  
  est = process_ratio(dfdf$pos_XGB, dfdf$count)
  dfdf$p_XGB = est$val
  dfdf$p_XGB_CI = est$error
  
  est = process_ratio(dfdf$pos_glm, dfdf$count)
  dfdf$p_glm = est$val
  dfdf$p_glm_CI = est$error
  
  est = process_ratio(dfdf$pos_nn, dfdf$count)
  dfdf$p_nn = est$val
  dfdf$p_nn_CI = est$error
  
  est = process_ratio(dfdf$pos_svm, dfdf$count)
  dfdf$p_svm = est$val
  dfdf$p_svm_CI = est$error
  
  # est <- process_ratio(dfdf$pos_XGB_symp, dfdf$count)
  # dfdf$p_XGB_symp <- est$val
  # dfdf$p_XGB_symp_CI <- est$error
  # 
   est = process_ratio(dfdf$cli, dfdf$count)
   dfdf$p_cli = est$val
   dfdf$p_cli_CI = est$error
   # dfdf$p_cli_low = est$low
   # dfdf$p_cli_high = est$high
  
  # est <- process_ratio(dfdf$cli_weight, dfdf$weight)
  # dfdf$p_cli_weight <- est$val
  # dfdf$p_cli_weight_CI <- est$error
  # # dfdf$p_cli_weight_low <- est$low
  # # dfdf$p_cli_weight_high <- est$high
  # 
    # est = process_ratio(dfdf$cliWHO, dfdf$count)
    # dfdf$p_cliWHO = est$val
    # dfdf$p_cliWHO_CI  est$error
  #  # dfdf$p_cliWHO_low = est$low
  #  # dfdf$p_cliWHO_high = est$high
  # est = process_ratio(dfdf$cliWHO_weight, dfdf$weight)
  # dfdf$p_cliWHO_weight = est$val
  # dfdf$p_cliWHO_weight_CI <- est$error
  # # dfdf$p_cliWHO_weight_low <- est$low
  # # dfdf$p_cliWHO_weight_high <- est$high
  # 
  #
#    reach <- (sum(dfdf$cli_local) * sum(dfdf$count)) / (sum(dfdf$cli)*sum(dfdf$count_local))
#    cat("\n", state, " Reach:", reach, "\n")
  # 
   est <- process_ratio(dfdf$cli_local, dfdf$reach)
   dfdf$p_cli_local <- est$val
   dfdf$p_cli_local_CI <- est$error
  # # dfdf$p_cli_local_low <- est$low
  # # dfdf$p_cli_local_high <- est$high
  
  # est <- process_ratio(dfdf$positive_recent, dfdf$test_recent)
  # dfdf$TPR <- est$val
  # dfdf$TPR_CI <- est$error
  # dfdf$TPR_low <- est$low
  # dfdf$TPR_high <- est$high
  
  return(dfdf)
}





ComputePredicates <- function(df, state) {
  
  ml_data = df %>% dplyr::select(-c("EndDatetime", "fips", "state_code", "state", "B2_14_TEXT", "B2"), 
                                 -contains(".NA"), -contains("B10")
                                 #, -contains("V1"), -contains("V2")
  )
  
  ml_data[is.na(ml_data)] = 0
  m_ml_data = as.matrix(ml_data)
  
  # Positive as classified by the Random Forest
  file_short_rds <- paste0(state,".rds")
  file_model <- paste0(models_rf_path, file_short_rds)
  # 
   if (file.exists(file_model)){
     cat("Running RF model for ", state, "\n")
     classifier_RF = readRDS(file=file_model)
     df$pos_RF = predict(classifier_RF, ml_data)
     df$pos_RF = round(df$pos_RF)
     #df$pos_RF = as.numeric(levels(df$pos_RF))[df$pos_RF]
   }
   else {
     cat("No RF model for ", state, "\n")
     df$pos_RF <- 0
   }
  # Correct the values that are tested in latest 14 days and positive or negative
   df$pos_RF[which((df$B10.1==1) & (df$B10c.2 == 1))] = 0
   df$pos_RF[which((df$B10.1==1) & (df$B10c.1==1))] = 1
  
  # Positive as classified by the XGBoost
  file_short_rds = paste0(state,".rds")
  file_short_model = paste0(state,".model")
  file_model <- paste0(xgb_path, file_short_model)
  if (file.exists(file_model)){
    cat("Running XGBoost model for ", state, "\n")
    classifier_XGB = xgb.load(file_model)
    pred = predict(classifier_XGB, m_ml_data)
    df$pos_XGB = round(pred)
    #df$pos_XGB <- as.numeric(levels(df$pos_XGB))[df$pos_XGB]
  }
  else {
    cat("No XGBoost model for ", state, "\n")
    df$pos_XGB = 0
  }
  # Correct the values that are tested in latest 14 days and positive or negative
  df$pos_XGB[which((df$B10.1==1) & (df$B10c.2 == 1))] = 0
  df$pos_XGB[which((df$B10.1==1) & (df$B10c.1==1))] = 1
  
  # Positive as classified by the linear regression
  file_short_rds <- paste0(state,".rds")
  file_model <- paste0(models_glm_path, file_short_rds)
  # 
  if (file.exists(file_model)){
    cat("Running glm model for ", state, "\n")
    classifier_glm = readRDS(file=file_model)
    df$pos_glm = predict(classifier_glm, ml_data, type = "response")
    df$pos_glm = round(df$pos_glm)
    #df$pos_RF = as.numeric(levels(df$pos_RF))[df$pos_RF]
  }
  else {
    cat("No glm model for ", state, "\n")
    df$pos_glm <- 0
  }
  # Correct the values that are tested in latest 14 days and positive or negative
  df$pos_glm[which((df$B10.1==1) & (df$B10c.2 == 1))] = 0
  df$pos_glm[which((df$B10.1==1) & (df$B10c.1==1))] = 1
  
  
  # Positive as classified by the svm
  file_short_rds <- paste0(state,".rds")
  file_model <- paste0(models_svm_path, file_short_rds)
  # 
  if (file.exists(file_model)){
    cat("Running svm model for ", state, "\n")
    classifier_svm = readRDS(file=file_model)
    df$pos_svm = predict(classifier_svm, ml_data)
    #df$pos_svm = round(df$pos_svm)
    df$pos_svm = as.numeric(df$pos_svm)
  }
  else {
    cat("No linear svm model for ", state, "\n")
    df$pos_svm <- 0
  }
  # Correct the values that are tested in latest 14 days and positive or negative
  df$pos_svm[which((df$B10.1==1) & (df$B10c.2 == 1))] = 0
  df$pos_svm[which((df$B10.1==1) & (df$B10c.1==1))] = 1
  
  
  # Positive as classified by the nn
  file_short_rds <- paste0(state,".rds")
  file_model <- paste0(models_nn_path, file_short_rds)
  # 
  if (file.exists(file_model)){
    cat("Running nn model for ", state, "\n")
    classifier_nn = readRDS(file=file_model)
    nn_data = as.matrix(ml_data)
    nn_data = matrix(as.numeric(nn_data), ncol = ncol(nn_data))
    df$pos_nn = predict(classifier_nn, nn_data)
    df$pos_nn = round(df$pos_nn)
  }
  else {
    cat("No nn model for ", state, "\n")
    df$pos_nn <- 0
  }
  # Correct the values that are tested in latest 14 days and positive or negative
  df$pos_nn[which((df$B10.1==1) & (df$B10c.2 == 1))] = 0
  df$pos_nn[which((df$B10.1==1) & (df$B10c.1==1))] = 1
  
  
  
  # # Positive as classified by the XGBoost with symptoms
  # file_short_rds <- paste0(state,".rds")
  # file_model <- paste0(models_XGB_symp_path, file_short_rds)
  # if (file.exists(file_model)){
  #   cat("Running XGBoost model with symptoms for ", state, "\n")
  #   classifier_XGB <- readRDS(file=file_model)
  #   pred = predict(classifier_XGB, as.matrix(ml_data))
  #   df$pos_XGB_symp = round(pred)
  #   # df$pos_XGB_symp = round(predict(classifier_XGB, as.matrix(ml_data)))
  #   # df$pos_XGB_symp <- as.numeric(levels(df$pos_XGB_symp))[df$pos_XGB_symp]
  # }
  # else {
  #   cat("No XGBoost model for ", state, "\n")
  #   df$pos_XGB_symp <- 0
  # }
  
  # To count the number of responses
  df$count = 1
  
  # To count the number of symptomatic responses
  df$symptomatic = 0
  with_symptoms = which(df$B2.1==1 | df$B2.2==1 | df$B2.3==1 | df$B2.4==1 | 
                           df$B2.5==1 | df$B2.6==1 | df$B2.7==1 | df$B2.8==1 | 
                           df$B2.9==1 | df$B2.10==1 | df$B2.11==1 | df$B2.12==1 | df$B2.13==1 | df$B2.14==1 | 
                          df$B2.15==1 | df$B2.16==1 | df$B2.17==1 | df$B2.18==1 | 
                          df$B2.19==1 | df$B2.20==1
  )
  df$symptomatic[with_symptoms] = 1
  
  
  # To count infected
  df$infected = 0
  df$infected[which(df$B13a.1 == 1)] = 1
  df$not_infected = 0
  df$not_infected[which(df$B13a.2 == 1)] = 1
  
  # To count vaccinated
  # "V1 Have you had a COVID-19 vaccination?"
  df$vaccinated <- 0
  df$vaccinated[which(df$V1.1 == 1)] = 1
  # df$vaccinated_weight <- 0
  # df$vaccinated_weight[which(df$vaccinated == 1)] = df$weight[which(df$vaccinated == 1)]
  df$unvaccinated <- 0
  df$unvaccinated[which(df$V1.2 == 1)] <- 1
  # df$vac1dose = 0
  # df$vac1dose[which(df$V1.1 == 1 & df$V2.1 == 1)] = 1
  # df$vac2doses = 0
  # df$vac2doses[which(df$V1.1 == 1 & df$V2.2 == 1)] = 1
  # 
  # COVID-like illness: fever, along with cough or difficulty breathing (recall that 1=yes, 2=no)
  df$cli = 0
  #df$cli[which((df$B2.1 == 1) & ((df$B2.2 == 1) | (df$B2.4 == 1)))] = 1
  df$cli[which((df$B2.1==1) & (df$B2.2==1 | df$B2.3==1 | df$B2.4==1 | 
                           df$B2.5==1 | df$B2.6==1 | df$B2.7==1 | df$B2.8==1 | 
                           df$B2.9==1 | df$B2.10==1 | df$B2.11==1 | df$B2.12==1 | df$B2.13==1 | df$B2.14==1 | 
                          df$B2.15==1 | df$B2.16==1 | df$B2.17==1 | df$B2.18==1 | 
                          df$B2.19==1 | df$B2.20==1))] = 1
  # df$cli_weight <- 0
  # df$cli_weight[which(df$cli == 1)] <- df$weight[which(df$cli == 1)]
  df$cli_vaccinated = 0
  df$cli_vaccinated[which(df$vaccinated == 1 & df$cli == 1)] = 1
  df$cli_unvaccinated = 0
  df$cli_unvaccinated[which(df$unvaccinated == 1 & df$cli == 1)] = 1
  # df$cli_vac1dose = 0
  # df$cli_vac1dose[which(df$vac1dose == 1 & df$cli == 1)] = 1
  # df$cli_vac2doses <- 0
  # df$cli_vac2doses[which(df$vac2doses == 1 & df$cli == 1)] = 1
  # 
  # COVID-like illness for world health organization: fever + cough + fatigue
   # df$cliWHO = 0
   # df$cliWHO[which((df$B2.1 == 1) & (df$B2.2 == 1) & (df$B2.5 == 1) )] = 1
   # df$cliWHO_weight = 0
   # df$cliWHO_weight[which(df$cliWHO == 1)] = df$weight[which(df$cliWHO == 1)]
  
  # # Compute the number of CLI (who is sick with a fever and at least one other symptom)
   df$cli_local = df$A2
   #df$cli_local = 0
   #df$cli_local[which((df$A1_1.1 == 1) & ((df$A1_2.1 == 1) | (df$A1_3.1 == 1) | (df$A1_4.1 == 1) | (df$A1_5.1 == 1)))] = 1
   df$cli_local[which((df$A1_1.2 == 1) | ((df$A1_2.2 == 1) & (df$A1_3.2 == 1) & (df$A1_4.2 == 1) & (df$A1_5.2 == 1)))] = 0
  
  # # To count the number of responses in cli_local_com
   df$count_local = 1
   df$count_local[which(is.na(df$cli_local))] = 0
  # 
  # # Compute reach
   #reach = (sum(df$cli_local, na.rm = TRUE) * sum(df$count)) / (sum(df$cli)*sum(df$count_local))
   #cat("\n", state, " Reach:", reach, "\n")
   #df$reach = 0
   #df$reach[which(df$count_local==1)] = reach
   df$reach = df$A5_1 + df$A5_2 + df$A5_3
  
  # Stringent CLI: Anosmia and (Fever or Muscle Pain or Cough)
  # df$stringent_cli <- 0
  # df$stringent_cli[which((df$B1_10.1 == 1) & ((df$B1_1.1 == 1) | (df$B1_6.1 == 1) | (df$B1_2.1 == 1)))] <- 1
  # df$stringent_cli_vaccinated <- 0
  # df$stringent_cli_vaccinated[which(df$vaccinated == 1 & df$stringent_cli == 1)] <- 1
  # df$stringent_cli_unvaccinated <- 0
  # df$stringent_cli_unvaccinated[which(df$unvaccinated == 1 & df$stringent_cli == 1)] <- 1
  # df$stringent_cli_vac1dose <- 0
  # df$stringent_cli_vac1dose[which(df$vac1dose == 1 & df$stringent_cli == 1)] <- 1
  # df$stringent_cli_vac2doses <- 0
  # df$stringent_cli_vac2doses[which(df$vac2doses == 1 & df$stringent_cli == 1)] <- 1
  
  # Classic CLI: Cough and (Fever or Muscle Pain or Anosmia)
  # df$classic_cli <- 0
  # df$classic_cli[which((df$B1_2.1 == 1) & ((df$B1_1.1 == 1) | (df$B1_6.1 == 1) | (df$B1_10.1 == 1)))] <- 1
  # df$classic_cli_vaccinated <- 0
  # df$classic_cli_vaccinated[which(df$vaccinated == 1 & df$classic_cli == 1)] <- 1
  # df$classic_cli_unvaccinated <- 0
  # df$classic_cli_unvaccinated[which(df$unvaccinated == 1 & df$classic_cli == 1)] <- 1
  # df$classic_cli_vac1dose <- 0
  # df$classic_cli_vac1dose[which(df$vac1dose == 1 & df$classic_cli == 1)] <- 1
  # df$classic_cli_vac2doses <- 0
  # df$classic_cli_vac2doses[which(df$vac2doses == 1 & df$classic_cli == 1)] <- 1
  
  # Broad CLI: Muscle Pain and (Fever or Cough or Anosmia)
  # df$broad_cli <- 0
  # df$broad_cli[which((df$B1_6.1 == 1) & ((df$B1_1.1 == 1) | (df$B1_2.1 == 1) | (df$B1_10.1 == 1)))] <- 1
  # df$broad_cli_vaccinated <- 0
  # df$broad_cli_vaccinated[which(df$vaccinated == 1 & df$broad_cli == 1)] <- 1
  # df$broad_cli_unvaccinated <- 0
  # df$broad_cli_unvaccinated[which(df$unvaccinated == 1 & df$broad_cli == 1)] <- 1
  # df$broad_cli_vac1dose <- 0
  # df$broad_cli_vac1dose[which(df$vac1dose == 1 & df$broad_cli == 1)] <- 1
  # df$broad_cli_vac2doses <- 0
  # df$broad_cli_vac2doses[which(df$vac2doses == 1 & df$broad_cli == 1)] <- 1
  
  # To count the number of people tested in latest 14 days
  df$test_recent = 0
  df$test_recent[which(df$B10.1 == 1 & (df$B10c.1 == 1 | df$B10c.2 == 1))] = 1
  
  # To count tested vaccinated
  df$tested_vaccinated = 0
  df$tested_vaccinated[which(df$vaccinated == 1 & df$test_recent == 1)] = 1
  df$tested_unvaccinated = 0
  df$tested_unvaccinated[which(df$unvaccinated == 1 & df$test_recent == 1)] = 1
  # df$tested_vac1dose = 0
  # df$tested_vac1dose[which(df$vac1dose == 1 & df$tested_recent == 1)] = 1
  # df$tested_vac2doses = 0
  # df$tested_vac2doses[which(df$vac2doses == 1 & df$tested_recent == 1)] = 1
  
  # To count the number of positive tests in latest 14 days
  df$positive_recent = 0
  df$positive_recent[which((df$B10.1 == 1) & (df$B10c.1 == 1))] <- 1
  
  # To count positive vaccinated
  df$positive_vaccinated = 0
  df$positive_vaccinated[which(df$vaccinated == 1 & df$positive_recent == 1)] <- 1
  # df$positive_vaccinated_weight <- 0
  # df$positive_vaccinated_weight[which(df$positive_vaccinated == 1)] <- df$weight[which(df$positive_vaccinated == 1)]
  df$positive_unvaccinated <- 0
  df$positive_unvaccinated[which(df$unvaccinated == 1 & df$positive_recent == 1)] <- 1
  # df$positive_vac1dose <- 0
  # df$positive_vac1dose[which(df$vac1dose == 1 & df$positive_recent == 1)] <- 1
  # df$positive_vac2doses <- 0
  # df$positive_vac2doses[which(df$vac2doses == 1 & df$positive_recent == 1)] <- 1
  
  # Positive with symptoms
  df$positive_symptomatic <- 0
  df$positive_symptomatic[which(df$symptomatic == 1 & df$positive_recent == 1)] <- 1
  
  # # To count pos_RF vaccinated
  # # "V1 Have you had a COVID-19 vaccination?"
  # df$pos_RF_vaccinated <- 0
  # df$pos_RF_vaccinated[which(df$vaccinated == 1 & df$pos_RF == 1)] <- 1
  # df$pos_RF_unvaccinated <- 0
  # df$pos_RF_unvaccinated[which(df$unvaccinated == 1 & df$pos_RF == 1)] <- 1
  # # df$pos_RF_vaccinated_weight <- 0
  # # df$pos_RF_vaccinated_weight[which(df$pos_RF_vaccinated == 1)] <- df$weight[which(df$pos_RF_vaccinated == 1)]
  # df$pos_RF_vac1dose <- 0
  # df$pos_RF_vac1dose[which(df$vac1dose == 1 & df$pos_RF == 1)] <- 1
  # df$pos_RF_vac2doses <- 0
  # df$pos_RF_vac2doses[which(df$vac2doses == 1 & df$pos_RF == 1)] <- 1
  # 
  # # Positive RF with symptoms
  # df$pos_RF_symptomatic <- 0
  # df$pos_RF_symptomatic[which(df$symptomatic == 1 & df$pos_RF == 1)] <- 1
  # 
  # # Positive with each symptom
  # df$positive_Fever_B2.1 = 0
  # df$positive_Fever_B2.1[which(df$B2.1 == 1 & df$positive_recent == 1)] = 1
  # df$positive_Cough_B2.2 = 0
  # df$positive_Cough_B2.2[which(df$B2.2 == 1 & df$positive_recent == 1)] = 1
  # df$positive_shortness_of_breathing_B2.3 = 0
  # df$positive_shortness_of_breathing_B2.3[which(df$B2.3 == 1 & df$positive_recent == 1)] = 1
  # df$positive_Difficulty_breathing_B2.4 = 0
  # df$positive_Difficulty_breathing_B2.4[which(df$B2.4 == 1 & df$positive_recent == 1)] = 1
  # df$positive_Fatigue_B2.5 = 0
  # df$positive_Fatigue_B2.5[which(df$B2.5 == 1 & df$positive_recent == 1)] = 1
  # df$positive_Stuffy_or_runny_nose_B2.20 = 0
  # df$positive_Stuffy_or_runny_nose_B2.20[which(df$B2.20 == 1 & df$positive_recent == 1)] = 1
  # df$positive_Aches_or_muscle_pain_B2.8 = 0
  # df$positive_Aches_or_muscle_pain_B2.8[which(df$B2.8 == 1 & df$positive_recent == 1)] = 1
  # df$positive_Sore_throat_B2.9 = 0
  # df$positive_Sore_throat_B2.9[which(df$B2.9 == 1 & df$positive_recent == 1)] = 1
  # df$positive_Chest_pain_B2.10 = 0
  # df$positive_Chest_pain_B2.10[which(df$B2.10 == 1 & df$positive_recent == 1)] = 1
  # df$positive_Nausea_B2.11 = 0
  # df$positive_Nausea_B2.11[which(df$B2.11 == 1 & df$positive_recent == 1)] = 1
  # df$positive_Loss_of_smell_or_taste_B2.13 = 0
  # df$positive_Loss_of_smell_or_taste_B2.13[which(df$B2.13 == 1 & df$positive_recent == 1)] = 1
  # df$positive_Headache_B2.18 = 0
  # df$positive_Headache_B2.18[which(df$B2.18 == 1 & df$positive_recent == 1)] = 1
  # df$positive_Chills_B2.17 = 0
  # df$positive_Chills_B2.17[which(df$B2.17 == 1 & df$positive_recent == 1)] = 1
  # 
  # # Positive vaccinated with each symptom
  # df$pos_vaccinated_Fever_B2.1 = 0
  # df$pos_vaccinated_Fever_B2.1[which(df$B2.1 == 1 & df$positive_vaccinated == 1)] = 1
  # df$pos_vaccinated_Cough_B2.2 = 0
  # df$pos_vaccinated_Cough_B2.2[which(df$B2.2 == 1 & df$positive_vaccinated == 1)] = 1
  # df$pos_vaccinated_shortness_of_breathing_B2.3 = 0
  # df$pos_vaccinated_shortness_of_breathing_B2.3[which(df$B2.3 == 1 & df$positive_vaccinated == 1)] = 1
  # df$pos_vaccinated_Difficulty_breathing_B2.4 = 0
  # df$pos_vaccinated_Difficulty_breathing_B2.4[which(df$B2.4 == 1 & df$positive_vaccinated == 1)] = 1
  # df$pos_vaccinated_Fatigue_B2.5 = 0
  # df$pos_vaccinated_Fatigue_B2.5[which(df$B2.5 == 1 & df$positive_vaccinated == 1)] = 1
  # df$pos_vaccinated_Stuffy_or_runny_nose_B2.20 = 0
  # df$pos_vaccinated_Stuffy_or_runny_nose_B2.20[which(df$B2.20 == 1 & df$positive_vaccinated == 1)] = 1
  # df$pos_vaccinated_Aches_or_muscle_pain_B2.8 = 0
  # df$pos_vaccinated_Aches_or_muscle_pain_B2.8[which(df$B2.8 == 1 & df$positive_vaccinated == 1)] = 1
  # df$pos_vaccinated_Sore_throat_B2.9 = 0
  # df$pos_vaccinated_Sore_throat_B2.9[which(df$B2.9 == 1 & df$positive_vaccinated == 1)] = 1
  # df$pos_vaccinated_Chest_pain_B2.10 = 0
  # df$pos_vaccinated_Chest_pain_B2.10[which(df$B2.10 == 1 & df$positive_vaccinated == 1)] = 1
  # df$pos_vaccinated_Nausea_B2.11 = 0
  # df$pos_vaccinated_Nausea_B2.11[which(df$B2.11 == 1 & df$positive_vaccinated == 1)] = 1
  # df$pos_vaccinated_Loss_of_smell_or_taste_B2.13 = 0
  # df$pos_vaccinated_Loss_of_smell_or_taste_B2.13[which(df$B2.13 == 1 & df$positive_vaccinated == 1)] = 1
  # df$pos_vaccinated_Headache_B2.18 = 0
  # df$pos_vaccinated_Headache_B2.18[which(df$B2.18 == 1 & df$positive_vaccinated == 1)] = 1
  # df$pos_vaccinated_B2.17 = 0
  # df$pos_vaccinated_B2.17[which(df$B2.17 == 1 & df$positive_vaccinated == 1)] = 1
  # 
  
  
  # # Positive RF with each symptom
  # df$pos_RF_Fever_B1_1 <- 0
  # df$pos_RF_Fever_B1_1[which(df$B1_1.1 == 1 & df$pos_RF == 1)] <- 1
  # df$pos_RF_Cough_B1_2 <- 0
  # df$pos_RF_Cough_B1_2[which(df$B1_2.1 == 1 & df$pos_RF == 1)] <- 1
  # df$pos_RF_Difficulty_breathing_B1_3 <- 0
  # df$pos_RF_Difficulty_breathing_B1_3[which(df$B1_3.1 == 1 & df$pos_RF == 1)] <- 1
  # df$pos_RF_Fatigue_B1_4 <- 0
  # df$pos_RF_Fatigue_B1_4[which(df$B1_4.1 == 1 & df$pos_RF == 1)] <- 1
  # df$pos_RF_Stuffy_or_runny_nose_B1_5 <- 0
  # df$pos_RF_Stuffy_or_runny_nose_B1_5[which(df$B1_5.1 == 1 & df$pos_RF == 1)] <- 1
  # df$pos_RF_Aches_or_muscle_pain_B1_6 <- 0
  # df$pos_RF_Aches_or_muscle_pain_B1_6[which(df$B1_6.1 == 1 & df$pos_RF == 1)] <- 1
  # df$pos_RF_Sore_throat_B1_7 <- 0
  # df$pos_RF_Sore_throat_B1_7[which(df$B1_7.1 == 1 & df$pos_RF == 1)] <- 1
  # df$pos_RF_Chest_pain_B1_8 <- 0
  # df$pos_RF_Chest_pain_B1_8[which(df$B1_8.1 == 1 & df$pos_RF == 1)] <- 1
  # df$pos_RF_Nausea_B1_9 <- 0
  # df$pos_RF_Nausea_B1_9[which(df$B1_9.1 == 1 & df$pos_RF == 1)] <- 1
  # df$pos_RF_Loss_of_smell_or_taste_B1_10 <- 0
  # df$pos_RF_Loss_of_smell_or_taste_B1_10[which(df$B1_10.1 == 1 & df$pos_RF == 1)] <- 1
  # df$pos_RF_Headache_B1_12 <- 0
  # df$pos_RF_Headache_B1_12[which(df$B1_12.1 == 1 & df$pos_RF == 1)] <- 1
  # df$pos_RF_Chills_B1_13 <- 0
  # df$pos_RF_Chills_B1_13[which(df$B1_13.1 == 1 & df$pos_RF == 1)] <- 1
  # 
  # # Positive RF vaccinated with each symptom
  # df$pos_RF_vaccinated_Fever_B1_1 <- 0
  # df$pos_RF_vaccinated_Fever_B1_1[which(df$B1_1.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  # df$pos_RF_vaccinated_Cough_B1_2 <- 0
  # df$pos_RF_vaccinated_Cough_B1_2[which(df$B1_2.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  # df$pos_RF_vaccinated_Difficulty_breathing_B1_3 <- 0
  # df$pos_RF_vaccinated_Difficulty_breathing_B1_3[which(df$B1_3.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  # df$pos_RF_vaccinated_Fatigue_B1_4 <- 0
  # df$pos_RF_vaccinated_Fatigue_B1_4[which(df$B1_4.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  # df$pos_RF_vaccinated_Stuffy_or_runny_nose_B1_5 <- 0
  # df$pos_RF_vaccinated_Stuffy_or_runny_nose_B1_5[which(df$B1_5.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  # df$pos_RF_vaccinated_Aches_or_muscle_pain_B1_6 <- 0
  # df$pos_RF_vaccinated_Aches_or_muscle_pain_B1_6[which(df$B1_6.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  # df$pos_RF_vaccinated_Sore_throat_B1_7 <- 0
  # df$pos_RF_vaccinated_Sore_throat_B1_7[which(df$B1_7.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  # df$pos_RF_vaccinated_Chest_pain_B1_8 <- 0
  # df$pos_RF_vaccinated_Chest_pain_B1_8[which(df$B1_8.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  # df$pos_RF_vaccinated_Nausea_B1_9 <- 0
  # df$pos_RF_vaccinated_Nausea_B1_9[which(df$B1_9.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  # df$pos_RF_vaccinated_Loss_of_smell_or_taste_B1_10 <- 0
  # df$pos_RF_vaccinated_Loss_of_smell_or_taste_B1_10[which(df$B1_10.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  # df$pos_RF_vaccinated_Headache_B1_12 <- 0
  # df$pos_RF_vaccinated_Headache_B1_12[which(df$B1_12.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  # df$pos_RF_vaccinated_Chills_B1_13 <- 0
  # df$pos_RF_vaccinated_Chills_B1_13[which(df$B1_13.1 == 1 & df$pos_RF_vaccinated == 1)] <- 1
  # 
  # 
  # Reorder the columns to place the new ones first
  #df <- subset(df, select=c("count", "symptomatic", "cli", "cli_weight", "cliWHO", "cliWHO_weight", "cli_local", "count_local",
  #                          "test_recent", "positive_recent", "positive_vaccinated", "positive_symptomatic",
  #                          colnames(df)[-c(count, symptomatic, cli, cli_weight, cliWHO, cliWHO_weight, cli_local, count_local,
  #   test_recent, positive_recent, positive_vaccinated, positive_symptomatic)]))
  
  
  #df$sum_A5i = df$A5_1 + df$A5_2 + df$A5_3
  #df$sum_A2 = df$A2
  
  
  return(df)
}




Aggregate_state = function(df) {
  #df = subset(df, select = -c(region_agg, age))
  
  
  df = df %>% group_by(state,date) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    arrange(desc(date))
  # df <- df %>% group_by(ISO2,ISO_3,country_agg) %>% 
  #   slice(seq_len(which.max(cumsum(count) >= 100))) %>%
  #   summarise_if(is.numeric, sum, na.rm = TRUE)
  
  df$days_aggregated = 1
  
  dfTotal = df[which(df$count >= min_responses),]
  
  rows_to_aggregate = which(df$count < min_responses)
  if (length(rows_to_aggregate) > 0) {
    # Aggregate the dates that have less than 100 responses in dfTotal
    nr = nrow(df)
    for (i in rows_to_aggregate) {
      dfa = df[i:nr,]
      dfa = dfa %>% 
        # slice(i:nr) %>% 
        dplyr::slice(seq_len(which.max(cumsum(count) >= min_responses))) # dfa has the slice that add 100 starting at pos i
      dfa$date = dfa$date[1]
      # dfa$count = dfa$count[1]
      dfTotal = rbind(dfTotal,dfa)
    }
    dfTotal = dfTotal %>% group_by(state,date) %>%
      summarise_if(is.numeric, sum, na.rm = TRUE) %>%
      filter(count>=min_responses)
  }
  dfTotal = dfTotal %>%
    arrange(state,date)
  return(dfTotal)
}





process_state <- function(state, allFiles) {
  cat("state:", state, "\n")
  
  
  # for (ver in allFilesVersion) {
  #   cat("Version:", ver, "\n")
  file_short_csv <- paste0(state,".csv")
  file_short_rds <- paste0(state,".rds")
  file_input <- paste0(dummy_path, file_short_rds)
  if (file.exists(file_input)){
    df = readRDS(file=file_input)
    cat("Total:", dim(df), state, "\n")
    

    
    if (nrow(df)>0) {
      # df = df %>% dplyr::select(-"B2b")
      # df = df[complete.cases(df),]
      df = ComputePredicates(df, state)
      cat("After predicates:", dim(df), state, "\n")
      
      # Sort by the time the survey was filled
      df = df[order(df$EndDatetime),]
      df$date <- as.Date(substr(df$EndDatetime,1,10))
      # Remove columns that are not likely to be used and hard to aggregate
      cols_to_remove <- intersect(cols_no_aggregate, colnames(df))
      df <- df %>% dplyr::select(-all_of(cols_to_remove))
      # Remove dummified columns
      df <- df %>% dplyr::select(-contains("."))
      
      # cat("\n", state, " Colnames:", colnames(df), "\n")
      
      
      
      
      # Aggregation at the state level
      df_aux <- Aggregate_state(df)
      if (is.data.frame(df_aux)) {
        df_aux <- process_cmu_file(df_aux, state)
        df_aux <- df_aux[which((df_aux$date >= start_date) & (df_aux$date <= end_date)),]
        fwrite(df_aux, file=paste0(aggreg_path, "state/", file_short_csv))
        # saveRDS(df_aux, file=paste0(aggreg_path, "state/", file_short_rds))
      }
      
#      df$p_sum = df$reach/df$cli_local
      
      cat("* After state:", dim(df_aux), state, "\n")
      
      # # Aggregation per age group
      # df_aux <- Aggregate_Age(df)
      # if (is.data.frame(df_aux)) {
      #   df_aux <- process_umd_file(df_aux, state)
      #   df_aux <- df_aux[which((df_aux$date >= start_date) & (df_aux$date <= end_date)),]
      #   fwrite(df_aux, file=paste0(aggreg_path, "age/", file_short_csv))
      #   # saveRDS(df_aux, file=paste0(aggreg_path, "age/", file_short_rds))
      # }
      # cat("* After age:", dim(df_aux), state, "\n")
      # 
      # # Aggregation at the region level
      # df_aux <- Aggregate_Region(df)
      # if (is.data.frame(df_aux)) {
      #   df_aux <- process_umd_file(df_aux, state)
      #   df_aux <- df_aux[which((df_aux$date >= start_date) & (df_aux$date <= end_date)),]
      #   fwrite(df_aux, file=paste0(aggreg_path, "region/", file_short_csv))
      #   # saveRDS(df_aux, file=paste0(aggreg_path, "region/", file_short_rds))
      # }
      # cat("* After region:", dim(df_aux), state, "\n")
    }
  }
}

dir.create(aggreg_path, showWarnings = F)
dir.create(paste0(aggreg_path, "state/"), showWarnings = F)

allFiles = list.files(dummy_path, pattern="*", full.names=FALSE, recursive=FALSE)
#states = unique(str_extract(allFiles, '.*(?=\\.rds)'))
states = "Alaska"

kk = lapply(states, process_state)
#kk = mclapply(states, process_state)
