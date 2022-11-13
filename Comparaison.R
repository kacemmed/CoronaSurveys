setwd("/Volumes/Simo")
library(stringr)
library(dplyr)
library(parallel)
library(randomForest)
library(xgboost)
library(data.table)
library(e1071)
library(neuralnet)




comp_path = "./Comparaison/"
dummy_path = "./dummies/"
sample_size = 1000


process_state<-function(state)
{
  dir.create(paste0(comp_path, state, "/"), showWarnings = F)
  
  cat("state:", state, "\n")
  file_short_rds = paste0(state,".rds")
  file_input = paste0(dummy_path, file_short_rds)
  
  if (file.exists(file_input)){
    
    df = readRDS(file=file_input)
    #cat("Total:", dim(df), state, "\n")
    
    golden = which(df$B10.1==1 & (df$B10c.1==1 | df$B10c.2==1))
    df_golden = df[golden,]
    
    df_golden$positive <- df_golden$B10c.1
    #df_golden$positive <- as.factor(df_golden$B10c.1)
    
    
    
    
    F1_glm = c()
    F1_RF = c()
    F1_XGB = c()
    F1_svm_lin = c()
    F1_svm_poly = c()
    F1_nn = c()
    
    
    #T_state = data.frame(row.names = states)
    
    for (i in 1:50){
      
      
      dir.create(paste0(comp_path, state, "/",i,"/"), showWarnings = F)
      
      set.seed(12345 + i*7)
      
      indices = sample(1:nrow(df_golden), min(sample_size, nrow(df_golden)))
      df_golden_i = df_golden[indices,]
      
      train_ind<-sample(seq_len(nrow(df_golden_i)),
                        size = 0.8*nrow(df_golden_i))
      
      
      train <-df_golden_i[train_ind,]
      test <- df_golden_i[-train_ind,]
      
      train[is.na(train)] = 0
      test[is.na(test)] = 0
      
      
      
      ml_data = train %>%
        dplyr::select(-c("EndDatetime", "fips", "state_code", "state", "B2_14_TEXT", "B2"), 
                      -contains(".NA"), -contains("B10")
                      #, -contains("V1"), -contains("V2")
        )
      
      ml_data_test = test %>%
        dplyr::select(-c("EndDatetime", "fips", "state_code", "state", "B2_14_TEXT", "B2"), 
                      -contains(".NA"), -contains("B10")
                      #, -contains("V1"), -contains("V2")
        )
      
      
      #cat("Total used:", dim(ml_data), state, "\n")
      
      
      
      data = as.matrix(ml_data %>% dplyr::select(-positive))
      classifier_Xgboost = xgboost(data = data,
                                   label = ml_data$positive,
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
      
      
      classifier_RF = randomForest(x = as.matrix(ml_data %>% dplyr::select(-positive)),
                                   y = as.factor(train$positive),
                                   ntree = 100,
                                   keep.forest = TRUE
                                   #,importance = TRUE
      )
      
      
      res.svc.lin=svm(train$positive~.,
                      data=as.matrix(ml_data %>% dplyr::select(-positive)), 
                      kernel="linear", 
                      scale=F,
                      cost=1, 
                      tolerance = 0.01, 
                      probability = TRUE, 
                      type = "C-classification"
                      )
      
      # res.svc.pol=svm(train$positive~.,
      #                 data=as.matrix(ml_data %>% dplyr::select(-positive)),
      #                 kernel="polynomial",
      #                 scale=F,
      #                 cost = 1,
      #                 tolerance = 0.01,
      #                 probability = TRUE,
      #                 type = "C-classification",
      #                 degree = 10
      #                 )
      
      nn = neuralnet(positive~., 
                     data = ml_data,
                     threshold = 0.01,
                     stepmax = 1e+06,
                     hidden = c(20, 10, 2),
                     #act.fct = "relu",
                     #n.epochs = 100,
                     linear.output = FALSE)
      
      res.glm = glm(train$positive~., 
                    data=ml_data %>% dplyr::select(-positive),
                    family=binomial
                    )
      
      
      
      
      pred_XGB = predict(classifier_Xgboost, as.matrix(ml_data_test %>% dplyr::select(-positive)))
      test$pos_XGB_prob = pred_XGB
      test$pos_XGB = round(pred_XGB)
      
      pred_RF = predict(classifier_RF, ml_data_test %>% dplyr::select(-positive), type = "prob")
      test$pos_RF_prob = pred_RF[,2]
      test$pos_RF = round(pred_RF[,2])
      
      pred_svm_lin = predict(res.svc.lin, ml_data_test %>% dplyr::select(-positive), probability = TRUE)
      test$pos_svm_lin_prob = attr(pred_svm_lin, "probabilities")[,2]
      test$pos_svm_lin = round(attr(pred_svm_lin, "probabilities")[,2])
      
      #pred_svm_pol = predict(res.svc.pol, ml_data_test %>% dplyr::select(-positive), probability = TRUE)
      # test$pos_svm_pol = round(pred_svm_pol)
      
      pred_nn = predict(nn, ml_data_test %>% dplyr::select(-positive))
      test$pos_nn_prob = pred_nn
      test$pos_nn = round(pred_nn)
      
      pred_glm = predict(res.glm, ml_data_test %>% dplyr::select(-positive), type = "response")
      test$pos_glm_prob = pred_glm
      test$pos_glm = round(pred_glm)
      
      
      
      
      df_golden_i$pos_XGB = df_golden_i$positive
      df_golden_i$pos_RF = df_golden_i$positive
      df_golden_i[-train_ind,]$pos_XGB = test$pos_XGB 
      df_golden_i[-train_ind,]$pos_RF = test$pos_RF
      
      
      
      
      TN_glm = length(which((test$B10.1==1) & (test$B10c.2 == 1) & (test$pos_glm == 0)))
      TP_glm = length(which((test$B10.1==1) & (test$B10c.1 == 1) & (test$pos_glm == 1)))
      FN_glm = length(which((test$B10.1==1) & (test$B10c.1 == 1) & (test$pos_glm == 0)))
      FP_glm = length(which((test$B10.1==1) & (test$B10c.2 == 1) & (test$pos_glm == 1)))
      
      Accuracy_glm = (TP_glm+TN_glm)/(TP_glm+FP_glm+FN_glm+TN_glm)
      Precision_glm =  TP_glm/(TP_glm+FP_glm)
      Recall_glm = TP_glm/(TP_glm+FN_glm)
      Specificity_glm = TN_glm/(TN_glm+FP_glm)
      F1_Score_glm = 2*(Recall_glm * Precision_glm) / (Recall_glm + Precision_glm)
      F1_glm = c(F1_glm, F1_Score_glm)
      
      cat("F1 Score model LR pour ",i," ", state, " F1 Score = ", F1_Score_glm, "\n")
      
      
      
      
      TN_RF = length(which((test$B10.1==1) & (test$B10c.2 == 1) & (test$pos_RF == 0)))
      TP_RF = length(which((test$B10.1==1) & (test$B10c.1 == 1) & (test$pos_RF == 1)))
      FN_RF = length(which((test$B10.1==1) & (test$B10c.1 == 1) & (test$pos_RF == 0)))
      FP_RF = length(which((test$B10.1==1) & (test$B10c.2 == 1) & (test$pos_RF == 1)))
      
      Accuracy_RF = (TP_RF+TN_RF)/(TP_RF+FP_RF+FN_RF+TN_RF)
      Precision_RF =  TP_RF/(TP_RF+FP_RF)
      Recall_RF = TP_RF/(TP_RF+FN_RF)
      Specificity_RF = TN_RF/(TN_RF+FP_RF)
      F1_Score_RF = 2*(Recall_RF * Precision_RF) / (Recall_RF + Precision_RF)
      F1_RF = c(F1_RF, F1_Score_RF)
      
      cat("F1 Score model RF pour ",i," ", state, " F1 Score = ", F1_Score_RF, "\n")
      
      
      # cat("RF pour", state_RF, "Accuracy = ", Accuracy_RF, ", Precision = ", Precision_RF
      #     , ", Recall = ", Recall_RF," ,Specificity = ", Specificity_RF, ", F1 Score = ", F1_Score_RF)
      # 
      
      
      
      
      
      TN_XGB = length(which((test$B10.1==1) & (test$B10c.2 == 1) & (test$pos_XGB == 0)))
      TP_XGB = length(which((test$B10.1==1) & (test$B10c.1 == 1) & (test$pos_XGB == 1)))
      FN_XGB = length(which((test$B10.1==1) & (test$B10c.1 == 1) & (test$pos_XGB == 0)))
      FP_XGB = length(which((test$B10.1==1) & (test$B10c.2 == 1) & (test$pos_XGB == 1)))
      
      Accuracy_XGB = (TP_XGB+TN_XGB)/(TP_XGB+FP_XGB+FN_XGB+TN_XGB)
      Precision_XGB =  TP_XGB/(TP_XGB+FP_XGB)
      Recall_XGB = TP_XGB/(TP_XGB+FN_XGB)
      Specificity_XGB = TN_XGB/(TN_XGB+FP_XGB)
      F1_Score_XGB = 2*(Recall_XGB * Precision_XGB) / (Recall_XGB + Precision_XGB)
      
      F1_XGB = c(F1_XGB, F1_Score_XGB)
      
      cat("F1 Score model XGB pour ",i," ", state, " F1 Score = ", F1_Score_XGB, "\n")
      
      
      
      TN_svm_lin = length(which((test$B10.1==1) & (test$B10c.2 == 1) & (test$pos_svm_lin == 0)))
      TP_svm_lin = length(which((test$B10.1==1) & (test$B10c.1 == 1) & (test$pos_svm_lin == 1)))
      FN_svm_lin = length(which((test$B10.1==1) & (test$B10c.1 == 1) & (test$pos_svm_lin == 0)))
      FP_svm_lin = length(which((test$B10.1==1) & (test$B10c.2 == 1) & (test$pos_svm_lin == 1)))
      
      Accuracy_svm_lin = (TP_svm_lin+TN_svm_lin)/(TP_svm_lin+FP_svm_lin+FN_svm_lin+TN_svm_lin)
      Precision_svm_lin =  TP_svm_lin/(TP_svm_lin+FP_svm_lin)
      Recall_svm_lin = TP_svm_lin/(TP_svm_lin+FN_svm_lin)
      Specificity_svm_lin = TN_svm_lin/(TN_svm_lin+FP_svm_lin)
      F1_Score_svm_lin = 2*(Recall_svm_lin * Precision_svm_lin) / (Recall_svm_lin + Precision_svm_lin)
      F1_svm_lin = c(F1_svm_lin, F1_Score_svm_lin)
      
      cat("F1 Score model SVM linéaire pour ",i," ", state, " F1 Score = ", F1_Score_svm_lin, "\n")
      
      
      
      
      TN_nn = length(which((test$B10.1==1) & (test$B10c.2 == 1) & (test$pos_nn == 0)))
      TP_nn = length(which((test$B10.1==1) & (test$B10c.1 == 1) & (test$pos_nn == 1)))
      FN_nn = length(which((test$B10.1==1) & (test$B10c.1 == 1) & (test$pos_nn == 0)))
      FP_nn = length(which((test$B10.1==1) & (test$B10c.2 == 1) & (test$pos_nn == 1)))
      
      Accuracy_nn = (TP_nn+TN_nn)/(TP_nn+FP_nn+FN_nn+TN_nn)
      Precision_nn =  TP_nn/(TP_nn+FP_nn)
      Recall_nn = TP_nn/(TP_nn+FN_nn)
      Specificity_nn = TN_nn/(TN_nn+FP_nn)
      F1_Score_nn = 2*(Recall_nn * Precision_nn) / (Recall_nn + Precision_nn)
      F1_nn = c(F1_nn, F1_Score_nn)
      
      cat("F1 Score model NN linéaire pour ",i," ", state, " F1 Score = ", F1_Score_nn, "\n")
      
      
      results <- test[,95:ncol(test)]
      
      path_test_i  =  paste0(comp_path,state,"/",i,"/test.csv" )
      path_res_i  =  paste0(comp_path,state,"/",i,"/results.csv" )
      path_train_i  =  paste0(comp_path,state,"/",i,"/train.csv" )
      path_i = paste0(comp_path,state,"/",i,"/df_golden_i.csv" )
      
      fwrite(test ,path_test_i)
      fwrite(results,path_res_i)
      fwrite(train ,path_train_i)
      fwrite(df_golden_i ,path_i)
      
      
    }
    
  }
  cat("Average F1 Score model RF pour", state, "F1 Score = ", mean(F1_RF), "\n")
  cat("Average F1 Score model XGB pour", state, "F1 Score = ", mean(F1_XGB))
  T[j, 1] = mean(F1_glm)
  T[j, 2] = mean(F1_XGB)
  T[j, 3] = mean(F1_RF)
  T[j, 4] = mean(F1_svm_lin)
  T[j, 5] = mean(F1_nn)
  j = j+1
}



dir.create(comp_path, showWarnings = F)


allFiles = list.files(dummy_path, pattern="*", full.names=FALSE, recursive=FALSE)
states = unique(str_extract(allFiles, '.*(?=\\.rds)'))

T = data.frame(row.names = states)
T$LR = NA
T$XGB = NA
T$RF = NA
T$SVM_lin = NA
T$NN = NA

j=1

kk = lapply(states, process_state)
#kk = mclapply(states, process_state)

path_T = paste0(comp_path, "F1_averages.csv")

fwrite(T, path_T)
