RemoveOutliersBeforeDummification <- function(df)
{
  # Filtering with the open responses using upper wisker of boxplot
  #B2b = For how many days have you had at least one new or unusual symptom?
  cutoff_B2b = 100  
  cutoff_A5_1 = 100
  cutoff_A5_2 = 100
  cutoff_A5_3 = 100
  cutoff_A2 = 100


  df = df[which(is.na(df$A2) | (df$A2 <= cutoff_A2)),]
  cat("* A2 :", cutoff_A2 , "dim:", dim(df), unique(df$state), "\n")

  df = df[which(is.na(df$B2b) | (df$B2b <= cutoff_B2b)),]
  cat("* B2b: Days with at least one new or unusual symptom:", cutoff_B2b, "dim:", dim(df), unique(df$state), "\n")
  
  df = df[which(is.na(df$A5_1) | (df$A5_1 <= cutoff_A5_1)),]
  cat("* A5_1: Persons living in the household -18:", cutoff_A5_1, "dim:", dim(df), unique(df$state), "\n")
  
  df = df[which(is.na(df$A5_2) | (df$A5_2 <= cutoff_A5_2)),]
  cat("* A5_2: Persons living in the household 18-64:", cutoff_A5_2, "dim:", dim(df), unique(df$state), "\n")
  
  df = df[which(is.na(df$A5_3) | (df$A5_3 <= cutoff_A5_3)),]
  cat("* A5_3: Persons living in the household +65:", cutoff_A5_3, "dim:", dim(df), unique(df$state), "\n")
  
  df = df[which(is.na(df$A5_1+df$A5_2+df$A5_3) | is.na(df$A2) | (df$A5_1 + df$A5_2 + df$A5_3 >= df$A2)),]
  cat("* A5_1 + A5_2 + A5_3 >= A2 : dim:", dim(df), unique(df$state), "\n")
  
  return(df)
}


RemoveBinaryOutliers <- function(Dftry1, selectedColumnsForOutliers){
  cutoff_coeff <- 1.5
  
  #The input for this Dftry1 should be the output of the dummies function
  dfOutlierDetect<-list()
  for (i in selectedColumnsForOutliers) {
    for (j in 0:2) {
      attr <- paste0(i,".",j)
      if (attr %in% colnames(Dftry1)) {
        dfOutlierDetect[[attr]] <- Dftry1[[attr]]
      } 
      # else {
      #   cat(attr, "not column\n")
      # }
    }
  }
  dfOutlierDetect <- as.data.frame(dfOutlierDetect)
  #Taking the mean of each column
  dfOutlierDetectMean <- colMeans(dfOutlierDetect)
  #Calculating the distance to the mean for each response 
  if (nrow(dfOutlierDetect)>0) {
    for (i in (1:ncol(dfOutlierDetect))) {
      dfOutlierDetect[,i] <- abs(dfOutlierDetect[,i] - dfOutlierDetectMean[i])
    }
    # Assign the mean distance as a column, and select just the ones lowers than the upper whisker
    Dftry1$Distance <- rowMeans(dfOutlierDetect)
    cutoff <- boxplot.stats(Dftry1$Distance, coef=cutoff_coeff)$stats[5]
    Dftry1<-Dftry1[Dftry1$Distance <= cutoff,]
    Dftry1$Distance <- NULL
  }
  return(Dftry1)
}


# arw for outlier detection 
#arw_custom <- function(x, m0, c0, alpha, pcrit) {}

#RemoveOutliersBeforeDummification_v2 <- function(df){}
  
  

