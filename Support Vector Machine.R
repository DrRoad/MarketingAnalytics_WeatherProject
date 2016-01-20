#Load packages
library(RODBC)
library(ROCR)
library(dplyr)
library(rtf)
library(SwarmSVM)
library(classyfire)

###################
#Quotation Analysis
###################

#Loading data (we exclude unlikely/impossible temperature and humidity data)----------
  db = odbcConnect("MySQL_R_Connection", uid="root", pwd="yundai&1")
  sqlQuery(db, "USE meteo")
  query= "SELECT quotation, minute+hour*60 as minute, weekday, temp, humidity, pressure
            FROM visitweather
            WHERE (temp>-20) AND (temp<25) AND (humidity<=100);"
  
  visitquot=sqlQuery(db,query)
  odbcClose(db)
  
  visitquot$quotation <- as.factor(visitquot$quotation)

#Sample the full data set ---------
  set.seed(123)
  samp.visitquot <- sample_n(visitquot, length(visitquot$quotation))
  
  #Removing weekdays from samp.visitquot
  samp.visitquot <- samp.visitquot[,-3]
  
#Create training and testing samples ----------
  ## 70% and 40% of the sample size
  smp_size  <- floor(0.70 * nrow(visitquot))
  smp_size2 <-  floor(0.001 * nrow(visitquot))
  
  ## set the seed to make the partition reproductible
  set.seed(123)
  sample.under            <- sample_n(visitquot, size = nrow(visitquot))
  sample.under            <- sample.under[,-3]
  
  ## divide the data between training and testing sample
  undersamp.test          <- sample.under[(smp_size + 1):nrow(visitquot),]
  undersamp.train.inter   <- sample.under[1:smp_size2,]
  undersamp.train.pos     <- undersamp.train.inter[undersamp.train.inter$quotation==1,]
  undersamp.train.neg     <- undersamp.train.inter[undersamp.train.inter$quotation==0,]
  undersamp.train.neg     <- undersamp.train.neg[1:length(undersamp.train.pos$quotation), ]
  undersamp.train         <- rbind_list(undersamp.train.pos, undersamp.train.neg)
  
  undersamp.train         <- matrix(unlist(undersamp.train), ncol = 5, byrow = FALSE)
  undersamp.train[,1]     <- ifelse(undersamp.train[,1]==2,1,0)
  colnames(undersamp.train) <- c("quotation", "minute", "temperature", "humidity", "pressure")
  
  # remove unecessary data
  rm(sample.under, undersamp.train.neg,undersamp.train.pos,undersamp.train.inter)

#Run classyfire SVM models ----------
  svm <- dcSVM(undersamp.train[,-1], undersamp.train[,1], k = 100, 5, kernel = 3, 20, early = 0,
               final.training = FALSE, pre.scale = FALSE, seed = NULL,
               verbose = TRUE, valid.x = NULL, valid.y = NULL, valid.metric = NULL,
               cluster.method = "kmeans", cluster.fun = NULL, cluster.predict = NULL, replace = TRUE)
  svm <- cfBuild(inputData = undersamp.train[,-1], inputClass = undersamp.train[,1], bootNum = 100,
                 ensNum = 100, parallel = TRUE, cpus = 2, type = "SOCK")
  
  
  svm <- svm(undersamp.train[,-1], undersamp.train[,1], scale = TRUE, type = NULL, kernel =
               "radial", degree = 3, gamma = if (is.vector(undersamp.train[,-1])) 1 else 1 / ncol(undersamp.train[,-1]),
             coef0 = 0, cost = 1, nu = 0.5,
             class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
             shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE,subset, na.action = na.omit)
  