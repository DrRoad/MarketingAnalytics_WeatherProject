#Load packages
library(RODBC)
library(ROCR)
library(dplyr)
library(rtf)
library(kknn)

###################
#Quotation Analysis
###################

#Loading data (we exclude unlikely/impossible temperature and humidity data)----------
db = odbcConnect("MySQL_R_Connection", uid="root", pwd="yundai&1")
sqlQuery(db, "USE meteo")
query= "SELECT quotation, minute+hour*60 as minute, temp, humidity, pressure
FROM visitweather
WHERE (temp>-20) AND (temp<25) AND (humidity<=100);"

visitquot=sqlQuery(db,query)
odbcClose(db)

visitquot$quotation <- as.factor(visitquot$quotation)

visitquot$min.Radiant <- visitquot$minute*pi/1440
visitquot$min.Sin1 <- sin(visitquot$min.Radiant)
visitquot$min.Cos1 <- cos(visitquot$min.Radiant)
visitquot$min.Sin2 <- sin(2*visitquot$min.Radiant)
visitquot$min.Cos2 <- cos(2*visitquot$min.Radiant)
visitquot$min.Sin3 <- sin(3*visitquot$min.Radiant)
visitquot$min.Cos3 <- cos(3*visitquot$min.Radiant)

#Sample the full data set ---------
set.seed(123)
samp.visitquot <- sample_n(visitquot, length(visitquot$quotation))

#Create training and testing samples ----------
  ## 70% and 40% of the sample size
  smp_size  <- floor(0.70 * nrow(visitquot))
  smp_size2 <-  floor(0.40 * nrow(visitquot))
  
  ## set the seed to make the partition reproductible
  set.seed(123)
  sample.under            <- sample_n(visitquot, size = nrow(visitquot))
  
  ## divide the data between training and testing sample
  undersamp.test          <- sample.under[(smp_size + 1):nrow(visitquot),]
  undersamp.test          <- matrix(unlist(undersamp.test), ncol = length(samp.visitquot), byrow = FALSE)
  undersamp.test[,1]      <- ifelse(undersamp.test[,1]==2,1,0)
  colnames(undersamp.test) <- c("quotation", "minute", "temperature", "humidity", "pressure", "min.Radiant", "min.Sin1","min.Cos1","min.Sin2","min.Cos2","min.Sin3","min.Cos3")
  
  undersamp.train.inter   <- sample.under[1:smp_size,]
  undersamp.train.pos     <- undersamp.train.inter[undersamp.train.inter$quotation==1,]
  undersamp.train.neg     <- undersamp.train.inter[undersamp.train.inter$quotation==0,]
  undersamp.train.neg     <- undersamp.train.neg[1:length(undersamp.train.pos$quotation), ]
  undersamp.train         <- rbind_list(undersamp.train.pos, undersamp.train.neg)
  
  undersamp.train         <- matrix(unlist(undersamp.train), ncol = length(samp.visitquot), byrow = FALSE)
  undersamp.train[,1]     <- ifelse(undersamp.train[,1]==2,1,0)
  colnames(undersamp.train) <- c("quotation", "minute", "temperature", "humidity", "pressure", "min.Radiant", "min.Sin1","min.Cos1","min.Sin2","min.Cos2","min.Sin3","min.Cos3")
  
  undersamp.train <- as.data.frame(undersamp.train)
  undersamp.test <- as.data.frame(undersamp.test)
  
  undersamp.test$quotation <- as.factor(undersamp.test$quotation)
  undersamp.train$quotation <- as.factor(undersamp.train$quotation)
  
  # remove unecessary data
  rm(sample.under, undersamp.train.neg,undersamp.train.pos,undersamp.train.inter)
  
#Build the model -----------
  knn.result <- list()
  
  
  knn.result$model.1$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 7, distance = 2, kernel = "triangular")
  knn.result$model.2$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 7, distance = 2, kernel = "rectangular")
  knn.result$model.3$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 7, distance = 2, kernel = "epanechnikov")
  knn.result$model.4$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 7, distance = 2, kernel = "biweight")
  knn.result$model.5$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 7, distance = 2, kernel = "cos")
  knn.result$model.6$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 7, distance = 2, kernel = "inv")
  knn.result$model.7$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 7, distance = 2, kernel = "gaussian")
  knn.result$model.8$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 7, distance = 2, kernel = "rank")
  knn.result$model.9$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 7, distance = 2, kernel = "optimal")
  
  knn.result$model.10$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 7, distance = 0.5, kernel = "triangular")
  knn.result$model.11$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 7, distance = 0.5, kernel = "rectangular")
  knn.result$model.12$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 7, distance = 0.5, kernel = "epanechnikov")
  knn.result$model.13$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 7, distance = 0.5, kernel = "biweight")
  knn.result$model.14$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 7, distance = 0.5, kernel = "cos")
  knn.result$model.15$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 7, distance = 0.5, kernel = "inv")
  knn.result$model.16$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 7, distance = 0.5, kernel = "gaussian")
  knn.result$model.17$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 7, distance = 0.5, kernel = "rank")
  knn.result$model.18$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 7, distance = 0.5, kernel = "optimal")
  
  knn.result$model.19$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 20, distance = 2, kernel = "triangular")
  knn.result$model.20$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 20, distance = 2, kernel = "rectangular")
  knn.result$model.21$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 20, distance = 2, kernel = "epanechnikov")
  knn.result$model.22$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 20, distance = 2, kernel = "biweight")
  knn.result$model.23$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 20, distance = 2, kernel = "cos")
  knn.result$model.24$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 20, distance = 2, kernel = "inv")
  knn.result$model.25$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 20, distance = 2, kernel = "gaussian")
  knn.result$model.26$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 20, distance = 2, kernel = "rank")
  knn.result$model.27$model <- kknn(formula = quotation ~ ., undersamp.train, undersamp.test, k = 20, distance = 2, kernel = "optimal")
  
  for (i in 1:length(knn.result)) {
    knn.result[[i]]$analyse$confMatrix         <- table(undersamp.test$quotation, knn.result[[i]]$model$fit)
    knn.result[[i]]$analyse$sensitivity        <- knn.result[[i]]$analyse$confMatrix[[4]]/(knn.result[[i]]$analyse$confMatrix[[3]]+knn.result[[i]]$analyse$confMatrix[[4]])
    knn.result[[i]]$analyse$specificity        <- knn.result[[i]]$analyse$confMatrix[[1]]/(knn.result[[i]]$analyse$confMatrix[[2]]+knn.result[[i]]$analyse$confMatrix[[1]])
    knn.result[[i]]$analyse$positivePrecision  <- knn.result[[i]]$analyse$confMatrix[[4]]/(knn.result[[i]]$analyse$confMatrix[[4]]+knn.result[[i]]$analyse$confMatrix[[2]])
    knn.result[[i]]$analyse$negativePrecision  <- knn.result[[i]]$analyse$confMatrix[[1]]/(knn.result[[i]]$analyse$confMatrix[[3]]+knn.result[[i]]$analyse$confMatrix[[1]])
    knn.result[[i]]$analyse$accuracy           <- (knn.result[[i]]$analyse$confMatrix[[1]]+knn.result[[i]]$analyse$confMatrix[[4]])/(knn.result[[i]]$analyse$confMatrix[[3]]+knn.result[[i]]$analyse$confMatrix[[1]]+knn.result[[i]]$analyse$confMatrix[[4]]+knn.result[[i]]$analyse$confMatrix[[2]])
    knn.result[[i]]$roc$prediction             <- prediction(knn.result[[i]]$model$prob[,2], undersamp.test$quotation)
    knn.result[[i]]$roc$performance            <- performance(knn.result[[i]]$roc$prediction, "tpr", "fpr")
    knn.result[[i]]$roc$lift                   <- performance(knn.result[[i]]$roc$prediction, "lift", "rpp")
    knn.result[[i]]$roc$gain                   <- performance(knn.result[[i]]$roc$prediction, "tpr", "rpp")
  }
  
  #Matrix to compare the different models
  model.comparison = matrix(, nrow = length(knn.result), ncol = 5, dimnames = list(paste0("model.", seq_along(knn.result)), c("Sensitivity","Specificity","Accuracy","Positive.Precision","Negative.Precision")))
  for(i in 1:length(knn.result)){
    model.comparison[i,'Sensitivity'] <- knn.result[[i]]$analyse$sensitivity
    model.comparison[i,'Specificity'] <- knn.result[[i]]$analyse$specificity
    model.comparison[i,'Accuracy'] <- knn.result[[i]]$analyse$accuracy
    model.comparison[i,'Positive.Precision'] <- knn.result[[i]]$analyse$positivePrecision
    model.comparison[i,'Negative.Precision'] <- knn.result[[i]]$analyse$negativePrecision
  }
  
  #Plot performances
  plot(knn.result$model.10$roc$performance, col="blue") 
  plot(knn.result$model.11$roc$performance, add=TRUE,col="red")
  plot(knn.result$model.12$roc$performance, add=TRUE,col="green")
  abline(0,1,col="grey")