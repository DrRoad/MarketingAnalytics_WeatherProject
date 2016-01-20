#Load packages
library(RODBC)
library(ROCR)
library(dplyr)
library(rtf)
library(nnet)

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

#Create training and testing samples ----------
  ## 70% and 40% of the sample size
  smp_size  <- floor(0.70 * nrow(visitquot))
  smp_size2 <-  floor(0.40 * nrow(visitquot))

  ## set the seed to make the partition reproductible
  set.seed(123)
  sample.under            <- sample_n(visitquot, size = nrow(visitquot))
  
  ## divide the data between training and testing sample
  undersamp.test          <- sample.under[(smp_size + 1):nrow(visitquot),]
  undersamp.test          <- matrix(unlist(undersamp.test), ncol = length(sample.under), byrow = FALSE)
  undersamp.test[,1]      <- ifelse(undersamp.test[,1]==2,1,0)
  colnames(undersamp.test) <- c("quotation", "minute", "temperature", "humidity", "pressure", "min.Radiant", "min.Sin1","min.Cos1","min.Sin2","min.Cos2","min.Sin3","min.Cos3")
  
#   undersamp.train.inter   <- sample.under[1:smp_size,]
#   undersamp.train.pos     <- undersamp.train.inter[undersamp.train.inter$quotation==1,]
#   undersamp.train.neg     <- undersamp.train.inter[undersamp.train.inter$quotation==0,]
#   undersamp.train.neg     <- undersamp.train.neg[1:length(undersamp.train.pos$quotation), ]
#   undersamp.train         <- rbind_list(undersamp.train.pos, undersamp.train.neg)
#   
#   undersamp.train         <- matrix(unlist(undersamp.train), ncol = length(sample.under), byrow = FALSE)
#   undersamp.train[,1]     <- ifelse(undersamp.train[,1]==2,1,0)
#   colnames(undersamp.train) <- c("quotation", "minute", "temperature", "humidity", "pressure", "min.Radiant", "min.Sin1","min.Cos1","min.Sin2","min.Cos2","min.Sin3","min.Cos3")
  

  undersamp.train         <- sample.under[1:smp_size,]
  
  undersamp.train         <- matrix(unlist(undersamp.train), ncol = length(sample.under), byrow = FALSE)
  undersamp.train[,1]     <- ifelse(undersamp.train[,1]==2,1,0)
  colnames(undersamp.train) <- c("quotation", "minute", "temperature", "humidity", "pressure", "min.Radiant", "min.Sin1","min.Cos1","min.Sin2","min.Cos2","min.Sin3","min.Cos3")
  
  
  undersamp.train <- as.data.frame(undersamp.train)
  undersamp.test <- as.data.frame(undersamp.test)
  
  undersamp.test$quotation <- as.factor(undersamp.test$quotation)
  undersamp.train$quotation <- as.factor(undersamp.train$quotation)

# remove unecessary data
  rm(sample.under, undersamp.train.neg,undersamp.train.pos,undersamp.train.inter)
  
# add weights
  logit.weight <- ifelse(undersamp.train$quotation==1,68,1)

# Build linear and logit models -----------
  logit.result <- list()
  
  logit.result$model.1$model <- multinom(formula = quotation ~ temperature + humidity + pressure + min.Sin1 + min.Cos1, data = undersamp.train, weights = logit.weight)
  logit.result$model.2$model <- multinom(formula = quotation ~ temperature + humidity + pressure + min.Sin2 + min.Cos2, data = undersamp.train, weights = logit.weight)
  logit.result$model.3$model <- multinom(formula = quotation ~ temperature + humidity + pressure + min.Sin3 + min.Cos3, data = undersamp.train, weights = logit.weight)
  logit.result$model.4$model <- multinom(formula = quotation ~ temperature + humidity + pressure + min.Sin1 + min.Cos1 + min.Sin3 + min.Cos3, data = undersamp.train, weights = logit.weight)
  logit.result$model.5$model <- multinom(formula = quotation ~ temperature + humidity + pressure + min.Sin3 + min.Cos2 + min.Sin2 + min.Cos3, data = undersamp.train, weights = logit.weight)
  logit.result$model.6$model <- multinom(formula = quotation ~ temperature + humidity + pressure + min.Sin1 + min.Cos1 + min.Cos2 + min.Sin2 + min.Sin3 + min.Cos3, data = undersamp.train, weights = logit.weight)
  logit.result$model.7$model <- multinom(formula = quotation ~ temperature + humidity + pressure + min.Sin1 + min.Cos1 + min.Sin2 + min.Cos2, data = undersamp.train, weights = logit.weight)
  
  for (i in 1:length(logit.result)) {
    logit.result[[i]]$data$predClass             <- predict(logit.result[[i]]$model, newdata=undersamp.test, type="class")
    logit.result[[i]]$data$predProba             <- predict(logit.result[[i]]$model, newdata=undersamp.test, type="probs")
    logit.result[[i]]$analyse$confMatrix         <- table(undersamp.test$quotation, logit.result[[i]]$data$predClass)
    logit.result[[i]]$analyse$sensitivity        <- logit.result[[i]]$analyse$confMatrix[[4]]/(logit.result[[i]]$analyse$confMatrix[[3]]+logit.result[[i]]$analyse$confMatrix[[4]])
    logit.result[[i]]$analyse$specificity        <- logit.result[[i]]$analyse$confMatrix[[1]]/(logit.result[[i]]$analyse$confMatrix[[2]]+logit.result[[i]]$analyse$confMatrix[[1]])
    logit.result[[i]]$analyse$positivePrecision  <- logit.result[[i]]$analyse$confMatrix[[4]]/(logit.result[[i]]$analyse$confMatrix[[4]]+logit.result[[i]]$analyse$confMatrix[[2]])
    logit.result[[i]]$analyse$negativePrecision  <- logit.result[[i]]$analyse$confMatrix[[1]]/(logit.result[[i]]$analyse$confMatrix[[3]]+logit.result[[i]]$analyse$confMatrix[[1]])
    logit.result[[i]]$analyse$accuracy           <- (logit.result[[i]]$analyse$confMatrix[[1]]+logit.result[[i]]$analyse$confMatrix[[4]])/(logit.result[[i]]$analyse$confMatrix[[3]]+logit.result[[i]]$analyse$confMatrix[[1]]+logit.result[[i]]$analyse$confMatrix[[4]]+logit.result[[i]]$analyse$confMatrix[[2]])
    logit.result[[i]]$roc$prediction             <- prediction(logit.result[[i]]$data$predProba, undersamp.test$quotation)
    logit.result[[i]]$roc$performance            <- performance(logit.result[[i]]$roc$prediction, "tpr", "fpr")
    logit.result[[i]]$roc$lift                   <- performance(logit.result[[i]]$roc$prediction, "lift", "rpp")
    logit.result[[i]]$roc$gain                   <- performance(logit.result[[i]]$roc$prediction, "tpr", "rpp")
  }
  
  #Matrix to compare the different models
  model.comparison = matrix(, nrow = length(logit.result), ncol = 5, dimnames = list(paste0("model.", seq_along(logit.result)), c("Sensitivity","Specificity","Accuracy","Positive.Precision","Negative.Precision")))
  for(i in 1:length(logit.result)){
    model.comparison[i,'Sensitivity'] <- logit.result[[i]]$analyse$sensitivity
    model.comparison[i,'Specificity'] <- logit.result[[i]]$analyse$specificity
    model.comparison[i,'Accuracy'] <- logit.result[[i]]$analyse$accuracy
    model.comparison[i,'Positive.Precision'] <- logit.result[[i]]$analyse$positivePrecision
    model.comparison[i,'Negative.Precision'] <- logit.result[[i]]$analyse$negativePrecision
  }
  
  plot(logit.result$model.1$roc$performance, col="blue") 
  plot(logit.result$model.2$roc$performance, add=TRUE,col="red")
  plot(logit.result$model.3$roc$performance, add=TRUE,col="green")
  plot(logit.result$model.4$roc$performance, add=TRUE,col="green")
  abline(0,1,col="grey")