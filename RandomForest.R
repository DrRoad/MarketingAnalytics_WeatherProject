#Load packages
library(RODBC)
library(party)
library(ROCR)
library(dplyr)
#library(Rborist)
library(ranger)
library(rtf)

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

#Undersample data to rebalance them ----------
  ## 70% and 40% of the sample size
  smp_size  <- floor(0.70 * nrow(visitquot))
  smp_size2 <-  floor(0.40 * nrow(visitquot))
  
  ## set the seed to make the partition reproductible
  set.seed(123)
  sample.under            <- sample_n(visitquot, size = nrow(visitquot))
  
  ## divide the data between training and testing sample
  undersamp.test          <- sample.under[(smp_size + 1):nrow(visitquot),]
  undersamp.train.inter   <- sample.under[1:smp_size,]
  undersamp.train.pos     <- undersamp.train.inter[undersamp.train.inter$quotation==1,]
  undersamp.train.neg     <- undersamp.train.inter[undersamp.train.inter$quotation==0,]
  undersamp.train.neg     <- undersamp.train.neg[1:length(undersamp.train.pos$quotation), ]
  undersamp.train         <- rbind_list(undersamp.train.pos, undersamp.train.neg)
  
#Run different random forest models ------------
  nfold = 4
  ntree = 100
  forest.result.balance <- list()
  
#   #Create multiple forests in a list based on training data
#   forest.result.balance$model.1$forest <- cforest(formula = quotation ~ minute + temp + humidity, data=undersamp.train, controls = cforest_unbiased(ntree=50, mtry=3))
#   forest.result.balance$model.2$forest <- cforest(formula = quotation ~ minute + temp, data=undersamp.train, controls = cforest_unbiased(ntree=50, mtry=3))
#   forest.result.balance$model.3$forest <- cforest(formula = quotation ~ minute + humidity, data=undersamp.train, controls = cforest_unbiased(ntree=50, mtry=3))
#   forest.result.balance$model.4$forest <- cforest(formula = quotation ~ minute + temp + humidity + pressure, data=undersamp.train, controls = cforest_unbiased(ntree=50, mtry=3))
  
  #With Ranger package
  forest.result.balance$model.1$forest <- ranger(formula = quotation ~ minute + temp + humidity, data=undersamp.train, num.trees=ntree, write.forest = TRUE, probability = TRUE)
  forest.result.balance$model.2$forest <- ranger(formula = quotation ~ minute + temp, data=undersamp.train, num.trees=ntree, write.forest = TRUE, probability = TRUE)
  forest.result.balance$model.3$forest <- ranger(formula = quotation ~ minute + humidity, data=undersamp.train, num.trees=ntree, write.forest = TRUE, probability = TRUE)
  forest.result.balance$model.4$forest <- ranger(formula = quotation ~ minute + temp + humidity + pressure, data=undersamp.train, num.trees=ntree, write.forest = TRUE, probability = TRUE)
  
  
  #Add all the other interesting features to the list
  for (i in 1:nfold) {
    # Assign in-sample and out-of-sample observations
    forest.result.balance[[i]]$data$real                  <- undersamp.test$quotation
    forest.result.balance[[i]]$data$prediction            <- predict(forest.result.balance[[i]]$forest, data=undersamp.test, type="response")
    forest.result.balance[[i]]$data$predClass             <- ifelse(forest.result.balance[[i]]$data$prediction$predictions[,2]>0.5,1,0)
    forest.result.balance[[i]]$analyse$confMatrix         <- table(forest.result.balance[[i]]$data$predClass,forest.result.balance[[i]]$data$real)
    forest.result.balance[[i]]$analyse$sensitivity        <- forest.result.balance[[i]]$analyse$confMatrix[[4]]/(forest.result.balance[[i]]$analyse$confMatrix[[3]]+forest.result.balance[[i]]$analyse$confMatrix[[4]])
    forest.result.balance[[i]]$analyse$specificity        <- forest.result.balance[[i]]$analyse$confMatrix[[1]]/(forest.result.balance[[i]]$analyse$confMatrix[[2]]+forest.result.balance[[i]]$analyse$confMatrix[[1]])
    forest.result.balance[[i]]$analyse$positivePrecision  <- forest.result.balance[[i]]$analyse$confMatrix[[4]]/(forest.result.balance[[i]]$analyse$confMatrix[[4]]+forest.result.balance[[i]]$analyse$confMatrix[[2]])
    forest.result.balance[[i]]$analyse$negativePrecision  <- forest.result.balance[[i]]$analyse$confMatrix[[1]]/(forest.result.balance[[i]]$analyse$confMatrix[[3]]+forest.result.balance[[i]]$analyse$confMatrix[[1]])
    forest.result.balance[[i]]$analyse$accuracy           <- (forest.result.balance[[i]]$analyse$confMatrix[[1]]+forest.result.balance[[i]]$analyse$confMatrix[[4]])/(forest.result.balance[[i]]$analyse$confMatrix[[3]]+forest.result.balance[[i]]$analyse$confMatrix[[1]]+forest.result.balance[[i]]$analyse$confMatrix[[4]]+forest.result.balance[[i]]$analyse$confMatrix[[2]])
    forest.result.balance[[i]]$roc$prediction             <- prediction(forest.result.balance[[i]]$data$prediction$predictions[,2], forest.result.balance[[i]]$data$real)
    forest.result.balance[[i]]$roc$performance            <- performance(forest.result.balance[[i]]$roc$prediction, "tpr", "fpr")
    forest.result.balance[[i]]$roc$lift                   <- performance(forest.result.balance[[i]]$roc$prediction, "lift", "rpp")
    forest.result.balance[[i]]$roc$gain                   <- performance(forest.result.balance[[i]]$roc$prediction, "tpr", "rpp")
  }
  
  #Add a table that sum up OOB, sensitivity, specificity, accuracy, positive precision and negative precision
  model.comparison = matrix(, nrow = nfold, ncol = 6, dimnames = list(paste0("model.", seq_along(forest.result.balance)), c("OOB","Sensitivity","Specificity","Accuracy","Positive.Precision","Negative.Precision")))
  for(i in 1:nfold){
    model.comparison[i,'OOB'] <- forest.result.balance[[i]]$forest$prediction.error
    model.comparison[i,'Sensitivity'] <- forest.result.balance[[i]]$analyse$sensitivity
    model.comparison[i,'Specificity'] <- forest.result.balance[[i]]$analyse$specificity
    model.comparison[i,'Accuracy'] <- forest.result.balance[[i]]$analyse$accuracy
    model.comparison[i,'Positive.Precision'] <- forest.result.balance[[i]]$analyse$positivePrecision
    model.comparison[i,'Negative.Precision'] <- forest.result.balance[[i]]$analyse$negativePrecision
  }
  
  #Matrix to compare the best tree and the best random forest
  model.comparison = matrix(, nrow = 2, ncol = 5, dimnames = list(c("Best.Tree","Best.RF"), c("Sensitivity","Specificity","Accuracy","Positive.Precision","Negative.Precision")))
  for(i in 1:2){
    model.comparison[i,'Sensitivity'] <- forest.result.balance[[i]]$analyse$sensitivity
    model.comparison[i,'Specificity'] <- forest.result.balance[[i]]$analyse$specificity
    model.comparison[i,'Accuracy'] <- forest.result.balance[[i]]$analyse$accuracy
    model.comparison[i,'Positive.Precision'] <- forest.result.balance[[i]]$analyse$positivePrecision
    model.comparison[i,'Negative.Precision'] <- forest.result.balance[[i]]$analyse$negativePrecision
  }
  
  rtffile <- RTF("model.comparison.Tree.RF.doc")
  addParagraph(rtffile, "This is the output of the table we made :\n")
  addTable(rtffile, model.comparison, row.names = TRUE)
  done(rtffile)
  
  #ROC comparison
  plot(forest.result.balance$model.1$roc$performance, col="blue") 
  plot(forest.result.balance$model.2$roc$performance, add=TRUE,col="red")
  plot(forest.result.balance$model.2$roc$performance, add=TRUE,col="green")
  plot(forest.result.balance$model.2$roc$performance, add=TRUE,col="black")
  abline(0,1,col="grey")
  
  #Lift comparison
  plot(forest.70.n100.model.4$roc$lift, col="blue") 
  plot(forest.70.n300.model.4$roc$lift, add=TRUE,col="red")
  

#Computing the two best models with 100 trees (different train sample size) : -----------
  ## 70% and 40% of the sample size
  smp_size  <- floor(0.90 * nrow(visitquot))
  smp_size2 <-  floor(0.40 * nrow(visitquot))
  
  ## set the seed to make the partition reproductible
  set.seed(123)
  sample.under            <- sample_n(visitquot, size = nrow(visitquot))
  
  ## divide the data between training and testing sample
  undersamp.test          <- sample.under[(smp_size + 1):nrow(visitquot),]
    #Big training sample
    undersamp.train.inter   <- sample.under[1:smp_size,]
    undersamp.train.pos     <- undersamp.train.inter[undersamp.train.inter$quotation==1,]
    undersamp.train.neg     <- undersamp.train.inter[undersamp.train.inter$quotation==0,]
    undersamp.train.neg     <- undersamp.train.neg[1:length(undersamp.train.pos$quotation), ]
    undersamp.train.big     <- rbind_list(undersamp.train.pos, undersamp.train.neg)
    #Small training sample
    undersamp.train.inter   <- sample.under[1:smp_size2,]
    undersamp.train.pos     <- undersamp.train.inter[undersamp.train.inter$quotation==1,]
    undersamp.train.neg     <- undersamp.train.inter[undersamp.train.inter$quotation==0,]
    undersamp.train.neg     <- undersamp.train.neg[1:length(undersamp.train.pos$quotation), ]
    undersamp.train.small   <- rbind_list(undersamp.train.pos, undersamp.train.neg)
  
  #Run different random forest models
  nfold = 4
  ntree = 100
  forest.result.balance <- list()
  
  #Compute the two models
  forest.result.balance$model.1$forest <- ranger(formula = quotation ~ minute + temp + humidity, data=undersamp.train.small, num.trees=ntree, write.forest = TRUE, probability = TRUE)
  forest.result.balance$model.2$forest <- ranger(formula = quotation ~ minute + temp + humidity + pressure, data=undersamp.train.big, num.trees=ntree, write.forest = TRUE, probability = TRUE)
  