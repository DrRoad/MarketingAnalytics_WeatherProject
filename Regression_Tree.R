#Load packages
library(RODBC)
library(party)
library(ROCR)
library(dplyr)

#detach("package:party", unload=TRUE)

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


#Build 2 trees on full data set-------------
#Compare trees with different p-values
tree.model.95 <- ctree(formula = quotation ~ minute + temp, data=visitquot, controls = ctree_control(mincriterion = 0.95))
tree.model.99 <- ctree(formula = quotation ~ minute + temp, data=visitquot, controls = ctree_control(mincriterion = 0.99))


# Plot and print the trees
png("tree.model.95.png", res=80, height=1200, width=2400)
plot(tree.model.95, type = "simple")
dev.off()
#
png("tree.model.99.png", res=80, height=1200, width=2400)
plot(tree.model.99, type = "simple")
dev.off()

#Sample the full data ---------
  set.seed(123)
  samp.visitquot <- sample_n(visitquot, length(visitquot$quotation))
  
      
  
  
  

#Build 10 trees on sub data ----------

  #Divide the data
  list.sub.visitquot <- df_split(samp.visitquot,10)

  #Create each sub-tree
  #Sub-tree 1
  tree.submodel.1.95 <- ctree(formula = quotation ~ minute + temp, data=list.sub.visitquot[[1]], controls = ctree_control(mincriterion = 0.95))
  #Sub-tree 2
  tree.submodel.2.95 <- ctree(formula = quotation ~ minute + temp, data=list.sub.visitquot[[2]], controls = ctree_control(mincriterion = 0.95))
  #Sub-tree 3
  tree.submodel.3.95 <- ctree(formula = quotation ~ minute + temp, data=list.sub.visitquot[[3]], controls = ctree_control(mincriterion = 0.95))
  #Sub-tree 4
  tree.submodel.4.95 <- ctree(formula = quotation ~ minute + temp, data=list.sub.visitquot[[4]], controls = ctree_control(mincriterion = 0.95))
  #Sub-tree 5
  tree.submodel.5.95 <- ctree(formula = quotation ~ minute + temp, data=list.sub.visitquot[[5]], controls = ctree_control(mincriterion = 0.95))
  #Sub-tree 6
  tree.submodel.6.95 <- ctree(formula = quotation ~ minute + temp, data=list.sub.visitquot[[6]], controls = ctree_control(mincriterion = 0.95))
  #Sub-tree 7
  tree.submodel.7.95 <- ctree(formula = quotation ~ minute + temp, data=list.sub.visitquot[[7]], controls = ctree_control(mincriterion = 0.95))
  #Sub-tree 8
  tree.submodel.8.95 <- ctree(formula = quotation ~ minute + temp, data=list.sub.visitquot[[8]], controls = ctree_control(mincriterion = 0.95))
  #Sub-tree 9
  tree.submodel.9.95 <- ctree(formula = quotation ~ minute + temp, data=list.sub.visitquot[[9]], controls = ctree_control(mincriterion = 0.95))
  #Sub-tree 10
  tree.submodel.9.95 <- ctree(formula = quotation ~ minute + temp, data=list.sub.visitquot[[10]], controls = ctree_control(mincriterion = 0.95))
  
  #Plotting those trees :
  #Sub-tree 1
  png("tree.submodel.1.95.png", res=80, height=1200, width=2400)
  plot(tree.submodel.1.95, type = "simple")
  dev.off()
  #Sub-tree 2
  png("tree.submodel.2.95.png", res=80, height=1200, width=2400)
  plot(tree.submodel.2.95, type = "simple")
  dev.off()
  #Sub-tree 3
  png("tree.submodel.3.95.png", res=80, height=1200, width=2400)
  plot(tree.submodel.3.95, type = "simple")
  dev.off()
  #Sub-tree 4
  png("tree.submodel.4.95.png", res=80, height=1200, width=2400)
  plot(tree.submodel.4.95, type = "simple")
  dev.off()
  #Sub-tree 5
  png("tree.submodel.5.95.png", res=80, height=1200, width=2400)
  plot(tree.submodel.5.95, type = "simple")
  dev.off()
  #Sub-tree 6
  png("tree.submodel.6.95.png", res=80, height=1200, width=2400)
  plot(tree.submodel.6.95, type = "simple")
  dev.off()
  #Sub-tree 7
  png("tree.submodel.7.95.png", res=80, height=1200, width=2400)
  plot(tree.submodel.7.95, type = "simple")
  dev.off()
  #Sub-tree 8
  png("tree.submodel.8.95.png", res=80, height=1200, width=2400)
  plot(tree.submodel.8.95, type = "simple")
  dev.off()
  #Sub-tree 9
  png("tree.submodel.9.95.png", res=80, height=1200, width=2400)
  plot(tree.submodel.9.95, type = "simple")
  dev.off()
  #Sub-tree 10
  png("tree.submodel.10.95.png", res=80, height=1200, width=2400)
  plot(tree.submodel.10.95, type = "simple")
  dev.off()

  
  
  

#Run a nfold cross-validation on unbalanced sample -------

  nfold = 3
  nobs = nrow(samp.visitquot)
  threshold = 0.015
  p.value = 0.05
  tree.result <- vector("list",nfold)
  names(tree.result) <- paste0("model.", seq_along(tree.result))
  
  #Create index
  for (i in 1:nfold) {
    if(i == 1) {
      index   <- matrix(rep(i, round(nobs/nfold)-1), ncol = 1)
    }
    else {
      transp  <- matrix(rep(i, round(nobs/nfold)-1), ncol = 1)
      index   <- rbind(index, transp)
    }
  }
  
  #Create multiple trees in a list
  for (i in 1:nfold) {
    # Assign in-sample and out-of-sample observations
    insample = which(index == i)
    outsample = which(index != i)
    # Run model on in-sample data only
    tree.result[[i]]$tree <- ctree(formula = quotation ~ minute + temp + humidity, data=samp.visitquot[insample,], controls = ctree_control(mincriterion = 1-p.value))
  }
  
  #Add all the other interesting features to the list
  for (i in 1:nfold) {
    # Assign in-sample and out-of-sample observations
    insample = which(index == i)
    outsample = which(index != i)
    tree.result[[i]]$data$real                  <- samp.visitquot[outsample,]$quotation
    tree.result[[i]]$data$predClass             <- predict(tree.result[[i]]$tree, newdata=samp.visitquot[outsample,], type="response")
    tree.result[[i]]$data$predNode              <- predict(tree.result[[i]]$tree, newdata=samp.visitquot[outsample,], type="node")
    tree.result[[i]]$data$predProb              <- sapply(predict(tree.result[[i]]$tree, newdata=samp.visitquot[outsample,],type="prob"),'[[',2)
    tree.result[[i]]$data$predProbClass         <- ifelse(tree.result[[i]]$data$predProb >= threshold, 1,0)
    tree.result[[i]]$analyse$confMatrix         <- table(tree.result[[i]]$data$predProbClass,tree.result[[i]]$data$real)
    tree.result[[i]]$analyse$sensitivity        <- tree.result[[i]]$analyse$confMatrix[[4]]/(tree.result[[i]]$analyse$confMatrix[[3]]+tree.result[[i]]$analyse$confMatrix[[4]])
    tree.result[[i]]$analyse$specificity        <- tree.result[[i]]$analyse$confMatrix[[1]]/(tree.result[[i]]$analyse$confMatrix[[2]]+tree.result[[i]]$analyse$confMatrix[[1]])
    tree.result[[i]]$analyse$positivePrecision  <- tree.result[[i]]$analyse$confMatrix[[4]]/(tree.result[[i]]$analyse$confMatrix[[4]]+tree.result[[i]]$analyse$confMatrix[[2]])
    tree.result[[i]]$analyse$negativePrecision  <- tree.result[[i]]$analyse$confMatrix[[1]]/(tree.result[[i]]$analyse$confMatrix[[3]]+tree.result[[i]]$analyse$confMatrix[[1]])
    tree.result[[i]]$analyse$accuracy           <- (tree.result[[i]]$analyse$confMatrix[[1]]+tree.result[[i]]$analyse$confMatrix[[4]])/(tree.result[[i]]$analyse$confMatrix[[3]]+tree.result[[i]]$analyse$confMatrix[[1]]+tree.result[[i]]$analyse$confMatrix[[4]]+tree.result[[i]]$analyse$confMatrix[[2]])
    tree.result[[i]]$roc$prediction             <- prediction(tree.result[[i]]$data$predProb, tree.result[[i]]$data$real)
    tree.result[[i]]$roc$performance            <- performance(tree.result[[i]]$roc$prediction, "tpr", "fpr")
    tree.result[[i]]$roc$lift                   <- performance(tree.result[[i]]$roc$prediction, "lift", "rpp")
    tree.result[[i]]$roc$gain                   <- performance(tree.result[[i]]$roc$prediction, "tpr", "rpp")
  }
  

#Undersample data to rebalance them ----------
  ## 70% of the sample size
  smp_size <- floor(0.70 * nrow(visitquot))
  
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
  
  ## cleaning space
  rm(undersamp.train.neg,undersamp.train.pos, undersamp.train.inter)

#Run tree models on balanced data -----------
  nfold = 4
  p.value = 0.05
  tree.result.balance <- vector("list",nfold)
  names(tree.result.balance) <- paste0("model.", seq_along(tree.result.balance))
  
  #Create multiple trees in a list based on training data
  tree.result.balance$model.1$tree <- ctree(formula = quotation ~ minute + temp + humidity, data=undersamp.train, controls = ctree_control(mincriterion = 1-p.value))
  tree.result.balance$model.2$tree <- ctree(formula = quotation ~ minute + temp, data=undersamp.train, controls = ctree_control(mincriterion = 1-p.value))
  tree.result.balance$model.3$tree <- ctree(formula = quotation ~ minute + humidity, data=undersamp.train, controls = ctree_control(mincriterion = 1-p.value))
  tree.result.balance$model.4$tree <- ctree(formula = quotation ~ minute + temp + humidity + pressure, data=undersamp.train, controls = ctree_control(mincriterion = 1-p.value))
  
  
  #Add all the other interesting features to the list
  for (i in 1:nfold) {
    # Assign in-sample and out-of-sample observations
    tree.result.balance[[i]]$data$real                  <- undersamp.test$quotation
    tree.result.balance[[i]]$data$predClass             <- Predict(tree.result.balance[[i]]$tree, newdata=undersamp.test, type="response")
    tree.result.balance[[i]]$data$predNode              <- predict(tree.result.balance[[i]]$tree, newdata=undersamp.test, type="node")
    tree.result.balance[[i]]$data$predProb              <- sapply(predict(tree.result.balance[[i]]$tree, newdata=undersamp.test,type="prob"),'[[',2)
    tree.result.balance[[i]]$analyse$confMatrix         <- table(tree.result.balance[[i]]$data$predClass,tree.result.balance[[i]]$data$real)
    tree.result.balance[[i]]$analyse$sensitivity        <- tree.result.balance[[i]]$analyse$confMatrix[[4]]/(tree.result.balance[[i]]$analyse$confMatrix[[3]]+tree.result.balance[[i]]$analyse$confMatrix[[4]])
    tree.result.balance[[i]]$analyse$specificity        <- tree.result.balance[[i]]$analyse$confMatrix[[1]]/(tree.result.balance[[i]]$analyse$confMatrix[[2]]+tree.result.balance[[i]]$analyse$confMatrix[[1]])
    tree.result.balance[[i]]$analyse$positivePrecision  <- tree.result.balance[[i]]$analyse$confMatrix[[4]]/(tree.result.balance[[i]]$analyse$confMatrix[[4]]+tree.result.balance[[i]]$analyse$confMatrix[[2]])
    tree.result.balance[[i]]$analyse$negativePrecision  <- tree.result.balance[[i]]$analyse$confMatrix[[1]]/(tree.result.balance[[i]]$analyse$confMatrix[[3]]+tree.result.balance[[i]]$analyse$confMatrix[[1]])
    tree.result.balance[[i]]$analyse$accuracy           <- (tree.result.balance[[i]]$analyse$confMatrix[[1]]+tree.result.balance[[i]]$analyse$confMatrix[[4]])/(tree.result.balance[[i]]$analyse$confMatrix[[3]]+tree.result.balance[[i]]$analyse$confMatrix[[1]]+tree.result.balance[[i]]$analyse$confMatrix[[4]]+tree.result.balance[[i]]$analyse$confMatrix[[2]])
    tree.result.balance[[i]]$roc$prediction             <- prediction(tree.result.balance[[i]]$data$predProb, tree.result.balance[[i]]$data$real)
    tree.result.balance[[i]]$roc$performance            <- performance(tree.result.balance[[i]]$roc$prediction, "tpr", "fpr")
    tree.result.balance[[i]]$roc$lift                   <- performance(tree.result.balance[[i]]$roc$prediction, "lift", "rpp")
    tree.result.balance[[i]]$roc$gain                   <- performance(tree.result.balance[[i]]$roc$prediction, "tpr", "rpp")
  }
  
  ##Plotting the models
  plot(tree.result.balance$model.1$roc$lift, col="blue") 
  plot(tree.result.balance$model.2$roc$lift, add=TRUE,col="red")
  plot(tree.result.balance$model.3$roc$lift, add=TRUE,col="green")
  plot(tree.result.balance$model.4$roc$lift, add=TRUE,col="black")

#Run several tree models on different sample size ----------
  nfold = 2
  nobs = nrow(undersamp.visitquot)
  p.value = 0.05
  tree.under.result <- list()
  
  #Create index
  for (i in 1:nfold) {
    if(i == 1) {
      index <- matrix(rep(i, round(nobs/nfold)-1), ncol = 1)
    }
    else {
      transp <- matrix(rep(i, round(nobs/nfold)-1), ncol = 1)
      index <- rbind(index, transp)
    }
  }
  
  #Create multiple trees in a list
  # Assign in-sample and out-of-sample observations
  insample = which(index == 1)
  outsample = which(index != 1)
  # Run model on in-sample data only
  tree.under.result$model.1$tree <- ctree(formula = quotation ~ minute + temp + humidity, data = undersamp.visitquot)
  tree.under.result$model.2$tree <- ctree(formula = quotation ~ minute + temp, data = undersamp.visitquot)
  tree.under.result$model.3$tree <- ctree(formula = quotation ~ minute + temp + humidity + pressure, data = undersamp.visitquot)
  
  
  #Add all the other interesting features to the list
  for (i in 1:length(tree.under.result)) {
    tree.under.result[[i]]$data$real                  <- samp.visitquot[outsample,]$quotation
    tree.under.result[[i]]$data$predClass             <- predict(tree.under.result[[i]]$tree, newdata=samp.visitquot[outsample,], type="response")
    tree.under.result[[i]]$data$predNode              <- predict(tree.under.result[[i]]$tree, newdata=samp.visitquot[outsample,], type="node")
    tree.under.result[[i]]$data$predProb              <- sapply(predict(tree.under.result[[i]]$tree, newdata=samp.visitquot[outsample,],type="prob"),'[[',2)
    tree.under.result[[i]]$analyse$confMatrix         <- table(tree.under.result[[i]]$data$predClass,tree.under.result[[i]]$data$real)
    tree.under.result[[i]]$analyse$sensitivity        <- tree.under.result[[i]]$analyse$confMatrix[[4]]/(tree.under.result[[i]]$analyse$confMatrix[[3]]+tree.under.result[[i]]$analyse$confMatrix[[4]])
    tree.under.result[[i]]$analyse$specificity        <- tree.under.result[[i]]$analyse$confMatrix[[1]]/(tree.under.result[[i]]$analyse$confMatrix[[2]]+tree.under.result[[i]]$analyse$confMatrix[[1]])
    tree.under.result[[i]]$analyse$positivePrecision  <- tree.under.result[[i]]$analyse$confMatrix[[4]]/(tree.under.result[[i]]$analyse$confMatrix[[4]]+tree.under.result[[i]]$analyse$confMatrix[[2]])
    tree.under.result[[i]]$analyse$negativePrecision  <- tree.under.result[[i]]$analyse$confMatrix[[1]]/(tree.under.result[[i]]$analyse$confMatrix[[3]]+tree.under.result[[i]]$analyse$confMatrix[[1]])
    tree.under.result[[i]]$analyse$accuracy           <- (tree.under.result[[i]]$analyse$confMatrix[[1]]+tree.under.result[[i]]$analyse$confMatrix[[4]])/(tree.under.result[[i]]$analyse$confMatrix[[3]]+tree.under.result[[i]]$analyse$confMatrix[[1]]+tree.under.result[[i]]$analyse$confMatrix[[4]]+tree.under.result[[i]]$analyse$confMatrix[[2]])
    tree.under.result[[i]]$roc$prediction             <- prediction(tree.under.result[[i]]$data$predProb, tree.under.result[[i]]$data$real)
    tree.under.result[[i]]$roc$performance            <- performance(tree.under.result[[i]]$roc$prediction, "tpr", "fpr")
    tree.under.result[[i]]$roc$lift                   <- performance(tree.under.result[[i]]$roc$prediction, "lift", "rpp")
    tree.under.result[[i]]$roc$gain                   <- performance(tree.under.result[[i]]$roc$prediction, "tpr", "rpp")
  }
  

#Run the best models from unbalanced and balanced dataset on same testing sample -----------
  ## 70% of the sample size to get the same 30% testing sample
  smp_size        <- floor(0.70 * nrow(visitquot))
  smp_unbal_size  <- floor(0.166* nrow(visitquot))
  ## set the seed used for both models previously
  set.seed(123)
  sample.under            <- sample_n(visitquot, size = nrow(visitquot))
  ## create all the required data set
  sample.test             <- sample.under[(smp_size + 1):nrow(visitquot),]
  
  sample.unbalanced.train <- sample.under[1:smp_unbal_size,]
  
  undersamp.train.inter   <- sample.under[1:smp_size,]
  undersamp.train.pos     <- undersamp.train.inter[undersamp.train.inter$quotation==1,]
  undersamp.train.neg     <- undersamp.train.inter[undersamp.train.inter$quotation==0,]
  undersamp.train.neg     <- undersamp.train.neg[1:length(undersamp.train.pos$quotation), ]
  sample.balanced.train   <- rbind_list(undersamp.train.pos, undersamp.train.neg)
  ## run all the models on there designated dataset with 2 p-value tested
  nfold = 4
  p.value.05 = 0.05
  p.value.01 = 0.01
  threshold = 0.015
  tree.result.compare <- list()
  
  tree.result.compare$model.unbal.05$tree <- ctree(formula = quotation ~ minute + temp + humidity, data=sample.unbalanced.train, controls = ctree_control(mincriterion = 1-p.value.05))
  tree.result.compare$model.unbal.01$tree <- ctree(formula = quotation ~ minute + temp + humidity, data=sample.unbalanced.train, controls = ctree_control(mincriterion = 1-p.value.01))
  tree.result.compare$model.bal.05$tree   <- ctree(formula = quotation ~ minute + temp + humidity, data=sample.balanced.train, controls = ctree_control(mincriterion = 1-p.value.05))
  tree.result.compare$model.bal.01$tree   <- ctree(formula = quotation ~ minute + temp + humidity, data=sample.balanced.train, controls = ctree_control(mincriterion = 1-p.value.01))
  
  ## generate analytic values
  #### for unbalanced data
  for (i in 1:2) {
    # Assign in-sample and out-of-sample observations
    tree.result.compare[[i]]$data$real                  <- sample.test$quotation
    tree.result.compare[[i]]$data$predClass             <- predict(tree.result.compare[[i]]$tree, newdata=sample.test, type="response")
    tree.result.compare[[i]]$data$predNode              <- predict(tree.result.compare[[i]]$tree, newdata=sample.test, type="node")
    tree.result.compare[[i]]$data$predProb              <- sapply(predict(tree.result.compare[[i]]$tree, newdata=sample.test,type="prob"),'[[',2)
    tree.result.compare[[i]]$data$predProbClass         <- ifelse(tree.result.compare[[i]]$data$predProb >= threshold, 1,0)
    tree.result.compare[[i]]$analyse$confMatrix         <- table(tree.result.compare[[i]]$data$predProbClass,tree.result.compare[[i]]$data$real)
    tree.result.compare[[i]]$analyse$sensitivity        <- tree.result.compare[[i]]$analyse$confMatrix[[4]]/(tree.result.compare[[i]]$analyse$confMatrix[[3]]+tree.result.compare[[i]]$analyse$confMatrix[[4]])
    tree.result.compare[[i]]$analyse$specificity        <- tree.result.compare[[i]]$analyse$confMatrix[[1]]/(tree.result.compare[[i]]$analyse$confMatrix[[2]]+tree.result.compare[[i]]$analyse$confMatrix[[1]])
    tree.result.compare[[i]]$analyse$positivePrecision  <- tree.result.compare[[i]]$analyse$confMatrix[[4]]/(tree.result.compare[[i]]$analyse$confMatrix[[4]]+tree.result.compare[[i]]$analyse$confMatrix[[2]])
    tree.result.compare[[i]]$analyse$negativePrecision  <- tree.result.compare[[i]]$analyse$confMatrix[[1]]/(tree.result.compare[[i]]$analyse$confMatrix[[3]]+tree.result.compare[[i]]$analyse$confMatrix[[1]])
    tree.result.compare[[i]]$analyse$accuracy           <- (tree.result.compare[[i]]$analyse$confMatrix[[1]]+tree.result.compare[[i]]$analyse$confMatrix[[4]])/(tree.result.compare[[i]]$analyse$confMatrix[[3]]+tree.result.compare[[i]]$analyse$confMatrix[[1]]+tree.result.compare[[i]]$analyse$confMatrix[[4]]+tree.result.compare[[i]]$analyse$confMatrix[[2]])
    tree.result.compare[[i]]$roc$prediction             <- prediction(tree.result.compare[[i]]$data$predProb, tree.result.compare[[i]]$data$real)
    tree.result.compare[[i]]$roc$performance            <- performance(tree.result.compare[[i]]$roc$prediction, "tpr", "fpr")
    tree.result.compare[[i]]$roc$lift                   <- performance(tree.result.compare[[i]]$roc$prediction, "lift", "rpp")
    tree.result.compare[[i]]$roc$gain                   <- performance(tree.result.compare[[i]]$roc$prediction, "tpr", "rpp")
  }
  
  #### for balanced data
  for (i in 3:4) {
    # Assign in-sample and out-of-sample observations
    tree.result.compare[[i]]$data$real                  <- sample.test$quotation
    tree.result.compare[[i]]$data$predClass             <- predict(tree.result.compare[[i]]$tree, newdata=sample.test, type="response")
    tree.result.compare[[i]]$data$predNode              <- predict(tree.result.compare[[i]]$tree, newdata=sample.test, type="node")
    tree.result.compare[[i]]$data$predProb              <- sapply(predict(tree.result.compare[[i]]$tree, newdata=sample.test,type="prob"),'[[',2)
    tree.result.compare[[i]]$analyse$confMatrix         <- table(tree.result.compare[[i]]$data$predClass,tree.result.compare[[i]]$data$real)
    tree.result.compare[[i]]$analyse$sensitivity        <- tree.result.compare[[i]]$analyse$confMatrix[[4]]/(tree.result.compare[[i]]$analyse$confMatrix[[3]]+tree.result.compare[[i]]$analyse$confMatrix[[4]])
    tree.result.compare[[i]]$analyse$specificity        <- tree.result.compare[[i]]$analyse$confMatrix[[1]]/(tree.result.compare[[i]]$analyse$confMatrix[[2]]+tree.result.compare[[i]]$analyse$confMatrix[[1]])
    tree.result.compare[[i]]$analyse$positivePrecision  <- tree.result.compare[[i]]$analyse$confMatrix[[4]]/(tree.result.compare[[i]]$analyse$confMatrix[[4]]+tree.result.compare[[i]]$analyse$confMatrix[[2]])
    tree.result.compare[[i]]$analyse$negativePrecision  <- tree.result.compare[[i]]$analyse$confMatrix[[1]]/(tree.result.compare[[i]]$analyse$confMatrix[[3]]+tree.result.compare[[i]]$analyse$confMatrix[[1]])
    tree.result.compare[[i]]$analyse$accuracy           <- (tree.result.compare[[i]]$analyse$confMatrix[[1]]+tree.result.compare[[i]]$analyse$confMatrix[[4]])/(tree.result.compare[[i]]$analyse$confMatrix[[3]]+tree.result.compare[[i]]$analyse$confMatrix[[1]]+tree.result.compare[[i]]$analyse$confMatrix[[4]]+tree.result.compare[[i]]$analyse$confMatrix[[2]])
    tree.result.compare[[i]]$roc$prediction             <- prediction(tree.result.compare[[i]]$data$predProb, tree.result.compare[[i]]$data$real)
    tree.result.compare[[i]]$roc$performance            <- performance(tree.result.compare[[i]]$roc$prediction, "tpr", "fpr")
    tree.result.compare[[i]]$roc$lift                   <- performance(tree.result.compare[[i]]$roc$prediction, "lift", "rpp")
    tree.result.compare[[i]]$roc$gain                   <- performance(tree.result.compare[[i]]$roc$prediction, "tpr", "rpp")
  }
  

#Removing nodes from a tree ---------
  insample = which(index == 1)
  NewWeigths <- rep(1, length(samp.visitquot[insample,]$quotation)) # Setting a weights vector which will be passed into the `weights` attribute in `ctree`
  Node <- c(31,32,33,20,22,23,8,9,10) # Selecting node #5
  n <- list()
  for (i in 1:length(Node)) {
    n[[i]] <- nodes(tree.result$model.1$tree, Node[i])[[1]] # Retrieving the weights of that node
    NewWeigths[which(as.logical(n[[i]]$weights))] <- 0
  }
   # Setting these weigths to zero, so `ctree` will disregard them
  tree.result$model.1.reweight$tree <- ctree(quotation ~ minute + temp + humidity, data=samp.visitquot[insample,], controls = ctree_control(mincriterion = 1-p.value), weights = NewWeigths) # creating the new tree with new weights
  
  
  
  tree.result$model.1.reweight$data$real                  <- samp.visitquot[outsample,]$quotation
  tree.result$model.1.reweight$data$predProb              <- sapply(predict(tree.result$model.1.reweight$tree, newdata=samp.visitquot[outsample,],type="prob"),'[[',2)
  tree.result$model.1.reweight$roc$prediction             <- prediction(tree.result$model.1.reweight$data$predProb, tree.result$model.1.reweight$data$real)
  tree.result$model.1.reweight$roc$performance            <- performance(tree.result$model.1.reweight$roc$prediction, "tpr", "fpr")
  tree.result$model.1.reweight$roc$lift                   <- performance(tree.result$model.1.reweight$roc$prediction, "lift", "rpp")
  tree.result$model.1.reweight$roc$gain                   <- performance(tree.result$model.1.reweight$roc$prediction, "tpr", "rpp")
  
#Best model threshold with several models -------------
  #Build the data
  threshold.multi <- vector("list",nfold)
  names(threshold.multi) <- paste0("model.", seq_along(threshold.multi))
  threshold.vector <- seq(0.005,0.025,0.001)
  for (i in 1:nfold) {
    threshold.multi[[i]]$data$predProb              <- tree.result[[i]]$data$predProb
    threshold.multi[[i]]$data$predProbClass         <- vector("list", length( threshold.vector ))
    names(threshold.multi[[i]]$data$predProbClass)  <- paste0("threshold=",threshold.vector)
    threshold.multi[[i]]$data$real                  <- ifelse(as.numeric(tree.result[[i]]$data$real)==2,1,0)
    for (j in 1:length(threshold.vector)) {
      threshold.multi[[i]]$data$predProbClass[[j]]  <- ifelse(threshold.multi[[i]]$data$predProb >= threshold.vector[j], 1,0)
    }
  }
  
  #Apply analytic methods
  for (i in 1:nfold) {
    threshold.multi[[i]]$analyse$confMatrix         <- vector("list", length( threshold.vector ))
    names(threshold.multi[[i]]$analyse$confMatrix)  <- paste0("threshold=",threshold.vector)
    threshold.multi[[i]]$analyse$sensitivity        <- vector("numeric",length( threshold.vector ))
    threshold.multi[[i]]$analyse$specificity        <- vector("numeric",length( threshold.vector ))
    threshold.multi[[i]]$analyse$positivePrecision  <- vector("numeric",length( threshold.vector ))
    threshold.multi[[i]]$analyse$negativePrecision  <- vector("numeric",length( threshold.vector ))
    threshold.multi[[i]]$analyse$accuracy           <- vector("numeric",length( threshold.vector ))
    
    #Add ROC analysis
    threshold.multi[[i]]$roc$prediction             <- prediction(threshold.multi[[i]]$data$predProb, threshold.multi[[i]]$data$real)
    threshold.multi[[i]]$roc$performance            <- performance(threshold.multi[[i]]$roc$prediction, "tpr", "fpr")
    threshold.multi[[i]]$roc$lift                   <- performance(threshold.multi[[i]]$roc$prediction, "lift", "rpp")
    threshold.multi[[i]]$roc$gain                   <- performance(threshold.multi[[i]]$roc$prediction, "tpr", "rpp")
    
    
    #Count positive and negative real values
    p <- sum(threshold.multi[[i]]$data$real)
    n <- length(threshold.multi[[i]]$data$real) - p
    for (j in 1:length(threshold.vector)) {
      #Check if all values are of class 1 for a given threshold
      if (sum(threshold.multi[[i]]$data$predProbClass[[j]])==p+n) {
        threshold.multi[[i]]$analyse$confMatrix[[j]]         <- table(threshold.multi[[i]]$data$predProbClass[[j]],threshold.multi[[i]]$data$real)
        threshold.multi[[i]]$analyse$sensitivity[[j]]        <- 1
        threshold.multi[[i]]$analyse$specificity[[j]]        <- 0
        threshold.multi[[i]]$analyse$positivePrecision[[j]]  <- p/(p+n)
        threshold.multi[[i]]$analyse$negativePrecision[[j]]  <- 0
        threshold.multi[[i]]$analyse$accuracy[[j]]           <- p/(p+n)
        
      }
      #Check if all values are of class 0 for a given threshold
      else if(sum(threshold.multi[[i]]$data$predProbClass[[j]])==0) {
        threshold.multi[[i]]$analyse$confMatrix[[j]]         <- table(threshold.multi[[i]]$data$predProbClass[[j]],threshold.multi[[i]]$data$real)
        threshold.multi[[i]]$analyse$sensitivity[[j]]        <- 0
        threshold.multi[[i]]$analyse$specificity[[j]]        <- 1
        threshold.multi[[i]]$analyse$positivePrecision[[j]]  <- 0
        threshold.multi[[i]]$analyse$negativePrecision[[j]]  <- n/(p+n)
        threshold.multi[[i]]$analyse$accuracy[[j]]           <- n/(p+n)
      }
      else {
        threshold.multi[[i]]$analyse$confMatrix[[j]]         <- table(threshold.multi[[i]]$data$predProbClass[[j]],threshold.multi[[i]]$data$real)
        threshold.multi[[i]]$analyse$sensitivity[[j]]        <- threshold.multi[[i]]$analyse$confMatrix[[j]][[4]]/(threshold.multi[[i]]$analyse$confMatrix[[j]][[3]]+threshold.multi[[i]]$analyse$confMatrix[[j]][[4]])
        threshold.multi[[i]]$analyse$specificity[[j]]        <- threshold.multi[[i]]$analyse$confMatrix[[j]][[1]]/(threshold.multi[[i]]$analyse$confMatrix[[j]][[2]]+threshold.multi[[i]]$analyse$confMatrix[[j]][[1]])
        threshold.multi[[i]]$analyse$positivePrecision[[j]]  <- threshold.multi[[i]]$analyse$confMatrix[[j]][[4]]/(threshold.multi[[i]]$analyse$confMatrix[[j]][[4]]+threshold.multi[[i]]$analyse$confMatrix[[j]][[2]])
        threshold.multi[[i]]$analyse$negativePrecision[[j]]  <- threshold.multi[[i]]$analyse$confMatrix[[j]][[1]]/(threshold.multi[[i]]$analyse$confMatrix[[j]][[3]]+threshold.multi[[i]]$analyse$confMatrix[[j]][[1]])
        threshold.multi[[i]]$analyse$accuracy[[j]]           <- (threshold.multi[[i]]$analyse$confMatrix[[j]][[1]]+threshold.multi[[i]]$analyse$confMatrix[[j]][[4]])/(threshold.multi[[i]]$analyse$confMatrix[[j]][[3]]+threshold.multi[[i]]$analyse$confMatrix[[j]][[1]]+threshold.multi[[i]]$analyse$confMatrix[[j]][[4]]+threshold.multi[[i]]$analyse$confMatrix[[j]][[2]])
      }
    }
  }

#Best model threshold for one model ----------
  #Build the data
  threshold.multi <- list()
  threshold.vector <- seq(0.005,0.025,0.001)
  
  threshold.multi$data$predProb              <- tree.result.compare$model.unbal.01$data$predProb
  threshold.multi$data$predProbClass         <- vector("list", length( threshold.vector ))
  names(threshold.multi$data$predProbClass)  <- paste0("threshold=",threshold.vector)
  threshold.multi$data$real                  <- ifelse(as.numeric(tree.result.compare$model.unbal.01$data$real)==2,1,0)
  for (j in 1:length(threshold.vector)) {
    threshold.multi$data$predProbClass[[j]]  <- ifelse(threshold.multi$data$predProb >= threshold.vector[j], 1,0)
  }
  
  
  #Apply analytic methods
  
  threshold.multi$analyse$confMatrix         <- vector("list", length( threshold.vector ))
  names(threshold.multi$analyse$confMatrix)  <- paste0("threshold=",threshold.vector)
  threshold.multi$analyse$sensitivity        <- vector("numeric",length( threshold.vector ))
  threshold.multi$analyse$specificity        <- vector("numeric",length( threshold.vector ))
  threshold.multi$analyse$positivePrecision  <- vector("numeric",length( threshold.vector ))
  threshold.multi$analyse$negativePrecision  <- vector("numeric",length( threshold.vector ))
  threshold.multi$analyse$accuracy           <- vector("numeric",length( threshold.vector ))
  
  #Add ROC analysis
  threshold.multi$roc$prediction             <- prediction(threshold.multi$data$predProb, threshold.multi$data$real)
  threshold.multi$roc$performance            <- performance(threshold.multi$roc$prediction, "tpr", "fpr")
  threshold.multi$roc$lift                   <- performance(threshold.multi$roc$prediction, "lift", "rpp")
  threshold.multi$roc$gain                   <- performance(threshold.multi$roc$prediction, "tpr", "rpp")
  
  
  #Count positive and negative real values
  p <- sum(threshold.multi$data$real)
  n <- length(threshold.multi$data$real) - p
  for (j in 1:length(threshold.vector)) {
    #Check if all values are of class 1 for a given threshold
    if (sum(threshold.multi$data$predProbClass[[j]])==p+n) {
      threshold.multi$analyse$confMatrix[[j]]         <- table(threshold.multi$data$predProbClass[[j]],threshold.multi$data$real)
      threshold.multi$analyse$sensitivity[[j]]        <- 1
      threshold.multi$analyse$specificity[[j]]        <- 0
      threshold.multi$analyse$positivePrecision[[j]]  <- p/(p+n)
      threshold.multi$analyse$negativePrecision[[j]]  <- 0
      threshold.multi$analyse$accuracy[[j]]           <- p/(p+n)
      
    }
    #Check if all values are of class 0 for a given threshold
    else if(sum(threshold.multi$data$predProbClass[[j]])==0) {
      threshold.multi$analyse$confMatrix[[j]]         <- table(threshold.multi$data$predProbClass[[j]],threshold.multi$data$real)
      threshold.multi$analyse$sensitivity[[j]]        <- 0
      threshold.multi$analyse$specificity[[j]]        <- 1
      threshold.multi$analyse$positivePrecision[[j]]  <- 0
      threshold.multi$analyse$negativePrecision[[j]]  <- n/(p+n)
      threshold.multi$analyse$accuracy[[j]]           <- n/(p+n)
    }
    else {
      threshold.multi$analyse$confMatrix[[j]]         <- table(threshold.multi$data$predProbClass[[j]],threshold.multi$data$real)
      threshold.multi$analyse$sensitivity[[j]]        <- threshold.multi$analyse$confMatrix[[j]][[4]]/(threshold.multi$analyse$confMatrix[[j]][[3]]+threshold.multi$analyse$confMatrix[[j]][[4]])
      threshold.multi$analyse$specificity[[j]]        <- threshold.multi$analyse$confMatrix[[j]][[1]]/(threshold.multi$analyse$confMatrix[[j]][[2]]+threshold.multi$analyse$confMatrix[[j]][[1]])
      threshold.multi$analyse$positivePrecision[[j]]  <- threshold.multi$analyse$confMatrix[[j]][[4]]/(threshold.multi$analyse$confMatrix[[j]][[4]]+threshold.multi$analyse$confMatrix[[j]][[2]])
      threshold.multi$analyse$negativePrecision[[j]]  <- threshold.multi$analyse$confMatrix[[j]][[1]]/(threshold.multi$analyse$confMatrix[[j]][[3]]+threshold.multi$analyse$confMatrix[[j]][[1]])
      threshold.multi$analyse$accuracy[[j]]           <- (threshold.multi$analyse$confMatrix[[j]][[1]]+threshold.multi$analyse$confMatrix[[j]][[4]])/(threshold.multi$analyse$confMatrix[[j]][[3]]+threshold.multi$analyse$confMatrix[[j]][[1]]+threshold.multi$analyse$confMatrix[[j]][[4]]+threshold.multi$analyse$confMatrix[[j]][[2]])
    }
  }
  
  

#Model testing -----------

  #Print the sensitivity
  for (i in 1:nfold){
    if (length(tree.result[[i]]$analyse$sensitivity) != 0){
      plot(tree.result[[i]]$tree, type = "simple")
    }
  }
  

#Plotting ROC predictions with 3 folds ----------
  plot(tree.result.compare$model.1$roc$performance, col="blue") 
  plot(tree.result.compare$model.2$roc$performance, add=TRUE,col="red")
  plot(tree.result.compare$model.3$roc$performance, add=TRUE,col="green")
  plot(tree.result.compare$model.3$roc$performance, add=TRUE,col="black")
  abline(0,1,col="grey")
  
  plot(tree.result$model.1$roc$lift, col="blue") 
  plot(tree.result$model.2$roc$lift, add=TRUE,col="red")
  plot(tree.result$model.3$roc$lift, add=TRUE,col="green")
  plot(tree.result$model.1.reweight$roc$lift, add=TRUE,col="black")
  
  plot(tree.result$model.1$roc$gain, col="blue") 
  plot(tree.result$model.2$roc$gain, add=TRUE,col="red")
  plot(tree.result$model.3$roc$gain, add=TRUE,col="green")

#/!\ Sensitivity and specifity per node and model--------------
  for (i in 1:nfold){
    cat("\n###Model ",i," ###\n")
    for (j in unique(tree.result[[i]]$data$predNode)){
      c <- table(tree.result[[i]]$data$predProbClass[tree.result[[i]]$data$predNode == j],tree.result[[i]]$data$real[tree.result[[i]]$data$predNode == j])
      cat("\nNode : ", j, "\n", "weight : ",length(tree.result[[i]]$data$predProbClass[tree.result[[i]]$data$predNode == j])/length(tree.result[[i]]$data$predProbClass)*100,"%","\n","sensitivity : ",
          ifelse(tree.result[[i]]$data$predProbClass[tree.result[[i]]$data$predNode == j][1]==1, c[1,2]/(c[1,2]+c[1,1]),0),"\n","specificity : ",
          ifelse(tree.result[[i]]$data$predProbClass[tree.result[[i]]$data$predNode == j][1]==0, c[1,1]/(c[1,2]+c[1,1]),0),"\n")
    }
  }

#Working with nodes() ----------
nodes <- nodes(tree.model, unique(where(tree.model)))

#Output all informations from the tree
freq <- matrix(,nrow = max(quotnode[,2]), ncol = 2)
for (i in 1:max(quotnode[,2])){
  subquot <- subset(quotnode, quotnode[,2]==i)
  freq[i,1] <- sum(subquot[,1])/length(subquot[,1])
  freq[i,2] <- i
  print(paste("Node ", i, " : sum = ",sum(subquot[,1]), " and count = ", length(subquot[,1])))
}

format_tree(tree.model@tree)
