####################
#Functions to use
####################

#Compute subsample trees
ctree_sampling <- function (df, split, stat=0.95){
  list_sample <- split(df, split)
  for (i in 1:split){
    assign(paste("tree.submodel.",i,".",stat), ctree(formula = quotation ~ minute + temp, data=list_sample[[i]], controls = ctree_control(mincriterion = stat)))
  }
}

df_split <- function (df, number){
  sizedf      <- length(df[,1])
  bound       <- sizedf/number
  list        <- list() 
  for (i in 1:number){
    list[i] <- list(df[((i*bound+1)-bound):(i*bound),])
  }
  return(list)
}


#To be finished when as.formula() error fixed
multiple_ctree <- function (list, stat){
  sizelist <- length(list)
  for (i in 1:sizelist){
    assign(paste("tree.submodel.",i,".",stat), ctree(formula = quotation ~ minute + temp, data=list_sample[[i]], controls = ctree_control(mincriterion = stat)))
  }
}

#Format Tree upgraded from :
#http://stackoverflow.com/questions/20189286/how-to-print-all-nodes-with-weight-and-prediction-value-of-a-plot-created-using
format_tree <- function(x, res=NULL) {
  if (!x$terminal) {
    ## left branch
    res_l <- c(res,
               sprintf("%s<=%.3f", x$psplit$variableName, x$psplit$splitpoint))
    if (x$left$terminal) {
      format_tree(x$left,
                  c(res_l,
                    sprintf("criterion=%.3f, statistic=%.3f",
                            x$criterion$maxcriterion,
                            max(x$criterion$statistic))))
    } else {
      format_tree(x$left, res_l)
    }
    ## right branch
    res_r <- c(res,
               sprintf("%s>%.3f", x$psplit$variableName, x$psplit$splitpoint))
    if (x$right$terminal) {
      format_tree(x$right,
                  c(res_r,
                    sprintf("criterion=%.3f, statistic=%.3f",
                            x$criterion$maxcriterion,
                            max(x$criterion$statistic))))
      
    } else {
      format_tree(x$right, res_r)
    }
  } else {
    cat(paste(res, collapse=", "), ", weights=", sum(x$weights), ", proba.0=", x$prediction[[1]], ", proba.1=", x$prediction[[2]],
        "\n", sep="")
  }
  invisible(NULL)
}



#To use the boxcox function for a single data vector instead of a linear model
boxcox_vector <- function(vector){
  out <- boxcox(vector~1)
  return(out)
}

multi_distr_test <- function(vector){
  f1 <- fitdist(vector,"norm")
  plotdist(vector,"norm",para=list(mean=f1$estimate[1],sd=f1$estimate[2]))
  f1 <- fitdist(vector,"logis")
  plotdist(vector,"logis",para=list(location=f1$estimate[1],scale=f1$estimate[2]))
  f1 <- fitdist(vector, "cauchy")
  plotdist(vector,"cauchy",para=list(location=f1$estimate[1],scale=f1$estimate[2]))
  f1 <- fitdist(vector,"lnorm")
  plotdist(vector,"lnorm",para=list(meanlog=f1$estimate[1],sdlog=f1$estimate[2]))
}


multi_summary <- function (list)
{
  lengthlist <- length(list)
  x <- matrix(, nrow = lengthlist, ncol = 8)
  colnames(x)  <- c("Min","Max","1st Qu.","Median","3rd Qu.","Count","Mean", "Std")
  
  for (i in 1:lengthlist){
    x[i,1] <- min(list[[i]])
    x[i,2] <- max(list[[i]])
    x[i,3] <- quantile(list[[i]],0.25)
    x[i,4] <- median(list[[i]])
    x[i,5] <- quantile(list[[i]],0.75)
    x[i,6] <- length(list[[i]])
    x[i,7] <- mean(list[[i]])
    x[i,8] <- sd(list[[i]])
  }
  return(x)
}

f_summary <- function(data_to_plot, name) #Originaly from Mike on stackoverflow
{
  ## univariate data summary
  require(nortest)
  #data <- as.numeric(scan ("data.txt"))
  data <- na.omit(as.numeric(as.character(data_to_plot)))
  dataFull <- as.numeric(as.character(data_to_plot))
  
  # first job is to save the graphics parameters currently used
  def.par <- par(no.readonly = TRUE)
  par("plt" = c(.2,.95,.2,.8))
  layout( matrix(c(1,1,2,2,1,1,2,2,4,5,6,7,4,5,6,7,3,3,3,3,3,3,3,3), 6, 4, byrow = TRUE))
  
  #histogram on the top left
  h <- hist(data, breaks = "Sturges", plot = FALSE)
  xfit<-seq(min(data),max(data),length=100)
  yfit<-yfit<-dnorm(xfit,mean=mean(data),sd=sd(data))
  yfit <- yfit*diff(h$mids[1:2])*length(data)
  plot (h, axes = TRUE, main = paste(deparse(substitute(name))), cex.main=2, xlab=NA)
  lines(xfit, yfit, col="blue", lwd=2)
  add1 <- paste("min = ", round(min(data), digits = 4))
  add2 <- paste("max = ", round(max(data), digits = 4))
  add3 <- paste("median = ", round(median(data), digits = 4))
  leg1 <- paste("mean = ", round(mean(data), digits = 4))
  leg2 <- paste("sd = ", round(sd(data),digits = 4))
  count <- paste("count = ", sum(!is.na(dataFull)))
  missing <- paste("missing = ", sum(is.na(dataFull)))
  legend(x = "topright", c(add1, add2, add3, leg1,leg2,count,missing), bty = "n")
  
  ## normal qq plot
  qqnorm(data, bty = "n", pch = 20)
  qqline(data)
  p <- ad.test(data)
  leg <- paste("Anderson-Darling p = ", round(as.numeric(p[2]), digits = 4))
  legend(x = "topleft", leg, bty = "n")
  
  ## boxplot (bottom left)
  boxplot(data, horizontal = TRUE)
  leg1 <- paste("median = ", round(median(data), digits = 4))
  lq <- quantile(data, 0.25)
  leg2 <- paste("25th percentile =  ", round(lq,digits = 4))
  uq <- quantile(data, 0.75)
  leg3 <- paste("75th percentile = ", round(uq,digits = 4))
  legend(x = "top", leg1, bty = "n")
  legend(x = "bottom", paste(leg2, leg3, sep = "; "), bty = "n")
  
  ## the various histograms with different bins
  h2 <- hist(data,  breaks = (0:20 * (max(data) - min (data))/20)+min(data), plot = FALSE)
  plot (h2, axes = TRUE, main = "20 bins")
  
  h3 <- hist(data,  breaks = (0:10 * (max(data) - min (data))/10)+min(data), plot = FALSE)
  plot (h3, axes = TRUE, main = "10 bins")
  
  h4 <- hist(data,  breaks = (0:8 * (max(data) - min (data))/8)+min(data), plot = FALSE)
  plot (h4, axes = TRUE, main = "8 bins")
  
  h5 <- hist(data,  breaks = (0:6 * (max(data) - min (data))/6)+min(data), plot = FALSE)
  plot (h5, axes = TRUE,main = "6 bins")
  
  ## reset the graphics display to default
  par(def.par)
  
  #original code for f_summary by respiratoryclub
  
}

f_boxplot <- function(data_1, name1, data_2=NULL, name2=NULL, data_3=NULL, name3=NULL, data_4=NULL, name4=NULL, data_5=NULL, name5=NULL, data_6=NULL, name6=NULL)
{
  nargs <- nargs()
  data1 <- na.omit(as.numeric(as.character(data_1)))
  data2 <- na.omit(as.numeric(as.character(data_2)))
  data3 <- na.omit(as.numeric(as.character(data_3)))
  data4 <- na.omit(as.numeric(as.character(data_4)))
  data5 <- na.omit(as.numeric(as.character(data_5)))
  data6 <- na.omit(as.numeric(as.character(data_6)))
  
  
  #Create the Layout matrix
  mat <- matrix(, nrow = 4, ncol = (nargs)/2)
  for (i in 1:4){
    for (j in 1:(nargs)/2){
      mat[i,j] <- j      
    }
  }
  layout(mat)
  
  
  ## boxplot1
  boxplot(data1, horizontal = FALSE)
  leg1 <- paste("median = ", round(median(data1), digits = 4))
  lq <- quantile(data1, 0.25)
  leg2 <- paste("25th percentile =  ", round(lq,digits = 4))
  uq <- quantile(data1, 0.75)
  leg3 <- paste("75th percentile = ", round(uq,digits = 4))
  legend(x = "top", leg1, bty = "n")
  legend(x = "bottom", paste(leg2, leg3, sep = "; "), bty = "n")
  title(main=paste(deparse(substitute(name1))))
  
  ## boxplot2
  if(nargs >=3){
    boxplot(data2, horizontal = FALSE)
    leg1 <- paste("median = ", round(median(data2), digits = 4))
    lq <- quantile(data2, 0.25)
    leg2 <- paste("25th percentile =  ", round(lq,digits = 4))
    uq <- quantile(data2, 0.75)
    leg3 <- paste("75th percentile = ", round(uq,digits = 4))
    legend(x = "top", leg1, bty = "n")
    legend(x = "bottom", paste(leg2, leg3, sep = "; "), bty = "n")
    title(main=paste(deparse(substitute(name2))))
  }    
  
  ## boxplot3
  if(nargs >=5){
    boxplot(data3, horizontal = FALSE)
    leg1 <- paste("median = ", round(median(data3), digits = 4))
    lq <- quantile(data3, 0.25)
    leg2 <- paste("25th percentile =  ", round(lq,digits = 4))
    uq <- quantile(data3, 0.75)
    leg3 <- paste("75th percentile = ", round(uq,digits = 4))
    legend(x = "top", leg1, bty = "n")
    legend(x = "bottom", paste(leg2, leg3, sep = "; "), bty = "n")
    title(main=paste(deparse(substitute(name3))))
  }
  
  ## boxplot4
  if(nargs >=7){
    boxplot(data4, horizontal = FALSE)
    leg1 <- paste("median = ", round(median(data4), digits = 4))
    lq <- quantile(data4, 0.25)
    leg2 <- paste("25th percentile =  ", round(lq,digits = 4))
    uq <- quantile(data4, 0.75)
    leg3 <- paste("75th percentile = ", round(uq,digits = 4))
    legend(x = "top", leg1, bty = "n")
    legend(x = "bottom", paste(leg2, leg3, sep = "; "), bty = "n")
    title(main=paste(deparse(substitute(name4))))
  }
  
  
  ## boxplot5
  if(nargs >=9){
    boxplot(data5, horizontal = FALSE)
    leg1 <- paste("median = ", round(median(data5), digits = 4))
    lq <- quantile(data5, 0.25)
    leg2 <- paste("25th percentile =  ", round(lq,digits = 4))
    uq <- quantile(data5, 0.75)
    leg3 <- paste("75th percentile = ", round(uq,digits = 4))
    legend(x = "top", leg1, bty = "n")
    legend(x = "bottom", paste(leg2, leg3, sep = "; "), bty = "n")
    title(main=paste(deparse(substitute(name5))))
  }
  
  ## boxplot6
  if(nargs >=11){
    boxplot(data6, horizontal = FALSE)
    leg1 <- paste("median = ", round(median(data6), digits = 4))
    lq <- quantile(data6, 0.25)
    leg2 <- paste("25th percentile =  ", round(lq,digits = 4))
    uq <- quantile(data6, 0.75)
    leg3 <- paste("75th percentile = ", round(uq,digits = 4))
    legend(x = "top", leg1, bty = "n")
    legend(x = "bottom", paste(leg2, leg3, sep = "; "), bty = "n")
    title(main=paste(deparse(substitute(name6))))
  }
  
}  
