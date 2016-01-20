#Load packages
library(RODBC)
library(smbinning)
library(ggplot2)
library(splines)
library(rtf)
library(MASS)
library(moments)
library(fitdistrplus)

###################
#Quotation Analysis
###################

#Open ODBC Connection
db = odbcConnect("MySQL_R_Connection", uid="root", pwd="yundai&1")
sqlQuery(db, "USE meteo")

#Load data (we exclude unlikely/impossible temperature and humidity data)
query= "SELECT *
FROM visitweather
WHERE (temp>-20) AND (temp<25) and (humidity<=100) AND (quotation=1) AND (NOT(amount IS NULL));"

quotamount=sqlQuery(db,query)

productlist <- list("pbauto"=quotamount$amount[quotamount$pbauto==1],
                    "pbmoto"=quotamount$amount[quotamount$pbmoto==1],
                    "pbrachat"=quotamount$amount[quotamount$pbrachat==1],
                    "pbperso"=quotamount$amount[quotamount$pbperso==1],
                    "pbtrav"=quotamount$amount[quotamount$pbtrav==1],
                    "rev"=quotamount$amount[quotamount$rev==1],
                    "other"=quotamount$amount[quotamount$other==1])

#Problem with some optimizations that are producing NaN results. 
#It is due to MLE optimisation process going into negative values. 
#I need to investigate this further
productstat <- multi_summary(productlist)
rownames(productstat) <- c("pbauto","pbmoto","pbrachat","pbperso","pbtrav","rev","other")

#productstat :
#          Min    Max 1st Qu. Median 3rd Qu. Count      Mean       Std
#pbauto   5000  75000    6000  10000   14000  5622 11185.194  6976.210
#pbmoto   5000  35000    5000   6500    9000   128  7817.695  4516.629
#pbrachat 3000 100000   11000  20000   31000  2985 23812.457 17452.569
#pbperso  1000  40000    3000   5000   10000 50298  7695.684  7113.142
#pbtrav   3800  75000    6000  10000   20000  3265 15183.103 13759.908
#rev       500  21500    1500   2000    3000  8525  2261.243  1236.878
#other    4000  80000    7000  15000   30000    37 21627.027 18147.232

#Drawing histograms for each product to try to identify a known density function 
hist.FD(productlist$pbauto)
mtext("Pbauto")
hist.FD(productlist$pbmoto)
mtext("Pbmoto")
hist.FD(productlist$pbrachat)
mtext("Pbrachat")
hist.FD(productlist$pbperso)
mtext("Pbperso")
hist.FD(productlist$pbtrav)
mtext("Pbtrav")
hist.FD(productlist$rev)
mtext("Rev")
hist.FD(productlist$other)
mtext("Other")
#None are following any recognisable probaility law.


#Multi-testing distribution (norm, lnorm, cauchy, logis)
multi_distr_test(productlist$pbauto)
multi_distr_test(productlist$pbmoto)
multi_distr_test(productlist$pbrachat)
multi_distr_test(productlist$pbperso)
multi_distr_test(productlist$pbtrav)
multi_distr_test(productlist$rev)
multi_distr_test(productlist$other)
#None of those sub-samples of amount are following any of those 4 laws


#We reject the hypothesis that amount any know probability law due to being a stratified data set