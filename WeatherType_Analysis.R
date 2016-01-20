#Load packages
library(RODBC)
library(smbinning)
library(ggplot2)
library(splines)
library(rtf)
library(MASS)
library(fitdistrplus)
library(moments)

###################
#Quotation Analysis
###################

#Open ODBC Connection
db = odbcConnect("MySQL_R_Connection", uid="root", pwd="yundai&1")
sqlQuery(db, "USE meteo")

#Load data (we exclude unlikely/impossible temperature and humidity data)
query= "SELECT *
          FROM visitweather
          WHERE (temp>-20) AND (temp<25) and (humidity<=100);"

visit=sqlQuery(db,query)

#Prepare data for analyses of categorical variables
  #Set to categorical the variable needed
  visit$pbauto <- as.factor(visit$pbauto)
  visit$pbmoto <- as.factor(visit$pbmoto)
  visit$pbtrav <- as.factor(visit$pbtrav)
  visit$pbperso <- as.factor(visit$pbperso)
  visit$rev <- as.factor(visit$rev)
  visit$pbrachat <- as.factor(visit$pbrachat)
  visit$drizzle <- as.factor(visit$drizzle)
  visit$rain <- as.factor(visit$rain)
  visit$atmosphere <- as.factor(visit$atmosphere)
  visit$clouds <- as.factor(visit$clouds)
  visit$thunderstorm <- as.factor(visit$thunderstorm)
  visit$snow <- as.factor(visit$snow)
  
  #Number of quotations
  drizzle <- sum(quotation[visit$drizzle == 1])
  rain <- sum(quotation[visit$rain == 1])
  cloud <- sum(quotation[visit$cloud == 1])
  atmosphere <- sum(quotation[visit$atmosphere == 1])
  snow <- sum(quotation[visit$snow == 1])
  thunderstorm <- sum(quotation[visit$thunderstorm == 1])

  #Number of visits
  ndrizzle <- nrow(visit[visit$drizzle == 1,])
  nrain <- nrow(visit[visit$rain == 1,])
  ncloud <- nrow(visit[visit$cloud == 1,])
  natmosphere <- nrow(visit[visit$atmosphere == 1,])
  nsnow <- nrow(visit[visit$snow == 1,])
  nthunderstorm <- nrow(visit[visit$thunderstorm == 1,])
  
  #Create a matrix storing those data mwq = Matrix Weather Quotation
  mwq <- matrix(c(drizzle,ndrizzle,rain,nrain,cloud, ncloud,atmosphere,natmosphere,snow,nsnow,thunderstorm,nthunderstorm),byrow=TRUE,ncol=2)
  mwq <- cbind(mwq, mwq[,2]-mwq[,1],mwq[,1]/mwq[,2])
  mwq <- mwq[,-2]
  
  #Run the prop.test on each weather type and add it to mwq
  res <- Map(prop.test, mwq[,1], mwq[,1] + mwq[,2])
  mwq <- cbind(mwq, t(sapply(res,"[[","conf.int")))
  dimnames(mwq) <- list(c("Drizzle","Rain","Cloud","Atmosphere","Snow","Thunderstorm"),c("Yes","No","Proba","Lower","Upper"))
  names(dimnames(mwq)) <- c("WeatherType","Quotation")
  mwq <- mwq[order(mwq[,3], decreasing = TRUE),]
  
  #Results
  #                Quotation
  # WeatherType      Yes      No       Proba       Lower      Upper
  #   Drizzle       1864  124810 0.014714938 0.014062563 0.01539692
  #   Cloud        64317 4317849 0.014676989 0.014564707 0.01479012
  #   Rain         16022 1151041 0.013728479 0.013518538 0.01394163
  #   Atmosphere    2566  184360 0.013727357 0.013207158 0.01426764
  #   Snow            59    4873 0.011962693 0.009197172 0.01551305
  #   Thunderstorm     2     288 0.006896552 0.001195438 0.02741374
  
  rtffile <- RTF("weathertype2.doc")  # this can be an .rtf or a .doc
  addParagraph(rtffile, "This is the output of the table we made :\n")
  addTable(rtffile, mwq, row.names = TRUE)
  done(rtffile)

  
############################
#Amount-Weathertype Analysis
############################

#Open ODBC Connection
db = odbcConnect("MySQL_R_Connection", uid="root", pwd="yundai&1")
sqlQuery(db, "USE meteo")

#Load data (we exclude unlikely/impossible temperature and humidity data)
query= "SELECT *
        FROM visitweather
        WHERE (quotation = 1) AND (temp>-20) AND (temp<25) and (humidity<=100)  AND (NOT(amount IS NULL));"

quotamount=sqlQuery(db,query)


f_summary(quotamount$amount, TotAmount)
f_summary(quotamount$amount[quotamount$drizzle==1], drizzle)
f_summary(quotamount$amount[quotamount$rain==1], Rain)
f_summary(quotamount$amount[quotamount$atmosphere==1], Atmosphere)
f_summary(quotamount$amount[quotamount$cloud==1], Cloud)
f_summary(quotamount$amount[quotamount$snow==1], Snow)
# Thunderstorm only has 2 events with an event different of NA...
# f_summary(quotamount$amount[quotamount$thunderstorm==1], Thunderstorm)


f_boxplot(  quotamount$amount[quotamount$drizzle==1], Drizzle, 
          + quotamount$amount[quotamount$rain==1], Rain, 
          + quotamount$amount[quotamount$atmosphere==1], Atmosphere,
          + quotamount$amount[quotamount$cloud==1], Cloud,
          + quotamount$amount[quotamount$snow==1], Snow)

weatherlist <- list("drizzle"=quotamount$amount[quotamount$drizzle==1],
                    "rain"=quotamount$amount[quotamount$rain==1],
                    "cloud"=quotamount$amount[quotamount$cloud==1],
                    "atmosphere"=quotamount$amount[quotamount$atmosphere==1],
                    "snow"=quotamount$amount[quotamount$snow==1])

weatherstat <- multi_summary(weatherlist)
rownames(weatherstat) <- c("drizzle","rain","cloud","atmosphere","snow")

#Results
#Min    Max 1st Qu. Median 3rd Qu. Count     Mean      Std
#drizzle     500 100000    3000   5000   10000  1518 8010.277 8454.161
#rain        500  99000    3000   5000   10000 13397 8332.193 8700.089
#cloud       500 100000    3000   5000   10000 53829 8368.006 8970.989
#atmosphere  500 100000    3000   5000   10000  2066 8282.770 9111.399
#snow       1000  35000    3000   5000    9250    48 6996.625 6737.716


rtffile <- RTF("weatherstat.doc")  # this can be an .rtf or a .doc
addParagraph(rtffile, "This is the output of the table we made :\n")
addTable(rtffile, weatherstat, row.names = TRUE)
done(rtffile)

#We can hardly conclude to any difference between weathertypes regarding quotation amounts :(


#####################
#Pure Amount analysis
#####################

amount <- quotamount$amount

#First descriptive statistics
amountsum = list(amount)
amountsum <- multi_summary(amountsum)
rownames(amountsum) <- c("amount")
#Min   Max 1st Qu. Median 3rd Qu. Count    Mean      Std
#500 1e+05    3000   5000   10000 70860 8350.15 8912.296

rtffile <- RTF("amountstat.doc")  # this can be an .rtf or a .doc
addParagraph(rtffile, "This is the output of the table we made :\n")
addTable(rtffile, amountsum, row.names = TRUE)
done(rtffile)

#Multi-testing distribution (norm, lnorm, cauchy, logis)
multi_distr_test(amount)
#Amount is not following any of the laws tested

#Ploting the histogram of amount using Freedman-Diaconis bins rule for the histogram 
#because the Sturges method is not working well for n>200 
#See https://en.wikipedia.org/wiki/Freedman%E2%80%93Diaconis_rule
hist.FD(amount)
skewness(amount)
#result = 3.01

#Following this tutorial :
#http://www.isixsigma.com/tools-templates/normality/dealing-non-normal-data-strategies-and-tools
#Amount cxdata are note distributed normaly due to a "Natural limit"
#Therefor we need to apply a Box-Cox transformation to see wether we can get a normal distribution :
#boxcox_vector is a self-made function to allow non-modeling use of boxcox()
boxcox_vector(amount)

#Result : (lambda | max(log-likelihood)) = 0
#Hence, we can apply a log transformation to our data
logamount <- log(amount)

hist.FD(logamount)
mtext("Logamount")
skewness(logamount)
#Result = -0.056
#Almost symetrical !
kurtosis(logamount)
#Result = 2.713

f <- fitdist(logamount,"norm")
plotdist(logamount,"norm",para=list(mean=f$estimate[1],sd=f$estimate[2]))
#We now have a variable with a normal "enough" distribution with a big left tail

#Normality tests for large samples almost always lead to rejecting the null hypothesis
#See https://stat.ethz.ch/pipermail/r-help/2007-April/129620.html
#Bellow an example :
#The one sample Kolmogorov-Smirnov test completely reject the normal hypothesis
ks.test(logamount, "pnorm")
#Results :
#data:  logamount
#D = 1, p-value < 2.2e-16
#alternative hypothesis: two-sided


