install.packages("surveillance")
install.packages("tsoutliers")

library(tsoutliers)
library(surveillance)

data("ha")

str(ha)

?sim.pointSource

sps <- sim.pointSource(p = 0.99, r = 0.5, length = 400,
                       A = 1, alpha = 1, beta = 0, phi = 0, frequency = 1,
                       state = NULL, K = 1.7)


?algo.bayes

ha.b662 <- algo.bayes(aggregate(ha), control = list(range = 209:290,
                                                    + b = 2, w = 6, alpha = 0.01))

?algo.cusum


data(kh)

kh$
  
  ha.cusum <- algo.cusum(aggregate(ha), control = list(k = kh$k,
                                                       + h = kh$h,
                                                       m = "glm", trans = "rossi",
                                                       range = 209:290))




data(ipi)

str(ipi)

## http://ftp.auckland.ac.nz/software/CRAN/doc/packages/surveillance.pdf

## https://www.researchgate.net/publication/303403602_Monitoring_Count_Time_Series_in_R_Aberration_Detection_in_Public_Health_Surveillance

## https://cran.r-project.org/web/packages/surveillance/vignettes/surveillance.pdf

## http://stackoverflow.com/questions/40804155/convert-dataframe-to-list-for-farrington-algorithm-algo-farrington

data(k1)



data.frame(WEEK=c(1:10),YEAR=2000,
           NUMBER=c(0,1,4,25,9,7,4,2,9,12))


x <- iraq.data.dailyex.2003

## str(x)

## https://www.jstatsoft.org/article/view/v070i10



str(x)
x$year<-lubridate::year(x$idate)
x$yday<-lubridate::yday(x$idate)
head(x)

xdf<- x %>% select(sum_kill,year,yday)

xsts <- sts(observed = xdf$sum_kill, start = c(2003, 79), frequency = 365)

xsts

stsd<-sts2disProg(sts = xsts)

str(xsts)
str(stsd)

?algo.farrington only works with weeks
res <- algo.farrington(stsd)

plot(res)


k1.b660 <- algo.bayes(k1,
                      + control = list(range = 27:192, b = 0, w = 6, alpha = 0.01))
> plot(k1.b660, disease = "k1", firstweek = 1, startyear = 2001)



# ?algo.farrington
# https://www.jstatsoft.org/article/view/v070i10

##

## x



## ?earsC


?earsC


res1 <- earsC(xsts, control = list( method="C1",baseline=28))

plot(res1)



## ## A statistical algorithm for the early detection of outbreaks of infectious disease, Farrington, C.P., Andrews, N.J, Beale A.D. and Catchpole, M.A. (1996), J. R. Statist. Soc. A, 159, 547-563
## ### To handle such data, the R package surveillance provides the S4 class "sts" (surveillance time series), which supersedes the "disProg" class. To convert your data to an "sts" object:


vignette("monitoringCounts")

## https://rpubs.com/cosmopolitanvan/r_isis_tweets_analytics
citation()