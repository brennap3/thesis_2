R.Version()
install.packages("installr")
library("installr")
require(installr)} #load / install+load installr

# using the package:
updateR() 
install.packages("Rcpp")
library(Rcpp)
install.packages("devtools")
library(devtools)
devtools::install_github("twitter/BreakoutDetection")
library(BreakoutDetection)

library(magrittr)
library(dplyr)


iraq.data<-gti_data %>% filter(country_txt=='Iraq')

iraq.data$idate<-ymd(paste(iraq.data$iyear,str_pad(iraq.data$imonth,2,pad="0"),
                           str_pad(iraq.data$iday,2,pad="0")))

iraq.data.daily<-iraq.data %>% dplyr::select(idate,nkill) %>% group_by(idate) %>% summarize(sum_kill=sum(nkill)) %>% arrange(idate) %>% as.data.frame()


iraq_data.clean<-iraq.data.daily %>% filter(!is.na(idate))  %>% as.data.frame()

iraq_data.clean$iraq_data.clean<-dplyr::coalesce(iraq_data.clean$sum_kill,0)

## str(iraq_data.clean)

## 

## str(iraq.data.daily)

iraq.data<-gti_data %>% filter(country_txt=='Iraq')

iraq.data$idate<-ymd(paste(iraq.data$iyear,str_pad(iraq.data$imonth,2,pad="0"),
                           str_pad(iraq.data$iday,2,pad="0")))

c<-seq(ymd('1975-03-01'),ymd('2015-12-31'),by='1 day')
d<-c(rep(0,times=14916)) %>% as.data.frame()
d$date<-c
colnames(d)<-c('count_kills','idate')
iraq.dataex<-dplyr::left_join(d, iraq.data, by = "idate")
## str(iraq.dataex)

iraq.dataex$nkill<-dplyr::coalesce(iraq.dataex$nkill,0)

## head(iraq.dataex %>% dplyr::select(idate,nkill))

iraq.data.dailyex<-iraq.dataex %>% dplyr::select(idate,nkill) %>% group_by(idate) %>% summarize(sum_kill=sum(nkill)) %>% arrange(idate) %>% as.data.frame()

##
library(BreakoutDetection)
## data(Scribe)

## str(iraq.data.dailyex)


help(breakout)

## str(iraq.data.dailyex)

iraq.data.dailyex.2003 <- iraq.data.dailyex %>% filter(idate>="2003-03-20")

res = breakout(iraq.data.dailyex.2003$sum_kill, min.size=7, method='multi', percent=.2, degree=1, plot=TRUE)

## res$plot


res$loc
res$time
res$
  
## (a) Segment the data Post invasion and Pre Pull out
## (b) Segment
##   dmy("31/12/2012")) 

iraq.data.dailyex.2003.post.invasion <- iraq.data.dailyex %>% 
                    filter(idate>="2003-03-20" & idate<="2012-12-31")  

iraq.data.dailyex.2003.post.pullout  <- iraq.data.dailyex %>% 
  filter(idate>"2012-12-31")

##
res.invasion = breakout(iraq.data.dailyex.2003.post.invasion$sum_kill, min.size=7, method='multi', percent=.05, degree=1, plot=TRUE)

res.invasion$plot


res.invasion$loc
res.invasion$time

  

##


##


##
res.post.pullout = breakout(iraq.data.dailyex.2003.post.pullout$sum_kill, min.size=7, method='multi', percent=.05, degree=1, plot=TRUE)

res.post.pullout$plot

res.post.pullout$loc
res.post.pullout$time

##
###  525/365.25
##













