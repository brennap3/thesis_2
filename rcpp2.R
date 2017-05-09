rm(list=ls())

library(lubridate)
library(dplyr)
library(lubridate)
library(purrr)
library(magrittr)
library(rts)
library(depmixS4)
library(TTR)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(KMsurv)
library(tibble)
library(stringr)
library(tidyr)
library(stargazer)
library(stringr)
library(stringi)


gti_data<-read.csv("C://Users//Peter//Desktop//gti//globalterrorismdb_0616dist.csv",header=TRUE,sep=",")

gti_data$nkill_clean<-coalesce(gti_data$nkill,0)


iraq.data<-gti_data %>% filter(country_txt=='Iraq')



iraq.data$idate<-ymd(paste(iraq.data$iyear,str_pad(iraq.data$imonth,2,pad="0"),
                           str_pad(iraq.data$iday,2,pad="0")))

max(iraq.data.postinvasion$idate)
min(iraq.data.postinvasion$idate)


c<-seq(ymd('1975-03-01'),ymd('2015-12-31'),by='1 day')
length(c)
d<-c(rep(0,times=14916)) %>% as.data.frame()
d$date<-c
colnames(d)<-c('count_kills','idate')
iraq.dataex<-dplyr::left_join(d, iraq.data, by = "idate")

iraq.dataex$nkill<-dplyr::coalesce(iraq.dataex$nkill,0)


iraq.data.postinvasion<-iraq.dataex %>% filter(idate>="2003-03-20")


iraq.data.postinvasion$year<-lubridate::year(iraq.data.postinvasion$idate)

iraq.data.postinvasion$week<-lubridate::week(iraq.data.postinvasion$idate)

## unique(iraq.data.postinvasion$week)

iraq.data.weekly <- iraq.data.postinvasion %>% 
  dplyr::select(year,week,nkill_clean) %>% group_by(year,week) %>% 
      summarize(sum_kill=sum(nkill_clean,na.rm=T)) %>% 
          arrange(year,week) %>% as.data.frame()

str(iraq.data.weekly)
library(lubridate)
citation(lubridate)


## str(iraq.dataex)

head(iraq.data.weekly)
tail(iraq.data.weekly,50)

library(ggplot2)
install.packages("lubridate")
library(lubridate)
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)
library(magrittr)

iraq.data.weekly$weekstrdate<-as.POSIXct(paste(iraq.data.weekly$year,iraq.data.weekly$week,"1",sep=" "), format = "%Y %U %u")




ggplot(iraq.data.weekly, aes(x=weekstrdate, y=sum_kill)) +
  geom_line(color = "blue")+
  ggtitle("Time series plot of the  \n  number of Deaths due to terrorism, \n averaged across all weeks (y-axis)")+
  xlab("Date")+
  ylab("number of deaths \n due to terrorism")



## post invasion 
## use 4 week period

?breakout

library(BreakoutDetection)

res.weekly.twitter.breakout = breakout(iraq.data.weekly$sum_kill, min.size=4, 
               method='multi', percent=.2, degree=1, plot=TRUE)

## 

res.weekly.twitter.breakout$plot


res.weekly.twitter.breakout$loc


##
install.packages("tseries")
library(tseries)
library(devtools)
install_github(repo = "Surus", username = "Netflix", subdir = "resources/R/RAD")

library(RAD)

X<-iraq.data.weekly$sum_kill


length(X)

## 678/4

res.iraq.anom<-RAD::AnomalyDetection.rpca(head(X,676), frequency = 4,  autodiff = T,
                      forcediff = F, scale = T,  verbose = F)


str(res.iraq.anom)

res.iraq.anom$S_transform ## The sparse outliers in the transformed space

?RAD::ggplot_AnomalyDetection.rpca

RAD::ggplot_AnomalyDetection.rpca(res.iraq.anom) + 
  ggplot2::theme_grey(base_size = 25)

str(res.iraq.anom)


str(res.iraq.anom$name)

res.iraq.anom

##
####
##

str(iraq.data.weekly)

iraq.data.weekly.ts<- iraq.data.weekly %>% dplyr::select(weekstrdate,sum_kill) %>% dplyr::filter(!is.na(weekstrdate))

head(iraq.data.weekly)

?AnomalyDetectionTs

data_anomaly = AnomalyDetectionTs(iraq.data.weekly.ts, max_anoms=0.01, direction="pos", plot=TRUE, e_value = T)

####
#######
#######
####

## str(iraq.data.weekly)

xdf<- iraq.data.weekly %>% dplyr::select(sum_kill,weekstrdate) 
## ?sts


head(
  xdf)

library(tsoutliers)
library(surveillance)

?sts

xsts <- sts(observed = xdf$sum_kill, start=c(2003,12)  ,epoch=as.numeric(xdf$weekstrdate))

nrow(xsts)
nrow(xdf)

plot(xsts)

?sts2disProg

stsd<-sts2disProg(sts = xsts)

plot(stsd)

?algo.cdc
?algo.farrington

?earsC
rm(res1)
?earsC

xsts
?earsC
res1x <- earsC(xsts, control = list( method="C3",baseline=18))


str(res1x)
str(xsts)

plot(res1x)
res1.df<-NULL
res2.df<-as.data.frame(res1)
nrow(res2.df)

str(res1.df)

str(xdf)

library(dplyr)

?mutate

nrow(res1.df)

max(xdf$weekstrdate)

res1.df.1 <- mutate(res1.df,
                              time = xdf$weekstrdate[22:677])

xas<-xdf[22:677,]

nrow(res1.df.1)
nrow(xas)

str(res1.df.1)

ggplot() +
  geom_bar(aes(x=xas$weekstrdate,y=xas$sum_kill), stat = "identity",color="darkblue") +
  viridis::scale_fill_viridis(discrete = TRUE, name = "Alarm:") +
  geom_step(data = res1.df.1, aes(time, upperbound)) +
  theme(legend.position = "bottom") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  ylab("No. of deaths") +
  xlab("Time") + ggtitle("Outbreaks (detection) of deaths due to terrorism in Iraq \n (EarsC3)") +
  theme(axis.text.x = element_text(angle = 60, size = 6)) +
  geom_point(data = filter(res1.df.1, alarm), aes(x = time), y = 0, color = "red") 


str(xsts)
str(stsd)

?algo.farrington ##only works with weeks



##farrignton 

plot(stsd)

stsfar <- xsts <- sts(observed = xdf$sum_kill, 
                      start=c(2003,12)  ,epoch=as.numeric(xdf$weekstrdate),freq=52)

plot(stsfar)


cntrl<-list(range=50:650,w=18,b=1,alpha=0.01)

?farrington
?farringtonFlexible

res.far <- farrington(stsfar,control=cntrl)

str(res.far)

surveillance::plot(res.far,legend.opts=NULL,
                   main="Farrington algorithm \n applied to Iraq data",
                   ylab="No. of Deaths",
                   xlab="Time"
                   )

?surveillance::plot


##
#### Redo for syria data


##

library(stringi)
library(stringr)


gti_data$nkill_clean<-coalesce(gti_data$nkill,0)


Syria.data<-gti_data %>% filter(country_txt=='Syria')



Syria.data$idate<-ymd(paste(Syria.data$iyear,str_pad(Syria.data$imonth,2,pad="0"),
                           str_pad(Syria.data$iday,2,pad="0")))

max(Syria.data$idate,na.rm=T)
min(Syria.data$idate,na.rm=T)


c<-seq(ymd('1974-08-14'),ymd('2015-12-31'),by='1 day')
length(c)
d<-c(rep(0,times=length(c))) %>% as.data.frame()
d$date<-c
colnames(d)<-c('count_kills','idate')

Syria.dataex<-dplyr::left_join(d, Syria.data, by = "idate")

2^30
2^16
2^8
Syria.dataex$nkill<-dplyr::coalesce(iraq.dataex$nkill,0)

Syria.dataex$year<-lubridate::year(Syria.dataex$idate)

Syria.dataex$week<-lubridate::week(Syria.dataex$idate)

## unique(iraq.data.postinvasion$week)

Syria.dataex.weekly <- Syria.dataex %>% 
  dplyr::select(year,week,nkill_clean) %>% group_by(year,week) %>% 
  summarize(sum_kill=sum(nkill_clean,na.rm=T)) %>% 
  arrange(year,week) %>% as.data.frame()

str(Syria.dataex.weekly)

Syria.dataex$weekstrdate<-as.POSIXct(paste(Syria.dataex$year,Syria.dataex$week,
                                           "1",sep=" "), format = "%Y %U %u")



## str(iraq.dataex)

library(ggplot2)
install.packages("lubridate")
library(lubridate)
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)
library(magrittr)









