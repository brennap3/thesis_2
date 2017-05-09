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
citation("lubridate")


## str(iraq.dataex)

head(iraq.data.weekly)
tail(iraq.data.weekly,50)

library(ggplot2)
install.packages("lubridate")
library(lubridate)
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

## citation("AnomalyDetection")
## citation("surveillance")

library(magrittr)
iraq.data.weekly$weekstrdate<-as.POSIXct(paste(iraq.data.weekly$year,iraq.data.weekly$week,"1",sep=" "), format = "%Y %U %u")

ggplot(iraq.data.weekly, aes(x=weekstrdate, y=sum_kill)) +
  geom_line(color = "blue")+
  ggtitle("Time series plot of the  \n  number of deaths due to terrorism, \n averaged across all weeks in Iraq post invasion")+
  xlab("Interval (week) ")+
  ylab("Number of Deaths")

## post invasion 
## use 4 week period

?breakout

library(BreakoutDetection)
?breakout

res.weekly.twitter.breakout = breakout(iraq.data.weekly$sum_kill, min.size=4, 
               method='multi', percent=.2, degree=1, plot=TRUE)

 

res.weekly.twitter.breakout$plot


breakouts.loc<-res.weekly.twitter.breakout$loc

iraq.surus.breakouts<-iraq.data.weekly[breakouts.loc,]

library(stargazer)
library()

iraqbreakout<-iraq.data.weekly[breakouts.loc,]

(iraqbreakout)




weekly.iraq.plot<-res.weekly.twitter.breakout$plot

## str(iraq.data.weekly)

iraq.data.weekly$wkyr<-paste(as.character(iraq.data.weekly$week), as.character(iraq.data.weekly$year), sep="-")

# get every 4th observation

sub1<- iraq.data.weekly[seq(1, nrow(iraq.data.weekly), by=20), ]
wkyr2<-sub1$wkyr

# replot

weekly.iraq.plot + labs(y="Deaths due to terrorism",
             x="Week-Year") + scale_x_continuous(breaks = c(seq(from = 1, to = nrow(iraq.data.weekly), by = 20)),
                                                 labels = wkyr2)+
  geom_line(color = "blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Twitter outbreak detection algorithm \n detected outbreaks of deaths due to terrorism")

## ignore Scale for 'x' is already present. Adding another scale for 'x', which will replace the
## existing scale.

weekly.iraq.plot + labs(y="Deaths due to terrorism",
                        x="Week-Year") + scale_x_continuous(breaks = c(seq(from = 1, to = nrow(iraq.data.weekly), by = 20)),
                                                            labels = wkyr2)+
  geom_line(color = "grey") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Twitter outbreak detection algorithm \n detected outbreaks of deaths due to terrorism")



##
install.packages("tseries")
library(tseries)
library(devtools)
install_github(repo = "Surus", username = "Netflix/Surus", subdir = "resources/R/RAD")

library(RAD)

X<-iraq.data.weekly$sum_kill

## 678/4

res.iraq.anom<-RAD::AnomalyDetection.rpca(head(X,676), frequency = 4,  autodiff = T,
                      forcediff = F, scale = T,  verbose = F)


str(res.iraq.anom)
head(res.iraq.anom)

row.names(res.iraq.anom)

## select where s transform greater 0

res.iraq.anom.loc<-res.iraq.anom %>% filter(S_transform>0) 

## res.iraq.anom.loc
## ?tibble::rownames_to_column

res.iraq.anom.loc$Week_Num<-NULL

str(res.iraq.anom.loc)
colnames(res.iraq.anom.loc)
summary(res.iraq.anom.loc)

iraq.data.weekly<-tibble::rownames_to_column(iraq.data.weekly,var="time")

str(iraq.data.weekly)
res.iraq.anom.loc$time<-res.iraq.anom.loc$time %>% as.character()



## str(iraq.data.weekly)
## str(res.iraq.anom.loc)
## str(iraq.data.weekly)
## str(iraq.data.weekly)

res.iraq.anom.loc<-dplyr::inner_join(res.iraq.anom.loc,iraq.data.weekly,by="time") 

##check join

##?arrange

res.iraq.anom.loc$time<-res.iraq.anom.loc$time %>% as.numeric()

##change back to order by

res.iraq.anom.loc<-res.iraq.anom.loc %>% dplyr::select(X_transform,L_transform,S_transform,S_transform,
                                                                              E_transform,wkyr,time) %>% dplyr::arrange(time)    
head(res.iraq.anom.loc)

res.iraq.anom.loc<-res.iraq.anom.loc %>% dplyr::select(X_transform,L_transform,S_transform,S_transform,
                                                       E_transform,wkyr)     



xtable(res.iraq.anom.loc)

head(res.iraq.anom$S_transform,50) ## The sparse outliers in the transformed space


head(res.iraq.anom.loc)

?RAD::ggplot_AnomalyDetection.rpca
?RAD::ggplot_AnomalyDetection.rpca

?legend.title

#' ggplot function which shows the low rank signal in blue, the random noise in green,
#' and any outliers in red. If a transformation was applied, these signals will be plotted
#' in the transformed space, along with the original time series

RAD::ggplot_AnomalyDetection.rpca(res.iraq.anom) + 
   scale_x_continuous(breaks = c(seq(from = 1, to = nrow(iraq.data.weekly), by = 20)),
                                                           labels = wkyr2)+
  
  geom_line(color = "blue") +xlab("Interval (week)")+ylab("Count of Deaths")+
  theme(legend.title = element_text(colour="black", size=8, face="bold"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("SURUS time series outlier detection algorithm \n detected outbreaks of deaths due to terrorism")+
  labs(size = "Outlier magnitude")
  
  


str(res.iraq.anom)

##fancy plot
#
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


?sts
xsts <- sts(observed = xdf$sum_kill, start=c(2003,12)  ,epoch=as.numeric(xdf$weekstrdate))

nrow(xsts)
nrow(xdf)

plot(xsts)

?sts2disProg
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
?earsC
res1x <- earsC(xsts, control = list( method="C3",baseline=18))


str(res1x)
str(xsts)

plot(res1x)
res1.df<-NULL
res1.df<-as.data.frame(res1x)
nrow(res2.df)

str(res1.df)
str(res2.df)
str(xdf)

library(dplyr)
nrow(res1.df)

max(xdf$weekstrdate)

res1.df.1 <- mutate(res1.df,
                              time = xdf$weekstrdate[22:677])

xas<-xdf[22:677,]

nrow(res1.df.1)
nrow(xas)
str(xas)
str(res1.df.1)

##get periods were alrms are raised

res1.df.1.alarm<- res1.df.1 %>% filter(alarm==T)

str(res1.df.1.alarm)

res1.df.1.alarm$time<-res1.df.1.alarm$time %>% as.character()



xtable(res1.df.1.alarm)
str(res1.df.1)
##citation("sts")

ggplot() +
  geom_bar(aes(x=xas$weekstrdate,y=xas$sum_kill), stat = "identity",color="darkblue") +
  viridis::scale_fill_viridis(discrete = TRUE, name = "Alarm:") +
  geom_step(data = res1.df.1, aes(time, upperbound)) +
  theme(legend.position = "bottom") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  ylab("Number of Deaths") +
  xlab("Interval (weeks)") + ggtitle("Outbreaks (detection) of deaths \n due to terrorism in Iraq \n (EarsC3)") +
  theme(axis.text.x = element_text(angle = 60, size = 6)) +
  geom_point(data = filter(res1.df.1, alarm), aes(x = time), y = 0, color = "red") 


str(xsts)
str(stsd)



##?algo.farrington ##only works with weeks



##farrignton 

plot(stsd)

stsfar <- xsts <- sts(observed = xdf$sum_kill, 
                      start=c(2003,12)  ,epoch=as.numeric(xdf$weekstrdate),freq=52)

plot(stsfar)


str(stsfar)

cntrl<-list(range=19:660,w=18,b=0,alpha=0.01)
## cntrol file must allow for 18 weeks previous first surveyed data in range
## and 18 weeks after
warnings()
?farrington
?farringtonFlexible

res.far<-NULL
res.far <- farrington(stsfar,control=cntrl)
res.far@start




str(res.far)
str(res1x)

##res1x <- earsC(xsts, control = list( method="C3",baseline=18))



plot(res1x,type=alarm ~ time )
plot(res.far,type=alarm ~ time )

res.far.df ##farrington
res1.df ##ears

plot(farir)


as.POSIXct(1077494400, origin = "1970-01-01", tz = "GMT")
res.far.df<-NULL
res.far.df<-res.far %>% as.data.frame()
head(res.far.df)
##add the time specified in our control file
str
res.far.df$time <- as.POSIXct(res.far.df$epoch, origin = "1970-01-01", tz = "GMT")
## head(res.far.df$time) head(res.far.df)
res.far.dfsbset<-res.far.df  %>% dplyr::select(observed,upperbound,alarm,time) %>% filter(alarm==1)
res.far.dfsbset$time<-as.Date(res.far.dfsbset$time)
res.far.dfsbset$time<-as.Date.character(res.far.dfsbset$time)
str(res.far.dfsbset)
res.far.dfsbset$time<-ymd(res.far.dfsbset$time)
res.far.dfsbset$time<- res.far.dfsbset$time %>% as.character()
str(res.far.dfsbset$time)
xtable(res.far.dfsbset)

head(res.far.dfsbset) ##farringtons

nrow(res.far.dfsbset)
res.far.dfsbset$method<-"farrington"
head(res.far.dfsbset$method)
nrow(res1.df.1.alarm)
res1.df.1.alarm$method<-"EarsC3"
str(res1.df.1.alarm)
str(res.far.dfsbset)

##

res1.df.1.alarm.als<-res1.df.1.alarm %>% dplyr::select(time,observed,method)
res.far.dfsbset.als<-res.far.dfsbset %>% dplyr::select(time,observed,method)
surv.als<- rbind(res1.df.1.alarm.als,res.far.dfsbset.als)

##check

surv.als$time<-ymd(surv.als$time)

##
#### add these two to surv.als
##

str(surv.als)



## str(iraq.surus.breakouts)

## str(res.iraq.anom.loc)





##
####
##




ggplot(surv.als, aes(x=time,y=observed)) + 
  geom_point(shape=1) + facet_grid(method~.) + 
  scale_color_manual(values=c('green', 'red'))+
  guides(color=FALSE) + theme_bw() +
  ggtitle("Alarm plot for EarC3 and \n Farringtons method")


## use cowplot to combine with both 

##

head(res.far.df)

str(res.far.df)
##check these are correct

##
#####
##

xfaras<-xdf[19:660,]
str(res.far.df)

##encode as true false

res.far.df$alarm_logi<-ifelse(res.far.df$alarm==1,T,F)

head(res.far.df)

res.far.df$time<- as.character(res.far.df$time)
(res.far.df %>% dplyr::filter(res.far.df$alarm==T))


ggplot() +
  geom_bar(aes(x=xfaras$weekstrdate,y=xfaras$sum_kill), stat = "identity",color="darkblue") +
  viridis::scale_fill_viridis(discrete = TRUE, name = "Alarm:") +
  geom_step(data = res.far.df, aes(time, upperbound)) +
  theme(legend.position = "bottom") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  ylab("Number of Deaths") +
  xlab("Interval (weeks)") + ggtitle("Outbreaks (detection) of deaths \n due to terrorism in Iraq \n (Farrington)") +
  theme(axis.text.x = element_text(angle = 60, size = 6)) +
  geom_point(data = filter(res.far.df, alarm_logi), aes(x = time), y = 0, color = "red")


surveillance::plot(res.far,legend.opts=NULL,
                   main="Farrington algorithm \n applied to Iraq data",
                   ylab="No. of Deaths",
                   xlab="Time"
                   )
##

res.far.df


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









