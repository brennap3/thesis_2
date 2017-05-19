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

library(ggplot2)
install.packages("lubridate")
library(lubridate)
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)
library(magrittr)


gti_data<-read.csv("C://Users//Peter//Desktop//gti//globalterrorismdb_0616dist.csv",header=TRUE,sep=",")

gti_data$nkill_clean<-coalesce(gti_data$nkill,0)


## colnames(gti_data)
## unique(gti_data$region_txt)
## Middle East & North Africa

Middle.east.data<-gti_data %>% filter(region_txt=='Middle East & North Africa')



Middle.east.data$idate<-ymd(paste(Middle.east.data$iyear,str_pad(Middle.east.data$imonth,2,pad="0"),
                           str_pad(Middle.east.data$iday,2,pad="0")))


c<-seq(ymd('1975-03-01'),ymd('2015-12-31'),by='1 day')
## length(c)

d<-c(rep(0,times=14916)) %>% as.data.frame()
d$date<-c
colnames(d)<-c('count_kills','idate')

Middle.east.data.dataex<-dplyr::left_join(d, 
          Middle.east.data, by = "idate")

Middle.east.data.dataex$nkill<-  dplyr::coalesce(Middle.east.data.dataex$nkill,
                                                 0)


Middle.east.data.dataex.post.iraq.data.postinvasion<-Middle.east.data.dataex %>% 
                                filter(idate>="2003-03-20")



Middle.east.data.dataex.post.iraq.data.postinvasion$year<-lubridate::year(Middle.east.data.dataex.post.iraq.data.postinvasion$idate)

Middle.east.data.dataex.post.iraq.data.postinvasion$week<-lubridate::week(Middle.east.data.dataex.post.iraq.data.postinvasion$idate)

## unique(iraq.data.postinvasion$week)
## str(Middle.east.data.dataex.post.iraq.data.postinvasion)

Middle.east.data.dataex.post.iraq.data.postinvasion.data.weekly <-
  Middle.east.data.dataex.post.iraq.data.postinvasion %>% 
  dplyr::select(year,week,nkill_clean,country_txt) %>% group_by(year,week,country_txt) %>% 
  summarize(sum_kill=sum(nkill_clean,na.rm=T)) %>% 
  arrange(year,week) %>% as.data.frame()

str(Middle.east.data.dataex.post.iraq.data.postinvasion.data.weekly)



Middle.east.data.dataex.post.iraq.data.postinvasion.data.weekly$weekstrdate<-as.POSIXct(paste(Middle.east.data.dataex.post.iraq.data.postinvasion.data.weekly$year,
                                                                                              Middle.east.data.dataex.post.iraq.data.postinvasion.data.weekly$week,"1",sep=" "), format = "%Y %U %u")

sbsetmideast<-Middle.east.data.dataex.post.iraq.data.postinvasion.data.weekly %>% filter(country_txt %in% c("Iraq","Syria","Yemen","Jordan"))

ggplot(sbsetmideast, aes(x=weekstrdate, y=sum_kill)) +
  geom_line(color = "blue")+facet_grid(country_txt ~ .)+
  ggtitle("Time series plot of the  \n  number of deaths due to terrorism, \n averaged across all weeks in Iraq, Syria, Yemen, Jordan post Iraq invasion")+
  xlab("Interval (week) ")+
  ylab("Number of Deaths")

## post invasion 
## use 4 week period


library(BreakoutDetection)

## ?breakout

str(sbsetmideast)

## ?breakout

subset_models <- sbsetmideast %>%
  group_by(
    country_txt
  ) %>%
  do(fit = breakout(.$sum_kill, min.size=12, 
                    method='multi', percent=.1, degree=1, plot=FALSE))


str(subset_models$fit[[1]]$loc)

subset_models_locs <- subset_models %>% do(as.data.frame((.$fit$loc)))

##?breakout

## lets get the time points of the different 


head(subset_models_locs)

library(broom)

sbsetmideast$country_txt<-droplevels(sbsetmideast$country_txt)

## str(sbsetmideast) str(sbsetmideast$weekstrdate)
## min.size=4, 
## method='multi', percent=.2, degree=1
subset_models <- sbsetmideast %>%
  split(
    .$country_txt
  ) %>%
  map(~ breakout(.$sum_kill, min.size=4, 
                    method='multi', percent=.2, degree=1, plot=FALSE)) %>%
    map("loc")  



## x<-lapply(subset_models, unlist) %>% cbind() ##does not work

## subset_models %>% map(unlist) %>% map(simplify2array) %>% as.data.frame() ## does not work
 ## use a padded data frame
## https://arxiv.org/pdf/1610.01178.pdf
## test list of lists to dataframe

na.pad <- function(x,len){
  x[1:len]
}

makePaddedDataFrame <- function(l,...){
  maxlen <- max(sapply(l,length))
  data.frame(lapply(l,na.pad,len=maxlen),...)
}

locs.df<-subset_models %>% map(unlist) %>% makePaddedDataFrame() 

str(locs.df)

locs.df[,"Iraq"]

listcountries<-unique(sbsetmideast$country_txt %>% as.character()) 

str(listcountries)
summary(listcountries)


str(country_outbreaks)

pl.holder.dfx<- data.frame()

for(i in listcountries){
  tsti<-sbsetmideast  %>% filter(country_txt==i)  
  country_outbreaks<-tsti[locs.df[,i],] %>% filter(!is.na(year))
  tsti$outbreak<-ifelse(tsti$weekstrdate %in% country_outbreaks$weekstrdate,"outbreak","no outbreak")
  pl.holder.dfx<-rbind(pl.holder.dfx,tsti)
  }


ggplot(pl.holder.dfx, aes(x=weekstrdate, y=sum_kill)) +
  geom_line(color = "blue")+geom_text(data=pl.holder.dfx %>% filter(outbreak=="outbreak"),aes(weekstrdate,sum_kill,label="o"))+facet_grid(country_txt ~ .)+
  ggtitle("Time series outbreak (SURUS) plot of the  \n  number of deaths due to terrorism, \n averaged across all weeks in Iraq, Syria, Yemen, Jordan post Iraq invasion")+
  xlab("Interval (week) ")+
  ylab("Number of Deaths")



str(pl.holder.dfx)
nrow(pl.holder.dfx)


i="Iraq"
locs.df[,i]
tsti<-sbsetmideast  %>% filter(country_txt==i) %>% as.data.frame()
country_outbreaks<-tsti[locs.df[,i],] %>% filter(!is.na(year))
head(country_outbreaks)




