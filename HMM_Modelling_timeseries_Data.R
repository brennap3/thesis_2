##script to model time series data
##load libraries

##



library(dplyr)
library(lubridate)
library(cowplot)
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

##death by year

gti_data<-read.csv("C://Users//Peter//Desktop//gti//globalterrorismdb_0616dist.csv",header=TRUE,sep=",")



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

## str(iraq.data.dailyex)


## str(iraq.data.daily)


gti_data.clean<-iraq.data.daily %>% filter(!is.na(idate))  %>% as.data.frame()



ggplot(gti_data.clean, aes(x=idate, y=sum_kill)) +
  geom_line(color = "blue")+
  ggtitle("Time series plot of the  \n  number of Deaths due to terrorism, \n averaged across all days (y-axis)")+
  xlab("Date")+
  ylab("number of deaths \n due to terrorism")

head(gti_data.clean)
str(gti_data.clean)



##
####
######
####
##

iraq.data<-gti_data %>% filter(country_txt=='Iraq')

iraq.data$idate<-ymd(paste(iraq.data$iyear,str_pad(iraq.data$imonth,2,pad="0"),
                           str_pad(iraq.data$iday,2,pad="0")))

iraq.data.daily<-iraq.data %>% dplyr::select(idate,nkill) %>% group_by(idate) %>% summarize(sum_kill=sum(nkill)) %>% arrange(idate) %>% as.data.frame()


iraq_data.clean<-iraq.data.daily %>% filter(!is.na(idate))  %>% as.data.frame()

iraq_data.clean$iraq_data.clean<-dplyr::coalesce(iraq_data.clean$sum_kill,0)

summary(iraq_data.clean)
str(iraq_data.clean)


summary(gti_data.clean)

head(gti_data.clean)

str(gti_data.clean)



##
##
#
##
##

set.seed(1)
mod <- depmix(sum_kill ~ 1, family = gaussian(), nstates = 2, data = iraq_data.clean)

#?depmix

fm2 <- fit(mod, verbose = TRUE)

sum.to.state.hmm<-summary(fm2, which="response")

# Classification (inference task)
probs <- posterior(fm2)             # Compute probability of being in each state
head(probs)
head(iraq_data.clean)
nrow(probs)
nrow(iraq_data.clean)
rowSums(head(probs)[,2:3])          # Check that probabilities sum to 1

pS2 <- probs[,3]                  # Pick out the "terror" state
pS1 <- probs[,2]                  # Pick out the "terror" state
iraq_data.clean$ps2<-pS2 
iraq_data.clean$ps1<-pS1 
# colnames(iraq_data.clean)
##wide to long transformation for plotting
##could use tidyr to do this as well don't know why i used melt

df <- melt(iraq_data.clean,id="idate",measure=c("sum_kill","ps1","ps2"))
# colnames(df) head(df)
# unique(df$variable)
# str(df) summary(df)

iraq_data.clean$epoch<-ifelse(iraq_data.clean$ps1>=0.5,1,0)
##added a classification 


df2 <- melt(iraq_data.clean,id="idate",measure=c("sum_kill","epoch"))

##make a copy
iraq_data.clean2<-iraq_data.clean
iraq_data.clean2$ym<-format(as.Date(iraq_data.clean2$idate), "%Y%m")

iraq.data.epoch.by.month<-iraq_data.clean2 %>% filter(idate > "2006/01/01" & idate < "2009/01/01" ) %>% 
  group_by(ym) %>%  summarise(sum_states_month = sum(epoch,na.rm=T),
                                        sum_kills_month = sum(sum_kill,na.rm=T)) %>% 
                  melt(id="ym",measure=c("sum_states_month","sum_kills_month"))
                      
colnames(iraq.data.epoch.by.month)<-c("month","variable","value")

## head(iraq.data.epoch.by.month)
## str(iraq.data.epoch.by.month)

library(scales)

integer_breaks <- function(n = 5, ...) {
  breaker <- pretty_breaks(n, ...)
  function(x) {
    breaks <- breaker(x)
    breaks[breaks == floor(breaks)]
  }
}

iraq.data.epoch.by.month$month<-as.Date(paste(iraq.data.epoch.by.month$month,"01",sep=""),"%Y%m%d")
## library(forcats)
## str(iraq.data.epoch.by.month)
## levels(iraq.data.epoch.by.month$variable)

iraq.data.epoch.by.month$variable<-fct_recode(iraq.data.epoch.by.month$variable, terror_epochs = "sum_states_month",
           Deaths = "sum_kills_month")


statesplot2006.2009<-qplot(month,value,data=iraq.data.epoch.by.month   
      ,geom="line",
      main = "Terrorist kills and terror \n epoch days by month for 2006-2009",
      ylab = "",xlab="Date") + 
  facet_grid(variable ~ ., scales="free_y")

dailyplot.2009<-ggplot(gti_data.clean %>% filter(idate > "2006/01/01" & idate < "2009/01/01" ), aes(x=idate, y=sum_kill)) +
  geom_line(color = "blue")+
  ggtitle("Time series plot of the  \n  number of Deaths due to terrorism 2006-2009, \n averaged across all days (y-axis)")+
  xlab("Date")+
  ylab("number of deaths \n due to terrorism")


plot_grid(statesplot2006.2009, dailyplot.2009, labels = "", ncol = 1, align = 'v')


iraq_data.clean %>% filter(idate > "2007/01/01" & idate < "2008/01/01" ) %>% 
  group_by(month(idate)) %>%  summarise(sum_states_month = sum(epoch,na.rm=T),
                                        sum_kills_month = sum(sum_kill,na.rm=T))

iraq_data.clean %>% filter(idate > "2008/01/01" & idate < "2009/01/01" ) %>% 
  group_by(month(idate)) %>%  summarise(sum_states_month = sum(epoch,na.rm=T),
                                        sum_kills_month = sum(sum_kill,na.rm=T))

iraq_data.clean %>% filter(idate > "2009/01/01" & idate < "2010/01/01" ) %>% 
  group_by(month(idate)) %>%  summarise(sum_states_month = sum(epoch,na.rm=T),
                                        sum_kills_month = sum(sum_kill,na.rm=T))



df$value<-dplyr::coalesce(df$value,0)

df2$value<-dplyr::coalesce(df2$value,0)

qplot(idate,value,data=df,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df2,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")


qplot(idate,value,data=df2  %>% filter(idate > "2003/01/01" & idate < "2004/01/01" )
      ,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df2  %>% filter(idate > "2003/01/01" & idate < "2004/01/01" )
      ,geom="point",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")


##
### change to class
## 
##

str(df)
summary(df)


qplot(idate,value,data=df  %>% filter(idate > "2004/01/01" & idate < "2005/01/01" )
      ,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df  %>% filter(idate > "2005/01/01" & idate < "2006/01/01" )
      ,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df  %>% filter(idate > "2006/01/01" & idate < "2007/01/01" )
      ,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df2  %>% filter(idate > "2006/01/01" & idate < "2007/01/01" )
      ,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df  %>% filter(idate > "2007/01/01" & idate < "2008/01/01" )
      ,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df  %>% filter(idate > "2007/01/01" & idate < "2008/01/01" )
      ,geom="",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df  %>% filter(idate > "2008/01/01" & idate < "2009/01/01" )
      ,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df2  %>% filter(idate > "2008/01/01" & idate < "2009/01/01" )
      ,geom="point",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df  %>% filter(idate > "2009/01/01" & idate < "2010/01/01" )
      ,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df  %>% filter(idate > "2010/01/01" & idate < "2011/01/01" )
      ,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

str(df2)


df3<-df2  %>% filter(idate > "2010/01/01" & idate < "2011/01/01" & variable=="epoch" )

qstates<-qplot(idate,value,data=df2  %>% filter(idate > "2003/01/01" & idate < "2004/01/01" & variable=="epoch" )
               ,geom="point",color = factor(as.character(value)),
               main = "Terrorist  epochs \n (1 is terrorst epoch 0 is non terrorist epoch)",
               ylab = "Epoch", xlab="Date interval (days)")+theme_set(theme_grey(base_size = 8)) + theme(legend.position="none")+ geom_point(size = 1)

qdeaths<-qplot(idate,value,data=df2  %>% filter(idate > "2003/01/01" & idate < "2004/01/01" & variable=="sum_kill" )
               ,geom="line",
               main = "Terrorist kills and by date",
               ylab = "Deaths", xlab="Date interval (days)")+theme_set(theme_grey(base_size = 8))

plot_grid(qdeaths, qstates, labels = "AUTO", ncol = 1, align = 'v')

##
####
##

qstates2006<-qplot(idate,value,data=df2  %>% filter(idate > "2006/01/01" & idate < "2007/01/01" & variable=="epoch" )
                   ,geom="point",color = factor(as.character(value)),
                   main = "Terrorist Deaths by date \n interval days",
                   ylab = "Deaths",xlab="Date interval (days)")+theme_set(theme_grey(base_size = 8)) + theme(legend.position="none")+ geom_point(size = 1)

qdeaths2006<-qplot(idate,value,data=df2  %>% filter(idate > "2006/01/01" & idate < "2007/01/01" & variable=="sum_kill" )
                   ,geom="line",
                   main = "Terrorist  epochs \n (1 is terrorst epoch 0 is non terrorist epoch)",
                   ylab = "State (1 corresponds \n to a 'terror epoch' \n 0 corresponds to a 'non terror epoch')",xlab="Date interval (days)")+theme_set(theme_grey(base_size = 8))

classhmm2006<-plot_grid(qdeaths2006, qstates2006, labels = "", ncol = 1, align = 'v')

plot(classhmm2006)


##
####
##


qstates2007<-qplot(idate,value,data=df2  %>% filter(idate > "2007/01/01" & idate < "2008/01/01" & variable=="epoch" )
               ,geom="point",color = factor(as.character(value)),
               main = "Terrorist  epochs \n (1 is terrorst epoch 0 is non terrorist epoch)",
               ylab = "State (1 corresponds \n to a 'terror epoch' \n 0 corresponds to a 'non terror epoch')",xlab="Date interval (days)")+theme_set(theme_grey(base_size = 8)) + theme(legend.position="none")+ geom_point(size = 1)

qdeaths2007<-qplot(idate,value,data=df2  %>% filter(idate > "2007/01/01" & idate < "2008/01/01" & variable=="sum_kill" )
               ,geom="line",
               main = "Terrorist Deaths by date",
               ylab = "Deaths",xlab="Date interval (days)")+theme_set(theme_grey(base_size = 8))

classhmm2007<-plot_grid(qdeaths2007, qstates2007, labels = "", ncol = 1, align = 'v')

plot(classhmm2007)

qstates2008<-qplot(idate,value,data=df2  %>% filter(idate > "2008/01/01" & idate < "2009/01/01" & variable=="epoch" )
               ,geom="point",color = factor(as.character(value)),
               main = "Terrorist  epochs \n (1 is terrorst epoch 0 is non terrorist epoch)",
               ylab = "State (1 corresponds \n to a 'terror epoch' \n 0 corresponds to a 'non terror epoch')",xlab="Date interval (days)")+theme_set(theme_grey(base_size = 8)) + theme(legend.position="none")+ geom_point(size = 1)

qdeaths2008<-qplot(idate,value,data=df2  %>% filter(idate > "2008/01/01" & idate < "2009/01/01" & variable=="sum_kill" )
               ,geom="line",
               main = "Terrorist kills and by date",
               ylab = "Deaths",xlab="Date interval (days)")+theme_set(theme_grey(base_size = 8))

classhmm2008<-plot_grid(qdeaths2008, qstates2008, labels = "", ncol = 1, align = 'v')

plot(classhmm2008)


qstates<-qplot(idate,value,data=df2  %>% filter(idate > "2010/01/01" & idate < "2011/01/01" & variable=="epoch" )
      ,geom="point",color = factor(as.character(value)),
      main = "Terrorist  epochs \n (1 is terrorst epoch 0 is non terrorist epoch)",
      ylab = "")+theme_set(theme_grey(base_size = 8)) + theme(legend.position="none")+ geom_point(size = 1)

qdeaths<-qplot(idate,value,data=df2  %>% filter(idate > "2010/01/01" & idate < "2011/01/01" & variable=="sum_kill" )
               ,geom="line",
               main = "Terrorist kills and by date",
               ylab = "")+theme_set(theme_grey(base_size = 8))

plot_grid(qdeaths, qstates, labels = "AUTO", ncol = 1, align = 'v')



qstates<-qplot(idate,value,data=df2  %>% filter(idate > "2014/01/01" & idate < "2015/01/01" & variable=="epoch" )
               ,geom="point",color = factor(as.character(value)),
               main = "Terrorist  epochs \n (1 is terrorst epoch 0 is non terrorist epoch)",
               ylab = "")+theme_set(theme_grey(base_size = 8)) + theme(legend.position="none")+ geom_point(size = 1)

qdeaths<-qplot(idate,value,data=df2  %>% filter(idate > "2014/01/01" & idate < "2015/01/01" & variable=="sum_kill" )
               ,geom="line",
               main = "Terrorist kills and by date",
               ylab = "")+theme_set(theme_grey(base_size = 8))

plot_grid(qdeaths, qstates, labels = "AUTO", ncol = 1, align = 'v')


qplot(idate,value,data=df  %>% filter(idate > "2011/01/01" & idate < "2012/01/01" )
      ,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df2  %>% filter(idate > "2011/01/01" & idate < "2012/01/01" )
      ,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df2  %>% filter(idate > "2011/01/01" & idate < "2012/01/01" )
      ,geom="point",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df  %>% filter(idate > "2012/01/01" & idate < "2013/01/01" )
      ,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df2  %>% filter(idate > "2012/01/01" & idate < "2013/01/01" )
      ,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df2  %>% filter(idate > "2012/01/01" & idate < "2013/01/01" )
      ,geom="point",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df  %>% filter(idate > "2013/01/01" & idate < "2014/01/01" )
      ,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df2  %>% filter(idate > "2013/01/01" & idate < "2014/01/01" )
      ,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df  %>% filter(idate > "2014/01/01" & idate < "2015/01/01" )
      ,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df2  %>% filter(idate > "2014/01/01" & idate < "2015/01/01" )
      ,geom="point",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")


qplot(idate,value,data=df  %>% filter(idate > "2012/01/01" & idate < "2014/01/01" )
      ,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df  %>% filter(idate > "2012/01/01" & idate < "2014/01/01" )
      ,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")


head(df)
?depmix# beta distribution
mod2 <- depmix(sum_kill ~ 1, family = gaussian(), nstates = 3, data = iraq_data.clean)
set.seed(1)
fm3 <- fit(mod2, verbose = FALSE)


# Classification (inference task)
probs <- posterior(fm3)             # Compute probability of being in each state
head(probs)
head(iraq_data.clean)
nrow(probs)
nrow(iraq_data.clean)
rowSums(head(probs)[,2:5])          # Check that probabilities sum to 1


ps3 <- probs[,3]
ps2<-  probs[,2]
ps1 <-probs[,1]
head(iraq_data.clean)

iraq_data.clean$ps2<-NULL

iraq_data.clean$ps3<-ps3 
iraq_data.clean$ps2<-ps2 
iraq_data.clean$ps1<-ps1

colnames(iraq_data.clean)

df <- melt(iraq_data.clean,id="idate",measure=c("sum_kill","ps3","ps2","ps1"))

colnames(df)

head(df)

unique(df$variable)

qplot(idate,value,data=df,geom="line",
      main = "Terrorist kills and 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

##lets look at 2008 - 2010
str(df)
df.2007.2008 <- df %>% filter(idate > "2007/01/01" & idate < "2008/01/01" ) 
summary(df.2008.2010)



df.2008.2010 <- df %>% filter(idate > "2008/01/01" & idate < "2010/01/01" ) 
summary(df.2008.2010)

qplot(idate,value,data=df.2007.2008,geom="line",
      main = "Terrorist kills and 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")
head(df)

qplot(idate,value,data=df.2008.2010,geom="line",
      main = "Terrorist kills and 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")
head(df)


sp <- ggplot(tips, aes(x=total_bill, y=tip/total_bill)) + geom_point(shape=1)

# Divide by levels of "sex", in the vertical direction
sp + facet_grid(sex ~ .)

ggplotly()

df.2010.2012 <- df %>% filter(idate > "2010/01/01" & idate < "2012/01/01" ) 
summary(df.2010.2012)

qplot(idate,value,data=df.2010.2012,geom="line",
      main = "Terrorist kills and 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

df.2012.2014 <- df %>% filter(idate > "2012/01/01" & idate < "2014/01/01" ) 
summary(df.2010.2012)

qplot(idate,value,data=df.2012.2014,geom="line",
      main = "Terrorist kills and 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y") 


###
#
###lets try poisson
# library(dplyr)
