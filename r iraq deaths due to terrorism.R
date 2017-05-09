rm(list=ls())

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

##death by year

gti_data<-read.csv("C://Users//Peter//Desktop//gti//globalterrorismdb_0616dist.csv",header=TRUE,sep=",")

gti_data$nkill_clean<-coalesce(gti_data$nkill,0)

iraq.kills.summary <- gti_data %>%
  filter(country_txt=='Iraq') %>%
  filter(iyear >= 1970 & iyear <=2015) %>%
  group_by(iyear) %>%
  summarize(kills_mean = mean(nkill_clean),
            kills_se = sqrt(var(nkill_clean)/length(nkill_clean)))

ggplot(iraq.kills.summary, aes(x = factor(iyear), y = kills_mean, fill = factor(iyear))) +  
  geom_bar(aes(fill = factor(iyear)), stat = "identity") + 
  geom_errorbar(aes(y = kills_mean, ymin = kills_mean-kills_se, ymax = kills_mean+kills_se), 
                color = "black", width = 0.4) + 
  ylim(0, 35) + 
  theme(legend.position = "none") +
  ggtitle("Bar graph of Deaths per year since 1970")

###
#####
###

iraq.data<-gti_data %>% filter(country_txt=='Iraq')


iraq.data$idate<-ymd(paste(iraq.data$iyear,str_pad(iraq.data$imonth,2,pad="0"),
                           str_pad(iraq.data$iday,2,pad="0")))

c<-seq(ymd('1975-03-01'),ymd('2015-12-31'),by='1 day')
d<-c(rep(0,times=14916)) %>% as.data.frame()
d$date<-c
colnames(d)<-c('count_kills','idate')
iraq.data<-dplyr::left_join(d, iraq.data, by = "idate")
iraq.data$nkill<-dplyr::coalesce(iraq.data$nkill,0)

iraq.data.daily<-iraq.data %>% dplyr::select(idate,nkill) %>% group_by(idate) %>% summarize(sum_kill=sum(nkill)) %>% arrange(idate) %>% as.data.frame()


gti_data.clean<-iraq.data.daily %>% filter(!is.na(idate))  %>% as.data.frame()

ggplot(gti_data.clean, aes(x=idate, y=sum_kill)) +
  geom_line(color = "blue")+
  ggtitle("time series plot of the  \n  number of kills, \n averaged across all days (y-axis)")+
  xlab("Date")+
  ylab("number of kills")


##by year


## colnames(gti_data)

gti_data.iraq<- gti_data%>% filter(country_txt=='Iraq')

fatalaties.by.year.AttackType.Iraq<-gti_data.iraq %>%
  group_by(iyear,attacktype1_txt) %>%
  summarise(num_of_killed = sum(nkill_clean,na.rm=T)) %>% as.data.frame()

str(fatalaties.by.year.AttackType.Iraq)

ggplot(data = fatalaties.by.year.AttackType.Iraq, aes(x = iyear, y = num_of_killed, fill = attacktype1_txt)) + 
  geom_bar(stat="identity")+
  ggtitle("Stacked bar plot of deaths by attack vector \n by year (Iraq only)")+
  labs(x="interval (year)", y="deaths",
       col="Attack type")+ guides(fill=guide_legend(title="Attack type"))


iraq.data<- gti_data %>%
  filter(country_txt=='Iraq')

iraq.data$incident_date<-ymd(paste(iraq.data$iyear,str_pad(iraq.data$imonth,2,pad=0),str_pad(iraq.data$iday,2,pad=0),sep=""))
##

iraq.data2pre<-iraq.data %>% dplyr::filter(iraq.data$incident_date < dmy("01/05/2003")) %>% mutate(Period="Pre_Invasion") %>% dplyr::select(eventid,Period) %>% merge(iraq.data)
iraq.data2post<-iraq.data %>% dplyr::filter(iraq.data$incident_date > dmy("01/05/2003") & iraq.data$incident_date < dmy("01/01/2007")) %>% mutate(Period="Post_Invasion") %>% dplyr::select(eventid,Period) %>% merge(iraq.data)
iraq.data2surge<-iraq.data %>% dplyr::filter(iraq.data$incident_date > dmy("01/01/2007") & iraq.data$incident_date < dmy("31/12/2007")) %>% mutate(Period="Surge") %>% dplyr::select(eventid,Period) %>% merge(iraq.data)
iraq.data2drawdown<-iraq.data %>% dplyr::filter(iraq.data$incident_date > dmy("01/01/2008") & iraq.data$incident_date < dmy("31/12/2009")) %>% mutate(Period="Drawdown") %>% dplyr::select(eventid,Period) %>% merge(iraq.data)
iraq.data2pullout<-iraq.data %>% dplyr::filter(iraq.data$incident_date > dmy("01/01/2010") & iraq.data$incident_date < dmy("31/12/2012")) %>% mutate(Period="Pullout") %>% dplyr::select(eventid,Period) %>% merge(iraq.data)
iraq.data2postpullout<-iraq.data %>% dplyr::filter(iraq.data$incident_date > dmy("31/12/2012")) %>% mutate(Period="PostPullout") %>% dplyr::select(eventid,Period) %>% merge(iraq.data)
##(x > dmy("01/08/2014"))
iraq.inherrentresolve<-iraq.data %>% dplyr::filter(iraq.data$incident_date > dmy("01/08/2014")) %>% mutate(Period="Inherrentresolve") %>% dplyr::select(eventid,Period) %>% merge(iraq.data)


iraq.data3<-rbind(iraq.data2pre,iraq.data2post,iraq.data2surge,iraq.data2drawdown,iraq.data2pullout,iraq.data2postpullout,iraq.inherrentresolve)

##str(iraq.data3)

myfunc <- function(x) {
  if(dmy("01/06/2006") <= x  &   dmy("01/01/2008") >= x){
    "ISI founded in Iraq"}
  else if(dmy("01/01/2008") <= x & dmy("01/01/2009") >= x){
    "ISI driven out of Iraq"}
  else if(dmy("01/01/2009") <= x & dmy("01/01/2009") >= x){
    "ISI strongly diminished"}
  else if(dmy("01/12/2009") <= x  & dmy("01/04/2010") >= x){
    "Maliki sectarian Policies garnishes support for ISIS"}
  else if(dmy("01/04/2010") <= x & dmy("01/07/2012") >= x){
    "Abu Bakr al Baghdadi becomes leader of ISIS"}
  else if(dmy("01/07/2012") <= x & dmy("01/07/2013") >= x){
     "ISIS breaking wall cmpgn Start"}
  else if(dmy("01/07/2013") <= x & dmy("01/01/2017") >= x){
    "ISIS cmpgn Soldier Harvest starts"}
  else("None")
}




myfunc(dmy("01/07/2007"))



iraq.data3 <- iraq.data3 %>% rowwise() %>%
mutate(Period_2 = myfunc(incident_date))

unique(iraq.data3$Period_2)

## check levels of iraqi insurgency are set correctly
## source
## see http://www.understandingwar.org/sites/default/files/Backgrounder_SoldiersHarvest.pdf
## 


## iraqpresidents sourced from 


IraqPres <- function(x) {
  if(x > dmy("01/01/1970") & x < dmy("01/07/1979")){
    "Ahmed Hassan Al-Bakr"}
  else if(x > dmy("01/07/1979") & x < dmy("01/04/2003")){
    "Saddam Hussein"}
  else if(x > dmy("01/04/2003") & x < dmy("01/06/2004")){
    "Iraqi Transition Council"}
  else if(x > dmy("01/06/2004") & x < dmy("01/05/2005")){
    "Ayad Allawi"}
  else if(x > dmy("01/05/2005") & x < dmy("01/06/2006")){
    "Ibrahim Al Jaafari"}
  else if(x > dmy("01/06/2006") & x < dmy("01/09/2014")){
    "Nouri al-Malaki"}
  else if(x > dmy("01/09/2014")){
    "Haider al-Abadi"}
  else("Not stated")
}

IraqPres(dmy("01/10/2014"))


iraq.data3 <- iraq.data3 %>% rowwise() %>%
  mutate(Iraq_Pres = IraqPres(incident_date))

unique(iraq.data3$Iraq_Pres)


colnames(iraq.data3)

head(iraq.data3)

## sourced from wikipedia

USPres <- function(x) {
  if(x < dmy("01/08/1974")){
    "Richard Nixon"}
  else if(x < dmy("01/01/1977")){
    "Gerald Ford"}
  else if(x < dmy("01/01/1981")){
    "Jimmy Carter"} 
  else if(x < dmy("01/01/1989")){
    "Ronald Reagan"}
  else if(x < dmy("01/01/1993")){
    "George Bush (Senior)"}
  else if(x < dmy("01/01/2001")){
    "Bill Clinton"}
  else if(x < dmy("01/01/2009")){
    "George Bush (Junior)"}
  else if(x < dmy("01/01/2017")){
    "Barack Obama"}
  else if(x > dmy("01/01/2017")){
    "Donald Trump"}
  else("Not stated")
}

##

USPres(dmy("01/01/2016"))

str(iraq.data3)
colnames(iraq.data3)
min(iraq.data3$incident_date)

##> min(iraq.data3$incident_date)
##[1] "1975-03-01"

iraq.data3 <- iraq.data3 %>% rowwise() %>%
  mutate(US_Pres = USPres(incident_date))

unique(iraq.data3$US_Pres)

##Update Period with US interventions 



##iraq.data3$Period<-ifelse(iraq.data$incident_date>dmy("01/08/2014"),"Inherrent Resolve Launched",iraq.data3$Period)
##https://www.defense.gov/News/Special-Reports/0814_Inherent-Resolve/
##rerun and add to original function
USTacticsUpdated <- function(x) {
  if(x > dmy("01/08/2014")){
    "Inherrent resolve launched"}
}

unique(iraq.data3$Period)

# iraq.data3 <- iraq.data3 %>% rowwise() %>%
#   mutate(Period = USTacticsUpdated(incident_date))

iraq.data3$month_since_197503<-month(as.period(interval(iraq.data3$incident_date[1],iraq.data3$incident_date),unit="month"))
# head(iraq.data3$month_since_197503)
# head(iraq.data,3)
# min(iraq.data3$incident_date)
# min(iraq.data3$month_since_1970)
# summary(iraq.data3) colnames(iraq.data3)
# head(month(iraq.data3$incident_date))

iraq.kills.summary.period <- iraq.data3 %>%
  group_by(month_since_197503,Period) %>%
  summarize(kills_sum = sum(nkill,na.rm=T)) %>% ungroup

# str(iraq.kills.summary.period)

iraq.kills.summary.period$kills_sum<-coalesce(iraq.kills.summary.period$kills_sum,0)

# head(iraq.kills.summary.period)

glmpoisson.iraq<-glm(kills_sum~.,data=iraq.kills.summary.period,family="poisson")

stargazer(glmpoisson.iraq,type="latex")


glmpoisson2.iraq<-glm(kills_sum~Period+month_since_197503+I(month_since_197503^2),data=iraq.kills.summary.period,family="poisson")
summary(glmpoisson.iraq)
# 1 - (19174/108067)
glmquasipoisson.iraq<-glm(kills_sum~.,data=iraq.kills.summary.period,family="quasipoisson")
summary(glmquasipoisson.iraq)
# 1 - (19174/108067)
# col

predquasipois<-predict(glmquasipoisson.iraq,iraq.kills.summary.period,type="response")
predpois<-predict(glmpoisson.iraq,iraq.kills.summary.period,type="response")

predpois2<-predict(glmpoisson2.iraq,iraq.kills.summary.period,type="response")

iraq.kills.summary.period<-cbind(iraq.kills.summary.period,predquasipois,predpois)
colnames(iraq.kills.summary.period)<-c("Months","Period","kills","predquas","predpois")

# col
# head(iraq.kills.summary.period)

ggplot(iraq.kills.summary.period, aes(Months)) + 
  geom_line(aes(y = kills, colour = "kills")) + 
  geom_line(aes(y = predpois, colour = "predpois"))+
  ggtitle("Deaths (due to  terrorism)  actual and \n predicted versus month for Poisson models")+
  xlab("Month since 197503")+
  ylab("Deaths due \n to terrorism")


##adding in extra variables

str(iraq.data3) colnames(iraq.data3)
min(iraq.data3$iyear)
min(iraq.data3$imonth)
min(iraq.data3$month_since_197503)


iraq.kills.summary.period.2 <- iraq.data3 %>%
  group_by(month_since_197503,Period,Period_2,Iraq_Pres) %>%
  summarize(kills_sum = sum(nkill,na.rm=T)) %>% ungroup


glmpoisson.iraq.2<-glm(kills_sum~.,data=iraq.kills.summary.period.2,
                       family="poisson")

summary(glmpoisson.iraq.2)

## install.packages("xtable")
library(xtable)

xtable(summary(glmpoisson.iraq.2))

summary(glmpoisson.iraq.2)

glmpoisson.iraq.2$dispersion
## install.packages("AER")
library(AER)
?dispersiontest
x<-dispersiontest(glmpoisson.iraq.2)
x$estimate
with(glmpoisson.iraq.2, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))


pchisq(glmpoisson.iraq.2$deviance, df=glmpoisson.iraq.2$df.residual, lower.tail=FALSE)


glmquasipoisson.iraq.2<-glm(kills_sum~.,data=iraq.kills.summary.period.2,
                       family="quasipoisson")

summary(glmpoisson.iraq.2)

summary(glmquasipoisson.iraq.2)



stargazer(glmpoisson.iraq.2,type="latex")


stargazer(glmpoisson.iraq.2,glmquasipoisson.iraq.2,title="Regression Results")

print(xtable(summary(glmpoisson.iraq.2), align = "r|llrc"))

pchisq(glmquasipoisson.iraq.2$deviance, df=glmquasipoisson.iraq.2$df.residual,
       lower.tail=FALSE)

library(pscl)

glmnegbinom.iraq.2<-glm.nb(kills_sum~.,data=iraq.kills.summary.period.2,
                            )

summary(glmpoisson.iraq.2)
summary(glmnegbinom.iraq.2) #255  degrees of freedom

X2 <- 2 * (logLik(glmnegbinom.iraq.2)-logLik(glmpoisson.iraq.2))
(X2) 
pchisq(X2,df=1,lower.tail=F)
## library(AER)
## citation("AER")

anova(glmnegbinom.iraq.2, test="Chisq")

with(glmnegbinom.iraq.2, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

stargazer(glmnegbinom.iraq.2,type="latex")




# install.packages("ResourceSelection")
library(ResourceSelection)
# kills_sum~.,data=iraq.kills.summary.period.2
library(stargazer)
stargazer(glmpoisson.iraq.2,type="latex")


## 1-(15585/110846)

predpois2<-predict(glmpoisson.iraq.2,iraq.kills.summary.period.2,type="response")

iraq.kills.summary.period2<-cbind(iraq.kills.summary.period.2,predpois2)


head(iraq.kills.summary.period2,50)

# colnames(iraq.kills.summary.period2)
# col
# head(iraq.kills.summary.period)

ggplot(iraq.kills.summary.period2, aes(month_since_197503)) + 
  geom_line(aes(y = kills_sum, colour = "kills")) + 
  geom_line(aes(y = predpois2, colour = "predpois"))+
  ggtitle("Kills (due to  terrorism)  actual and \n predicted versus month for poison models \n (second poisson model)")+
  xlab("Month since 197503")+
  ylab("Kills due \n to terrorism")



# install.packages("pscl")
library(pscl)
str(iraq.kills.summary.period.2)

str(iraq.kills.summary.period.3)
iraq.kills.summary.period.3<-iraq.kills.summary.period.2
iraq.kills.summary.period.3$month_since_197503<-as.factor(as.character(iraq.kills.summary.period.3$month_since_197503))

head(iraq.kills.summary.period.3)

iraq.kills.summary.period.4<-iraq.kills.summary.period.3 %>% filter(Period != "Pre_Invasion")

head(iraq.kills.summary.period.4)

?zeroinfl
##zro
zeroinfl.iraq.2<-zeroinfl(kills_sum~Period+Period_2+Iraq_Pres,
                            data=iraq.kills.summary.period.4,dist="negbin")

m0 <- update(zeroinfl.iraq.2, . ~ 1)

summary(zeroinfl.iraq.2)
pchisq(glmpoisson.iraq.2$deviance, df=glmpoisson.iraq.2$df.residual, lower.tail=FALSE)
zeroinfl.iraq.2$df.residual


pchisq(2 * (logLik(zeroinfl.iraq.2) - logLik(m0)), df = 35, lower.tail=FALSE)

## from the output above, we can see that our overall model is statistically significant.

vuong(glmpoisson.iraq.2, zeroinfl.iraq.2)

mean(iraq.kills.summary.period.2$kills_sum,na.rm=T)

lm.iraq.2<-lm(log(kills_sum+1)~.,data=iraq.kills.summary.period.2
                       )

##diagnostic plots
plot(lm.iraq.2)
 
##robust regression
rlm.iraq.2<-rlm(log(kills_sum+1)~.,data=iraq.kills.summary.period.2
)

summary(lm.iraq.2)
summary(rlm.iraq.2)

stargazer(lm.iraq.2,type="latex")
stargazer(rlm.iraq.2,type="latex")

str(iraq.kills.summary.period.2)
iraq.kills.summary.period.2 %>% filter(Iraq_Pres=="Not stated")

# 474/12 1975.25+39.5
# 362/12 1975.25+30.166 

r2ww <- function(x){
  SSe <- sum((x$w*x$resid)^2); #the residual sum of squares is weighted
  observed <- x$resid+x$fitted;
  SSt <- sum((x$w*(observed-mean(observed)))^2); #the total sum of squares is weighted      
  value <- 1-SSe/SSt;
  return(value);
  }

r2ww(rlm.iraq.2)

## install.packages("sfsmisc")
library(sfsmisc)
rlm.iraq.2$coefficients
# str(iraq.kills.summary.period.2)
# unique(iraq.kills.summary.period.2$Period)
# rlm.iraq.2$xlevels$Period
names(rlm.iraq.2$coefficients[1])
length(rlm.iraq.2$coefficients)

dft <- data.frame(Coef=character(),
                 F_statistic=character(), 
                 p_value=character())

for(i in seq(1:length(rlm.iraq.2$coefficients))){
 print(names(rlm.iraq.2$coefficients[i]))  
 x<-f.robftest(rlm.iraq.2, var = names(rlm.iraq.2$coefficients[i]))
 print(x$p.value[1])
 print(x$statistic[1])
 rwdft<-cbind(names(rlm.iraq.2$coefficients[i]),x$statistic[1],x$p.value[1])
 dft<-rbind(dft,rwdft)
}


colnames(dft)<-c('Variable','F-Statistic','p value')
rownames(dft)<-NULL
xtable(dft)

dft
?f.robftest
x<-f.robftest(rlm.iraq.2, var = "month_since_197503")
x$p.value
f.robftest(rlm.iraq.2, var = "month_since_197503") ##signif
f.robftest(rlm.iraq.2, var = "Iraq_PresNouri al-Malaki") ##not signif 
f.robftest(rlm.iraq.2, var = "Iraq_PresIbrahim Al Jaafari") ##not signif 







