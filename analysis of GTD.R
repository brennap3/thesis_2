install.packages("KMsurv")
install.packages("gridExtra")
install.packages("depmixS4")
install.packages("TTR")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("rts")
install.packages("purrr")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("plotly")
install.packages("stringr")

library(stringr)
library(plotly)
library(FactoMineR)
library(factoextra)
library(purrr)
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
library(plotly)
library(RColorBrewer)
library(magrittr)



attach(tongue)
help(tongue)
??tongue
## read in the data
gti_data<-read.csv("C://Users//Peter//Desktop//gti//globalterrorismdb_0616dist.csv",header=TRUE,sep=",")


fatalaties.by.region.month.year<-gti_data %>%
  group_by(iyear,imonth,region_txt) %>%
  summarise(num_of_killed = sum(nkill,na.rm=T)) %>% as.data.frame()

fatalaties.by.region.month.year$iyearimonthdt<-
  ymd(paste(as.character(fatalaties.by.region.month.year$iyear),
    as.character(fatalaties.by.region.month.year$imonth),"01"))


str(fatalaties.by.region.month.year)



ggplot(fatalaties.by.region.month.year, aes(x=iyearimonthdt, y=num_of_killed,colour=region_txt)) +
  geom_line()+
  ggtitle("Time series plot of number of deaths due to terrorism \n by region")+
  xlab("interval")+
  ylab("deaths")+ guides(colour=guide_legend(title="Region"))
  ##+  scale_color_brewer(palette = "Paired") ##dont 'use Rcolor brewer

fatalaties.by.month.year<-gti_data %>%
  group_by(iyear,imonth) %>%
  summarise(num_of_killed = sum(nkill,na.rm=T)) %>% as.data.frame()

fatalaties.by.month.year$iyearimonthdt<-
  ymd(paste(as.character(fatalaties.by.month.year$iyear),
            as.character(fatalaties.by.month.year$imonth),"01"))

summary(fatalaties.by.month.year)

ggplot(fatalaties.by.month.year, aes(x=iyearimonthdt, y=num_of_killed)) +
  geom_line()+
  ggtitle("Time Series plot of \n world wide deaths due to terrorism")+
  xlab("interval (month)")+
  ylab("deaths")



##lets plot

##fatalaties.by.region.month.year

str(fatalaties.by.region.month.year)
summary(fatalaties.by.region.month.year)

                                                                             

##death by year

gti_data$nkill_clean<-coalesce(gti_data$nkill,0)

fatalaties.by.year<-gti_data %>%
  group_by(iyear) %>%
  summarise(num_of_killed = sum(nkill_clean)) %>% as.data.frame()

ggplot(fatalaties.by.year, aes(x=iyear, y=num_of_killed)) +
  geom_line()+
  ggtitle("time series plot daths per year")+
  xlab("year")+
  ylab("deaths")

## by attack type
##str(gti_data)
##unique(gti_data$attacktype1_txt)


fatalaties.by.year.AttackType<-gti_data %>%
  group_by(iyear,attacktype1_txt) %>%
  summarise(num_of_killed = sum(nkill_clean)) %>% as.data.frame()

str(fatalaties.by.year.AttackType)

ggplot(data = fatalaties.by.year.AttackType, aes(x = iyear, y = num_of_killed, fill = attacktype1_txt)) + 
  geom_bar(stat="identity")+
  ggtitle("Stacked bar plot of deaths by attack vector \n by year")+
  labs(x="interval (year)", y="deaths",
       col="Attack type")+ guides(fill=guide_legend(title="Attack type"))

# unique(gti_data$region_txt)

##by atttack type and by year and filter by region (middle east)

fatalaties.by.year.AttackType.Middleeast<-gti_data %>% filter(region_txt=="Middle East & North Africa") %>%
  group_by(iyear,attacktype1_txt) %>%
  summarise(num_of_killed = sum(nkill_clean)) %>% as.data.frame()

str(fatalaties.by.year.AttackType)

ggplot(data = fatalaties.by.year.AttackType.Middleeast, aes(x = iyear, y = num_of_killed, fill = attacktype1_txt)) + 
  geom_bar(stat="identity")+
  ggtitle("Stacked bar plot of deaths by attack vector \n (Middle East only) by year")+
  labs(x="interval (year)", y="deaths",
       col="Attack type")+ guides(fill=guide_legend(title="Attack type"))


###
##### str(gti_data)
###

fatalaties.by.year.gname.iraq<-gti_data %>% filter(country_txt=="Iraq" & iyear>2000) %>%
  group_by(iyear,gname) %>%
  summarise(num_of_killed = sum(nkill_clean)
            ) %>% as.data.frame()

fatalaties.by.year.gname.iraq.tot<- fatalaties.by.year.gname.iraq %>%
    group_by(gname) %>% 
      summarize(tot_killed = sum(num_of_killed)) %>% as.data.frame()

fatalaties.by.year.gname.iraq.tot$Kills_rank<-
  order(-fatalaties.by.year.gname.iraq.tot$tot_killed)

## head(fatalaties.by.year.gname.iraq.tot)

##head(fatalaties.by.year.gname.iraq)

##

fatalaties.by.iraq.by.ranked.groups<-merge(fatalaties.by.year.gname.iraq,fatalaties.by.year.gname.iraq.tot,by.x="gname",by.y="gname",all.x=T)

fatalaties.by.iraq.by.ranked.groups$gname<-ifelse(fatalaties.by.iraq.by.ranked.groups$Kills_rank>30,"Misc Insurgents",
        as.character(fatalaties.by.iraq.by.ranked.groups$gname))

ggplot(data = fatalaties.by.iraq.by.ranked.groups, aes(x = iyear, y = num_of_killed, fill = gname)) + 
  geom_bar(stat="identity")+
  ggtitle("Stacked bar plot of deaths by group \n (Iraq only) by year")+
  labs(x="interval (year)", y="deaths",
       col="Group Name")+ guides(fill=guide_legend(title="Group Name"))



## weaptype1_txt

fatalaties.by.year.weapType<-gti_data %>%
  group_by(iyear,weaptype1_txt) %>%
  summarise(num_of_killed = sum(nkill_clean)) %>% as.data.frame()

## str(fatalaties.by.year.weapType)
## fatalaties.by.year.weapType$weaptype1_txt
## dont forget to escape pattern
## ?str_replace
## require(stringr)
## str(fatalaties.by.year.weapType$weaptype1_txt)
## x<-'Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)'

fatalaties.by.year.weapType$weaptype1_txt<-as.character(fatalaties.by.year.weapType$weaptype1_txt)

fatalaties.by.year.weapType$weaptype1_txt<-ifelse(
  fatalaties.by.year.weapType$weaptype1_txt=='Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)',
       'Vehicle',fatalaties.by.year.weapType$weaptype1_txt)

## head(fatalaties.by.year.weapType$weaptype1_txt,50)


ggplot(data = fatalaties.by.year.weapType, aes(x = iyear, 
                          y = num_of_killed, fill = weaptype1_txt)) + 
  geom_bar(stat="identity")+
  ggtitle("Stacked bar plot of deaths by attack vector \n by year")+
  labs(x="interval (year)", y="deaths",
       col="Weapon Type")+ guides(fill=guide_legend(title="weapon type"))

##
####Correspondence analysis for all countries
##




fatalaties.by.year.weapType<-gti_data %>%
  group_by(iyear,weaptype1_txt) %>%
  summarise(num_of_killed = sum(nkill_clean)) %>% as.data.frame()



## head(fatalaties.by.year.weapType)
## ?table

##with(fatalaties.by.year.weapType, table(iyear,weaptype1_txt))

fatalaties.by.year.weapType$iyear<-as.factor(as.character(fatalaties.by.year.weapType$iyear))

head(melt(fatalaties.by.year.weapType))
?dcast
dc1<-dcast(fatalaties.by.year.weapType,iyear~weaptype1_txt,sum)
## head(dc1,5)
## str(dc1)
## rownames(dc1)<-dc1$iyear
dc1<-dc1[,-1] %>% as.data.frame()
## head(dc1)
dc2<-as.table(as.matrix(dc1))
## str(dc2)
## head(dc2)
## col
## str(dc1)
## str(dc2)
## ?CA
## str(fatalaties.by.year.weapType)
##fatalaties.by.year.weapType,iyear~weaptype1_tx
## ?xtabs
## not remove bio and chem attacks as there was none!

hist(dc2)
resca<-CA(dc2[,c(3,5,6,8,11)],graph=TRUE)

## print(resca)
## ?plot

plot(resca)

## ?fviz_ca_biplot

fviz_ca_biplot(resca)

## unique(gti_data$targtype2_txt)
#
##
##gname correspondence analysis plotly
#
##

fatalaties.by.year.gname.global<-gti_data %>% filter() %>%
  group_by(iyear,gname) %>%
  summarise(num_of_killed = sum(nkill_clean)
  ) %>% as.data.frame()

fatalaties.by.year.gname.global.tot<- fatalaties.by.year.gname.global %>%
  group_by(gname) %>% 
  summarize(tot_killed = sum(num_of_killed)) %>% arrange(desc(tot_killed)) %>% as.data.frame()

head(fatalaties.by.year.gname.global.tot,50)

fatalaties.by.year.gname.global.tot$Kills_rank<-
  order(-fatalaties.by.year.gname.global.tot$tot_killed)

## head(fatalaties.by.year.gname.global.tot)

##head(fatalaties.by.year.gname.iraq)

##

fatalaties.by.year.gname.global.tot.groups<-
  merge(fatalaties.by.year.gname.global,
        fatalaties.by.year.gname.global.tot,
        by.x="gname",by.y="gname",all.x=T)

## fatalaties.by.year.gname.global.tot.groups$gname<-ifelse(fatalaties.by.year.gname.global.tot.groups$Kills_rank>30,
##                                                   "Misc Insurgents",
##                                                   as.character(fatalaties.by.year.gname.global.tot.groups$gname))


## head(fatalaties.by.year.gname.global.tot.groups)
## tail(fatalaties.by.year.gname.global.tot.groups)

## 
str(fatalaties.by.year.gname.global.tot.groups)

fatalaties.by.year.gname.global.tot.groups.top.50<-fatalaties.by.year.gname.global.tot.groups %>% 
  filter(Kills_rank<=50)

fatalaties.by.year.gname.global.tot.groups.top.50$iyear<-as.factor(as.character(fatalaties.by.year.gname.global.tot.groups.top.50$iyear))

head(melt(fatalaties.by.year.gname))
?dcast
dc2x<-dcast(fatalaties.by.year.gname.global.tot.groups.top.50,iyear~gname,sum)

## head(dc2x)

rownames(dc2x)<-dc2x$iyear

dc2x<-dc2x[,-1] %>% as.data.frame()

head(dc2x)

##

dc2x2<-as.table(as.matrix(dc2x))

head(dc2x2)

res.ca2<-CA(dc2x2)

str(res.ca2)

fviz_ca_biplot(res.ca2)

rowcoords<-res.ca2$row$coord[,c(1,2)] %>% as.data.frame()
colcoords<-res.ca2$col$coord[,c(1,2)] %>% as.data.frame()
str(colcoords)
str(rowcoords)
rownames
rownames(colcoords)

rowcoords$dimname<-"row"
colcoords$dimname<-"col"
colrowcoords<-rbind(rowcoords,colcoords)



str(colrowcoords)

colnames(colrowcoords)<-c("Comp.1","Comp.2","dimname")

str(colrowcoords)
colrowcoords$Comp.1
library(plotly)
p <- plot_ly(colrowcoords, x = colrowcoords$Comp.1 , y = colrowcoords$Comp.2,
             text = ~rownames(colrowcoords),
             mode = "text", color = colrowcoords$dimname,
             marker = list(size = 11)) 

p <- layout(p, title = "CA plot for ", 
            xaxis = list(title = "Dim 1"),
            yaxis = list(title = "Dim 2"))

p



fviz_ca_row(res.ca2, alpha.row="contrib")+
  theme_minimal()
fviz_ca_row(res.ca2)
fviz_ca_col(res.ca2)

##
#### end of plotly CA
###
##

##
####MCA weapons gname year
####
##
## str(gti_data) weaptype1_txt
fatalaties.by.year.gname.weaptyep<-gti_data %>%
  group_by(iyear,gname,weaptype1_txt) %>%
  summarise(num_of_killed = sum(nkill_clean)) %>% as.data.frame()

## str(fatalaties.by.year.gname.weaptyep)

fatalaties.by.year.gname.global.tot<- fatalaties.by.year.gname.global %>%
  group_by(gname) %>% 
  summarize(tot_killed = sum(num_of_killed)) %>% arrange(desc(tot_killed)) %>% as.data.frame()

## head(fatalaties.by.year.gname.global.tot,50)

fatalaties.by.year.gname.global.tot$Kills_rank<-
  order(-fatalaties.by.year.gname.global.tot$tot_killed)

## head(fatalaties.by.year.gname.global.tot)

##head(fatalaties.by.year.gname.iraq)

##

fatalaties.by.year.gname.global.tot.groups.weaptype<-
  merge(fatalaties.by.year.gname.weaptyep,
        fatalaties.by.year.gname.global.tot,
        by.x="gname",by.y="gname",all.x=T)

fatalaties.by.year.gname.global.tot.groups.weaptype.top.50<-
  fatalaties.by.year.gname.global.tot.groups.weaptype %>% 
      filter(Kills_rank<=50)

str(fatalaties.by.year.gname.global.tot.groups.weaptype.top.50)

fatalaties.by.year.gname.global.tot.groups.weaptype.top.50.sel<- fatalaties.by.year.gname.global.tot.groups.weaptype.top.50 %>%
  select(iyear,gname,weaptype1_txt,num_of_killed)

fatalaties.by.year.gname.global.tot.groups.weaptype.top.50.sel$iyear<-
  as.factor(as.character(fatalaties.by.year.gname.global.tot.groups.weaptype.top.50.sel$iyear))

## str(fatalaties.by.year.gname.global.tot.groups.weaptype.top.50.sel)
#
## head(fatalaties.by.year.gname.global.tot.groups.weaptype.top.50.sel)

rownames(fatalaties.by.year.gname.global.tot.groups.weaptype.top.50.sel)<-
  fatalaties.by.year.gname.global.tot.groups.weaptype.top.50.sel$iyear

# ?mtcars
# str(mtcars)
# 
# head(mtcars)

str(fatalaties.by.year.gname.global.tot.groups.weaptype.top.50.sel)

mdata<-melt(fatalaties.by.year.gname.global.tot.groups.weaptype.top.50.sel,
     id=c("num_of_killed"))


df.t<-t(fatalaties.by.year.gname.global.tot.groups.weaptype.top.50.sel)

head(df.t,1)


## head(dc2xsel)
## str (dc2xsel)

data(tea)
str(tea)
head(tea)

str(gti_data)
unique(gti_data$targtype1_txt)
unique(gti_data$weaptype1_txt)
unique(gti_data$gname)
unique(gti_data$iyear)
unique(gti_data$region_txt)

gti_data.sbset <- gti_data %>% filter(region_txt=='Middle East & North Africa') %>% select(gname,targtype1_txt,
                                      weaptype1_txt,iyear) %>% as.data.frame() 

str(gti_data.sbset)

gti_data.sbset$iyear <-as.factor(as.character(gti_data.sbset$iyear))

##

cats = apply(gti_data.sbset, 2, function(x) nlevels(as.factor(x)))

res.mca <- MCA(gti_data.sbset, graph = FALSE)

mca1_vars_df = data.frame(res.mca$var$coord, 
                          Variable = rep(names(cats), cats))

mca1_obs_df = data.frame(res.mca$ind$coord)

p<-ggplot(data=mca1_vars_df, 
          aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR")

ggplotly(p)



## head(fatalaties.by.year.gname.global.tot.groups.weaptype)

## limit to the top 50 groups

##
####
####
####
##

fatalaties.by.year.gname<-gti_data %>%
  group_by(iyear,gname) %>%
  summarise(num_of_killed = sum(nkill_clean)) %>% as.data.frame()



## str(fatalaties.by.year.AttackType)

ggplot(data = fatalaties.by.year.gname, aes(x = iyear, 
                                               y = num_of_killed, fill = gname)) + 
  geom_bar(stat="identity")+
  ggtitle("Stacked bar plot of deaths by attack vector \n by year")+
  labs(x="interval (year)", y="deaths",
       col="Group")+ guides(fill=guide_legend(title="Group"))




## deaths by year and region

fatalaties.by.year.region<-gti_data %>%
  group_by(iyear,region_txt) %>%
  summarise(num_of_killed = sum(nkill_clean)) %>% as.data.frame()

ggplot(fatalaties.by.year.region, aes(x=iyear, y=num_of_killed,
                                              colour=region_txt)) +
  geom_line()+
  ggtitle("time series plot")+
  xlab("interval")+
  ylab("deaths")

##

ggplot(data = fatalaties.by.year.region, aes(x = iyear, y = num_of_killed, fill = region_txt)) + 
  geom_bar(stat="identity")+
  ggtitle("Stacked bar plot")+
  labs(x="year", y="deaths",
       col="Region")+ guides(fill=guide_legend(title="Region"))

##

## lets look at Iraq

# --pre invasion since 1999
## post invasion since 2003
## surge 2008 Jan
## post surge since June 2009


########################################
##
##
##
########################################


#filter by Iraq
#filter from 200101010001

# str(gti_data)

fatalaties.by.year.iraq<-gti_data %>%
  filter(country_txt=='Iraq')%>%
  group_by(iyear) %>%
  summarise(num_of_killed = sum(nkill_clean)) %>% as.data.frame()

##
#
##

ggplot(data = fatalaties.by.year.iraq, aes(x = iyear, y = num_of_killed)) + 
  geom_bar(stat="identity")+
  ggtitle("Number of Deaths due to \n  terrorism in Iraq")+
  labs(x="year", y="deaths")


##
# fatalaties by year
##
str(gti_data)

fatalaties.by.year.attacktype.iraq<-gti_data %>%
  filter(country_txt=='Iraq')%>%
  group_by(iyear,attacktype1_txt) %>%
  summarise(num_of_killed = sum(nkill_clean,na.rm=T)) %>% as.data.frame()



##
###
##
fatalaties.by.year.syria<-gti_data %>%
  filter(country_txt=='Syria')%>%
  group_by(iyear) %>%
  summarise(num_of_killed = sum(nkill_clean)) %>% as.data.frame()

##
#
##

ggplot(data = fatalaties.by.year.syria, aes(x = iyear, y = num_of_killed)) + 
  geom_bar(stat="identity")+
  ggtitle("Number of Deaths due to\n terrorism Syria")+
  labs(x="year", y="deaths")


##
#
##

fatalaties.by.year.lebanon<-gti_data %>%
  filter(country_txt=='Lebanon')%>%
  group_by(iyear) %>%
  summarise(num_of_killed = sum(nkill_clean)) %>% as.data.frame()

##
#
##

ggplot(data = fatalaties.by.year.lebanon, aes(x = iyear, y = num_of_killed)) + 
  geom_bar(stat="identity")+
  ggtitle("Number of Deaths \n due to \n terrorism \n in Lebanon")+
  labs(x="year", y="deaths")


##
#
##

ggplot(data = fatalaties.by.year.lebanon, aes(x = iyear, y = num_of_killed)) + 
  geom_bar(stat="identity")+
  ggtitle("Number of Deaths due to terrorism Lebanon")+
  labs(x="year", y="deaths")

##
#
##


fatalaties.by.year.Turkey<-gti_data %>%
  filter(country_txt=='Turkey')%>%
  group_by(iyear) %>%
  summarise(num_of_killed = sum(nkill_clean)) %>% as.data.frame()



ggplot(data = fatalaties.by.year.Turkey, aes(x = iyear, y = num_of_killed)) + 
  geom_bar(stat="identity")+
  ggtitle("Number of Deaths due to terrorism, Turkey")+
  labs(x="year", y="deaths")


## filter by region and then look by country

fatalaties.by.year.middle.east<-gti_data %>%
    filter(region_txt=='Middle East & North Africa') %>%
  group_by(iyear,country_txt) %>%
  summarise(num_of_killed = sum(nkill_clean)) %>% as.data.frame()

###
#
###

ggplot(data = fatalaties.by.year.middle.east, aes(x = iyear, y = num_of_killed, fill = country_txt)) + 
  geom_bar(stat="identity")+
  ggtitle("Stacked bar plot")+
  labs(x="year", y="deaths",
       col="Country")+ guides(fill=guide_legend(title="Country"))


###
#
###


## with iraq lets look at the distribution of deaths
## filtered by year by 


fatalaties.year.iraq<-gti_data %>%
  filter(country_txt=='Iraq') %>%
  filter(iyear >= 2000 & iyear <=2015) %>%
  select(iyear,nkill_clean) %>%
    as.data.frame()

ggplot(fatalaties.year.iraq, aes(x = factor(iyear), y = log(nkill_clean))) + 
  geom_violin(aes(fill = factor(iyear))) + 
  geom_boxplot(width = 0.2)+
  ggtitle("Box violin overplot")+
  xlab("year")+ylab("log of number killed /n by terrorism")

ggplot(fatalaties.year.iraq, aes(x = factor(iyear), y = log(nkill_clean))) + 
  geom_boxplot(width = 0.2)+
  ggtitle("Box plot of Number of deaths \n in Iraq")+
  xlab("year")+ylab("log of number of killed")


fatalaties.year.iraq.2015<-gti_data %>%
  filter(country_txt=='Iraq') %>%
  filter(iyear ==2015) %>%
  select(nkill_clean) %>%
  as.data.frame()

hist(log(fatalaties.year.iraq.2015$nkill_clean))
hist((fatalaties.year.iraq.2015$nkill_clean))

fatalaties.year.iraq.2014<-gti_data %>%
  filter(country_txt=='Iraq') %>%
  filter(iyear ==2014) %>%
  select(nkill_clean) %>%
  as.data.frame()

hist((fatalaties.year.iraq.2014$nkill_clean))
hist(log(fatalaties.year.iraq.2014$nkill_clean))

fatalaties.year.iraq.2008<-gti_data %>%
  filter(country_txt=='Iraq') %>%
  filter(iyear ==2008) %>%
  select(nkill_clean) %>%
  as.data.frame()

hist((fatalaties.year.iraq.2008$nkill_clean))
hist(log(fatalaties.year.iraq.2008$nkill_clean))
boxplot(fatalaties.year.iraq.2008$nkill_clean)

iraq.kills.summary <- gti_data %>%
  filter(country_txt=='Iraq') %>%
  filter(iyear >= 2000 & iyear <=2015) %>%
  group_by(iyear) %>%
  summarize(kills_mean = mean(nkill_clean),
            kills_se = sqrt(var(nkill_clean)/length(nkill_clean)))

ggplot(iraq.kills.summary, aes(x = factor(iyear), y = kills_mean, fill = factor(iyear))) +  
  geom_bar(aes(fill = factor(iyear)), stat = "identity") + 
  geom_errorbar(aes(y = kills_mean, ymin = kills_mean-kills_se, ymax = kills_mean+kills_se), 
                color = "black", width = 0.4) + 
  ylim(0, 35) + 
  theme(legend.position = "none") +
  ggtitle("Bar graph")


qt(c(0.025,0.975),df=500000000)
1.32/0.39


install.packages("devtools")
library(devtools)

install.packages("digest")
library(digest)
install.packages("pack")
library(pack)

devtools::install_github("bnosac/pattern.nlp", args = "--no-multiarch")
library(pattern.nlp)

# Initialize Python Version 2.7.11 |Anaconda 2.5.0 (64-bit)| (default, Feb 16 2016, 09:58:36) [MSC v.1500 64 bit (AMD64)]

# x <- pattern_sentiment("i really really hate iphones", language = "english")
# y <- pattern_sentiment("We waren bijna bij de kooien toen er van boven 
#                        een hoeragejuich losbrak alsof Rudi Vuller door Koeman in z'n kloten was geschopt.", language = "dutch")
# z <- pattern_sentiment("j'aime Paris, c'est super", language = "french")
# rbind(x, y, z)

###
###
###

library(stringr)

iraq.data<-gti_data %>% filter(country_txt=='Iraq')

iraq.data$idate<-ymd(paste(iraq.data$iyear,str_pad(iraq.data$imonth,2,pad="0"),
                                  str_pad(iraq.data$iday,2,pad="0")))

iraq.data.daily<-iraq.data %>% select(idate,nkill) %>% group_by(idate) %>% summarize(sum_kill=sum(nkill)) %>% arrange(idate) %>% as.data.frame()


iraq_data.clean<-iraq.data.daily %>% filter(!is.na(idate))  %>% as.data.frame()

str(iraq_data.clean)
##
##
#
##
##
set.seed(1)
mod <- depmix(sum_kill ~ 1, family = gaussian(), nstates = 2, data = iraq_data.clean)
fm2 <- fit(mod, verbose = TRUE)
summary(fm2, which="response")

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
df <- melt(iraq_data.clean,id="idate",measure=c("sum_kill","ps1","ps2"))
# colnames(df)
# unique(df$variable)

qplot(idate,value,data=df,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df  %>% filter(idate > "2003/01/01" & idate < "2004/01/01" )
      ,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

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

qplot(idate,value,data=df  %>% filter(idate > "2007/01/01" & idate < "2008/01/01" )
      ,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df  %>% filter(idate > "2008/01/01" & idate < "2010/01/01" )
  ,geom="line",
      main = "Terrorist kills and \n 'non terror' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

qplot(idate,value,data=df  %>% filter(idate > "2010/01/01" & idate < "2012/01/01" )
      ,geom="line",
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

iraq.data<- gti_data %>%
  filter(country_txt=='Iraq')

iraq.data$incident_date<-ymd(paste(iraq.data$iyear,str_pad(iraq.data$imonth,2,pad=0),str_pad(iraq.data$iday,2,pad=0),sep=""))
##
## june 15 to july 22 2008 troop levels draw down
## february 27 2009 draw down begins
## august 31st draw down ends August 31 2010
## residual draw down begins
## ends december 12 2011
## colnames(iraq.data2)
## colnames(iraq.data2)
## head(iraq.data2postpullout)

iraq.data2pre<-iraq.data %>% dplyr::filter(iraq.data$incident_date < dmy("01/05/2003")) %>% mutate(Period="Pre_Invasion") %>% dplyr::select(eventid,Period) %>% merge(iraq.data)
iraq.data2post<-iraq.data %>% dplyr::filter(iraq.data$incident_date > dmy("01/05/2003") & iraq.data$incident_date < dmy("01/01/2007")) %>% mutate(Period="Post_Invasion") %>% dplyr::select(eventid,Period) %>% merge(iraq.data)
iraq.data2surge<-iraq.data %>% dplyr::filter(iraq.data$incident_date > dmy("01/01/2007") & iraq.data$incident_date < dmy("31/12/2007")) %>% mutate(Period="Surge") %>% dplyr::select(eventid,Period) %>% merge(iraq.data)
iraq.data2drawdown<-iraq.data %>% dplyr::filter(iraq.data$incident_date > dmy("01/01/2008") & iraq.data$incident_date < dmy("31/12/2009")) %>% mutate(Period="Drawdown") %>% dplyr::select(eventid,Period) %>% merge(iraq.data)
iraq.data2pullout<-iraq.data %>% dplyr::filter(iraq.data$incident_date > dmy("01/01/2010") & iraq.data$incident_date < dmy("31/12/2012")) %>% mutate(Period="Pullout") %>% dplyr::select(eventid,Period) %>% merge(iraq.data)
iraq.data2postpullout<-iraq.data %>% dplyr::filter(iraq.data$incident_date > dmy("31/12/2012")) %>% mutate(Period="PostPullout") %>% dplyr::select(eventid,Period) %>% merge(iraq.data)
iraq.data3<-rbind(iraq.data2pre,iraq.data2post,iraq.data2surge,iraq.data2drawdown,iraq.data2pullout,iraq.data2postpullout)
# iraq.data3$incident_date[1]

iraq.data3$month_since_1970<-month(as.period(interval(iraq.data3$incident_date[1],iraq.data3$incident_date),unit="month"))

# head(month(iraq.data3$incident_date))

?sum

iraq.kills.summary.period <- iraq.data3 %>%
  dplyr::group_by(month_since_1970,Period) %>%
  dplyr::summarize(kills_sum = sum(nkill,na.rm=T)) %>% as.data.frame()

# str(iraq.kills.summary.period)

iraq.kills.summary.period$kills_sum<-dplyr::coalesce(iraq.kills.summary.period$kills_sum,0)

# head(iraq.kills.summary.period)


glmpoisson.iraq<-glm(kills_sum~.,data=iraq.kills.summary.period,family="poisson")
summary(glmpoisson.iraq)
# 1 - (19330/41600)
glmquasipoisson.iraq<-glm(kills_sum~.,data=iraq.kills.summary.period,family="quasipoisson")
str(iraq.kills.summary.period)

summary(glmquasipoisson.iraq)
# 1 -(19329/41596)
#
predquasipois<-predict(glmquasipoisson.iraq,iraq.kills.summary.period,type="response")
predpois<-predict(glmpoisson.iraq,iraq.kills.summary.period,type="response")

iraq.kills.summary.period<-cbind(iraq.kills.summary.period,predquasipois,predpois)
colnames(iraq.kills.summary.period)<-c("Months","Period","kills","predquas","predpois")

# col
# head(iraq.kills.summary.period)

ggplot(iraq.kills.summary.period, aes(Months)) + 
  geom_line(aes(y = kills, colour = "kills")) + 
  geom_line(aes(y = predquasipois, colour = "predpois"))+
  ggtitle("Kills (due to  terrorism)  actual and \n predicted versus month for poison models")+
  xlab("Month since 1976")+
  ylab("Kills due \n to terrorism")


glmpoisson2.iraq<-glm(kills~Period+Months+I(Months^2),data=iraq.kills.summary.period,family="poisson")
predpois2<-predict(glmpoisson2.iraq,iraq.kills.summary.period,type="response")

iraq.kills.summary.period<-cbind(iraq.kills.summary.period,predquasipois,predpois2)

ggplot(iraq.kills.summary.period, aes(Months)) + 
  geom_line(aes(y = kills, colour = "kills")) + 
  geom_line(aes(y = predpois2, colour = "predpois2"))+
  geom_line(aes(y = predpois2, colour = "predpois"))+
  ggtitle("Kills (due to  terrorism)  actual and \n predicted versus month for poison models")+
  xlab("Month since 1976")+
  ylab("Kills due \n to terrorism")
summary(glmquasipoisson.iraq)



## build a  model for period
str(iraq.kills.summary.period)

iraq.kills.summary.period$Period<-iraq.kills.summary.period$Period %>% as.factor()




install.packages("dse")
library(dse)
data(Nile)

###the surge happened 2008 jan
install.packages("tsoutliers")
library(xts)
library(tsoutliers)
library(forecast)


str(iraq.kills.summary.period)

?sum
iraq.kills.summary.period

iraq.kills.summary.period$Deaths_At_Date<-as.Date("1970-02-01") %m+% months(iraq.kills.summary.period$Months)

library(zoo)

str(ln_ret)
ln_ret <- zoo::zoo(iraq.kills.summary.period$kills, as.Date(iraq.kills.summary.period$Deaths_At_Date, format = "%d/%m/%y"))
head(as.xts(ln_ret),50)
head(ln_ret_ts)


ln_ret_ts<- tseries::na.remove(as.ts(ln_ret))
outlier.iraq <- tsoutliers::tso(ln_ret_ts,types = c("AO","LS","TC"),maxit.iloop=10)
plot(outlier.iraq)


