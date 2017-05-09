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
library(ggmap)
library(readr)
library(googleVis)
library(ggplot2)
library(countrycode)

gti_data<-read.csv("C://Users//Peter//Desktop//gti//globalterrorismdb_0616dist.csv",header=TRUE,sep=",")

gti_data$nkill_clean<-coalesce(gti_data$nkill,0)

fatalaties.by.country.2015<-gti_data %>% filter(iyear==2015) %>%
  group_by(country_txt) %>%
  summarise(num_of_killed = sum(nkill_clean)) %>% as.data.frame()

##

##  head(gti_data$country)
## geocode("Westbank&Gaza")

fatalaties.by.country.2015$iso<-
  countrycode(fatalaties.by.country.2015$country_txt,"country.name","iso2c") %>% as.factor()

# str(fatalaties.by.country.2015)
# head(fatalaties.by.country.2015)
# 
# Intensity <- gvisIntensityMap(fatalaties.by.country.2015,
#                               locationvar ="country_txt",
#                               numvar = "num_of_killed")
# # ?gvisIntensityMap
# plot(Intensity)
##?gvisGeoChart

Geo=gvisGeoChart(fatalaties.by.country.2015, locationvar="country_txt", 
                 colorvar="num_of_killed",
                 options=list(projection="kavrayskiy-vii"))
plot(Geo)

## We can also look at the number of deaths aggregated by 




##str(df)

fatalaties.by.country.2010.2015<-gti_data %>% filter(iyear 
%in% c(2010,2011,2012,2013,2014,2015)) %>%
  group_by(country_txt) %>%
  summarise(num_of_killed = sum(nkill_clean)) %>% 
  as.data.frame()


Geo2010.2015=gvisGeoChart(fatalaties.by.country.2010.2015, locationvar="country_txt", 
                 colorvar="num_of_killed",
                 options=list(projection="kavrayskiy-vii"))

plot(Geo2010.2015)

##
#### 1980's to 1990's
##

fatalaties.by.country.1970.1979<-gti_data %>% filter(iyear 
  %in% c(1970,1971,1972,1973,1974,1975,1976,1977,1978,1979)) %>%
  group_by(country_txt) %>%
  summarise(num_of_killed = sum(nkill_clean)) %>% 
  as.data.frame()


Geo1970.1979<-gvisGeoChart(fatalaties.by.country.1970.1979, locationvar="country_txt", 
                          colorvar="num_of_killed",
                          options=list(projection="kavrayskiy-vii"))
plot(Geo1970.1979)

##
####
####
##

##
#### 1980's to 1990's
##

fatalaties.by.country.1980.1989<-gti_data %>% filter(iyear 
        %in% c(1980,1981,1982,1983,1984,1985,1986,1987,1988,1989)) %>%
  group_by(country_txt) %>%
  summarise(num_of_killed = sum(nkill_clean)) %>% 
  as.data.frame()


Geo1980.1989<-gvisGeoChart(fatalaties.by.country.1980.1989, locationvar="country_txt", 
                           colorvar="num_of_killed",
                           options=list(projection="kavrayskiy-vii"))
plot(Geo1980.1989)




##
#### 1990's to 2000's
##

fatalaties.by.country.1990.1999<-gti_data %>% filter(iyear 
    %in% c(1990,1991,1992,1993,1994,1995,1996,1997,1998,1999)) %>%
  group_by(country_txt) %>%
  summarise(num_of_killed = sum(nkill_clean)) %>% 
  as.data.frame()

## ?gvisGeoChart

Geo1990.1999<-gvisGeoChart(fatalaties.by.country.1990.1999, locationvar="country_txt", 
                           colorvar="num_of_killed",
                           options=list(projection="kavrayskiy-vii"  
                                        ))
plot(Geo1990.1999)



##
####
##



addGvisATLTitle <- function(gvisATL,title)  {
  
  if (!all(class(gvisATL) == c("gvis","list"))) {
    stop('ERROR in addGvisATLTitle: Incorrect type, expect gvisAnnotatedTimeLine.')
  }
  if (class(title) == "character") {
    gvisATL$html$chart['divChart'] <- paste(title,gvisATL$html$chart['divChart'],sep="")
  } else if (class(title) == "shiny.tag") {
    gvisATL$html$chart['divChart'] <- paste(as.character(title)[1],gvisATL$html$chart['divChart'],sep="")
  } else {
    stop('ERROR in addGvisATLTitle: Unknown title type.')
  }
  return(gvisATL)
}

GTM <- gvisMerge(Geo1970.1979, Geo1980.1989, horizontal=TRUE,
                 tableOptions="cellspacing=10")
plot(GTM)



plot(addGvisATLTitle(GTM,h1("My chart")))
# plot()

Geo1970.1979<-addGvisATLTitle(Geo1970.1979,"<h1> 1970 - 1979</h1>")
plot(Geo1970.1979)
Geo1980.1989<-addGvisATLTitle(Geo1980.1989,"<h1> 1980 - 1989</h1>")
plot(Geo1980.1989)
GTM <- gvisMerge(Geo1970.1979, Geo1980.1989, horizontal=TRUE,
                 tableOptions="cellspacing=10")
plot(GTM)

Geo1990.1999<-addGvisATLTitle(Geo1990.1999,"<h1> 1990 - 1999</h1>")
plot(Geo1990.1999)




fatalaties.by.country.2000.2009<-gti_data %>% filter(iyear 
   %in% c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009)) %>%
  group_by(country_txt) %>%
  summarise(num_of_killed = sum(nkill_clean)) %>% 
  as.data.frame()


Geo2000.2009=gvisGeoChart(fatalaties.by.country.2000.2009, locationvar="country_txt", 
                          colorvar="num_of_killed",
                          options=list(projection="kavrayskiy-vii"))

Geo2000.2009<-addGvisATLTitle(Geo2000.2009,"<h1> 2000 - 2009</h1>")
plot(Geo2000.2009)


GTM2 <- gvisMerge(Geo1990.1999, Geo2000.2009, horizontal=TRUE,
                 tableOptions="cellspacing=10")

plot(GTM2)

Geo2010.2015<-addGvisATLTitle(Geo2010.2015,"<h1> 2010 - 2015</h1>")

GTM2b <- gvisMerge(GTM2, Geo2010.2015, horizontal=TRUE,
                  tableOptions="cellspacing=10")

GTM3 <- gvisMerge(GTM, GTM2b, horizontal=FALSE,
                  tableOptions="cellspacing=10")

plot(GTM3)
