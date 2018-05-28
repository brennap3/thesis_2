##will make r markdown

rm(list=ls())

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
library(readr)
library(stringr)
library(stringi)
library(readxl)
require(RSelenium)
library(rvest)


gti_data<-read_csv("C:\\Users\\Peter\\Downloads\\GTD\\globalterrorismdb_0617dist.csv")

colnames(gti_data)



gti_data$idate<-ymd(paste(gti_data$iyear,str_pad(gti_data$imonth,2,pad="0"),
                           str_pad(gti_data$iday,2,pad="0")))

str(gti_data$iyear)

unique(gti_data$iyear)


gti_data %>% dplyr::filter(country_txt %in% c('Iraq','Syria')) %>%
  dplyr::filter(iyear==2016) %>% ungroup() -> gti_data_2016

## unique(gti_data_2016$country_txt)
## nrow(gti_data_2016)

write_csv(gti_data_2016,"C:\\Users\\Peter\\Downloads\\GTD\\globalterrorismdb_2016_iraq_syria.csv")

library(rvest)

## scrape aidworkersecurity.org

aid_workers_security <- read_html("https://aidworkersecurity.org/incidents/search")

aid_workers_security %>% 
  html_nodes("#incidents-table") %>%
    html_table() %>% as.data.frame() -> aid_workers_security_df


colnames(aid_workers_security_df)

# [1] "ID"                       
# [2] "Month"                    
# [3] "Day"                      
# [4] "Year"                     
# [5] "Country"                  
# [6] "UN"                       
# [7] "INGO"                     
# [8] "LNGO.and.NRCS"            
# [9] "ICRC"                     
# [10] "IFRC"                     
# [11] "Other"                    
# [12] "Nationals.killed"         
# [13] "Nationals.wounded"        
# [14] "Nationals.kidnapped"      
# [15] "Total.national.staff"     
# [16] "Internationals.killed"    
# [17] "Internationals.wounded"   
# [18] "Internationals.kidnapped" 
# [19] "Total.international.staff"
# [20] "Total.victims"            
# [21] "Gender.Male"              
# [22] "Gender.Female"            
# [23] "Gender.Unknown"           
# [24] "Means.of.attack"          
# [25] "Attack.context"           
# [26] "Location"                 
# [27] "Latitude"                 
# [28] "Longitude"                
# [29] "Details"  
# Key for means of attack
# AB: Aerial bombardment/missile/mortar/RPG/lobbed grenade
# BA: Bodily assault/beating/stabbing with non-fire weapons or no weapons
# B: Bombing (set explosives with a stationary target: building, facility, home)
# BBIED: Body-borne IED
# CX: Complex attack (explosives in conjunction with small arms)
# RIED: Roadside IED
# VBIED: Vehicle-born IED (unknown whether remote control or suicide)
# VBIED-RC: Vehicle-borne IED (remote control detonation)
# VBIED-S : Vehicle-borne IED (suicide)
# K: Kidnapping (not killed)
# KK: Kidnap-killing
# RSA: Rape or serious sexual assault
# LM: Landmine or UXO detonation
# S: Shooting (small arms / light weapons, e.g. pistols, rifles, machine guns)
# U: Unknown
# Key for attack context
# Am: Ambush/attack on road
# C: Combat (or police operations) / Crossfire
# IA: Individual attack or assassination
# MV: Mob violence, rioting
# R: Raid (armed incursion by group on home, office, or project site)
# D: Detention (by official government forces or police, where abuse takes place)
# U: Unknown
# Key for location
# H: Home (private home, not compound)
# OC: Office or organization compound/residence
# PS: Project site (village, camp, distribution point, hospital, etc.)
# P: Other public location (street, market, restaurant, etc.)
# R: Road (in transit)
# C: Custody (official forces/police)
# U: Unknown

aid_workers_security_df %>% dplyr::filter(Country %in% c('Iraq','Syrian Arab Republic')) %>%
  dplyr::filter(Year==2016) %>% dplyr::ungroup() -> security_2016

nrow(security_2016)

write_csv(security_2016,"C:\\Users\\Peter\\Downloads\\GTD\\kidnapsecurity_2016_iraq_syria.csv")

events<-readxl::read_xlsx(path="C:\\Users\\Peter\\Downloads\\GTD\\Urban-Social-Disorder\\events.xlsx",sheet="events")

colnames(events)
## str(events)

unique(events$BYEAR) %>% sort()

iraq_syria_social_disorder <- events %>% dplyr::filter(COUNTRY %in% c('Iraq','Syria')) 
                        & BYEAR ==2016) %>% ungroup()
  

# nrow(iraq_syria_social_disorder)

##nothing in 2016 only appears to go upto 2014

# UCDP


ged171 <- readxl::read_xlsx(path="C:\\Users\\Peter\\Downloads\\GTD\\UCDP Georeferenced Event Dataset\\ged171.xlsx",sheet="ged171")

# colnames(ged171)

# [1] "id"                "year"             
# [3] "active_year"       "type_of_violence" 
# [5] "conflict_new_id"   "conflict_name"    
# [7] "dyad_new_id"       "dyad_name"        
# [9] "side_a_new_id"     "gwnoa"            
# [11] "side_a"            "side_b_new_id"    
# [13] "gwnob"             "side_b"           
# [15] "number_of_sources" "source_article"   
# [17] "source_office"     "source_date"      
# [19] "source_headline"   "source_original"  
# [21] "where_prec"        "where_coordinates"
# [23] "adm_1"             "adm_2"            
# [25] "latitude"          "longitude"        
# [27] "geom_wkt"          "priogrid_gid"     
# [29] "country"           "country_id"       
# [31] "region"            "event_clarity"    
# [33] "date_prec"         "date_start"       
# [35] "date_end"          "deaths_a"         
# [37] "deaths_b"          "deaths_civilians" 
# [39] "deaths_unknown"    "best"             
# [41] "low"               "high"  

## year country

str(ged171)

# unique(ged171$year)

# unique(ged171$country)

# str(ged171$country)

ged171 %>% dplyr::filter(country %in% c('Iraq','Syria') & year== 2016 ) %>% dplyr::select(year)%>% unique() -> ged_iraq_years



ged171 %>% dplyr::filter(country %in% c('Iraq','Syria') & year==2016 ) %>% ungroup() -> iraq_syria_armed_conflict_2016  


write.csv(iraq_syria_armed_conflict_2016,"C:\\Users\\Peter\\Downloads\\GTD\\iraq_syria_armed_conflict_2016.csv")



# nrow(iraq_syria_armed_conflict_2016)



## murder of journalists
## cpj
## a bit on pipes
##https://github.com/ropensci/RSelenium/issues/14
##https://www.rdocumentation.org/packages/seleniumPipes/versions/0.3.7/topics/findElements
##http://rpubs.com/johndharrison/RSelenium-Basics
##set up of selenium
##use docker instance

# docker run -d -p 4444:4444 -v /dev/shm:/dev/shm selenium/standalone-firefox

remDr <- remoteDriver(remoteServerAddr = "192.168.99.100", port = 4444L)
##remDr$close
Sys.sleep(5) # give the binary a moment
remDr$open()
##remDr$close()
##remDr$close()
##errorDetails()
remDr$navigate("https://cpj.org/data/killed/?status=Killed&motiveConfirmed%5B%5D=Confirmed&type%5B%5D=Journalist&start_year=1992&end_year=2018&group_by=year")
remDr$errorDetails()

journalist_deaths <- xml2::read_html(remDr$getPageSource()[[1]])

# body > div.tab-content.report-builder-tab-content.pt-5 > div > div > div.container > div > div.col-sm-8 > div
# body > div.tab-content.report-builder-tab-content.pt-5 > div > div > div.container > div > div.col-sm-8 > div


htmlnode<-".js-report-builder-results"

x <- journalist_deaths %>%    html_nodes(htmlnode) 
print(x)

xta<-x%>% html_node("table") %>%    html_table()

## install.packages("seleniumPipes")

library(seleniumPipes)
webElem <- remDr$findElements(using = "class", value = "page-link") 

webElem[[8]]$getElementText()


webElem[[8]]$clickElement()
webElem$getElementText()
remDr$getCurrentUrl()

terrorist_deaths2 <- xml2::read_html(remDr$getPageSource()[[1]])

x2 <- terrorist_deaths2 %>%    html_nodes(htmlnode)
xta
head(xta)

xta <- xta %>% as.data.frame()

str(xta)
str(xta2)
xta2<-x2%>% html_node("table") %>%    html_table() %>% as.data.frame()
head(xta2)
nrow(xta2)
nrow(xta)
str(xta)
xta<-rbind(xta,xta2)
## use loop to add to data.frame() do while exists

for(i in 1:100){
  webElem <- remDr$findElements(using = "class", value = "page-link") 
  
  print(webElem[[8]]$getElementText())
  
  webElem[[8]]$clickElement()
  ##webElem$getElementText()
  remDr$getCurrentUrl()
  
  
  terrorist_deaths2 <- xml2::read_html(remDr$getPageSource()[[1]])
  
  x2 <- terrorist_deaths2 %>%    html_nodes(htmlnode)
  
  xta2<-x2%>% html_node("table") %>%    html_table() %>% as.data.frame()
  ##
  print(colnames(xta))
  xta<-rbind(xta,xta2)
}

xta <- xta %>% unique()

##actually change this in for loop to break if nrow does not equal unique nrow
## cats as unique values and break; for loop

nrow(xta)

xta %>% unique() %>% nrow()

##Jeez this is grim but intersting!

colnames(xta)

# [1] "Name"          "Organization"  "Date"         
# [4] "Location"      "Attack"        "Type.of.Death"
# [7] "Charge

xta$Location
unique(xta$Attack)
unique(xta$Charge)
unique(xta$Type.of.Death)

##lets geocode the locations
##

library(ggmap)


geocoded <- data.frame(stringsAsFactors = FALSE)




# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon

##

xta$lon<-NA
xta$lat<-NA

geocode(xta$Location[1], output = "latlona", source = "google")

for(i in 1:nrow(xta))
{
  # Print("Working...")
  result <- geocode(xta$Location[i], output = "latlona", source = "google")
  xta$lon[i] <- as.numeric(result[1])
  xta$lat[i] <- as.numeric(result[2])
}
# Write a CSV file containing origAddress to the working directory

head(xta)

##https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv

Countries_Regions<-read_csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")

colnames(Countries_Regions)

colnames(xta)

xta<-dplyr::left_join(xta,Countries_Regions,by=c("Location"="name"))

## now we have regions and subregions
# first write it out filtered for iraq and Syria for 2016
# colnames(xta)

str(xta$Date)

xta$DT<-strptime(xta$Date, format = "%B%d,%Y")

xta$DT <- xta$DT %>% as.POSIXct.POSIXlt()


xta %>% dplyr::filter(Location %in% c("Syria","Iraq")
                      & DT >"2015-12-31" & DT < "2017-01-01") -> Journalist_murders_Iraq_Syria

nrow(Journalist_murders_Iraq_Syria)


#write.csv(ged171,"C:\\Users\\Peter\\Downloads\\GTD\\ged171.csv")

write.csv(Journalist_murders_Iraq_Syria,'C:\\Users\\Peter\\Downloads\\GTD\\Journalist_murders_Iraq_Syria_2016.csv')


## for the mlist lets create a dataset with
## a count of  deaths by Type.of.Death

xta %>% dplyr::filter(Location %in% c("Syria","Iraq")
                      & DT >"2015-12-31" & DT < "2017-01-01") %>%
   select (Location,
              Type.of.Death,lat,lon) %>% group_by(Location,
                                          Type.of.Death,lat,lon)  %>% summarize(
                count_journalists_killed=n()
              )->Iraq_Syria_CDJ_2016

write.csv(Iraq_Syria_CDJ_2016,"C:\\Users\\Peter\\Downloads\\GTD\\Iraq_Syria_CDJ_2016.csv")

write_json(Iraq_Syria_CDJ_2016,"C:\\Users\\Peter\\Downloads\\GTD\\Iraq_Syria_CDJ_2016.json")

