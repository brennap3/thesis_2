install.packages('timeline', repos='http://cran.r-project.org')
library(rvest)
require(timeline)
?timeline
?ww2
data(ww2)
data(ww2.events)

str(ww2)
str(ww2.events)
# 0. "20/03/2001"Invasion start 
# 1. "01/05/2003", Period=Pre Invasion (Invasion ends) 
# 2. "01/05/2003" to "01/01/2007", Period =Post_Invasion
# 3. "01/01/2007" to "31/12/2007", Period =Surge
# 4. "01/01/2008" to "31/12/2009", Period =Post Surge
# 5. "01/01/2008" to "31/12/2009", Period =Drawdown
# 6. "01/01/2010" to "31/12/2012", Period =Pullout
# 7.  after "31/12/2012", Period = PostPullout
# 
# 8. Fall of Mosul 10/06/2014 http://www.thearabweekly.com/Special-Focus/1167/How-prison-breaks-helped-ISIS
# 9. Fall of Ramadi 17/05/2014 http://www.thearabweekly.com/Special-Focus/1167/How-prison-breaks-helped-ISIS
#10. Beginning of arab spring in in Syria

?read.csv
setwd("C:\\Users\\Peter\\Desktop")
iraqpresidents<-read.csv("iraqpresidents.csv")
iraqpresidents2<-read.csv("iraqpresidents.csv")

str(iraqpresidents2)

iraqpresidents2$Person<-as.character(iraqpresidents2$Person)
iraqpresidents2$Group<-as.character(iraqpresidents2$Group)
iraqpresidents2$StartDate<-lubridate::dmy(iraqpresidents2$StartDate)
iraqpresidents2$EndDate<-lubridate::dmy(iraqpresidents2$EndDate)

str(iraqpresidents)

iraqpresidents$Person<-as.character(iraqpresidents$Person)
iraqpresidents$Group<-as.character(iraqpresidents$Group)
iraqpresidents$StartDate<-lubridate::dmy(iraqpresidents$StartDate)
iraqpresidents$EndDate<-lubridate::dmy(iraqpresidents$EndDate)
str(iraqpresidents)
iraqevents<-read.csv("iraqevents.csv")
iraqevents$Date<-lubridate::dmy(iraqevents$Date)

data(ww2)
str(ww2)
str(ww2.events)
data(ww2.events)
?timeline

timeline(iraqpresidents2,iraqevents,event.spots=1, event.label='',
         event.above=FALSE,text.size = 2,event.label.method=2,event.text.size=3,event.line = T)


