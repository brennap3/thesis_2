library(caret)  

## clustering the GTD

str(gti_data)
unique(gti_data$nkill)

## weaptype1_txt
## nperps
## targtype
## attacktype1_txt
## suicide unique(gti_data$suicide)
## country_txt 
## region_txt
## iyear

## hierarchical clustering of gti_data
## clean nperps
## subset the data
## one hot encode the categorical variable
## convert year to the factor
## afterencoding aggregate by year
## then scale
## then create a distance matrix
## then plot

#str(gti_data$nperps)
#str(gti_data$nkill)

gti_data$nperps_clean<-ifelse(gti_data$nperps==-99,NA,gti_data$nperps)
gti_data$nperps_clean<-ifelse(is.na(gti_data$nperps_clean),0,gti_data$nperps_clean)

## subset the data

gti_data_clust <- gti_data %>%
      select(
        country_txt, 
        iyear
            )
?xtabs


## str(gti_data_clust)

##check
nrow(gti_data_clust2 %>% filter(country_txt=="Ireland"))
nrow(gti_data %>% filter(country_txt=="Ireland"))
nrow(gti_data %>% filter(country_txt=="Ireland" & iyear==2015) )
nrow(gti_data %>% filter(country_txt=="Ireland" & iyear==2014) )
nrow(gti_data %>% filter(country_txt=="Israel" & iyear==2013) )
nrow(gti_data %>% filter(country_txt=="Israel" & iyear==2014) )
nrow(gti_data %>% filter(country_txt=="Sweden"))
nrow(gti_data %>% filter(country_txt=="Finland" & iyear==2015) )


dummymodel <- dummyVars(" ~ .", data = gti_data_clust)

trsfm.df<-
  predict(dummymodel,newdata=gti_data_clust) %>% as.data.frame()

## head(trsfm.df)
## str(trsfm.df)


## remove special names
colnames(trsfm.df)<-gsub("[[:punct:]]", "", colnames(trsfm.df))
colnames(trsfm.df)<-gsub(" ", "", colnames(trsfm.df))
colnames(trsfm.df)<-gsub("countrytxt", "", colnames(trsfm.df))
colnames(trsfm.df)
str(trsfm.df)


trsfm.df.agg<-trsfm.df %>% group_by(iyear) %>% 
  summarise(
    Afghanistan =  sum(Afghanistan),
    Albania =  sum(Albania),
    Algeria =  sum(Algeria),
    Andorra =  sum(Andorra),
    Angola =  sum(Angola),
    AntiguaandBarbuda =  sum(AntiguaandBarbuda),
    Argentina =  sum(Argentina),
    Armenia =  sum(Armenia),
    Australia =  sum(Australia),
    Austria =  sum(Austria),
    Azerbaijan =  sum(Azerbaijan),
    Bahamas =  sum(Bahamas),
    Bahrain =  sum(Bahrain),
    Bangladesh =  sum(Bangladesh),
    Barbados =  sum(Barbados),
    Belarus =  sum(Belarus),
    Belgium =  sum(Belgium),
    Belize =  sum(Belize),
    Benin =  sum(Benin),
    Bhutan =  sum(Bhutan),
    Bolivia =  sum(Bolivia),
    BosniaHerzegovina =  sum(BosniaHerzegovina),
    Botswana =  sum(Botswana),
    Brazil =  sum(Brazil),
    Brunei =  sum(Brunei),
    Bulgaria =  sum(Bulgaria),
    BurkinaFaso =  sum(BurkinaFaso),
    Burundi =  sum(Burundi),
    Cambodia =  sum(Cambodia),
    Cameroon =  sum(Cameroon),
    Canada =  sum(Canada),
    CentralAfricanRepublic =  sum(CentralAfricanRepublic),
    Chad =  sum(Chad),
    Chile =  sum(Chile),
    China =  sum(China),
    Colombia =  sum(Colombia),
    Comoros =  sum(Comoros),
    CostaRica =  sum(CostaRica),
    Croatia =  sum(Croatia),
    Cuba =  sum(Cuba),
    Cyprus =  sum(Cyprus),
    CzechRepublic =  sum(CzechRepublic),
    Czechoslovakia =  sum(Czechoslovakia),
    DemocraticRepublicoftheCongo =  sum(DemocraticRepublicoftheCongo),
    Denmark =  sum(Denmark),
    Djibouti =  sum(Djibouti),
    Dominica =  sum(Dominica),
    DominicanRepublic =  sum(DominicanRepublic),
    EastGermanyGDR =  sum(EastGermanyGDR),
    EastTimor =  sum(EastTimor),
    Ecuador =  sum(Ecuador),
    Egypt =  sum(Egypt),
    ElSalvador =  sum(ElSalvador),
    EquatorialGuinea =  sum(EquatorialGuinea),
    Eritrea =  sum(Eritrea),
    Estonia =  sum(Estonia),
    Ethiopia =  sum(Ethiopia),
    FalklandIslands =  sum(FalklandIslands),
    Fiji =  sum(Fiji),
    Finland =  sum(Finland),
    France =  sum(France),
    FrenchGuiana =  sum(FrenchGuiana),
    FrenchPolynesia =  sum(FrenchPolynesia),
    Gabon =  sum(Gabon),
    Gambia =  sum(Gambia),
    Georgia =  sum(Georgia),
    Germany =  sum(Germany),
    Ghana =  sum(Ghana),
    Gibraltar =  sum(Gibraltar),
    Greece =  sum(Greece),
    Grenada =  sum(Grenada),
    Guadeloupe =  sum(Guadeloupe),
    Guatemala =  sum(Guatemala),
    Guinea =  sum(Guinea),
    GuineaBissau =  sum(GuineaBissau),
    Guyana =  sum(Guyana),
    Haiti =  sum(Haiti),
    Honduras =  sum(Honduras),
    HongKong =  sum(HongKong),
    Hungary =  sum(Hungary),
    Iceland =  sum(Iceland),
    India =  sum(India),
    Indonesia =  sum(Indonesia),
    International =  sum(International),
    Iran =  sum(Iran),
    Iraq =  sum(Iraq),
    Ireland =  sum(Ireland),
    Israel =  sum(Israel),
    Italy =  sum(Italy),
    IvoryCoast =  sum(IvoryCoast),
    Jamaica =  sum(Jamaica),
    Japan =  sum(Japan),
    Jordan =  sum(Jordan),
    Kazakhstan =  sum(Kazakhstan),
    Kenya =  sum(Kenya),
    Kosovo =  sum(Kosovo),
    Kuwait =  sum(Kuwait),
    Kyrgyzstan =  sum(Kyrgyzstan),
    Laos =  sum(Laos),
    Latvia =  sum(Latvia),
    Lebanon =  sum(Lebanon),
    Lesotho =  sum(Lesotho),
    Liberia =  sum(Liberia),
    Libya =  sum(Libya),
    Lithuania =  sum(Lithuania),
    Luxembourg =  sum(Luxembourg),
    Macau =  sum(Macau),
    Macedonia =  sum(Macedonia),
    Madagascar =  sum(Madagascar),
    Malawi =  sum(Malawi),
    Malaysia =  sum(Malaysia),
    Maldives =  sum(Maldives),
    Mali =  sum(Mali),
    Malta =  sum(Malta),
    Martinique =  sum(Martinique),
    Mauritania =  sum(Mauritania),
    Mauritius =  sum(Mauritius),
    Mexico =  sum(Mexico),
    Moldova =  sum(Moldova),
    Montenegro =  sum(Montenegro),
    Morocco =  sum(Morocco),
    Mozambique =  sum(Mozambique),
    Myanmar =  sum(Myanmar),
    Namibia =  sum(Namibia),
    Nepal =  sum(Nepal),
    Netherlands =  sum(Netherlands),
    NewCaledonia =  sum(NewCaledonia),
    NewHebrides =  sum(NewHebrides),
    NewZealand =  sum(NewZealand),
    Nicaragua =  sum(Nicaragua),
    Niger =  sum(Niger),
    Nigeria =  sum(Nigeria),
    NorthKorea =  sum(NorthKorea),
    NorthYemen =  sum(NorthYemen),
    Norway =  sum(Norway),
    Pakistan =  sum(Pakistan),
    Panama =  sum(Panama),
    PapuaNewGuinea =  sum(PapuaNewGuinea),
    Paraguay =  sum(Paraguay),
    PeoplesRepublicoftheCongo =  sum(PeoplesRepublicoftheCongo),
    Peru =  sum(Peru),
    Philippines =  sum(Philippines),
    Poland =  sum(Poland),
    Portugal =  sum(Portugal),
    Qatar =  sum(Qatar),
    RepublicoftheCongo =  sum(RepublicoftheCongo),
    Rhodesia =  sum(Rhodesia),
    Romania =  sum(Romania),
    Russia =  sum(Russia),
    Rwanda =  sum(Rwanda),
    SaudiArabia =  sum(SaudiArabia),
    Senegal =  sum(Senegal),
    Serbia =  sum(Serbia),
    SerbiaMontenegro =  sum(SerbiaMontenegro),
    Seychelles =  sum(Seychelles),
    SierraLeone =  sum(SierraLeone),
    Singapore =  sum(Singapore),
    SlovakRepublic =  sum(SlovakRepublic),
    Slovenia =  sum(Slovenia),
    SolomonIslands =  sum(SolomonIslands),
    Somalia =  sum(Somalia),
    SouthAfrica =  sum(SouthAfrica),
    SouthKorea =  sum(SouthKorea),
    SouthSudan =  sum(SouthSudan),
    SouthVietnam =  sum(SouthVietnam),
    SouthYemen =  sum(SouthYemen),
    SovietUnion =  sum(SovietUnion),
    Spain =  sum(Spain),
    SriLanka =  sum(SriLanka),
    StKittsandNevis =  sum(StKittsandNevis),
    StLucia =  sum(StLucia),
    Sudan =  sum(Sudan),
    Suriname =  sum(Suriname),
    Swaziland =  sum(Swaziland),
    Sweden =  sum(Sweden),
    Switzerland =  sum(Switzerland),
    Syria =  sum(Syria),
    Taiwan =  sum(Taiwan),
    Tajikistan =  sum(Tajikistan),
    Tanzania =  sum(Tanzania),
    Thailand =  sum(Thailand),
    Togo =  sum(Togo),
    TrinidadandTobago =  sum(TrinidadandTobago),
    Tunisia =  sum(Tunisia),
    Turkey =  sum(Turkey),
    Turkmenistan =  sum(Turkmenistan),
    Uganda =  sum(Uganda),
    Ukraine =  sum(Ukraine),
    UnitedArabEmirates =  sum(UnitedArabEmirates),
    UnitedKingdom =  sum(UnitedKingdom),
    UnitedStates =  sum(UnitedStates),
    Uruguay =  sum(Uruguay),
    Uzbekistan =  sum(Uzbekistan),
    Vanuatu =  sum(Vanuatu),
    VaticanCity =  sum(VaticanCity),
    Venezuela =  sum(Venezuela),
    Vietnam =  sum(Vietnam),
    WallisandFutuna =  sum(WallisandFutuna),
    WestBankandGazaStrip =  sum(WestBankandGazaStrip),
    WestGermanyFRG =  sum(WestGermanyFRG),
    WesternSahara =  sum(WesternSahara),
    Yemen =  sum(Yemen),
    Yugoslavia =  sum(Yugoslavia),
    Zaire =  sum(Zaire),
    Zambia =  sum(Zambia),
    Zimbabwe =  sum(Zimbabwe)
  ) %>% as.data.frame()

## str(trsfm.df.agg)

rownames(trsfm.df.agg)<-trsfm.df.agg$iyear

trsfm.df.agg$iyear<-NULL

preprocmodel<-caret::preProcess(trsfm.df.agg,method=c("scale"))

transformed.trsfm.df.agg <- predict(preprocmodel, trsfm.df.agg) %>% as.data.frame()

## str(transformed.trsfm.df.agg)
## head(transformed.trsfm.df.agg)

d <- dist(as.matrix(transformed.trsfm.df.agg))
##use gower distance instead
##d<- 

hcd<-hclust(d)
plot(hcd)

# str(xtabcountryyear) head(xtabcountryyear)
##now view by country
xtabcountryyear<-table(gti_data_clust$country_txt,gti_data_clust$iyear) %>% as.matrix

preprocmodelcntry<-caret::preProcess(xtabcountryyear,method=c("scale","center"))

transformed.xtabcountryyear <- predict(preprocmodelcntry, xtabcountryyear)

d2 <- dist(transformed.xtabcountryyear)
hcd2<-hclust(d)
plot(hcd2)
install.packages("ggdendro")
library(ggdendro)
gdendro<-ggdendrogram(hcd2, rotate = FALSE, size = 0.3)
plot(gdendro)
install.packages("dendextend")
library(dendextend)
?as.dendrogram
ggd1 <- as.dendrogram(hcd2,leadlab="textlike")
?as.ggdend
ggd1 <- as.ggdend(ggd1)
ptest <- ggplot(ggd1, horiz = F, theme = NULL) 
plot(ptest)
plotly_build(ptest)

ggplotly(gdendro)

plot(hcd2)
install.packages("gplots")
library(gplots)

x<-as.matrix(transformed.trsfm.df.agg)

heatmap.2(x)


install.packages("heatmaply")
library(heatmaply)

heatmaply(trsfm.df.agg)

gti_data %>% filter(country_txt=="Sweden")
gti_data %>% filter(country_txt=="Finland")
gti_data %>% filter(country_txt=="Uzbekistan")
gti_data %>% filter(country_txt=="Ireland")

transformed.trsfm.df.agg %>% select(countrytxtIreland_sum)
transformed.trsfm.df.agg

trsfm.df.agg %>% select(countrytxtIreland_sum)