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
        weaptype1_txt,
        nperps_clean,
        nkill_clean,
        targtype1_txt,
        attacktype1_txt,
        suicide, 
        country_txt, 
        region_txt,
        iyear
            )

## str(gti_data_clust)

dummymodel <- dummyVars(" ~ .", data = gti_data_clust)

trsfm.df<-
  predict(dummymodel,newdata=gti_data_clust) %>% as.data.frame()

## head(trsfm.df)
## str(trsfm.df)


## remove special names
colnames(trsfm.df)<-gsub("[[:punct:]]", "", colnames(trsfm.df))
colnames(trsfm.df)<-gsub(" ", "", colnames(trsfm.df))

colnames(trsfm.df)
str(trsfm.df)

trsfm.df %>% select(countrytxtSweden,iyear)

trsfm.df.agg<-trsfm.df %>% group_by(iyear) %>% 
  summarise(
    weaptype1txtBiological_sum =  sum(weaptype1txtBiological),
    weaptype1txtChemical_sum =  sum(weaptype1txtChemical),
    weaptype1txtExplosivesBombsDynamite_sum =  sum(weaptype1txtExplosivesBombsDynamite),
    weaptype1txtFakeWeapons_sum =  sum(weaptype1txtFakeWeapons),
    weaptype1txtFirearms_sum =  sum(weaptype1txtFirearms),
    weaptype1txtIncendiary_sum =  sum(weaptype1txtIncendiary),
    weaptype1txtMelee_sum =  sum(weaptype1txtMelee),
    weaptype1txtOther_sum =  sum(weaptype1txtOther),
    weaptype1txtRadiological_sum =  sum(weaptype1txtRadiological),
    weaptype1txtSabotageEquipment_sum =  sum(weaptype1txtSabotageEquipment),
    weaptype1txtUnknown_sum =  sum(weaptype1txtUnknown),
    weaptype1txtVehiclenottoincludevehicleborneexplosivesiecarortruckbombs_sum =  sum(weaptype1txtVehiclenottoincludevehicleborneexplosivesiecarortruckbombs),
    nperpsclean_sum =  sum(nperpsclean),
    nkillclean_sum =  sum(nkillclean),
    targtype1txtAbortionRelated_sum =  sum(targtype1txtAbortionRelated),
    targtype1txtAirportsAircraft_sum =  sum(targtype1txtAirportsAircraft),
    targtype1txtBusiness_sum =  sum(targtype1txtBusiness),
    targtype1txtEducationalInstitution_sum =  sum(targtype1txtEducationalInstitution),
    targtype1txtFoodorWaterSupply_sum =  sum(targtype1txtFoodorWaterSupply),
    targtype1txtGovernmentDiplomatic_sum =  sum(targtype1txtGovernmentDiplomatic),
    targtype1txtGovernmentGeneral_sum =  sum(targtype1txtGovernmentGeneral),
    targtype1txtJournalistsMedia_sum =  sum(targtype1txtJournalistsMedia),
    targtype1txtMaritime_sum =  sum(targtype1txtMaritime),
    targtype1txtMilitary_sum =  sum(targtype1txtMilitary),
    targtype1txtNGO_sum =  sum(targtype1txtNGO),
    targtype1txtOther_sum =  sum(targtype1txtOther),
    targtype1txtPolice_sum =  sum(targtype1txtPolice),
    targtype1txtPrivateCitizensProperty_sum =  sum(targtype1txtPrivateCitizensProperty),
    targtype1txtReligiousFiguresInstitutions_sum =  sum(targtype1txtReligiousFiguresInstitutions),
    targtype1txtTelecommunication_sum =  sum(targtype1txtTelecommunication),
    targtype1txtTerroristsNonStateMilitia_sum =  sum(targtype1txtTerroristsNonStateMilitia),
    targtype1txtTourists_sum =  sum(targtype1txtTourists),
    targtype1txtTransportation_sum =  sum(targtype1txtTransportation),
    targtype1txtUnknown_sum =  sum(targtype1txtUnknown),
    targtype1txtUtilities_sum =  sum(targtype1txtUtilities),
    targtype1txtViolentPoliticalParty_sum =  sum(targtype1txtViolentPoliticalParty),
    attacktype1txtArmedAssault_sum =  sum(attacktype1txtArmedAssault),
    attacktype1txtAssassination_sum =  sum(attacktype1txtAssassination),
    attacktype1txtBombingExplosion_sum =  sum(attacktype1txtBombingExplosion),
    attacktype1txtFacilityInfrastructureAttack_sum =  sum(attacktype1txtFacilityInfrastructureAttack),
    attacktype1txtHijacking_sum =  sum(attacktype1txtHijacking),
    attacktype1txtHostageTakingBarricadeIncident_sum =  sum(attacktype1txtHostageTakingBarricadeIncident),
    attacktype1txtHostageTakingKidnapping_sum =  sum(attacktype1txtHostageTakingKidnapping),
    attacktype1txtUnarmedAssault_sum =  sum(attacktype1txtUnarmedAssault),
    attacktype1txtUnknown_sum =  sum(attacktype1txtUnknown),
    suicide_sum =  sum(suicide),
    countrytxtAfghanistan_sum =  sum(countrytxtAfghanistan),
    countrytxtAlbania_sum =  sum(countrytxtAlbania),
    countrytxtAlgeria_sum =  sum(countrytxtAlgeria),
    countrytxtAndorra_sum =  sum(countrytxtAndorra),
    countrytxtAngola_sum =  sum(countrytxtAngola),
    countrytxtAntiguaandBarbuda_sum =  sum(countrytxtAntiguaandBarbuda),
    countrytxtArgentina_sum =  sum(countrytxtArgentina),
    countrytxtArmenia_sum =  sum(countrytxtArmenia),
    countrytxtAustralia_sum =  sum(countrytxtAustralia),
    countrytxtAustria_sum =  sum(countrytxtAustria),
    countrytxtAzerbaijan_sum =  sum(countrytxtAzerbaijan),
    countrytxtBahamas_sum =  sum(countrytxtBahamas),
    countrytxtBahrain_sum =  sum(countrytxtBahrain),
    countrytxtBangladesh_sum =  sum(countrytxtBangladesh),
    countrytxtBarbados_sum =  sum(countrytxtBarbados),
    countrytxtBelarus_sum =  sum(countrytxtBelarus),
    countrytxtBelgium_sum =  sum(countrytxtBelgium),
    countrytxtBelize_sum =  sum(countrytxtBelize),
    countrytxtBenin_sum =  sum(countrytxtBenin),
    countrytxtBhutan_sum =  sum(countrytxtBhutan),
    countrytxtBolivia_sum =  sum(countrytxtBolivia),
    countrytxtBosniaHerzegovina_sum =  sum(countrytxtBosniaHerzegovina),
    countrytxtBotswana_sum =  sum(countrytxtBotswana),
    countrytxtBrazil_sum =  sum(countrytxtBrazil),
    countrytxtBrunei_sum =  sum(countrytxtBrunei),
    countrytxtBulgaria_sum =  sum(countrytxtBulgaria),
    countrytxtBurkinaFaso_sum =  sum(countrytxtBurkinaFaso),
    countrytxtBurundi_sum =  sum(countrytxtBurundi),
    countrytxtCambodia_sum =  sum(countrytxtCambodia),
    countrytxtCameroon_sum =  sum(countrytxtCameroon),
    countrytxtCanada_sum =  sum(countrytxtCanada),
    countrytxtCentralAfricanRepublic_sum =  sum(countrytxtCentralAfricanRepublic),
    countrytxtChad_sum =  sum(countrytxtChad),
    countrytxtChile_sum =  sum(countrytxtChile),
    countrytxtChina_sum =  sum(countrytxtChina),
    countrytxtColombia_sum =  sum(countrytxtColombia),
    countrytxtComoros_sum =  sum(countrytxtComoros),
    countrytxtCostaRica_sum =  sum(countrytxtCostaRica),
    countrytxtCroatia_sum =  sum(countrytxtCroatia),
    countrytxtCuba_sum =  sum(countrytxtCuba),
    countrytxtCyprus_sum =  sum(countrytxtCyprus),
    countrytxtCzechRepublic_sum =  sum(countrytxtCzechRepublic),
    countrytxtCzechoslovakia_sum =  sum(countrytxtCzechoslovakia),
    countrytxtDemocraticRepublicoftheCongo_sum =  sum(countrytxtDemocraticRepublicoftheCongo),
    countrytxtDenmark_sum =  sum(countrytxtDenmark),
    countrytxtDjibouti_sum =  sum(countrytxtDjibouti),
    countrytxtDominica_sum =  sum(countrytxtDominica),
    countrytxtDominicanRepublic_sum =  sum(countrytxtDominicanRepublic),
    countrytxtEastGermanyGDR_sum =  sum(countrytxtEastGermanyGDR),
    countrytxtEastTimor_sum =  sum(countrytxtEastTimor),
    countrytxtEcuador_sum =  sum(countrytxtEcuador),
    countrytxtEgypt_sum =  sum(countrytxtEgypt),
    countrytxtElSalvador_sum =  sum(countrytxtElSalvador),
    countrytxtEquatorialGuinea_sum =  sum(countrytxtEquatorialGuinea),
    countrytxtEritrea_sum =  sum(countrytxtEritrea),
    countrytxtEstonia_sum =  sum(countrytxtEstonia),
    countrytxtEthiopia_sum =  sum(countrytxtEthiopia),
    countrytxtFalklandIslands_sum =  sum(countrytxtFalklandIslands),
    countrytxtFiji_sum =  sum(countrytxtFiji),
    countrytxtFinland_sum =  sum(countrytxtFinland),
    countrytxtFrance_sum =  sum(countrytxtFrance),
    countrytxtFrenchGuiana_sum =  sum(countrytxtFrenchGuiana),
    countrytxtFrenchPolynesia_sum =  sum(countrytxtFrenchPolynesia),
    countrytxtGabon_sum =  sum(countrytxtGabon),
    countrytxtGambia_sum =  sum(countrytxtGambia),
    countrytxtGeorgia_sum =  sum(countrytxtGeorgia),
    countrytxtGermany_sum =  sum(countrytxtGermany),
    countrytxtGhana_sum =  sum(countrytxtGhana),
    countrytxtGibraltar_sum =  sum(countrytxtGibraltar),
    countrytxtGreece_sum =  sum(countrytxtGreece),
    countrytxtGrenada_sum =  sum(countrytxtGrenada),
    countrytxtGuadeloupe_sum =  sum(countrytxtGuadeloupe),
    countrytxtGuatemala_sum =  sum(countrytxtGuatemala),
    countrytxtGuinea_sum =  sum(countrytxtGuinea),
    countrytxtGuineaBissau_sum =  sum(countrytxtGuineaBissau),
    countrytxtGuyana_sum =  sum(countrytxtGuyana),
    countrytxtHaiti_sum =  sum(countrytxtHaiti),
    countrytxtHonduras_sum =  sum(countrytxtHonduras),
    countrytxtHongKong_sum =  sum(countrytxtHongKong),
    countrytxtHungary_sum =  sum(countrytxtHungary),
    countrytxtIceland_sum =  sum(countrytxtIceland),
    countrytxtIndia_sum =  sum(countrytxtIndia),
    countrytxtIndonesia_sum =  sum(countrytxtIndonesia),
    countrytxtInternational_sum =  sum(countrytxtInternational),
    countrytxtIran_sum =  sum(countrytxtIran),
    countrytxtIraq_sum =  sum(countrytxtIraq),
    countrytxtIreland_sum =  sum(countrytxtIreland),
    countrytxtIsrael_sum =  sum(countrytxtIsrael),
    countrytxtItaly_sum =  sum(countrytxtItaly),
    countrytxtIvoryCoast_sum =  sum(countrytxtIvoryCoast),
    countrytxtJamaica_sum =  sum(countrytxtJamaica),
    countrytxtJapan_sum =  sum(countrytxtJapan),
    countrytxtJordan_sum =  sum(countrytxtJordan),
    countrytxtKazakhstan_sum =  sum(countrytxtKazakhstan),
    countrytxtKenya_sum =  sum(countrytxtKenya),
    countrytxtKosovo_sum =  sum(countrytxtKosovo),
    countrytxtKuwait_sum =  sum(countrytxtKuwait),
    countrytxtKyrgyzstan_sum =  sum(countrytxtKyrgyzstan),
    countrytxtLaos_sum =  sum(countrytxtLaos),
    countrytxtLatvia_sum =  sum(countrytxtLatvia),
    countrytxtLebanon_sum =  sum(countrytxtLebanon),
    countrytxtLesotho_sum =  sum(countrytxtLesotho),
    countrytxtLiberia_sum =  sum(countrytxtLiberia),
    countrytxtLibya_sum =  sum(countrytxtLibya),
    countrytxtLithuania_sum =  sum(countrytxtLithuania),
    countrytxtLuxembourg_sum =  sum(countrytxtLuxembourg),
    countrytxtMacau_sum =  sum(countrytxtMacau),
    countrytxtMacedonia_sum =  sum(countrytxtMacedonia),
    countrytxtMadagascar_sum =  sum(countrytxtMadagascar),
    countrytxtMalawi_sum =  sum(countrytxtMalawi),
    countrytxtMalaysia_sum =  sum(countrytxtMalaysia),
    countrytxtMaldives_sum =  sum(countrytxtMaldives),
    countrytxtMali_sum =  sum(countrytxtMali),
    countrytxtMalta_sum =  sum(countrytxtMalta),
    countrytxtMartinique_sum =  sum(countrytxtMartinique),
    countrytxtMauritania_sum =  sum(countrytxtMauritania),
    countrytxtMauritius_sum =  sum(countrytxtMauritius),
    countrytxtMexico_sum =  sum(countrytxtMexico),
    countrytxtMoldova_sum =  sum(countrytxtMoldova),
    countrytxtMontenegro_sum =  sum(countrytxtMontenegro),
    countrytxtMorocco_sum =  sum(countrytxtMorocco),
    countrytxtMozambique_sum =  sum(countrytxtMozambique),
    countrytxtMyanmar_sum =  sum(countrytxtMyanmar),
    countrytxtNamibia_sum =  sum(countrytxtNamibia),
    countrytxtNepal_sum =  sum(countrytxtNepal),
    countrytxtNetherlands_sum =  sum(countrytxtNetherlands),
    countrytxtNewCaledonia_sum =  sum(countrytxtNewCaledonia),
    countrytxtNewHebrides_sum =  sum(countrytxtNewHebrides),
    countrytxtNewZealand_sum =  sum(countrytxtNewZealand),
    countrytxtNicaragua_sum =  sum(countrytxtNicaragua),
    countrytxtNiger_sum =  sum(countrytxtNiger),
    countrytxtNigeria_sum =  sum(countrytxtNigeria),
    countrytxtNorthKorea_sum =  sum(countrytxtNorthKorea),
    countrytxtNorthYemen_sum =  sum(countrytxtNorthYemen),
    countrytxtNorway_sum =  sum(countrytxtNorway),
    countrytxtPakistan_sum =  sum(countrytxtPakistan),
    countrytxtPanama_sum =  sum(countrytxtPanama),
    countrytxtPapuaNewGuinea_sum =  sum(countrytxtPapuaNewGuinea),
    countrytxtParaguay_sum =  sum(countrytxtParaguay),
    countrytxtPeoplesRepublicoftheCongo_sum =  sum(countrytxtPeoplesRepublicoftheCongo),
    countrytxtPeru_sum =  sum(countrytxtPeru),
    countrytxtPhilippines_sum =  sum(countrytxtPhilippines),
    countrytxtPoland_sum =  sum(countrytxtPoland),
    countrytxtPortugal_sum =  sum(countrytxtPortugal),
    countrytxtQatar_sum =  sum(countrytxtQatar),
    countrytxtRepublicoftheCongo_sum =  sum(countrytxtRepublicoftheCongo),
    countrytxtRhodesia_sum =  sum(countrytxtRhodesia),
    countrytxtRomania_sum =  sum(countrytxtRomania),
    countrytxtRussia_sum =  sum(countrytxtRussia),
    countrytxtRwanda_sum =  sum(countrytxtRwanda),
    countrytxtSaudiArabia_sum =  sum(countrytxtSaudiArabia),
    countrytxtSenegal_sum =  sum(countrytxtSenegal),
    countrytxtSerbia_sum =  sum(countrytxtSerbia),
    countrytxtSerbiaMontenegro_sum =  sum(countrytxtSerbiaMontenegro),
    countrytxtSeychelles_sum =  sum(countrytxtSeychelles),
    countrytxtSierraLeone_sum =  sum(countrytxtSierraLeone),
    countrytxtSingapore_sum =  sum(countrytxtSingapore),
    countrytxtSlovakRepublic_sum =  sum(countrytxtSlovakRepublic),
    countrytxtSlovenia_sum =  sum(countrytxtSlovenia),
    countrytxtSolomonIslands_sum =  sum(countrytxtSolomonIslands),
    countrytxtSomalia_sum =  sum(countrytxtSomalia),
    countrytxtSouthAfrica_sum =  sum(countrytxtSouthAfrica),
    countrytxtSouthKorea_sum =  sum(countrytxtSouthKorea),
    countrytxtSouthSudan_sum =  sum(countrytxtSouthSudan),
    countrytxtSouthVietnam_sum =  sum(countrytxtSouthVietnam),
    countrytxtSouthYemen_sum =  sum(countrytxtSouthYemen),
    countrytxtSovietUnion_sum =  sum(countrytxtSovietUnion),
    countrytxtSpain_sum =  sum(countrytxtSpain),
    countrytxtSriLanka_sum =  sum(countrytxtSriLanka),
    countrytxtStKittsandNevis_sum =  sum(countrytxtStKittsandNevis),
    countrytxtStLucia_sum =  sum(countrytxtStLucia),
    countrytxtSudan_sum =  sum(countrytxtSudan),
    countrytxtSuriname_sum =  sum(countrytxtSuriname),
    countrytxtSwaziland_sum =  sum(countrytxtSwaziland),
    countrytxtSweden_sum =  sum(countrytxtSweden),
    countrytxtSwitzerland_sum =  sum(countrytxtSwitzerland),
    countrytxtSyria_sum =  sum(countrytxtSyria),
    countrytxtTaiwan_sum =  sum(countrytxtTaiwan),
    countrytxtTajikistan_sum =  sum(countrytxtTajikistan),
    countrytxtTanzania_sum =  sum(countrytxtTanzania),
    countrytxtThailand_sum =  sum(countrytxtThailand),
    countrytxtTogo_sum =  sum(countrytxtTogo),
    countrytxtTrinidadandTobago_sum =  sum(countrytxtTrinidadandTobago),
    countrytxtTunisia_sum =  sum(countrytxtTunisia),
    countrytxtTurkey_sum =  sum(countrytxtTurkey),
    countrytxtTurkmenistan_sum =  sum(countrytxtTurkmenistan),
    countrytxtUganda_sum =  sum(countrytxtUganda),
    countrytxtUkraine_sum =  sum(countrytxtUkraine),
    countrytxtUnitedArabEmirates_sum =  sum(countrytxtUnitedArabEmirates),
    countrytxtUnitedKingdom_sum =  sum(countrytxtUnitedKingdom),
    countrytxtUnitedStates_sum =  sum(countrytxtUnitedStates),
    countrytxtUruguay_sum =  sum(countrytxtUruguay),
    countrytxtUzbekistan_sum =  sum(countrytxtUzbekistan),
    countrytxtVanuatu_sum =  sum(countrytxtVanuatu),
    countrytxtVaticanCity_sum =  sum(countrytxtVaticanCity),
    countrytxtVenezuela_sum =  sum(countrytxtVenezuela),
    countrytxtVietnam_sum =  sum(countrytxtVietnam),
    countrytxtWallisandFutuna_sum =  sum(countrytxtWallisandFutuna),
    countrytxtWestBankandGazaStrip_sum =  sum(countrytxtWestBankandGazaStrip),
    countrytxtWestGermanyFRG_sum =  sum(countrytxtWestGermanyFRG),
    countrytxtWesternSahara_sum =  sum(countrytxtWesternSahara),
    countrytxtYemen_sum =  sum(countrytxtYemen),
    countrytxtYugoslavia_sum =  sum(countrytxtYugoslavia),
    countrytxtZaire_sum =  sum(countrytxtZaire),
    countrytxtZambia_sum =  sum(countrytxtZambia),
    countrytxtZimbabwe_sum =  sum(countrytxtZimbabwe),
    regiontxtAustralasiaOceania_sum =  sum(regiontxtAustralasiaOceania),
    regiontxtCentralAmericaCaribbean_sum =  sum(regiontxtCentralAmericaCaribbean),
    regiontxtCentralAsia_sum =  sum(regiontxtCentralAsia),
    regiontxtEastAsia_sum =  sum(regiontxtEastAsia),
    regiontxtEasternEurope_sum =  sum(regiontxtEasternEurope),
    regiontxtMiddleEastNorthAfrica_sum =  sum(regiontxtMiddleEastNorthAfrica),
    regiontxtNorthAmerica_sum =  sum(regiontxtNorthAmerica),
    regiontxtSouthAmerica_sum =  sum(regiontxtSouthAmerica),
    regiontxtSouthAsia_sum =  sum(regiontxtSouthAsia),
    regiontxtSoutheastAsia_sum =  sum(regiontxtSoutheastAsia),
    regiontxtSubSaharanAfrica_sum =  sum(regiontxtSubSaharanAfrica),
    regiontxtWesternEurope_sum =  sum(regiontxtWesternEurope)
  ) %>% as.data.frame()

## str(trsfm.df.agg)

rownames(trsfm.df.agg)<-trsfm.df.agg$iyear

trsfm.df.agg$iyear<-NULL

preprocmodel<-caret::preProcess(trsfm.df.agg,method=c("scale"))

transformed.trsfm.df.agg <- predict(preprocmodel, trsfm.df.agg) %>% as.data.frame()

## str(transformed.trsfm.df.agg)
## head(transformed.trsfm.df.agg)

d <- dist(as.matrix(transformed.trsfm.df.agg))

hcd<-hclust(d)
plot(hcd)
??plot_heatmap
?heatmap
heatmap(hcd)

install.packages("gplots")
library(gplots)

x<-as.matrix(transformed.trsfm.df.agg)

heatmap.2(x)


install.packages("heatmaply")
library(heatmaply)

heatmaply(transformed.trsfm.df.agg)

gti_data %>% filter(country_txt=="Sweden")
gti_data %>% filter(country_txt=="Finland")
gti_data %>% filter(country_txt=="Uzbekistan")
gti_data %>% filter(country_txt=="Ireland")

transformed.trsfm.df.agg %>% select(countrytxtIreland_sum)
transformed.trsfm.df.agg

trsfm.df.agg %>% select(countrytxtIreland_sum)