# WARNING 

rm(list=ls()) 

setwd("C:\\Dropbox\\Insect Biomass Trends/csvs") # work


load("completeData.Rdata")
load("completeDataArth.RData")
load("completeDataAB.RData")
load("E:/inlaF.RData")
load("metadata_per_dataset.RData")
completeDataArth$Stratum[completeDataArth$Plot_ID == 930 ]<- "Soil surface"

col.scheme.cont<-c( "Europe"="green3", "Latin America"= "magenta", "North America"= "orange","Asia" = "purple3", 
                    "Africa" = "blue", "Australia" = "red")
col.scheme.realm<-c(  "Freshwater"  = "dodgerblue2", "Terrestrial" = "peru")
col.scheme.strat<-c( "Air" = "peru", "Herb layer" = "peru", "Soil surface" = "peru", "Trees" = "peru", 
                     "Underground" = "peru"  ,"Water" = "dodgerblue2")
col.scheme.pa<- c( "Protected area" = "white", "Not protected area" = "red")


load("RandEfDataset.RData")
sum( RandEfDataset$'DataID_Slope_ 0.025quant' >0 & RandEfDataset$'DataID_Slope_ 0.975quant' >0 ) # 10 sign positive datasets
sum( RandEfDataset$'DataID_Slope_ 0.025quant' <0 & RandEfDataset$'DataID_Slope_ 0.975quant' <0 ) # 15 sign negative datasets
sum( RandEfDataset$'DataID_Slope_ 0.025quant' <0 & RandEfDataset$'DataID_Slope_ 0.975quant' >0 ) # 130 sign positive datasets



## make Data S1 #####

dataS1<- subset(completeData, Datasource_ID != 1514 & Datasource_ID != 1404 & 
                  Datasource_ID != 1339& Datasource_ID !=1416 & Datasource_ID != 1474 & Datasource_ID != 1475)

dataS1<- dataS1[, c("Plot_ID",   "Datasource_ID",  "Datasource_name" ,
                  "Year",      "Period",         "Unit" ,           "Number", 
                  "Realm",     "Stratum"  ,      "biome",           "BiomeCoarse",   
                  "Continent", "Region",         "Country",         "Country_State",  "Location", 
                  "Duration",  "Start_year",     "End_year",                                
                  "cYear",     "iYear",          "rYear",           "rYear2",         "Period_4INLA",        
                  "Plot_ID_4INLA",        "Datasource_ID_4INLA",   "Location_4INLA",      
                  "Plot_ID_4INLAR",       "Datasource_ID_4INLAR",  "Location_4INLAR",     
                  "cStartYear",           "cDuration",             "cEndYear",                    
                  "PA",                   "Start_forestArea",      "End_forestArea",       "Start_cropArea",
                  "End_cropArea",         "Start_urbanArea",       "End_urbanArea",        "urbanization",         
                  "cropification",        "frcCrop900m1992",       "frcUrban900m1992",     "CHELSAmnC",            
                  "mnC",                  "mnK",                   "deltaTmean",           "relDeltaTmean",         
                  "mnP",                  "deltaPrec",
                  "relDeltaPrec",         "CHELSAmnK",            "CHELSAdeltaTmean",      "CHELSArelDeltaTmean",  
                  "CHELSAmnP",            "CHELSAdeltaPrec",      "CHELSArelDeltaPrec"     )  ]
names(dataS1)
saveRDS(dataS1, file = "dataS1.rds" )

dataS2<- subset(completeDataAB, Datasource_ID != 1514 & Datasource_ID != 1404 & 
                  Datasource_ID != 1339& Datasource_ID !=1416 & Datasource_ID != 1474 & Datasource_ID != 1475)
dataS2<- dataS2[, c("Plot_ID"         ,     "Year"                , "Datasource_ID"     ,   "Period"          ,    
                    "Number"          ,    "Location"             , "Datasource_name"   ,   "Realm"           ,    
                    "Unit"            ,     "Continent"           , "Country"           ,   "Country_State"   ,    
                    "Region"          ,     "Stratum"             , "cYear"           ,    
                    "iYear"           ,     "rYear"               , "rYear2"            ,   "Period_4INLA"    ,   
                    "Plot_ID_4INLA"   ,     "Datasource_ID_4INLA" , "Location_4INLA"    ,  
                    "Plot_ID_4INLAR"  ,     "Datasource_ID_4INLAR", "Location_4INLAR"   ,   
                    "DSunit_4INLA"    ,     "Locunit_4INLA"       , "Plotunit_4INLA"    ,   "Plotunit_4INLAR" ,    
                    "DSunit_4INLAR"   ,     "Locunit_4INLAR"       )  ]



saveRDS(dataS2, file = "dataS2.rds")

# robustness checks

# effects of the priors



# PC prior #####
sdres <- sd(log10(completeData$Number+1),na.rm=T)

pcpriorAR1_0.6 <- list(theta1 = list(prior="pc.prec", param = c(u=3*sdres,0.01)),
                       theta2 = list(prior="pc.cor1", param=c(0.6, 0.75)))#75% likely that the autocorrelation is bigger than 0.6
pcpriorAR1_0.5 <- list(theta1 = list(prior="pc.prec", param = c(u=3*sdres,0.01)),
                       theta2 = list(prior="pc.cor1", param=c(0.5, 0.75)))#75% likely that the autocorrelation is bigger than 0.5
pcpriorAR1_0.86 <- list(theta1 = list(prior="pc.prec", param = c(u=3*sdres,0.01)),
                        theta2 = list(prior="pc.cor1", param=c(u=sqrt(3)/2, 3/4)))
pcpriorAR0_0.5 <- list(theta1 = list(prior="pc.prec", param = c(3*sdres,0.01)),
                       theta2 = list(prior="pc.cor0", param=c(0.5, 0.75)))#
#PC prior on random effects
pcpriorIID <- list(prec = list(prior="pc.prec", param = c(3*sdres,0.01)))
#prior prior on family
pcpriorFamily <- list(prec = list(prior="pc.prec", param = c(3*sdres,0.01)))



formul<-as.formula(paste("" ))

model <- inla( log10(Number+1) ~ cYear: Realm:Continent + Realm + Continent+ 
                 f(Period_4INLA,model='iid',hyper=pcpriorIID)+
                 f(Location_4INLA,model='iid',hyper=pcpriorIID)+
                 f(Plot_ID_4INLA,model='iid',hyper=pcpriorIID)+
                 f(Datasource_ID_4INLA,model='iid',hyper=pcpriorIID)+
                 f(Plot_ID_4INLAR,iYear,model='iid',hyper=pcpriorIID)+
                 f(Location_4INLAR,iYear,model='iid',hyper=pcpriorIID)+
                 f(Datasource_ID_4INLAR,iYear,model='iid',hyper=pcpriorIID)+
                 f(iYear,model='ar1', replicate=as.numeric(completeData$Plot_ID_4INLA), hyper = pcpriorAR0_0.5),
               control.compute = list(dic=TRUE,waic=TRUE, cpo = TRUE), 
               control.predictor = list(link = 1) , verbose = F, 
               control.family = list(hyper = pcpriorFamily),
               data=completeData)


inlaContPC1_0.5<- as.data.frame(readRDS("inlaContPC1_0.5SUMMARY.rds"))[8:19,]  
inlaContPC1_0.5
inlaCont<- as.data.frame(readRDS("inlaFcontSUMMARY.rds"))[8:19,]
plot(inlaContPC1_0.5$mean, inlaCont$mean) ;abline(a=0, b=1) # almost ideantical
plot(inlaContPC1_0.5$X0.025quant, inlaCont$X0.025quant) ;abline(a=0, b=1)
plot(inlaContPC1_0.5$X0.975quant, inlaCont$X0.975quant) ;abline(a=0, b=1)

inlaContPC1_0.86<- as.data.frame(readRDS("inlaContPC1_0.86SUMMARY.rds"))[8:19,]
inlaCont<- as.data.frame(readRDS("inlaFcontSUMMARY.rds"))[8:19,]
plot(inlaContPC1_0.86$mean, inlaCont$mean) ;abline(a=0, b=1) # almost ideantical
plot(inlaContPC1_0.86$X0.025quant, inlaCont$X0.025quant) ;abline(a=0, b=1)
plot(inlaContPC1_0.86$X0.975quant, inlaCont$X0.975quant) ;abline(a=0, b=1)

inlaRealmPC1_0.5<-  as.data.frame(readRDS("inlaRealmPC1_0.5SUMMARY.rds"))[3:4,]  
inlaRealmPC1_0.6<-  as.data.frame(readRDS("inlaRealmPC1_0.6SUMMARY.rds"))[3:4,]  
inlaRealmPC1_0.86<- as.data.frame(readRDS("inlaRealmPC1_0.86SUMMARY.rds"))[3:4,]
inlaRealm<- as.data.frame(readRDS("inlaRealmSUMMARY.rds"))[3:4,]
rownames(inlaRealmPC1_0.5)<- paste0(rownames(inlaRealmPC1_0.5), "PC1_0.5")
rownames(inlaRealmPC1_0.6)<- paste0(rownames(inlaRealmPC1_0.6), "PC1_0.6")
rownames(inlaRealmPC1_0.86)<- paste0(rownames(inlaRealmPC1_0.86), "PC1_0.86")


allPCmodels<- rbind(inlaRealm[, colnames(inlaRealmPC1_0.5)], inlaRealmPC1_0.5, inlaRealmPC1_0.6,inlaRealmPC1_0.86)
allPCmodels$varname<- rownames(allPCmodels)
allPCmodels<- arrange(allPCmodels, rownames(allPCmodels)) # almost identical results 
allPCmodels

allPCmodels$mean[1] / max(allPCmodels$mean[2:4] )
allPCmodels$mean[1] / min(allPCmodels$mean[2:4] )

allPCmodels$mean[5] / min(allPCmodels$mean[6:8] )
allPCmodels$mean[5] / max(allPCmodels$mean[6:8] )








#############
# test different subsets


# Excl North America ####


# almost half of the data out

inlaFrealmExclNAm <- inla(log10(Number+1) ~ cYear: Realm+ Realm  +
                            f(Period_4INLA,model='iid')+
                            f(Location_4INLA,model='iid')+
                            f(Plot_ID_4INLA,model='iid')+
                            f(Datasource_ID_4INLA,model='iid')+
                            f(Plot_ID_4INLAR,iYear,model='iid')+
                            f(Location_4INLAR,iYear,model='iid')                      +
                            f(Datasource_ID_4INLAR,iYear,model='iid')+
                            f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                          control.compute = list(dic=TRUE,waic=TRUE),
                          data=subset(completeData, Continent != "North America"))

inlaFrealmExclNAm<-  as.data.frame(read_rds("inlaFrealmExclNAmSUMMARY.rds"))
inlaRealmExclNAmTEST<- readRDS("inlaFrealmExclNAmTEST.rds")

# get probabilities of including 0 , one sided test
ps<- NULL
for(i in 3: nrow(inlaRealmExclNAmTEST$summary.fixed)){
  p<-inla.pmarginal(0, inlaRealmExclNAmTEST$marginals.fixed[[i]])
  ps<- c(ps, p) }; ps

data.frame(
  var =   c("FW 1 yr" ,"Terr 1 yr", "FW 10 yr", "Terr 10 yr"), 
  CI2.5 =  c((10^(inlaFrealmExclNAm  [3:4,4] )-1 ) *100, (10^(inlaFrealmExclNAm  [3:4,4] *10)-1)  *100),#0.025 CI
  mean =   c((10^(inlaFrealmExclNAm  [3:4,1] )-1)  *100, (10^(inlaFrealmExclNAm  [3:4,1] *10)-1)  *100), # proportional changes per year
  CI97.5 = c((10^(inlaFrealmExclNAm  [3:4,12] )-1 ) *100, (10^(inlaFrealmExclNAm  [3:4,12] *10)-1)  *100)# 0.975
)



metadata_realm.ExclNa<-  subset(completeData, Continent != "North America") %>% 
  group_by(Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID))) 

realmSlope<- inlaFrealmExclNAm[ 3:4,]
rownames(realmSlope)<- c("Freshwater" , "Terrestrial" )

realmPlot<-ggplot(data.frame(realmSlope))+
  geom_errorbar(aes(x=rownames(realmSlope),ymin=X0.025quant,ymax= X0.975quant, color = rownames(realmSlope)),
                 position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=rownames(realmSlope),ymin=X0.1quant,ymax= X0.9quant, color = rownames(realmSlope)), alpha = 0.6,
                size = 2, width=0, position=position_dodge(width= 0.7))+  
  geom_point(aes(x=rownames(realmSlope),   y=mean, shape = rownames(realmSlope),
                 color = rownames(realmSlope), fill = rownames(realmSlope)), 
             size = 4, position=  position_dodge(width = 0.7), alpha=1 )+
  scale_fill_manual(values = col.scheme.realm)+
  coord_flip()+  ylim(-0.01, 0.02)+  xlab ("")+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab ("Trend slope")+
  ggtitle("Realms") +
  theme_clean +
  theme(legend.position="bottom" ,
        legend.title=element_blank()) 
  

# excl biomass#####
# relation estimates incl biomass and excluding biomass datasets 

contExclB<- as.data.frame(read_rds("inlaFcontExclBSUMMARY.rds"))
cont<- as.data.frame(read_rds("inlaFcontSUMMARY.rds"))

names(contExclB)<- paste0(names(contExclB), "exclB")
cont<- cbind(cont, contExclB)
cont<- cont[8:19,]
vars<-data.frame(do.call(rbind, strsplit(rownames(cont), split = ":")))
cont<-cbind(cont, vars)
cont$Realm<-gsub("Realm", "", cont$X1)
cont$Continent<-gsub("Continent", "", cont$X2)
ABSlope$AB <-ABSlope$Unit

cont.correlation <- ggplot(cont, aes(x = mean, y = meanexclB, shape = Realm , color = Continent)) + 
  geom_point( size = 2)+
#  geom_errorbar(aes(x=mean ,ymin=meanexclB-sdexclB, ymax=meanexclB+sdexclB))+
#  geom_errorbarh(aes(y=meanexclB ,xmin=mean-sd, xmax=mean+sd))+
  geom_abline(aes(intercept = 0, slope = 1))+
  scale_color_manual(values = col.scheme.cont)+
  xlab ("full model estimates") + ylab("esimates of model excluding biomass-only datasets") +
theme_clean

png("cont.correlation.png", width=2000, height=1500, res = 360)
cont.correlation
dev.off()




biomExclB<- as.data.frame(read_rds("inlaFbiomExclBSUMMARY.rds"))
biom<- as.data.frame(read_rds("inlaFbiomSUMMARY.rds"))

names(biomExclB)<- paste0(names(biomExclB), "exclB")
biom<- cbind(biom, biomExclB)
biom<- biom[6:13,]
vars<-data.frame(do.call(rbind, strsplit(rownames(biom), split = ":")))
biom<-cbind(biom, vars)
biom$Realm<-gsub("Realm", "", biom$X1)
biom$Climatic_zone<-gsub("BiomeCoarse", "", biom$X2)

biom.correlation <- ggplot(biom, aes(x = mean, y = meanexclB, shape = Realm , color = Climatic_zone)) + 
  geom_point( size = 2)+
#  geom_errorbar(aes(x=mean ,ymin=meanexclB-sdexclB, ymax=meanexclB+sdexclB))+
#  geom_errorbarh(aes(y=meanexclB ,xmin=mean-sd, xmax=mean+sd))+
  geom_abline(aes(intercept = 0, slope = 1))+
  scale_color_manual(values = col.scheme.biom)+
  xlab ("full model estimates") + ylab("esimates of model excluding biomass-only datasets") +
  theme_clean

png("biom.correlation.png", width=2000, height=1500, res = 360)
biom.correlation
dev.off()










# only europe and NA 



completeDataE<- subset(completeData,  Continent == "Europe")
inlaFrealmEur <- inla(log10(Number+1) ~ cYear: Realm+ Realm  +
                           f(Period_4INLA,model='iid')+
                           f(Location_4INLA,model='iid')+
                           f(Plot_ID_4INLA,model='iid')+
                           f(Datasource_ID_4INLA,model='iid')+
                           f(Plot_ID_4INLAR,iYear,model='iid')+
                           f(Location_4INLAR,iYear,model='iid')                      +
                           f(Datasource_ID_4INLAR,iYear,model='iid')+
                           f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                         control.compute = list(dic=TRUE,waic=TRUE),
                         data=completeDataE)

completeDataNA<- subset(completeData,  Continent == "North America")
inlaFrealmNA <- inla(log10(Number+1) ~ cYear: Realm+ Realm  +
                        f(Period_4INLA,model='iid')+
                        f(Location_4INLA,model='iid')+
                        f(Plot_ID_4INLA,model='iid')+
                        f(Datasource_ID_4INLA,model='iid')+
                        f(Plot_ID_4INLAR,iYear,model='iid')+
                        f(Location_4INLAR,iYear,model='iid')                      +
                        f(Datasource_ID_4INLAR,iYear,model='iid')+
                        f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                      control.compute = list(dic=TRUE,waic=TRUE),
                      data=completeDataNA)





###############################

# outliers #####

RandEfDataset<- readRDS("RandEfDataset.rds")
hist(RandEfDataset$slope)

# definition: values outside the 1.5* the interquantile distance above or below the 0.25 and 0.75 quantiles

lowOutl<-  RandEfDataset$Datasource_ID[RandEfDataset$slope < 
                        quantile(RandEfDataset$slope, 0.25)- 1.5*IQR(RandEfDataset$slope)]
hiOutl<-  RandEfDataset$Datasource_ID[RandEfDataset$slope >
                                         quantile(RandEfDataset$slope, 0.75)+ 1.5*IQR(RandEfDataset$slope)]
outliers<- c(lowOutl, hiOutl)
sort(outliers)
completeData5<- completeData[! completeData$Datasource_ID %in% outliers, ]

# 5% highest and lowest slopes:
sortedSlopes<- arrange(RandEfDataset, slope)
head(sortedSlopes, 4)
tail(sortedSlopes, 4)
outliers5perc<- c(sortedSlopes$Datasource_ID[1:4] , rev(sortedSlopes$Datasource_ID)[1:4])
outliers5perc

realmExclO<- (read_rds("realmExclOTEST.rds"))
realmExclOsum<- as.data.frame(read_rds("realmExclOSUMMARY.rds"))

ps<- NULL
for(i in 3: nrow(realmExclO$summary.fixed )){
  p<-inla.pmarginal(0, realmExclO$marginals.fixed[[i]])
  ps<- c(ps, p) };ps

# check if the outliers were correctly excluded
test<- merge(RandEfDataset,realmExclO$summary.random$Datasource_ID_4INLAR, by.x="Datasource_ID_4INLAR", by.y="ID")
sort(test$Datasource_ID) # correctly all missing 

realm<- as.data.frame(read_rds("inlaRealmSUMMARY.rds"))[3:4,]
rownames(realmExclO)<- paste0(rownames(realmExclO), "exclO")
rbind(realm, realmExclO)

data.frame(
  var =   c("FW 1 yr" ,"Terr 1 yr", "FW 10 yr", "Terr 10 yr"), 
  CI2.5 =  c((10^(realmExclOsum  [3:4,4]  )-1 ) *100, (10^(realmExclOsum [3:4,4] *10)-1 )  *100),#0.025 CI
  mean =   c((10^(realmExclOsum  [3:4,1]  )-1)  *100, (10^(realmExclOsum  [3:4,1] *10)-1 )  *100), # proportional changes per year
  CI97.5 = c((10^(realmExclOsum  [3:4,12] )-1 ) *100, (10^(realmExclOsum  [3:4,12] *10)-1)  *100)# 0.975
)

# continents
contExclO<- read_rds("contExclOTEST.rds")
contExclOsum<- as.data.frame(read_rds("contExclOSUMMARY.rds"))
ps<- NULL
for(i in 8: nrow(contExclO$summary.fixed )){
  p<-inla.pmarginal(0, contExclO$marginals.fixed[[i]])
  ps<- c(ps, p) }; ps

cbind(contExclO$summary.fixed [8: nrow(contExclO$summary.fixed ),], ps)
# for north America
data.frame(
  var =   c("FW 1 yr" ,"Terr 1 yr"), 
  CI2.5 =  c((10^(contExclOsum  [18:19,4]  )-1 ) *100),#0.025 CI
  mean =   c((10^(contExclOsum  [18:19,1]  )-1)  *100), # proportional changes per year
  CI97.5 = c((10^(contExclOsum  [18:19,12] )-1 ) *100)# 0.975
)

# australia
data.frame(
  var =   c("FW 1 yr" ,"Terr 1 yr" ), 
  CI2.5 =  c((10^(contExclOsum  [14:15,4]  )-1 ) *100),#0.025 CI
  mean =   c((10^(contExclOsum  [14:15,1]  )-1)  *100), # proportional changes per year
  CI97.5 = c((10^(contExclOsum  [14:15,12] )-1 ) *100)# 0.975
)


cont<- as.data.frame(read_rds("inlaFcontSUMMARY.rds"))
names(contExclOsum)<- paste0(names(contExclOsum), "exclO")
cbind(contExclOsum[8:19,], ps)
cont<- cbind(cont, contExclOsum)
cont<- cont[8:19,]
vars<-data.frame(do.call(rbind, strsplit(rownames(cont), split = ":")))
cont<-cbind(cont, vars)
cont$Realm<-gsub("Realm", "", cont$X1)
cont$Continent<-gsub("Continent", "", cont$X2)

cont.correlation <- ggplot(subset(cont, Continent != "Africa"), aes(x = mean, y = meanexclO, shape = Realm , color = Continent)) + 
  
  geom_errorbarh(aes(y=meanexclO ,xmin = X0.025quant, xmax=X0.975quant))+
  geom_errorbarh(aes(y=meanexclO ,xmin=X0.1quant, xmax=X0.9quant), size =1.5)+
  
  geom_errorbar(aes(x=mean ,ymin=X0.025quantexclO, ymax=X0.975quantexclO))+
  geom_errorbar(aes(x=mean ,ymin=X0.1quantexclO, ymax=X0.9quantexclO), size = 1.5)+
  
  geom_point( size = 3, color = "black")+
  geom_abline(aes(intercept = 0, slope = 1))+
  geom_hline(aes(yintercept = 0))+
  geom_vline(xintercept = 0,linetype="dashed")+
  scale_color_manual(values = col.scheme.cont)+
  xlab ("full model estimates") + ylab("esimates of model excluding outliers") +
  theme_clean

# Climatic zones
biomExclO<- (read_rds("biomExclOTEST.rds"))
biomExclOsum<- as.data.frame(read_rds("biomExclOSUMMARY.rds"))

ps<- NULL
for(i in 6: nrow(biomExclO$summary.fixed )){
  p<-inla.pmarginal(0, biomExclO$marginals.fixed[[i]])
  ps<- c(ps, p) }; ps

cbind( biomExclO$summary.fixed[6: nrow(biomExclO$summary.fixed),], ps)
# for temperate
data.frame(
  var =   c(c('RealmFreshwater:BiomeCoarseTemperate:cYear', 'RealmTerrestrial:BiomeCoarseTemperate:cYear')), 
  CI2.5 =  c((10^(biomExclOsum  [c('RealmFreshwater:BiomeCoarseTemperate:cYear', 'RealmTerrestrial:BiomeCoarseTemperate:cYear'),4]  )-1 ) *100),#0.025 CI
  mean =   c((10^(biomExclOsum  [c('RealmFreshwater:BiomeCoarseTemperate:cYear', 'RealmTerrestrial:BiomeCoarseTemperate:cYear'),1]  )-1)  *100), # proportional changes per year
  CI97.5 = c((10^(biomExclOsum  [c('RealmFreshwater:BiomeCoarseTemperate:cYear', 'RealmTerrestrial:BiomeCoarseTemperate:cYear'),12] )-1 ) *100)# 0.975
)

# for drylands
data.frame(
  var =   c('RealmFreshwater:BiomeCoarseDrylands:cYear' , 'RealmTerrestrial:BiomeCoarseTemperate:cYear'), 
  CI2.5 =  c((10^(biomExclOsum  [c('RealmFreshwater:BiomeCoarseDrylands:cYear' , 'RealmTerrestrial:BiomeCoarseTemperate:cYear'),'X0.025quant']  )-1 ) *100),#0.025 CI
  mean =   c((10^(biomExclOsum  [c('RealmFreshwater:BiomeCoarseDrylands:cYear' , 'RealmTerrestrial:BiomeCoarseTemperate:cYear'),'mean']  )-1)  *100), # proportional changes per year
  CI97.5 = c((10^(biomExclOsum  [c('RealmFreshwater:BiomeCoarseDrylands:cYear' , 'RealmTerrestrial:BiomeCoarseTemperate:cYear'),'X0.975quant'] )-1 ) *100)# 0.975
)


biom<- as.data.frame(read_rds("inlaFbiomSUMMARY.rds"))


names(biomExclOsum)<- paste0(names(biomExclOsum), "exclO")
cbind(biomExclOsum[6:13, ], ps)
biom<- cbind(biom, biomExclOsum)
biom<- biom[6:13,]
vars<-data.frame(do.call(rbind, strsplit(rownames(biom), split = ":")))
biom<-cbind(biom, vars)
biom$Realm<-gsub("Realm", "", biom$X1)
biom$biome<-gsub("BiomeCoarse", "", biom$X2)

biom.correlation <- ggplot(biom, aes(x = mean, y = meanexclO, shape = Realm , color = biome)) + 
  geom_errorbarh(aes(y=meanexclO ,xmin = X0.025quant, xmax=X0.975quant))+
  geom_errorbarh(aes(y=meanexclO ,xmin=X0.1quant, xmax=X0.9quant), size =1.5)+
  geom_errorbar(aes(x=mean ,ymin=X0.025quantexclO, ymax=X0.975quantexclO))+
  geom_errorbar(aes(x=mean ,ymin=X0.1quantexclO, ymax=X0.9quantexclO), size = 1.5)+
  geom_point( size = 3, color = 1)+
  
  geom_abline(aes(intercept = 0, slope = 1))+
  geom_hline(aes(yintercept = 0))+
  geom_vline(xintercept = 0,linetype="dashed")+
  #  scale_color_manual(values = col.scheme.cont)+
  xlab ("full model estimates") + ylab("esimates of model excluding outliers") +
  theme_clean




##########







###################################################################################################

# excluding biomass
inlaFcontExclB <- get(load("inlaFcontExclB.RData"))


metadata_cont<-  subset(completeData, Unit != "biomass") %>% 
  group_by(Continent, Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
metadata_cont

contSlope<- inlaFcontExclB$summary.fixed[8:19,]
vars<-data.frame(do.call(rbind, strsplit(rownames(contSlope), split = ":")))
contSlope<-cbind(contSlope, vars)
contSlope$Realm<-gsub("Realm", "", contSlope$X1);  contSlope$Continent<-gsub("Continent", "", contSlope$X2)
contSlope<- merge(contSlope, metadata_cont)
contSlope$text = paste0("(", contSlope$Datasources, " | ", contSlope$Plots, ")")
contSlope$Continent<- ordered(contSlope$Continent, levels = rev(c("Europe", "North America" , "Asia", "Latin America", "Australia", "Africa" )))

# link % change to slope in log space
brks<- c(-0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
e<- c("","","","","","","")

contPlot<- ggplot(data.frame(subset(contSlope, Continent != "Africa"   )))+ # exclude africa,bc it has too wide CI's 
  geom_crossbar(aes(x=Continent,   y=mean, fill = Realm, 
                    ymin=X0.025quant,ymax=X0.975quant),position="dodge",alpha=0.8 ,  width =0.7)+
  scale_fill_manual(values = col.scheme.realm)+
  geom_hline(yintercept=0,linetype="dashed") +
  geom_text(aes(x = Continent , y = 0.032, label = text, fill = Realm),  
            position = position_dodge(width = 0.7), size = 3, color = 1) +
  coord_flip()+
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.028,0.036))+
  xlab ("")+ ylab("")+ #Trend slope | \n % change per year
  theme_clean +
  theme(legend.key=element_blank(),
        legend.position='none', 
        axis.text.x=element_blank()) +
  geom_text(aes(x = 5.3 , y = 0.025, label = "excl biomass"),  
            size = 6, color = 1) 



metadata_biom<-  subset(completeData, Unit != "biomass" ) %>% 
  group_by(BiomeCoarse, Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
metadata_biom



biomSlope<- as.data.frame(readRDS("inlaFbiomExclB12SUMMARY.rds"))[6:13,]
vars<-data.frame(do.call(rbind, strsplit(rownames(biomSlope), split = ":")))
biomSlope<-cbind(biomSlope, vars)
biomSlope$Realm<-gsub("Realm", "", biomSlope$X1);  biomSlope$BiomeCoarse<-gsub("BiomeCoarse", "", biomSlope$X2)
biomSlope$Biome <-biomSlope$BiomeCoarse
biomSlope<- merge(biomSlope, metadata_biom)
biomSlope$text = paste0("(", biomSlope$Datasources, " | ", biomSlope$Plots, ") ")

biomSlope$Biome<- ordered(biomSlope$Biome, levels = rev(c("Boreal/Alpine", "Temperate" , "Drylands", "Tropical"  )))

brks<- c(-0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")


biomPlot<- ggplot(data.frame(biomSlope))+
  geom_crossbar(aes(x=Biome,   y=mean, fill = Realm,
                    ymin=X0.025quant,ymax=X0.975quant),position="dodge",alpha=0.8 ,  width =0.7)+
  scale_fill_manual(values = col.scheme.realm)+
  geom_hline(yintercept=0,linetype="dashed") +
  geom_text(aes(x = Biome , y = 0.032, label = text, fill = Realm),  
            position = position_dodge(width = 0.7), size = 3, color = 1) +
  coord_flip()+
  #ylim(-0.025, 0.035)+
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.029,0.036))+
  xlab ("")+ ylab("Trend slope  \n % change per year")+
  theme_clean +
  theme(legend.key=element_blank(), 
        legend.position="bottom")  + 
  geom_text(aes(x = 4.3 , y = -0.025, label = "B"),  
            size = 6, color = 1) 



# R2 #####
inla0 <- inla(log10(Number+1)~ 1, data=completeData)
inla1<- readRDS("inla1TEST.rds")
inlaRealm<- readRDS("inlaRealmTEST.rds")
inlaBiom<- readRDS("inlaFbiomTEST.rds")
inlaCont<- readRDS("inlaFcontTEST.rds")

outFE<-inla0$model.matrix #just get the values of year and realm for each data row
coefFE<-as.matrix(inla0$summary.fixed[,1],ncol=1) #the fixed effect coefficient for the effects of year and realm
inla_dfFE<-outFE%*%coefFE #multiply them as written in the model equation to get the model predictions based on fixed effects
varF0<-var(as.numeric(inla_dfFE[,1])) #get the variance of these values
varF0 # variation explained by year-only  model 
varTotal<- bri.hyperpar.summary(inla0)[1]

outFE<-inla1$model.matrix #just get the values of year and realm for each data row
coefFE<-as.matrix(inla1$summary.fixed[,1],ncol=1) #the fixed effect coefficient for the effects of year and realm
inla_dfFE<-outFE%*%coefFE #multiply them as written in the model equation to get the model predictions based on fixed effects
varF<-var(as.numeric(inla_dfFE[,1])) #get the variance of these values
varF # variation explained by year-only  model 


outFE<-inlaRealm$model.matrix #just get the values of year and realm for each data row
coefFE<-as.matrix(inlaRealm$summary.fixed[,1],ncol=1) #the fixed effect coefficient for the effects of year and realm
inla_dfFE<-outFE%*%coefFE #multiply them as written in the model equation to get the model predictions based on fixed effects
varFrealm<-var(as.numeric(inla_dfFE[,1])) #get the variance of these values
varFrealm # variation explained by year+realm   model 

outFE<-inlaBiom$model.matrix #just get the values of year and realm for each data row
coefFE<-as.matrix(inlaBiom$summary.fixed[,1],ncol=1) #the fixed effect coefficient for the effects of year and realm
inla_dfFE<-outFE%*%coefFE #multiply them as written in the model equation to get the model predictions based on fixed effects
varFbiom<-var(as.numeric(inla_dfFE[,1])) #get the variance of these values
varFbiom # variation explained by year+realm   model 

outFE<-inlaCont$model.matrix #just get the values of year and realm for each data row
coefFE<-as.matrix(inlaCont$summary.fixed[,1],ncol=1) #the fixed effect coefficient for the effects of year and realm
inla_dfFE<-outFE%*%coefFE #multiply them as written in the model equation to get the model predictions based on fixed effects
varFcont<-var(as.numeric(inla_dfFE[,1])) #get the variance of these values
varFcont # variation explained by year+realm   model 

varFcont/varTotal # 0.061
varFbiom/varTotal # 0.027
varFrealm/varTotal # 0.01


inlaT0<-inla(log10(Number+1)~ 1, data=subset(completeData, Realm == "Terrestrial"))
inlaFW0<-inla(log10(Number+1)~ 1, data=subset(completeData, Realm == "Freshwater"))

inla1T<- readRDS("inla1TerrTEST.rds")
inla1FW<- readRDS("inla1FWTEST.rds")

inlaChangesT<- readRDS("inlaFChangesTerrTEST.rds")
inlaChangesFW<- readRDS("inlaFChangesFWTEST.rds")
inlaLUt<- readRDS("inlaFlanduseTTEST.rds")
inlaLUfw<- readRDS("inlaFlanduseFWTEST.rds")

varTotalTerr<- bri.hyperpar.summary(inlaT0)[1]
varTotalFW<- bri.hyperpar.summary(inlaFW0)[1]


outFE<-inlaChangesT$model.matrix #just get the values of year and realm for each data row
coefFE<-as.matrix(inlaChangesT$summary.fixed[,1],ncol=1) #the fixed effect coefficient for the effects of year and realm
inla_dfFE<-outFE%*%coefFE #multiply them as written in the model equation to get the model predictions based on fixed effects
varTchange<-var(as.numeric(inla_dfFE[,1])) #get the variance of these values
varTchange # variation explained by year-only  model 

varTchange/varTotalTerr

outFE<-inlaChangesT$model.matrix #just get the values of year and realm for each data row
coefFE<-as.matrix(inlaChangesT$summary.fixed[,1],ncol=1) #the fixed effect coefficient for the effects of year and realm
inla_dfFE<-outFE%*%coefFE #multiply them as written in the model equation to get the model predictions based on fixed effects
varTchange<-var(as.numeric(inla_dfFE[,1])) #get the variance of these values
varTchange # variation explained by year-only  model 

varTchange/varTotalTerr


outFE<-inla1T$model.matrix #just get the values of year and realm for each data row
coefFE<-as.matrix(inla1T$summary.fixed[,1],ncol=1) #the fixed effect coefficient for the effects of year and realm
inla_dfFE<-outFE%*%coefFE #multiply them as written in the model equation to get the model predictions based on fixed effects
varT1<-var(as.numeric(inla_dfFE[,1])) #get the variance of these values
varT1 # variation explained by year-only  model 

outFE<-inlaChangesFW$model.matrix #just get the values of year and realm for each data row
coefFE<-as.matrix(inlaChangesFW$summary.fixed[,1],ncol=1) #the fixed effect coefficient for the effects of year and realm
inla_dfFE<-outFE%*%coefFE #multiply them as written in the model equation to get the model predictions based on fixed effects
varFWchange<-var(as.numeric(inla_dfFE[,1])) #get the variance of these values
varFWchange # variation explained by year-only  model 

varFWchange/varTotalFW

outFE<-inlaLUt$model.matrix #just get the values of year and realm for each data row
coefFE<-as.matrix(inlaLUt$summary.fixed[,1],ncol=1) #the fixed effect coefficient for the effects of year and realm
inla_dfFE<-outFE%*%coefFE #multiply them as written in the model equation to get the model predictions based on fixed effects
varTlu<-var(as.numeric(inla_dfFE[,1])) #get the variance of these values
varTlu # variation explained by year-only  model 

varTlu/varTotalTerr







# all arthropods #####

inlaFA <- inla(log10(Number+1) ~ cYear + 
                 f(Period_4INLA,model='iid')+
                 f(Location_4INLA,model='iid')+
                 f(Plot_ID_4INLA,model='iid')+
                 f(Datasource_ID_4INLA,model='iid')+
                 f(Plot_ID_4INLAR,iYear,model='iid')+
                 f(Location_4INLAR,iYear,model='iid')                      +
                 f(Datasource_ID_4INLAR,iYear,model='iid')+
                 f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
               control.compute = list(dic=TRUE,waic=TRUE),
                    data=completeDataArth, verbose = F)



inlaFArealm <- inla(log10(Number+1) ~ cYear: Realm + Realm +
                      f(Period_4INLA,model='iid')+
                      f(Location_4INLA,model='iid')+
                      f(Plot_ID_4INLA,model='iid')+
                      f(Datasource_ID_4INLA,model='iid')+
                      f(Plot_ID_4INLAR,iYear,model='iid')+
                      f(Location_4INLAR,iYear,model='iid')                      +
                      f(Datasource_ID_4INLAR,iYear,model='iid')+
                      f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                    control.compute = list(dic=TRUE,waic=TRUE),
                   data=completeDataArth, verbose = F)
load("E:/inlaFArealm.RData")

metadata_realm<-  completeData %>% 
  group_by(Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID))) 

realmSlope<- inlaFArealm$summary.fixed[ 3:4,]
rownames(realmSlope)<- c("Freshwater" , "Terrestrial" )

realmPlotArth<-ggplot(data.frame(realmSlope))+
  geom_crossbar(aes(x=rownames(realmSlope),y=mean, fill = rownames(realmSlope),
                    ymin=X0.025quant,ymax=X0.975quant),position="dodge", width = 0.7)+
  scale_fill_manual(values = col.scheme.realm)+
  coord_flip()+  ylim(-0.01, 0.02)+  xlab ("")+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab ("Trend slope")+
ggtitle("Realms") +
theme_clean +
  theme(legend.position="bottom" ,
          legend.title=element_blank()) 
  






 # Strata

inlaFAstrat <- inla(log10(Number+1) ~ cYear:Stratum + Stratum  + 
                      f(Period_4INLA,model='iid')+
                      f(Location_4INLA,model='iid')+
                      f(Plot_ID_4INLA,model='iid')+
                      f(Datasource_ID_4INLA,model='iid')+
                      f(Plot_ID_4INLAR,iYear,model='iid')+
                      f(Location_4INLAR,iYear,model='iid')                      +
                      f(Datasource_ID_4INLAR,iYear,model='iid')+
                      f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                    control.compute = list(dic=TRUE,waic=TRUE),
                   data=completeDataArth)


load("E:/inlaFAstrat.RData")
stratSlope<- inlaFAstrat$summary.fixed[7:12,]
vars<-data.frame(do.call(rbind, strsplit(rownames(stratSlope), split = ":")))
stratSlope<-cbind(stratSlope, vars)
stratSlope$X1<-gsub("Stratum", "", stratSlope$X1)
stratSlope<- merge(stratSlope, metadata_strata, by.x = "X1", by.y = "Stratum")
stratSlope$text = paste0("(", stratSlope$Datasources, " / ", stratSlope$Plots, ")")
rownames(stratSlope)<-stratSlope$X1
stratSlope$X1<- ordered(stratSlope$X1, levels = c("Water", "Underground" , "Soil surface", "Herb layer", "Trees", "Air" ))

stratPlotArth<- ggplot(data.frame(stratSlope))+
  geom_crossbar(aes(x=X1,y=mean, fill = X1,
                    ymin=X0.025quant,ymax=X0.975quant),position="dodge", width = 0.7, fill = "grey70")+
  ggtitle ("Capture strata")+
  coord_flip()+
  ylim(-0.015, 0.03)+
  xlab ("")+ ylab ("Trend slope")+
  geom_hline(yintercept=0,linetype="dashed")+
theme_clean 



# continent

inlaFAcont <- inla(log10(Number+1) ~ cYear: Realm:Continent + Realm + Continent + 
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Location_4INLAR,iYear,model='iid')                      +
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                  data=completeDataArth)
#plot
load("E:/inlaFAcont.RData")

metadata_cont<-  completeDataArth %>% 
  group_by(Continent, Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
metadata_cont

contSlope<- inlaFAcont$summary.fixed[8:19,]
vars<-data.frame(do.call(rbind, strsplit(rownames(contSlope), split = ":")))
contSlope<-cbind(contSlope, vars)
contSlope$Realm<-gsub("Realm", "", contSlope$X1);  contSlope$Continent<-gsub("Continent", "", contSlope$X2)
contSlope<- merge(contSlope, metadata_cont)
contSlope$text = paste0("(", contSlope$Datasources, " / ", contSlope$Plots, ")")
contSlope$Continent<- ordered(contSlope$Continent, levels = rev(c("Europe", "North America" , "Asia", "Latin America", "Australia", "Africa" )))

contPlotArth<-ggplot(data.frame(subset(contSlope, Continent != "Africa"   )))+ # exclude africa,bc it has too wide CI's 
  geom_crossbar(aes(x=Continent,   y=mean, fill = Realm, 
                    ymin=X0.025quant,ymax=X0.975quant),position="dodge",alpha=0.8 ,  width =0.7)+
  scale_fill_manual(values = col.scheme.realm)+
  geom_hline(yintercept=0,linetype="dashed") +
  xlab ("")+ ylab("Trend slope")+
  # geom_text(aes(x = Continent , y = 0.028, label = text, fill = Realm),  
  #          position = position_dodge(width = 1), size = 3, color = 1) +
  coord_flip()+
  scale_y_continuous(breaks = c(-0.02, -0.01, 0,0.01, 0.02)) +
  ylim(-0.025, 0.03)+
 theme_clean +
  theme(legend.position ="") +
  ggtitle("Continents")



# Biome 
inlaFAbiom <- inla(log10(Number+1) ~ cYear: Realm:BiomeCoarse + Realm + BiomeCoarse + 
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Location_4INLAR,iYear,model='iid')                      +
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                   data=completeDataArth)
#
load("E:/inlaFAbiom.RData")

metadata_biom<-  completeDataArth %>% 
  group_by(BiomeCoarse, Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
metadata_biom



biomSlope<- inlaFAbiom$summary.fixed[6:13,]
vars<-data.frame(do.call(rbind, strsplit(rownames(biomSlope), split = ":")))
biomSlope<-cbind(biomSlope, vars)
biomSlope$Realm<-gsub("Realm", "", biomSlope$X1);  biomSlope$BiomeCoarse<-gsub("BiomeCoarse", "", biomSlope$X2)
biomSlope$Biome <-biomSlope$BiomeCoarse
biomSlope<- merge(biomSlope, metadata_biom)
biomSlope$text = paste0("(", biomSlope$Datasources, " / ", biomSlope$Plots, ") ")
biomSlope$Biome<- ordered(biomSlope$Biome, levels = rev(c("Boreal/Alpine", "Temperate" , "Drylands", "Tropical"  )))

biomPlotArth<- ggplot(data.frame(biomSlope))+
  geom_crossbar(aes(x=Biome,   y=mean, fill = Realm,
                    ymin=X0.025quant,ymax=X0.975quant),position="dodge",alpha=0.8 ,  width =0.7)+
  scale_fill_manual(values = col.scheme.realm)+
  xlab ("")+ ylab ("")+geom_hline(yintercept=0,linetype="dashed")+
  coord_flip()+
  scale_y_continuous(breaks = c(-0.02, -0.01, 0,0.01, 0.02)) +
  ylim(-0.03, 0.032)+  
 theme_clean + 
  theme(legend.position = "")+
  ggtitle("Biomes")

library(gridExtra)
grid.arrange(realmPlotArth, biomPlotArth,  stratPlotArth, contPlotArth,nrow = 2)




# swedish data more restricted (only datasets with data standardized to 1m2)
# can only affect Europe fw and fw boreal and temperate 










# Abundance / biomass relation #####

load( "ABcomparison.arth.RData")
load("ABcomparison.insect.RData")

# deselect redundant taxa in Russia FW6 
ABcomparison.insect<- subset(ABcomparison.insect, Datasource_ID !=1453 | Taxon != "Zoobentos"  )
# remove russia Freshwater , it has no biomass values for 1960 for chironomidae
ABcomparison.insect<- subset(ABcomparison.insect, Datasource_ID != 1448)
# remove other datasets with only 2 points (Georgia FW and Wiscnsin mayflies: 
ABcomparison.insect<- subset(ABcomparison.insect, Datasource_ID != 1425 & Datasource_ID != 1415)

# merge all values over all species to get community abundance and biomass
all.aggrAB<-dcast(ABcomparison.insect,  Datasource_ID + Datasource_name+ Location + 
                    Stratum  + Plot_ID + Plot_name +  
                    Year + Period + Unit +Date  + Realm +Continent~ "Number",    value.var = "Number", sum, na.rm = TRUE)

# put abundance and biomass side by side
all.aggrAB.test<-dcast(all.aggrAB, Datasource_ID + Datasource_name+ Location + Stratum  + Plot_ID + Plot_name +  
                         Year + Period  +Date  + Realm +Continent ~ Unit , value.var = "Number", sum, na.rm = TRUE)

#replace missing abundance data with density data
all.aggrAB.test$abundance[all.aggrAB.test$abundance == 0]<- all.aggrAB.test$density[all.aggrAB.test$abundance == 0]

# remove 0's
all.aggrAB.test <-subset(   all.aggrAB.test, biomass != 0 & abundance != 0)

qplot(x=abundance,y=biomass, data=all.aggrAB.test,colour=as.factor(Plot_ID))+
  scale_x_log10()+
  scale_y_log10()+
  xlab("Abundance (Number of individuals)") + ylab  ("Biomass (variable units)")+
  facet_wrap(~Datasource_ID ,scales="free")+
  theme_clean+
  theme(legend.position="none", 
        axis.text.x  = element_text(angle=28, vjust=1, hjust = 1)) 

#################################################################################

# Fig S3 diver effect size  #####

setwd("C:/Dropbox/Insect Biomass Trends/csvs/")



drivers<- read_rds("Results drivers.rds")
drivers$ptxt<- paste('p =', format(round(drivers$ps, 3), nsmall = 3))

# reorder
drivers$variable<- factor(drivers$variable, levels = rev(c("cYear:scale(urbanization)"   , 
                                                     "cYear:scale(cropification)"   , 
                                                    "cYear:scale(End_urbanArea)"       , 
                                                    "cYear:scale(End_cropArea)"  ,
                                                   "cYear:scale(frcUrban900m1992)"   ,
                                                   "cYear:scale(frcCrop900m1992)"     ,
                                                    "cYear:scale(relDeltaTmean)"      ,
                                                     "cYear:scale(relDeltaPrec)"       ,
                                                   "cYear:scale(CHELSArelDeltaTmean)" ,
                                                    "cYear:scale(CHELSArelDeltaPrec)" )))                                              

labs<-  c('cYear:scale(urbanization)' =     expression(Delta* ' urban cover (landscape scale)'), 
          'cYear:scale(cropification)' =    expression(Delta* ' cropland cover (landscape scale)'),
          'cYear:scale(End_cropArea)' =    'Cropland cover (landscape scale)',
          'cYear:scale(End_urbanArea)' =   'Urban cover (landscape scale)',
          'cYear:scale(frcCrop900m1992)' = 'Cropland cover (local scale)',
          'cYear:scale(frcUrban900m1992)' ='Urban cover (local scale)',
          'cYear:scale(relDeltaTmean)' =    expression('Relative ' *Delta* ' mean temperature (landscape scale)'),
          'cYear:scale(relDeltaPrec)' =     expression('Relative ' *Delta* ' monthly precipitation (landscape scale)'),
          'cYear:scale(CHELSArelDeltaTmean)' = expression('Relative ' *Delta* ' mean temperature (local scale)'),
          'cYear:scale(CHELSArelDeltaPrec)' = expression('Relative ' *Delta* ' montly precipitation (local scale)'))


drivers$Realm <- factor(drivers$Realm, levels = rev(c("Terrestrial", "Freshwater" )))
brks<- c(-0.006, -0.004, -0.002, 0, 0.002, 0.004, 0.006)



driverEffects<- ggplot( drivers   ) + 
  xlab ("")+ ylab ("")+geom_hline(yintercept=0,linetype="dashed")+
  geom_errorbar(aes(x=variable,ymin=X0.025quant,ymax= X0.975quant, color = Realm),alpha = 0.5,
                size = 1, width=0,   position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=variable ,ymin=X0.05quant,ymax= X0.95quant, color = Realm),alpha = 0.75,
                size = 2, width = 0,  position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=variable ,ymin=X0.1quant,ymax= X0.9quant, color = Realm), alpha = 1,
                size = 3, width=0, position=position_dodge(width= 0.7))+  
  geom_point(aes(x=variable ,   y=mean,  shape = Realm), 
             size = 2.5, position=  position_dodge(width = 0.7), color = "black", fill = "black",  alpha=1 )+  scale_x_discrete("", labels = labs)+
  scale_color_manual(values = col.scheme.realm)+
  scale_y_continuous("Parameter estimate",  breaks = brks, limits=c(-0.007,0.0075))+
  geom_text(aes(x = variable , y = 0.0072, label = ptxt, fill = Realm),  
            position = position_dodge(width = 0.7), size = 2.5, color = 1) +
  coord_flip()+
   theme_clean + 
  theme(legend.position = "", 
        strip.text = element_blank(), 
        strip.background = element_blank())







# Fig S4 #####
# loop for creating figs S4 

# load 
ESAterr<- as.data.frame(read_rds("inlaFlanduseESAterrSUMMARY.rds"))
ESAfw<- as.data.frame(read_rds("inlaFlanduseESAfwSUMMARY.rds"))
LUHterr<- as.data.frame(readRDS("inlaFlanduseTSUMMARY.rds"))
LUHfw<- as.data.frame(readRDS("inlaFlanduseFWSUMMARY.rds"))
deltaLUHterr<- as.data.frame(readRDS("inlaFchangesTerrSUMMARY.rds"))
deltaLUHfw<- as.data.frame(readRDS("inlaFchangesFWSUMMARY.rds"))


objects<- list(deltaLUHterr, deltaLUHfw, LUHterr, LUHfw, ESAterr, ESAfw  )

realms<- c("Terrestrial", "Freshwater","Terrestrial", "Freshwater","Terrestrial", "Freshwater")  

drivers<- c("Delta cover (Landscape scale)", "Delta cover (Landscape scale)", 
            "Land cover (Landscape scale)","Land cover (Landscape scale)", 
            "Land cover (local scale)", "Land cover (local scale)")


fulldf<- NULL
for (k in 1:6) {
  mod<- objects[[k]]
  rownames(mod)<- sub("Crop", "crop", rownames(mod) )
  rownames(mod)<- sub("Urban", "urban", rownames(mod) )
  names(completeData)<- sub("Crop", "crop", names(completeData) )
  names(completeData)<- sub("Urban", "urban", names(completeData) )
  
  
  urbanVariable<- rownames(mod)[grep("urban",  rownames(mod)[3:4])+2]
  urbanVariable<- gsub("scale(","" , urbanVariable, fixed = T)
  urbanVariable<- gsub(")","" , urbanVariable, fixed = T)
  cropVariable<- rownames(mod)[grep("crop",  rownames(mod)[3:4])+2]
  cropVariable<- gsub("scale(","" , cropVariable, fixed = T)
  cropVariable<- gsub(")","" , cropVariable, fixed = T)
  
  
  minCrop       = min(completeData[completeData$Realm == realms[k]  , names(completeData) == cropVariable], na.rm = T)    
  maxCrop       = max(completeData[completeData$Realm == realms[k]  , names(completeData) == cropVariable], na.rm = T)    
  minUrb       = min(completeData[completeData$Realm == realms[k]  , names(completeData) == urbanVariable], na.rm = T)    
  maxUrb       = max(completeData[completeData$Realm == realms[k]  , names(completeData) == urbanVariable], na.rm = T)    
  sdCrop       = sd(completeData[completeData$Realm == realms[k]  , names(completeData) == cropVariable], na.rm = T)    
  meanCrop       = mean(completeData[completeData$Realm == realms[k]  , names(completeData) == cropVariable], na.rm = T)    
  meanUrb       = mean(completeData[completeData$Realm == realms[k]  , names(completeData) == urbanVariable], na.rm = T)    
  sdUrb       = sd(completeData[completeData$Realm == realms[k]  , names(completeData) == urbanVariable], na.rm = T)    
  
  
  
  crop_value<- seq(minCrop,maxCrop, length.out = 101 )
  scCrop_value<- (crop_value-meanCrop) / sdCrop
  urb_value<- seq(minUrb,maxUrb, length.out = 101 )
  scUrb_value<- (urb_value-meanUrb) / sdUrb
  
  
  SlpYrmean<- mod["cYear" , "mean"]   
  SlpYrSd <-  mod["cYear" , "sd"]
  SlpYrCropmean <-mod[grep("crop",  rownames(mod)[5:6])+4 , "mean"] 
  SlpYrCropsd    <-mod[grep("crop",  rownames(mod)[5:6])+4, "sd"]
  SlpYrUrbmean   <-mod[grep("urban", rownames(mod)[5:6])+4 , "mean"] 
  SlpYrUrbsd     <-mod[grep("urban", rownames(mod)[5:6])+4 , "sd"]
  
  
  cropEstimate = matrix(data=NA,ncol=10000,nrow=length(scCrop_value))
  urbanEstimate = matrix(data=NA,ncol=10000,nrow=length(scUrb_value))
  # rownames(trendEstimate)<- scCrop_value
  for(c in 1: length (scCrop_value)){
    for(i in 1:10000){
      cropEstimate[c,i]  = rnorm(1,SlpYrmean, SlpYrSd) + scCrop_value[c] * rnorm(1,SlpYrCropmean,SlpYrCropsd)
      urbanEstimate[c,i] = rnorm(1,SlpYrmean, SlpYrSd) + scUrb_value[c]  * rnorm(1,SlpYrUrbmean, SlpYrUrbsd)
      
    }
  }
  
  mns<- apply(cropEstimate, 1, mean)  
  sds<- apply(cropEstimate, 1, sd)  
  upperCI<- apply(cropEstimate, 1, quantile, probs = c(0.025))
  lowerCI<- apply(cropEstimate, 1, quantile, probs = c(0.975))
  cropEsts<- data.frame(Realm = realms[k], Process = cropVariable, biotope = "Cropland", driver = drivers[k], mns, sds, cover = crop_value, upperCI, lowerCI)
  
  mns<- apply(urbanEstimate, 1, mean)  
  sds<- apply(urbanEstimate, 1, sd)  
  upperCI<- apply(urbanEstimate, 1, quantile, probs = c(0.025))
  lowerCI<- apply(urbanEstimate, 1, quantile, probs = c(0.975))
  urbanEsts<- data.frame(Realm = realms[k], Process = urbanVariable, biotope = "Urban", driver = drivers[k],mns, sds, cover = urb_value, upperCI, lowerCI)
  
  res<- rbind(cropEsts, urbanEsts)
  
  fulldf<- rbind(fulldf, res)
}


# separate plots per factor

cropificationPlot<-  ggplot(subset(fulldf, Process == "cropification"), 
                            aes(x = cover, y = mns, color = Realm))+
  geom_line(size = 1.5)+
  geom_ribbon(aes(x = cover,  ymin = lowerCI, ymax = upperCI, fill = Realm, color = Realm), alpha = 0.5  )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_fill_manual(values = col.scheme.realm, guide = F)+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  ylab ("")+ xlab (expression(Delta* ' crop cover (landscape scale)') )+
  ylim(-0.025, 0.03)+
  ggtitle  ("B")+
  theme_clean + 
  theme(  strip.background = element_blank(), 
          plot.title = element_text(hjust = 0.5))#,

urbanizationPlot<-  ggplot(subset(fulldf, Process == "urbanization"), 
                           aes(x = cover, y = mns, color = Realm))+
  geom_line(size = 1.5)+
  geom_ribbon(aes(x = cover,  ymin = lowerCI, ymax = upperCI, fill = Realm, color = Realm), alpha = 0.5  )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_fill_manual(values = col.scheme.realm, guide = F)+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  ylab ("")+ xlab (expression(Delta* ' urban cover (landscape scale)') )+
  ylim(-0.025, 0.03)+
  ggtitle  ("A")+
  theme_clean + 
  theme(  strip.background = element_blank(), 
          plot.title = element_text(hjust = 0.5))#,

cropPlot<-  ggplot(subset(fulldf, Process == "End_cropArea"), 
                   aes(x = cover, y = mns, color = Realm))+
  geom_line(size = 1.5)+
  geom_ribbon(aes(x = cover,  ymin = lowerCI, ymax = upperCI, fill = Realm, color = Realm), alpha = 0.5  )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_fill_manual(values = col.scheme.realm, guide = F)+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab ("")+ xlab ("Crop cover (landscape scale)" )+
  ylim(-0.025, 0.03)+
  ggtitle  ("D")+
  theme_clean + 
  theme(  strip.background = element_blank(), 
          plot.title = element_text(hjust = 0.5))#,

urbanPlot<-  ggplot(subset(fulldf, Process == "End_urbanArea"), 
                    aes(x = cover, y = mns, color = Realm))+
  geom_line(size = 1.5)+
  geom_ribbon(aes(x = cover,  ymin = lowerCI, ymax = upperCI, fill = Realm, color = Realm), alpha = 0.5  )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_fill_manual(values = col.scheme.realm, guide = F)+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab ("Trend slope")+ xlab ("Urban cover (landscape scale)" )+
  ylim(-0.025, 0.03)+
  ggtitle  ("C")+
  theme_clean + 
  theme(  strip.background = element_blank(), 
          plot.title = element_text(hjust = 0.5))#,

crop900mPlot<-  ggplot(subset(fulldf, Process == "frccrop900m1992"), 
                       aes(x = cover, y = mns, color = Realm))+
  geom_line(size = 1.5)+
  geom_ribbon(aes(x = cover,  ymin = lowerCI, ymax = upperCI, fill = Realm, color = Realm), alpha = 0.5  )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_fill_manual(values = col.scheme.realm, guide = F)+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab ("")+ xlab ("Crop cover (local scale)" )+
  ylim(-0.025, 0.03)+
  ggtitle  ("F")+
  theme_clean + 
  theme(  strip.background = element_blank(), 
          plot.title = element_text(hjust = 0.5))#,

urban900mPlot<-  ggplot(subset(fulldf, Process == "frcurban900m1992"), 
                        aes(x = cover, y = mns, color = Realm))+
  geom_line(size = 1.5)+
  geom_ribbon(aes(x = cover,  ymin = lowerCI, ymax = upperCI, fill = Realm, color = Realm), alpha = 0.5  )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_fill_manual(values = col.scheme.realm, guide = F)+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab ("")+ xlab ("Urban cover (local scale)" )+
  ylim(-0.025, 0.03)+
  theme_clean + 
  ggtitle  ("E")+
  theme(   plot.title = element_text(hjust = 0.5)
  )#,strip.background = element_blank(),



library(gridExtra)
grid.arrange(urbanizationPlot, cropificationPlot, 
             urbanPlot,cropPlot, 
             urban900mPlot, crop900mPlot,
             nrow = 3)



# Fig S5 #####

inlaFClimChangesTerr<- readRDS("inlaFClimChangesTerrSUMMARY.rds")
inlaFClimChangesFW<-as.data.frame(readRDS("inlaFClimChangesFWSUMMARY.rds")) 
inlaFCHELSAChangesTerr<- readRDS("inlaFCHELSAChangesTerrSUMMARY.rds")
inlaFCHELSAChangesFW<- readRDS("inlaFCHELSAChangesFWSUMMARY.rds")



objects<- list(inlaFClimChangesTerr, inlaFClimChangesFW,  inlaFCHELSAChangesTerr, inlaFCHELSAChangesFW  )

realms<- c("Terrestrial", "Freshwater","Terrestrial", "Freshwater","Terrestrial", "Freshwater")  

drivers<- c("CRU", "CRU", "CHELSA", "CHELSA")


# loop
climdf<- NULL
for (k in 1:4) {
  mod<- as.data.frame(objects[[k]])
  
  
  tempVariable<- rownames(mod)[grep("Tmean",  rownames(mod)[3:4])+2]
  tempVariable<- gsub("scale(","" , tempVariable, fixed = T)
  tempVariable<- gsub(")","" , tempVariable, fixed = T)
  precVariable<- rownames(mod)[grep("Prec",  rownames(mod)[3:4])+2]
  precVariable<- gsub("scale(","" , precVariable, fixed = T)
  precVariable<- gsub(")","" , precVariable, fixed = T)
  
  
  minPrec       = min(completeData[completeData$Realm == realms[k]  , names(completeData) == precVariable], na.rm = T)    
  maxPrec       = max(completeData[completeData$Realm == realms[k]  , names(completeData) == precVariable], na.rm = T)    
  minTemp       = min(completeData[completeData$Realm == realms[k]  , names(completeData) == tempVariable], na.rm = T)    
  maxTemp       = max(completeData[completeData$Realm == realms[k]  , names(completeData) == tempVariable], na.rm = T)    
  sdPrec       = sd(completeData[completeData$Realm == realms[k]  , names(completeData) == precVariable], na.rm = T)    
  meanPrec       = mean(completeData[completeData$Realm == realms[k]  , names(completeData) == precVariable], na.rm = T)    
  meanTemp       = mean(completeData[completeData$Realm == realms[k]  , names(completeData) == tempVariable], na.rm = T)    
  sdTemp       = sd(completeData[completeData$Realm == realms[k]  , names(completeData) == tempVariable], na.rm = T)    
  
  
  
  prec_value<- seq(minPrec,maxPrec, length.out = 101 )
  scprec_value<- (prec_value-meanPrec) / sdPrec
  temp_value<- seq(minTemp,maxTemp, length.out = 101 )
  sctemp_value<- (temp_value-meanTemp) / sdTemp
  
  SlpYrmean<- mod["cYear" , "mean"]   
  SlpYrSd <-  mod["cYear" , "sd"]
  SlpYrPrecmean <-mod[grep("Prec",  rownames(mod)[5:6])+4 , "mean"] 
  SlpYrPrecsd    <-mod[grep("Prec",  rownames(mod)[5:6])+4, "sd"]
  SlpYrTmean   <-mod[grep("Tmean", rownames(mod)[5:6])+4 , "mean"] 
  SlpYrTsd     <-mod[grep("Tmean", rownames(mod)[5:6])+4 , "sd"]
  
  
  precEstimate = matrix(data=NA,ncol=10000,nrow=length(scprec_value))
  tempEstimate = matrix(data=NA,ncol=10000,nrow=length(sctemp_value))
  #rownames(trendEstimate)<- scCrop_value
  for(c in 1: length (scprec_value)){
    for(i in 1:10000){
      precEstimate[c,i]  = rnorm(1,SlpYrmean, SlpYrSd) + scprec_value[c] * rnorm(1,SlpYrPrecmean,SlpYrPrecsd)
      tempEstimate[c,i] = rnorm(1,SlpYrmean, SlpYrSd) + sctemp_value[c]  * rnorm(1,SlpYrTmean, SlpYrTsd)
      
    }
  }
  
  mns<- apply(precEstimate, 1, mean)  
  sds<- apply(precEstimate, 1, sd)  
  upperCI<- apply(precEstimate, 1, quantile, probs = c(0.025))
  lowerCI<- apply(precEstimate, 1, quantile, probs = c(0.975))
  precEsts<- data.frame(Realm = realms[k], Process = precVariable, weather = "Precipitation", driver = drivers[k], mns, sds,
                        change = prec_value,   upperCI, lowerCI)
  
  mns<- apply(tempEstimate, 1, mean)  
  sds<- apply(tempEstimate, 1, sd)  
  upperCI<- apply(tempEstimate, 1, quantile, probs = c(0.025))
  lowerCI<- apply(tempEstimate, 1, quantile, probs = c(0.975))
  tempEsts<- data.frame(Realm = realms[k], Process = tempVariable, weather = "Mean temperature", driver = drivers[k],mns, sds, 
                        change = temp_value, upperCI, lowerCI)
  
  res<- rbind(tempEsts, precEsts)
  
  climdf<- rbind(climdf, res)
}




tempPlotCCI<-  ggplot(subset(climdf, Process == "relDeltaTmean"), 
                      aes(x = change, y = mns, color = Realm))+
  geom_line(size = 1.5)+
  geom_ribbon(aes(x = change,  ymin = lowerCI, ymax = upperCI, fill = Realm, color = Realm), alpha = 0.5  )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_fill_manual(values = col.scheme.realm, guide = F)+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  ylab ("Trend slope")+ 
  xlab( expression(atop('Relative ' *Delta* ' mean temperature', 'landscape scale ('*Delta*'T / ' *mu*'T (K))')))+
  ylim(-0.05, 0.05)+ xlim(-0.0032, 0.01)+
  ggtitle  ("A")+
  theme_clean + 
  theme(  strip.background = element_blank(), 
          plot.title = element_text(hjust = 0.5))#,

precPlotCCI<-  ggplot(subset(climdf, Process == "relDeltaPrec"), 
                      aes(x = change, y = mns, color = Realm))+
  geom_line(size = 1.5)+
  geom_ribbon(aes(x = change,  ymin = lowerCI, ymax = upperCI, fill = Realm, color = Realm), alpha = 0.5  )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_fill_manual(values = col.scheme.realm, guide = F)+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  ylab ("")+ 
  xlab( expression(atop('Relative ' *Delta* ' precipitation', 'landscape scale ('*Delta*'T / ' *mu*'T (K))')))+
  ylim(-0.05, 0.05)+ xlim(-0.8, 0.8)+
  ggtitle  ("B")+
  theme_clean + 
  theme(  strip.background = element_blank(), 
          plot.title = element_text(hjust = 0.5))#,


tempPlotCHEL<-  ggplot(subset(climdf, Process == "CHELSArelDeltaTmean"), 
                       aes(x = change, y = mns, color = Realm))+
  geom_line(size = 1.5)+
  geom_ribbon(aes(x = change,  ymin = lowerCI, ymax = upperCI, fill = Realm, color = Realm), alpha = 0.5  )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_fill_manual(values = col.scheme.realm, guide = F)+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  ylab ("Trend slope")+ 
  xlab( expression(atop('Relative ' *Delta* ' mean temperature', 'local scale ('*Delta*'T / ' *mu*'T (K))')))+
  ylim(-0.05, 0.05)+xlim(-0.003, 0.01)+
  ggtitle  ("C")+
  theme_clean + 
  theme(  strip.background = element_blank(), 
          plot.title = element_text(hjust = 0.5))#,


precPlotCHEL<-  ggplot(subset(climdf, Process == "CHELSArelDeltaPrec"), 
                       aes(x = change, y = mns, color = Realm))+
  geom_line(size = 1.5)+
  geom_ribbon(aes(x = change,  ymin = lowerCI, ymax = upperCI, fill = Realm, color = Realm), alpha = 0.5  )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_fill_manual(values = col.scheme.realm, guide = F)+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  ylab ("")+ 
  xlab( expression(atop('Relative ' *Delta* ' precipitation', 'local scale ('*Delta*'T / ' *mu*'T (K))')))+
  ylim(-0.05, 0.05)+ xlim(-0.8, 0.8)+
  ggtitle  ("D")+
  theme_clean + 
  theme(  strip.background = element_blank(), 
          plot.title = element_text(hjust = 0.5))#,


grid.arrange(tempPlotCCI, precPlotCCI, 
             tempPlotCHEL,precPlotCHEL, 
             nrow = 2)


  

# Fig S 6 land cover representation #####

setwd("C:\\Dropbox\\Insect Biomass Trends/csvs")


load("globalurbanDF.RData")
load("globalcropDF.RData")
load("globalforestDF.RData")
load("LU.RData")
LU <- LU[ LU$Plot_ID %in% unique(completeData$Plot_ID) , ]

col.scheme.global<- c(  "Global"  = "grey10", "Observed" = "grey70")  #coral4
col.scheme.black<- c(  "Global"  = "black", "Observed" = "black")  #coral4


str(globalurbanDF)
str(globalcropDF)




hstUrbGlob<-hist(globalurbanDF$X1152)
hstUrbGlob$rel<- hstUrbGlob$counts/nrow(globalurbanDF)
hstUrbObs<-hist(LU$End_urbanArea)
hstUrbObs$rel<- hstUrbObs$counts/nrow(LU)


dfGlob<-data.frame(
  variable = "Global",
  data = globalurbanDF$X1152 [!is.na(globalurbanDF$X1152)],
  n = length(globalurbanDF$X1152 [!is.na(globalurbanDF$X1152)]))
dfObs<- data.frame(
  variable = "Observed", 
  data = LU$End_urbanArea , 
  n = length(LU$End_urbanArea))

urbPlot<- ggplot() + 
  geom_histogram(data = dfGlob, aes( y=..count../max(..count..)*100,x=data,fill = "Global"),
                 color = "black",   alpha = 0.7,binwidth = 0.02)+
  geom_histogram(data = dfObs,  aes( y=..count../max(..count..)*100,x=data,fill = "Observed"),
                 color = "black",   alpha = 0.4,binwidth = 0.02) +
  scale_fill_manual(values = col.scheme.global)+scale_color_manual (values = col.scheme.black)+
  expand_limits(x=1, y=0) +
  scale_x_continuous(expand = c(0, 0.01) ) + scale_y_continuous(expand = c(0, 0))+
  ggtitle ("Urban cover in 2002") + 
  xlab ("") + ylab ("")+ #Number of cells/mean number of cells
  guides(fill  = guide_legend(title="Data \ndistribution"))   +
  theme_clean +
  theme(legend.position= c( 0.85, 0.35), legend.title = element_text(size = 8), 
        legend.text = element_text(size = 8))



logurbPlot<- ggplot() + 
  geom_histogram(data = dfGlob, aes( y=..count../max(..count..)*100,x=data,fill = "Global"),
                 color = "black",   alpha = 0.7,binwidth = 0.02)+
  geom_histogram(data = dfObs,  aes( y=..count../max(..count..)*100,x=data,fill = "Observed"),
                 color = "black",   alpha = 0.4,binwidth = 0.02)+
  scale_fill_manual(values = col.scheme.global)+scale_color_manual (values = col.scheme.black)+
  expand_limits(x=1, y=0) +
  scale_x_continuous(expand = c(0, 0.01) ) + 
  scale_y_log10 (expand = c(0, 0)) +
  ggtitle (expression(Log["10"]*' urban cover in 2002')) + 
  guides(fill=guide_legend(title="Data distribution")) +
  xlab ("") + ylab ("")+
  theme_clean + theme(legend.position= "")







# Cropland density plots 
hist(globalcropDF$layer.103 )

dfGlobCrop<-data.frame(
  variable = "Global",
  data = globalcropDF$layer.103[!is.na(globalcropDF$layer.103)] )
dfObsCrop<- data.frame(
  variable = "Observed", 
  data = LU$End_cropArea)

cropPlot<-ggplot() + 
  geom_histogram(data = dfGlobCrop, aes( y=..count../max(..count..)*100,x=data,fill = "Global"),
                 color = "black",   alpha = 0.7,binwidth = 0.02)+
  geom_histogram(data = dfObsCrop,  aes( y=..count../max(..count..)*100,x=data,fill = "Observed"),
                 color = "black", alpha = 0.4,binwidth = 0.02)+
  scale_fill_manual(values = col.scheme.global)+scale_color_manual (values = col.scheme.black)+
  expand_limits(x=1, y=0) +
  scale_x_continuous(expand = c(0, 0.01) ) + scale_y_continuous(expand = c(0, 0))+
  ggtitle ("Crop cover in 2002") + 
  xlab ("") + ylab ("Relative frequency")+
  guides(fill=guide_legend(title="Data distribution")) +
  theme_clean +theme(legend.position= "")



logcropPlot<-ggplot() + 
  geom_histogram(data = dfGlobCrop, aes( y=..count../max(..count..)*100,
                                         x=data,fill = "Global"),
                 color = "black" ,     alpha = 0.7,binwidth = 0.02)+
  geom_histogram(data = dfObsCrop,  aes( y=..count../max(..count..)*100,x=data,fill = "Observed"),
                 color = "black" , alpha = 0.4,binwidth = 0.02)+
  scale_fill_manual(values = col.scheme.global)+scale_color_manual (values = col.scheme.black)+
  expand_limits(x=1, y=0) +
  scale_y_log10(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0.01)) +
  ggtitle (expression(Log["10"]*' crop cover in 2002')) + 
  xlab ("") + ylab ("")+
  guides(fill=guide_legend(title="Data distribution")) +
  theme_clean +
  theme(legend.position= "")





dfGlobFor<-data.frame(
  variable = "Global",
  data = globalforestDF$layer.103[!is.na(globalforestDF$layer.103)] )
dfObsFor<- data.frame(
  variable = "Observed", 
  data = LU$End_forestArea)

forestPlot<- ggplot() + 
  geom_histogram(data = dfGlobFor, aes( y=..count../max(..count..)*100,
                                        x=data,fill = "Global"),
                 color = "black" ,     alpha = 0.7,binwidth = 0.02)+
  geom_histogram(data = dfObsFor,  aes( y=..count../max(..count..)*100,
                                        x=data,fill = "Observed"),
                 color = "black" ,     alpha = 0.4,binwidth = 0.02)+
  scale_fill_manual(values = col.scheme.global)+scale_color_manual (values = col.scheme.black)+
  scale_x_continuous(expand = c(0, 0.01)) + scale_y_continuous(expand = c(0, 0))+
  #scale_y_log10() +
  ggtitle ("Forest cover in 2002") + 
  xlab (bquote('Fraction cover in 25 km'^2)) + ylab ("")+
  guides(fill=guide_legend(title="Data distribution")) +
  theme_clean +  theme(legend.position= "")


logforestPlot <-  ggplot() + 
  geom_histogram(data = dfGlobFor, aes( y=..count../max(..count..)*100,
                                        x=data,
                                        fill = "Global"), 
                 color = "black", alpha = 0.7, binwidth = 0.02)+
  geom_histogram(data = dfObsFor,  aes( y=..count../max(..count..)*100,
                                        x=data,fill = "Observed"),
                 color = "black" ,     alpha = 0.4,binwidth = 0.02)+
  scale_fill_manual(values = col.scheme.global)+scale_color_manual (values = col.scheme.black)+
  scale_x_continuous(expand = c(0, 0.01)) +
  scale_y_log10(expand = c(0, 0)) +
  ggtitle (expression(Log["10"]*' forest cover in 2002')) + 
  xlab (bquote('Fraction cover in 25 km'^2)) + ylab ("")+
  theme_clean +  theme(legend.position= "")




library(gridExtra)
grid.arrange(urbPlot, logurbPlot, cropPlot, logcropPlot, forestPlot, logforestPlot, 
             nrow = 3)



















# Trash #####







# random effects and overall error with narrower prior 
prec.prior <- list(prec = list(prior = "loggamma", param = c(1, 0.5)))
inlaFprior0.5 <- inla(log10(Number+1) ~ cYear:Realm+ Realm + 
                        f(Period_4INLA,model='iid', hyper = prec.prior)+ # prior for random effects 
                        f(Location_4INLA,model='iid', hyper = prec.prior)+
                        f(Plot_ID_4INLA,model='iid', hyper = prec.prior)+
                        f(Datasource_ID_4INLA,model='iid', hyper = prec.prior)+
                        f(Plot_ID_4INLAR,iYear,model='iid', hyper = prec.prior)+
                        f(Location_4INLAR,iYear,model='iid', hyper = prec.prior) +
                        f(Datasource_ID_4INLAR,iYear,model='iid', hyper = prec.prior)+
                        f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                      control.family = list(
                        hyper = list(
                          prec = list( prior = "loggamma", param = c(1,0.5)))), # this is the prior for the error
                      control.predictor = list(link = 1) ,
                      control.compute = list(dic=TRUE,waic=TRUE),
                      data=completeData) 

load("inlaFprior0.5-5651182.RData")
summary(inlaFprior0.5)

prec.prior <- list(prec = list(prior = "loggamma", param = c(0.1, 0.1)))
inlaFprior0.1 <- inla(log10(Number+1) ~ cYear:Realm+ Realm + 
                        f(Period_4INLA,model='iid', hyper = prec.prior)+ # prior for random effects 
                        f(Location_4INLA,model='iid', hyper = prec.prior)+
                        f(Plot_ID_4INLA,model='iid', hyper = prec.prior)+
                        f(Datasource_ID_4INLA,model='iid', hyper = prec.prior)+
                        f(Plot_ID_4INLAR,iYear,model='iid', hyper = prec.prior)+
                        f(Location_4INLAR,iYear,model='iid', hyper = prec.prior) +
                        f(Datasource_ID_4INLAR,iYear,model='iid', hyper = prec.prior)+
                        f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                      control.family = list(
                        hyper = list(
                          prec = list( prior = "loggamma", param = c(0.1,0.1)))), # this is the prior for the error
                      control.predictor = list(link = 1) ,
                      control.compute = list(dic=TRUE,waic=TRUE),
                      data=completeData) 



load("E:/inlaFprior0.5-5434477.RData")
load("E:/inlaFprior0.1-5434916.RData")
load("E:/inlaF-5433204.RData") # load normal file
summary(inlaFprior0.5)# no effects left . prior is too narrow 
summary(inlaFprior0.1)
summary(inlaF)



#excluding plos with only 2 yrs data 
library(tidyverse)
plotDuration<- completeData %>% 
  group_by(Plot_ID) %>%  summarise(
    Datasource_ID = unique(Datasource_ID),
    Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1  , 
    yrsData =   length(unique(Year[!is.na(Number)]   ))  )

