# scripts for alternative dataset runs: 

rm(list=ls()) 

setwd("C:\\Dropbox\\Dropbox\\Insect Biomass Trends/csvs") # work


load("completeData.Rdata")
load("completeDataArth.RData")
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



# subsampling : excluding plos with only 2 yrs data 
library(tidyverse)
plotDuration<- completeData %>% 
  group_by(Plot_ID) %>%  summarise(
    Datasource_ID = unique(Datasource_ID),
    Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1  , 
    yrsData =   length(unique(Year[!is.na(Number)]   ))  )



incompleteData<- merge(completeData, plotDuration)
incompleteData<- incompleteData[incompleteData$yrsData >2, ]

inlaSubset <- inla(log10(Number+1) ~ cYear:Realm + Realm + 
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Location_4INLAR,iYear,model='iid')                      +
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
               data=incompleteData, verbose = F)

# not good







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

realmSlope<- inlaFArealm$summary.fixed[ 2:3,]
rownames(realmSlope)<- c("Freshwater" , "Terrestrial" )

ggplot(data.frame(realmSlope))+
  geom_crossbar(aes(x=rownames(realmSlope),y=mean, fill = rownames(realmSlope),
                    ymin=X0.025quant,ymax=X0.975quant),position="dodge", width = 0.7)+
  scale_fill_manual(values = col.scheme.realm, guide=FALSE)+
  coord_flip()+  ylim(-0.01, 0.02)+  xlab ("")+
  geom_hline(yintercept=0,linetype="dashed")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Realms")







 # Strata

inlaFAstrat <- inla(log10(Number+1) ~ cYear: Realm:Stratum + Realm + Stratum + 
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
stratSlope<- inlaFAstrat$summary.fixed[9:14,]
vars<-data.frame(do.call(rbind, strsplit(rownames(stratSlope), split = ":")))
stratSlope<-cbind(stratSlope, vars)
stratSlope$X1<-gsub("Stratum", "", stratSlope$X1)
stratSlope<- merge(stratSlope, metadata_strata, by.x = "X1", by.y = "Stratum")
stratSlope$text = paste0("(", stratSlope$Datasources, " / ", stratSlope$Plots, ")")
rownames(stratSlope)<-stratSlope$X1
stratSlope$X1<- ordered(stratSlope$X1, levels = c("Water", "Underground" , "Soil surface", "Herb layer", "Trees", "Air" ))

ggplot(data.frame(stratSlope))+
  geom_crossbar(aes(x=X1,y=mean, fill = X1,
                    ymin=X0.025quant,ymax=X0.975quant),position="dodge", width = 0.7, fill = "grey70")+
  #scale_fill_manual(values = col.scheme.strat, guide=FALSE)+
  coord_flip()+
  ylim(-0.015, 0.03)+
  xlab ("")+
  geom_hline(yintercept=0,linetype="dashed")+
 # geom_text(aes(x = X1 , y = 0.027, label = text), size = 3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



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
contSlope<- inlaFcont$summary.fixed[8:19,]
vars<-data.frame(do.call(rbind, strsplit(rownames(contSlope), split = ":")))
contSlope<-cbind(contSlope, vars)
contSlope$Realm<-gsub("Realm", "", contSlope$X1);  contSlope$Continent<-gsub("Continent", "", contSlope$X2)
contSlope<- merge(contSlope, metadata_cont)
contSlope$text = paste0("(", contSlope$Datasources, " / ", contSlope$Plots, ")")
contSlope$Continent<- ordered(contSlope$Continent, levels = rev(c("Europe", "North America" , "Asia", "Latin America", "Australia", "Africa" )))

ggplot(data.frame(subset(contSlope, Continent != "Africa"   )))+ # exclude africa,bc it has too wide CI's 
  geom_crossbar(aes(x=Continent,   y=mean, fill = Realm, 
                    ymin=X0.025quant,ymax=X0.975quant),position="dodge",alpha=0.8 ,  width =0.7)+
  scale_fill_manual(values = col.scheme.realm)+
  geom_hline(yintercept=0,linetype="dashed") +
  xlab ("")+ ylab("Slope")+
  # geom_text(aes(x = Continent , y = 0.028, label = text, fill = Realm),  
  #          position = position_dodge(width = 1), size = 3, color = 1) +
  coord_flip()+
  scale_y_continuous(breaks = c(-0.02, -0.01, 0,0.01, 0.02)) +
  ylim(-0.025, 0.03)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key=element_blank()) +
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

biomSlope<- inlaFAbiom$summary.fixed[6:13,]
vars<-data.frame(do.call(rbind, strsplit(rownames(biomSlope), split = ":")))
biomSlope<-cbind(biomSlope, vars)
biomSlope$Realm<-gsub("Realm", "", biomSlope$X1);  biomSlope$BiomeCoarse<-gsub("BiomeCoarse", "", biomSlope$X2)
biomSlope$Biome <-biomSlope$BiomeCoarse
biomSlope<- merge(biomSlope, metadata_biom)
biomSlope$text = paste0("(", biomSlope$Datasources, " / ", biomSlope$Plots, ") ")
biomSlope$Biome<- ordered(biomSlope$Biome, levels = rev(c("Boreal/Alpine", "Temperate" , "Drylands", "Tropical"  )))

ggplot(data.frame(biomSlope))+
  geom_crossbar(aes(x=Biome,   y=mean, fill = Realm,
                    ymin=X0.025quant,ymax=X0.975quant),position="dodge",alpha=0.8 ,  width =0.7)+
  scale_fill_manual(values = col.scheme.realm)+
  xlab ("")+ ylab ("Slope")+geom_hline(yintercept=0,linetype="dashed")+
  coord_flip()+
  scale_y_continuous(breaks = c(-0.02, -0.01, 0,0.01, 0.02)) +
  ylim(-0.03, 0.032)+  
 # geom_text(aes(x = Biome , y = 0.027, fill = Realm,  label = text), position = position_dodge(width = 1), size = 3, color = 1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black") , 
        legend.key=element_blank())+
  ggtitle("Biomes")





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

qplot(abundance,biomass, data=all.aggrAB.test,colour=as.factor(Plot_ID))+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~Datasource_name,scales="free")+
  theme(legend.position="none") +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"))

#################################################################################

# Tables Drivers for DIC comparisons #####


# protected areas
PAmodels<- data.frame(modelName=(character()),
                      fixedEffects=character(), 
                      DIC=numeric(),
                      WAIC=numeric(),
                      stringsAsFactors=FALSE) 


load("E:/inlaFpaInt.RData")
PAmodels[1,1]<- "3way"   
PAmodels[1,2]<- "Year: PA:Realm"
PAmodels[1,3]<- summary(inlaFpaInt)$dic$dic
PAmodels[1,4]<- summary(inlaFpaInt)$waic$waic


load("E:/inlaFpa.RData")
PAmodels[2,1]<- "2way"   
PAmodels[2,2]<- "Year:PA + PA:Realm"
PAmodels[2,3]<- summary(inlaFpa)$dic$dic
PAmodels[2,4]<- summary(inlaFpa)$waic$waic


load("E:/inlaFpa2.RData")
PAmodels[3,1]<- "additive"   
PAmodels[3,2]<- "Year:Realm + PA"
PAmodels[3,3]<- summary(inlaFpa2)$dic$dic
PAmodels[3,4]<- summary(inlaFpa2)$waic$waic


PAmodels[4,1]<- "none"   
PAmodels[4,2]<- "Year:Realm "
PAmodels[4,3]<- summary(inlaF)$dic$dic
PAmodels[4,4]<- summary(inlaF)$waic$waic

PAmodels

# plot for PA's
metadata_pa<-  completeData %>% 
  group_by( Realm,PA) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)))
metadata_pa

paSlope<- inlaFpaInt$summary.fixed[4:7,]
vars<-data.frame(do.call(rbind, strsplit(rownames(paSlope), split = ":")))
paSlope<-cbind(paSlope, vars)
paSlope$PA<-gsub("PA", "", paSlope$X1);  paSlope$Realm<-gsub("Realm", "", paSlope$X2)
paSlope<- merge(paSlope, metadata_pa)
paSlope$text = paste0("(", paSlope$Datasources, " / ", paSlope$Plots, ")")
paSlope$PA[paSlope$PA == "yes"  ]<-"Protected area" ;  paSlope$PA[paSlope$PA == "no"  ]<-"Not protected area"

ggplot(data.frame(paSlope))+
  geom_crossbar(aes(x=Realm,   y=mean, fill = PA,
                    ymin=X0.025quant,ymax=X0.975quant), width = 0.7, position="dodge")+
  scale_fill_manual(values = col.scheme.pa)+
  coord_flip()+
  geom_hline(yintercept=0,linetype="dashed")+
  ylim(-0.01, 0.02)+  xlab ("")+ ylab ("Slope")+
  geom_text(aes(x = Realm , y = 0.017, fill = PA,  label = text), size = 4, position = position_dodge(width = 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.key=element_blank()) + 
   guides(fill=guide_legend(title=NULL))


######################################################################################
# Crop

cropModels <- data.frame(modelName=(character()),
                         fixedEffects=character(), 
                         DIC=numeric(),
                         WAIC=numeric(),
                         stringsAsFactors=FALSE)

load("E:/inlaFcrop 3way.RData")
cropModels[1,1] <-"3way"
cropModels[1,2] <- "Realm*cYear*cropArea"
cropModels[1,3] <-summary(inlaFcropTRY)$dic$dic
cropModels[1,4] <-summary(inlaFcropTRY)$waic$waic

load("E:/inlaFcrop1 only2ways.RData")
cropModels[2,1] <-"2way"
cropModels[2,2] <- "cYear* Realm + cYear*cropArea"
cropModels[2,3] <-summary(inlaFcrop1)$dic$dic
cropModels[2,4] <-summary(inlaFcrop1)$waic$waic

load("E:/inlaFcrop2.RData")
cropModels[3,1] <-"additive"
cropModels[3,2] <- "Year* Realm + cropCover"
cropModels[3,3] <-summary(inlaFcrop2)$dic$dic
cropModels[3,4] <-summary(inlaFcrop2)$waic$waic

cropModels[4,1] <-"none"
cropModels[4,2] <- "Year* Realm "
cropModels[4,3] <-summary(inlaF)$dic$dic
cropModels[4,4] <-summary(inlaF)$waic$waic

cropModels


#################################################################################
# Urban

urbanModels <- data.frame(modelName=(character()),
                          fixedEffects=character(), 
                          DIC=numeric(),
                          WAIC=numeric(),
                          stringsAsFactors=FALSE) 

load("E:/inlaFurban.RData")
     
urbanModels[1,1] <-"3way"
urbanModels[1,2] <- "Realm*cYear*urbanArea"
urbanModels[1,3] <-summary(inlaFurban)$dic$dic
urbanModels[1,4] <-summary(inlaFurban)$waic$waic


load("E:/inlaFurban1 2way inter.RData")

urbanModels[2,1] <-"2way"
urbanModels[2,2] <- "Year*UrbanCover + Realm*Year"
urbanModels[2,3] <-summary(inlaFurban1)$dic$dic
urbanModels[2,4] <-summary(inlaFurban1)$waic$waic

load("E:/inlaFurban2 no inter.RData")


urbanModels[3,1] <-"additive"
urbanModels[3,2] <- "Realm*Year +UrbanCover"
urbanModels[3,3] <-summary(inlaFurban2)$dic$dic
urbanModels[3,4] <-summary(inlaFurban2)$waic$waic

urbanModels[4,1] <-"none"
urbanModels[4,2] <- "Year* Realm "
urbanModels[4,3] <-summary(inlaF)$dic$dic
urbanModels[4,4] <-summary(inlaF)$waic$waic



#######################################################################################


##################################################################################################
# mean Temperature change

meanTModels <- data.frame(modelName=(character()),
                          fixedEffects=character(), 
                          DIC=numeric(),
                          WAIC=numeric(),
                          stringsAsFactors=FALSE)

load("E:/inlaFmeanT.RData")
meanTModels[1,1] <-"3way"
meanTModels[1,2] <- "Year*Realm*relDeltaTmean"
meanTModels[1,3] <-summary(inlaFmeanT)$dic$dic
meanTModels[1,4] <-summary(inlaFmeanT)$waic$waic


load("E:/inlaFmeanT1.RData")
meanTModels[2,1] <-"2way"
meanTModels[2,2] <- "Year*Realm*relDeltaTmean"
meanTModels[2,3] <-summary(inlaFmeanT1)$dic$dic
meanTModels[2,4] <-summary(inlaFmeanT1)$waic$waic


load("E:/inlaFmeanT2.RData")
meanTModels[3,1] <-"additive"
meanTModels[3,2] <- "relDeltaTmean+cYear*Realm "
meanTModels[3,3] <-summary(inlaFmeanT2)$dic$dic
meanTModels[3,4] <-summary(inlaFmeanT2)$waic$waic

# dic and waic for simplest model
meanTModels[4,1] <-"none"
meanTModels[4,2] <- "Year* Realm "
meanTModels[4,3] <-summary(inlaF)$dic$dic
meanTModels[4,4] <-summary(inlaF)$waic$waic




###################################################################################
#precipitation

