# scripts for alternative dataset runs: 

rm(list=ls()) 

setwd("C:\\Dropbox\\Dropbox\\Insect Biomass Trends/csvs") # work


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



# robustness checks

# effects of the priors

# random effects and overall error with narrower prior 
inlaFprior0.5 <- inla(log10(Number+1) ~ cYear:Realm+ Realm + 
                        f(Period_4INLA,model='iid', hyper = list(prec= list(prior = "loggamma", param  = c(1,0.5))))+ # prior for random effects 
                        f(Location_4INLA,model='iid', hyper = list(prec= list(prior = "loggamma", param  = c(1,0.5))))+
                        f(Plot_ID_4INLA,model='iid', hyper = list(prec= list(prior = "loggamma", param  = c(1,0.5))))+
                        f(Datasource_ID_4INLA,model='iid', hyper = list(prec= list(prior = "loggamma", param  = c(1,0.5))))+
                        f(Plot_ID_4INLAR,iYear,model='iid', hyper = list(prec= list(prior = "loggamma", param  = c(1,0.5))))+
                        f(Location_4INLAR,iYear,model='iid', hyper = list(prec= list(prior = "loggamma", param  = c(1,0.5)))) +
                        f(Datasource_ID_4INLAR,iYear,model='iid', hyper = list(prec= list(prior = "loggamma", param  = c(1,0.5))))+
                        f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                      control.family = list(
                        hyper = list(
                          prec = list( prior = "loggamma", param = c(1,0.5)))), # this is the prior for the error
                      control.compute = list(dic=TRUE,waic=TRUE),
                      data=completeData) 


load("E:/inlaFprior0.5-5434477.RData")
load("E:/inlaFprior0.1-5434916.RData")
load("E:/inlaF-5433204.RData") # load normal file
summary(inlaFprior0.5)# no effects left . prior is too narrow 
summary(inlaFprior0.1)
summary(inlaF)



# differences between biomass and abundance data (uses dataframe with both Units for datasets that have both)
metadata_AB<-  completeDataAB %>% 
  group_by( Realm, Unit) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)))
metadata_AB



# abundance vs biomass (overall model)
inla1.AB <- inla(log10(Number+1) ~ cYear: Unit +  Unit +
                   f(Period_4INLA,model='iid')+
                   f(Plot_ID_4INLA,model='iid')+
                   f(Location_4INLA,model='iid')+
                   f(Datasource_ID_4INLA,model='iid')+
                   f(Plot_ID_4INLAR,iYear,model='iid')+
                   f(Location_4INLAR,iYear,model='iid')                      +
                   f(Datasource_ID_4INLAR,iYear,model='iid')+
                   f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                 control.compute = list(dic=TRUE,waic=TRUE),
                 data=completeDataAB) # 
save(inla1.AB, file = "/data/Roel/inla1.AB.RData")

load("E:/inlaABmodels-5429694.RData")
summary(inla1.AB) # biomass is actually positive! This is maybe due to the FW datasets (fw = 21 vs terr =11 datasets)

# abundance vs biomass and realm
inla1.ABrealm <- inla(log10(Number+1) ~ cYear: Realm: Unit +  Unit +Realm +
                        f(Period_4INLA,model='iid')+
                        f(Plot_ID_4INLA,model='iid')+
                        f(Location_4INLA,model='iid')+
                        f(Datasource_ID_4INLA,model='iid')+
                        f(Plot_ID_4INLAR,iYear,model='iid')+
                        f(Location_4INLAR,iYear,model='iid')+
                        f(Datasource_ID_4INLAR,iYear,model='iid')+
                        f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                      control.compute = list(dic=TRUE,waic=TRUE),
                      data=completeDataAB) # 
save(inla1.ABrealm, file = "/data/Roel/inla1.ABrealm.RData")

#graph
load("E:/inlaABrealm-5429693.RData")
summary(inla1.ABrealm)
data.frame(
  var =   rownames(inla1.ABrealm$summary.fixed)[4:7], 
  mean = (10^(inla1.ABrealm$summary.fixed[4:7,1] )-1)  *100, # proportional changes per year
  CI2.5 = (10^(inla1.ABrealm$summary.fixed[4:7,3] )-1 ) *100,#0.025 CI
  CI97.5 = (10^(inla1.ABrealm$summary.fixed[4:7,5] )-1 ) *100# 0.975
)
10^(inla1.ABrealm$summary.fixed[4:7,1] *10)-1 # proportional changes per decade



ABSlope<- inla1.ABrealm$summary.fixed[4:7,]
vars<-data.frame(do.call(rbind, strsplit(rownames(ABSlope), split = ":")))
ABSlope<-cbind(ABSlope, vars)
ABSlope$Realm<-gsub("Realm", "", ABSlope$X2)
ABSlope$Unit<-gsub("Unit", "", ABSlope$X1)
ABSlope$AB <-ABSlope$Unit
ABSlope<- merge(ABSlope, metadata_AB)
ABSlope$text = paste0("(", ABSlope$Datasources, " | ", ABSlope$Plots, ") ")


brks<- c(-0.010, -0.005, 0, 0.005, 0.01, 0.015)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
e<- c("","","","","","","")


ABplot<- ggplot(data.frame(ABSlope))+
  geom_crossbar(aes(x=Realm,   y=mean, fill = Unit,
                    ymin=X0.025quant , ymax=X0.975quant), position="dodge",alpha=0.8 ,  width =0.7)+
  geom_hline(yintercept=0,linetype="dashed")+
  coord_flip()+
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.01, 0.024))+
  xlab ("")+ ylab("Trend slope  \n % change per year")+
  geom_text(aes(x = Realm , y = 0.020, fill = Unit,  label = text), position = position_dodge(width = 1), size = 3, color = 1) +
  scale_fill_manual(name="Unit",
                    breaks=c("biomass", "abundance"),
                    labels=c("Biomass", "Abundance"), 
                    values = col.scheme.AB) + 
  theme_clean

png("AB plot.png", width=2000, height=700, res = 360)
PAplot
dev.off()



#############
# test different subsets


# Excluding North America

completeData1<- subset(completeData, Continent != "North America")
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
                          data=completeData1)

load("E:/inlaRealmExclNAm-5429691.RData")

metadata_realm.ExclNa<-  completeData1 %>% 
  group_by(Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID))) 

realmSlope<- inlaFrealmExclNAm $summary.fixed[ 3:4,]
rownames(realmSlope)<- c("Freshwater" , "Terrestrial" )

realmPlot<-ggplot(data.frame(realmSlope))+
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


# only europe and NA 

completeData2<- subset(completeData, Continent == "North America" | Continent == "Europe")
# almost half of the data out

inlaFrealmNAmEur <- inla(log10(Number+1) ~ cYear: Realm+ Realm  +
                            f(Period_4INLA,model='iid')+
                            f(Location_4INLA,model='iid')+
                            f(Plot_ID_4INLA,model='iid')+
                            f(Datasource_ID_4INLA,model='iid')+
                            f(Plot_ID_4INLAR,iYear,model='iid')+
                            f(Location_4INLAR,iYear,model='iid')                      +
                            f(Datasource_ID_4INLAR,iYear,model='iid')+
                            f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                          control.compute = list(dic=TRUE,waic=TRUE),
                          data=completeData2)
#still sign 


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

completeDataLA<- subset(completeData,  Continent == "Latin America")
inlaFrealmLA <- inla(log10(Number+1) ~ cYear: Realm+ Realm  +
                       f(Period_4INLA,model='iid')+
                       f(Location_4INLA,model='iid')+
                       f(Plot_ID_4INLA,model='iid')+
                       f(Datasource_ID_4INLA,model='iid')+
                       f(Plot_ID_4INLAR,iYear,model='iid')+
                       f(Location_4INLAR,iYear,model='iid')                      +
                       f(Datasource_ID_4INLAR,iYear,model='iid')+
                       f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                     control.compute = list(dic=TRUE,waic=TRUE),
                     data=completeDataLA)

completeDataAs<- subset(completeData,  Continent == "Asia")
inlaFrealmAs <- inla(log10(Number+1) ~ cYear: Realm+ Realm  +
                       f(Period_4INLA,model='iid')+
                       f(Location_4INLA,model='iid')+
                       f(Plot_ID_4INLA,model='iid')+
                       f(Datasource_ID_4INLA,model='iid')+
                       f(Plot_ID_4INLAR,iYear,model='iid')+
                       f(Location_4INLAR,iYear,model='iid')                      +
                       f(Datasource_ID_4INLAR,iYear,model='iid')+
                       f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                     control.compute = list(dic=TRUE,waic=TRUE),
                     data=completeDataAs)



completeDataAU<- subset(completeData,  Continent == "Australia")
inlaFrealmAU <- inla(log10(Number+1) ~ cYear: Realm+ Realm  +
                       f(Period_4INLA,model='iid')+
                       f(Location_4INLA,model='iid')+
                       f(Plot_ID_4INLA,model='iid')+
                       f(Datasource_ID_4INLA,model='iid')+
                       f(Plot_ID_4INLAR,iYear,model='iid')+
                       f(Location_4INLAR,iYear,model='iid')                      +
                       f(Datasource_ID_4INLAR,iYear,model='iid')+
                       f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                     control.compute = list(dic=TRUE,waic=TRUE),
                     data=completeDataAU)







##########
#excluding plos with only 2 yrs data 
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





###################################################################################################

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

# Tables Drivers for DIC comparisons #####

names(all.results)

urbanizationFW <- cbind(Realm = "Freshwater",  all.results$Changes_LUH2_FW[5:6,], variable = rownames(all.results$Changes_LUH2_FW[5:6,]))
urbanizationTer<- cbind(Realm = "Terrestrial", all.results$Changes_LUH2_Terr[5:6,], variable = rownames(all.results$Changes_LUH2_Terr[5:6,]) )
cover25kmFW    <- cbind(Realm = "Freshwater",  (all.results$Landuse_LUH2_FW)[5:6,], variable = rownames(all.results$Landuse_LUH2_FW[5:6,])  )
cover25kmTer   <- cbind(Realm = "Terrestrial", all.results$Landuse_LUH2_Terrestrial[5:6,], variable = rownames(all.results$Landuse_LUH2_Terrestrial[5:6,])  )
cover0.81FW    <- cbind(Realm = "Freshwater", all.results$Landuse_ESA_FW  [5:6,], variable = rownames(all.results$Landuse_ESA_FW[5:6,])    )
cover0.81Ter   <- cbind(Realm = "Terrestrial",all.results$Landuse_ESA_terr[5:6,], variable = rownames(all.results$Landuse_ESA_terr[5:6,])     )
deltaFwCRU     <- cbind(Realm = "Freshwater", all.results$relClimate_Change_CRU_FW[5:6,], variable = rownames(all.results$relClimate_Change_CRU_FW[5:6,])  )
deltaTerCRU    <- cbind(Realm = "Terrestrial",all.results$relClimate_Change_CRU_Terr[5:6,], variable = rownames(all.results$relClimate_Change_CRU_Terr[5:6,]) ) 
deltaFwCHELSA  <- cbind(Realm = "Freshwater", all.results$relClimate_Change_CHELSA_FW[5:6,], variable = rownames(all.results$relClimate_Change_CHELSA_FW[5:6,])  )
deltaTerCHELSA <- cbind(Realm = "Terrestrial",all.results$relClimate_Change_CHELSA_Terr[5:6,], variable = rownames(all.results$relClimate_Change_CHELSA_Terr[5:6,]) )


   
drivers<- rbind(urbanizationTer, urbanizationFW, cover25kmFW   , cover25kmTer, cover0.81Ter, cover0.81FW, 
             deltaTerCRU, deltaFwCRU, deltaTerCHELSA, deltaFwCHELSA)
drivers$scale <- NA
drivers$scale [drivers$mean + drivers$`0.975quant` <0.5]<-"small"
drivers$scale [drivers$mean + drivers$`0.975quant` >0.5]<-"mid"
drivers$scale [drivers$mean + drivers$`0.975quant` >5]<-"large"


# rename variables 

drivers$name <- NA
drivers$name[drivers$variable == "cYear:urbanization"] <- "Change in urban cover per 625 km2"
drivers$name[drivers$variable == "cYear:cropification"] <- "Change in cropland cover per 625 km2"
drivers$name[drivers$variable == "cYear:sqrt(End_cropArea)"] <- "sqrt fraction Cropland cover per 625 km2 at end"
drivers$name[drivers$variable == "cYear:sqrt(End_urbanArea)"] <- "sqrt fraction Urban cover per 625 km2 at end"
drivers$name[drivers$variable == "cYear:frcCrop900m"] <- "sqrt fraction Cropland cover per 0.81 km2 at end"
drivers$name[drivers$variable == "cYear:frcUrban900m"] <- "sqrt fraction Urban cover per 0.81 km2 at end"
drivers$name[drivers$variable == "cYear:relDeltaTmean"] <- "Relative change in mean temperature per 0.25 degr2 at end"
drivers$name[drivers$variable == "cYear:relDeltaPrec"] <- "Relative change in monthly precipitation per 0.25 degr2 at end"
drivers$name[drivers$variable == "cYear:CHELSArelDeltaTmean"] <- "Relative change in mean temperature per 1 km2 at end"
drivers$name[drivers$variable == "cYear:CHELSArelDeltaPrec"] <- "Relative change in monthly precipitation per 1 km2 at end"

drivers$name<- factor(drivers$name, levels = rev(c( "Change in urban cover per 625 km2", 
                                                   "Change in cropland cover per 625 km2", 
                                                   "sqrt fraction Urban cover per 625 km2 at end", 
                                                   "sqrt fraction Cropland cover per 625 km2 at end", 
                                                   "sqrt fraction Urban cover per 0.81 km2 at end",
                                                   "sqrt fraction Cropland cover per 0.81 km2 at end",
                                                   "Relative change in mean temperature per 0.25 degr2 at end",
                                                   "Relative change in monthly precipitation per 0.25 degr2 at end",
                                                   "Relative change in mean temperature per 1 km2 at end" ,
                                                   "Relative change in monthly precipitation per 1 km2 at end")))                                              

sml<- ggplot( subset(drivers, scale =="small")   ) + 
   geom_crossbar(aes(x=name,   y=mean, fill = Realm,
                    ymin=`0.025quant` ,ymax= `0.975quant` ), position="dodge",alpha=1 ,  width =0.7)+
 scale_fill_manual(values = col.scheme.realm)+
  xlab ("")+ ylab ("")+geom_hline(yintercept=0,linetype="dashed")+
  coord_flip()+
   theme_clean + 
  theme(legend.position = "", 
        strip.text = element_blank(), 
        strip.background = element_blank())+
  facet_wrap(.~scale, scales = "free", nrow = 3)

md<- ggplot( subset(drivers, scale =="mid")   ) + 
  geom_crossbar(aes(x=name,   y=mean, fill = Realm,
                    ymin=`0.025quant` ,ymax= `0.975quant` ), position="dodge",alpha=1 ,  width =0.7)+
  scale_fill_manual(values = col.scheme.realm)+
  xlab ("")+ ylab ("")+geom_hline(yintercept=0,linetype="dashed")+
  coord_flip()+
  theme_clean + 
  theme(legend.position = "", 
        strip.text = element_blank(), 
        strip.background = element_blank())+
  facet_wrap(.~scale, scales = "free", nrow = 3)

lrg<- ggplot( subset(drivers, scale =="large")   ) + 
  geom_crossbar(aes(x=name,   y=mean, fill = Realm,
                    ymin=`0.025quant` ,ymax= `0.975quant` ), position="dodge",alpha=1 ,  width =0.7)+
  scale_fill_manual(values = col.scheme.realm)+
  ylab ("Model estimate")+ xlab ("")+geom_hline(yintercept=0,linetype="dashed")+
  coord_flip()+
  theme_clean + 
  theme(legend.position = "", 
        strip.text = element_blank(), 
        strip.background = element_blank())+
  facet_wrap(.~scale, scales = "free", nrow = 3)


library(gridExtra)
grid.arrange(sml, md, lrg, nrow = 3,  heights = c(7,3, 2.2) )




