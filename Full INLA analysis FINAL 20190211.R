rm(list=ls()) 

library(INLA)
library(ggplot2)
library(ggnewscale)
library(tidyverse)
library(plyr)


setwd("C:/Users/roelv/Dropbox/Insect Biomass Trends/csvs") # home
setwd("C:\\Dropbox\\Dropbox\\Insect Biomass Trends/csvs") # work
setwd("~/Dropbox/Insect Biomass Trends/csvs")#diana mac


load("completeData.Rdata")
load("completeDataArth.RData")
load("completeDataClim.RData")
load("completeDataClimPrec.RData")
load("inla1.RData")
completeData$Stratum[completeData$Stratum == "air"]<-"Air"
completeData$Stratum[completeData$Plot_ID == 930 ]<- "Soil surface"


col.scheme.cont<-c( "Europe"="green3", "Latin America"= "magenta", "North America"= "orange","Asia" = "purple3", 
                    "Africa" = "blue", "Australia" = "red")
col.scheme.realm<-c(  "Freshwater"  = "dodgerblue2", "Terrestrial" = "peru")
col.scheme.strat<-c( "Air" = "peru", "Herb layer" = "peru", "Soil surface" = "peru", "Trees" = "peru", 
                     "Underground" = "peru"  ,"Water" = "dodgerblue2")
col.scheme.realm2<- c(  "Freshwater"  = "blue", "Terrestrial" = "brown")
# 1) select most appropriate model

#Decision 1: which random slopes to include?
#Plot or Datasource_ID
#Run model with both random slope and include whichever is largest
#or Both?
inla1.2RS <- inla(log10(Number+1) ~ cYear+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1',replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE,waic=TRUE),
                  data=completeData); beep(2)


# additions 2019.03: add country and location level random slopes? 
inla1.4RS <- inla(log10(Number+1) ~ cYear+
                  f(Period_4INLA,model='iid')+
                  f(Location_4INLA,model='iid')+
                  f(Plot_ID_4INLA,model='iid')+
                  f(Datasource_ID_4INLA,model='iid')+
                  f(Country_State_4INLA) 
                     f(Plot_ID_4INLAR,iYear,copy='Plot_ID_4INLA')+
                     f(Location_4INLAR)                      +
                     f(Datasource_ID_4INLAR,iYear,copy='Datasource_ID_4INLA')+
                     f(Country_State_4INLAR)  +
                  f(iYear,model='ar1',replicate=as.numeric(Plot_ID_4INLA)),
                control.compute = list(dic=TRUE,waic=TRUE),
                data=completeData); beep(2)
save(E:/inla1.4RS)


#load outputs of the 3 models: 
load("~/inla1.datasetRS.RData")
load("~/inla1.2RS.RData")
load("~/inla1.plotRS.RData")

data.frame(inla1.plotRS$dic$dic,  inla1.datasetRS$dic$dic   , inla1.2RS$dic$dic, inla1.4RS$dic$dic)
data.frame(inla1.plotRS$waic$waic,  inla1.datasetRS$waic$waic   , inla1.2RS$waic$waic, inla1.4RS$waic$waic)

# both IC's agree that the model with both random slopes is better



#Decision : include both random slopes

#Decision 2: including correlation between random intercept and slopes?
max.plots <-max(completeData$Plot_ID_4INLA)
max.datasourceId <-max(completeData$Datasource_ID_4INLA)
max.locations <-max(completeData$Location_4INLA)

#this model includes it for both plot and datasource
inla2 <- inla(log10(Number+1) ~ cYear+
                f(Period_4INLA,model='iid')+
                f(Location_4INLA,model='iid')+
                f(Plot_ID_4INLA,model='iid2d',n=2*max.plots)+
                f(Datasource_ID_4INLA,model='iid2d',n=2*max.datasourceId)+
                f(Plot_ID_4INLAR,iYear,copy='Plot_ID_4INLA')+
                f(Datasource_ID_4INLAR,iYear,copy='Datasource_ID_4INLA')+
                f(iYear,model='ar1',replicate=as.numeric(Plot_ID_4INLA)),
              control.compute = list(dic=TRUE,waic=TRUE),
              data=completeData); beep(2)

#check whether the correlation term (rho) overlaps zero : it doesn't for plot-level
#if so remove the correlation
#Decision is:  not to include the correlations










#2) run final models for : overall slope, continents, biomes, strata

# final overall model 

inla1 <- inla(log10(Number+1) ~ cYear + 
                f(Period_4INLA,model='iid')+
                f(Location_4INLA,model='iid')+
                f(Plot_ID_4INLA,model='iid')+
                f(Datasource_ID_4INLA,model='iid')+
                f(Plot_ID_4INLAR,iYear,model='iid')+
                f(Datasource_ID_4INLAR,iYear,model='iid')+
                f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
              control.compute = list(dic=TRUE,waic=TRUE),
              data=completeData) # has lower WAIC and DIC than inlaF2
save(inla1, file = "inla1.RData")


# Check for confounding factors####

inla1.1 <- inla(log10(Number+1) ~ cYear: cStartYear + cYear: cDuration + cYear:Country_state
                f(Period_4INLA,model='iid')+
                f(Location_4INLA,model='iid')+
                f(Plot_ID_4INLA,model='iid')+
                f(Datasource_ID_4INLA,model='iid')+
                f(Plot_ID_4INLAR,iYear,model='iid')+
                 f(Datasource_ID_4INLAR,iYear,model='iid')+
                   f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
              control.compute = list(dic=TRUE,waic=TRUE),
              data=completeData) # has lower WAIC and DIC than inlaF2
save(inla1, file = "inla1.1.RData")





############################################################
#Pull out the random effects and slopes from the grand model
load("E:/inla1.RData")
#get list of unique plots and datasourceID
summary_df <- unique(completeData[,c("Plot_ID","Datasource_ID",
                                     "Plot_ID_4INLA","Datasource_ID_4INLA",
                                     "Plot_ID_4INLAR","Datasource_ID_4INLAR")])

datasource_random_ef<-unique(completeData[,c("Datasource_ID", "Datasource_ID_4INLA", "Datasource_ID_4INLAR")])

#pull out random intercepts and slopes:

#data source ID
intercepts <- inla1$summary.random$Datasource_ID_4INLA
slopes <- inla1$summary.random$Datasource_ID_4INLAR
names(intercepts)[2:8] <- paste("DataID_Intercept_", names(intercepts)[2:8])
names(slopes)[2:8] <- paste("DataID_Slope_", names(slopes)[2:8])
summary_df <- merge(datasource_random_ef,intercepts, by.x="Datasource_ID_4INLA", by.y="ID")
summary_df <- merge(summary_df,slopes, by.x="Datasource_ID_4INLAR", by.y="ID")

# add up ovrall slope and random slopes
metadata_per_dataset<- read.csv(file = "metadata per dataset.csv", header =T)
RandEfDataset<- merge(summary_df, metadata_per_dataset, by = "Datasource_ID")
RandEfDataset$fixedSlp<- inla1$summary.fixed$mean[2]
RandEfDataset$fixedIntercept<- inla1$summary.fixed$mean[1]
RandEfDataset$slope <- RandEfDataset$'DataID_Slope_ mean'+ RandEfDataset$fixedSlp # sum of fixed and random slopes  
RandEfDataset$intercept <- RandEfDataset$'DataID_Intercept_ mean'+ RandEfDataset$fixedIntercept # sum of fixed and random slopes  

save(RandEfDataset, file = "RandEfDataset.RData")


# plot spagetti plot (Dornelas)
load("randomFitsFullPer.RData") # wiggly line
randomFits$abun<- 10^ (randomFits$RW.mean+ randomFits$intercept)
randomFits$c0.025quant<- randomFits$RW.0.025quant + randomFits$intercept
randomFits$c0.975quant<- randomFits$RW.0.975quant+ randomFits$intercept

load("RandEfDataset.RData") # random slopes and intercepts
load("metadata_per_dataset.RData") # 
source("pframe.R") # predict lines

randomFits$Realm<- factor(randomFits$Realm, levels=c('Terrestrial','Freshwater'))
pframe$Realm <- factor(pframe$Realm, levels=c('Terrestrial','Freshwater'))

ggplot(data = randomFits, aes(x= Year, y=abun,   colour = Realm)) + 
  scale_y_log10() +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),  strip.text = element_blank())+
  labs(x = "", y = "Insect abundance / biomass") +
  geom_line(data=pframe, aes(x= Year, y=unlog, group = Datasource_ID,  colour = Realm), size =0.6, alpha = 0.8)+
  scale_colour_manual(values = col.scheme.realm)+
  new_scale_color()+
   geom_line(data = randomFits,    aes(x=Year,  y=abun, colour=Realm), size = 1.2)+
  scale_colour_manual(values = col.scheme.realm2) +
  geom_ribbon(data = randomFits, aes(x=Year, ymin = 10^(intercept + RW.0.025quant),  
                                    ymax = 10^(intercept+RW.0.975quant), fill=Realm),alpha=0.4, color = NA)+
  scale_fill_manual (values = col.scheme.realm)+
  facet_grid(Realm~.)
 




# plot map #####
load("RandEfDataset.RData")




source("map_preparation.R")
# scale slopes 
pts.wgs$slope.scal[pts.wgs$slope.scal<(-0.02)]<- -0.02 # 
pts.wgs$slope.scal[pts.wgs$slope.scal>(0.02)]<- 0.02

# plot on map  by Datasource # scale is set to be symmetrical over most extreme value
p.wgs+
 # geom_point(data = subset(pts.wgs, Realm =="Freshwater")@data, pch = 21, 
 #            size = 1.3, aes(x = mean_long, y = mean_lat, group = NULL),  colour = 1) +
  geom_point(data = subset(pts.wgs, Realm =="Freshwater")@data , size = 1.3, pch = 21,
             aes(x = mean_long,   y = mean_lat,  fill = slope.scal, group = NULL), 
             position=position_jitter(h=1, w=1)) +  #
  scale_fill_gradient2(low = "#d7191c", mid = "#ffffbf", high = "#2c7bb6", space = "Lab" , 
                       limits = c(min(pts.wgs$slope.scal), -min(pts.wgs$slope.scal)), name = 'Abundance trend') +# "PuBuGn"
  ggtitle("Freshwater fauna") 

  

# terrestrial
p.wgs+
##  geom_point(data = subset(pts.wgs, Realm =="Terrestrial")@data, size = 1.3, aes(x = mean_long, y = mean_lat, group = NULL),  colour = 1) +
 geom_point(data = subset(pts.wgs, Realm =="Terrestrial")@data, size = 1.3, pch = 21,
            aes(x = mean_long,   y = mean_lat,  fill = slope.scal, group = NULL) , 
            position=position_jitter(h=1, w=1)) +
  scale_fill_gradient2(low = "#d7191c", mid = "#ffffbf", high = "#2c7bb6", space = "Lab" , 
                       limits = c(min(pts.wgs$slope.scal), -min(pts.wgs$slope.scal)), name = 'Abundance trend') +# "PuBuGn"
  ggtitle("Terrestrial fauna") 

# both on one map
library(ggnewscale)
p.wgs+
  geom_point(data = subset(pts.wgs, Realm =="Terrestrial")@data, size = 1.3, aes(x = mean_long, y = mean_lat, group = NULL),  colour = 1) +
  geom_point(data = subset(pts.wgs, Realm =="Terrestrial")@data , size = 1, aes(x = mean_long, y = mean_lat, colour = slope.scal, group = NULL)) +
  scale_colour_distiller("Slope terrestrial fauna",palette = "RdYlGn", direction = +1, limits = c(min(pts.wgs$slope.scal), -min(pts.wgs$slope.scal)))+ # "PuBuGn"
new_scale_color()+
  geom_point(data = subset(pts.wgs, Realm =="Freshwater")@data, size = 1.3, aes(x = mean_long, y = mean_lat, group = NULL),  colour = 1) +
  geom_point(data = subset(pts.wgs, Realm =="Freshwater")@data , size = 1, aes(x = mean_long,   y = mean_lat,  colour = slope.scal, group = NULL)) +
  scale_colour_distiller("Slope freshwater fauna", palette = "PuBuGn", direction = +1, limits = c(min(pts.wgs$slope.scal), -min(pts.wgs$slope.scal))) # ""
   
###################################################################################################################################################






#FINAL model for realms (no correlation slope and interceps)
inlaF <- inla(log10(Number+1) ~ cYear:Realm+ Realm + 
                f(Period_4INLA,model='iid')+
                f(Location_4INLA,model='iid')+
                f(Plot_ID_4INLA,model='iid')+
                f(Datasource_ID_4INLA,model='iid')+
                f(Plot_ID_4INLAR,iYear,model='iid')+
                f(Datasource_ID_4INLAR,iYear,model='iid')+
                f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
              control.compute = list(dic=TRUE,waic=TRUE),
              data=completeData) # has lower WAIC and DIC than inlaF2

load("E:/inlaF.RData")

summary(inlaF)
10^(0.007*10) 
1- (10^(summary(inlaF)$fixed[4] *10)) # 10 year % change terrestrial
1- (10^(summary(inlaF)$fixed[12] *10)) # upper CI 10 year % change terrestrial
1- (10^(summary(inlaF)$fixed[20] *10)) # lower CI 10 year % change terrestrial

(10^(summary(inlaF)$fixed[3] *10)) # 10 year % change FW
(10^(summary(inlaF)$fixed[11] *10)) # upper CI 10 year % change fw
(10^(summary(inlaF)$fixed[19] *10)) # lower CI 10 year % change fw



# check if correlation slope and intercept is insignificant # correct 
inlaF2 <- inla(log10(Number+1) ~ cYear:Realm+ Realm + 
                 f(Period_4INLA,model='iid')+
                 f(Location_4INLA,model='iid')+
                 f(Plot_ID_4INLA,model='iid2d',n=2*max.plots)+
                 f(Datasource_ID_4INLA,model='iid')+
                 f(Plot_ID_4INLAR,iYear,copy='Plot_ID_4INLA')+
                 f(Datasource_ID_4INLAR,iYear,model='iid')+
                 f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
               control.compute = list(dic=TRUE,waic=TRUE),
               data=completeData); beep(2)


#strata ####

metadata_strata<-  completeData %>% 
  group_by(Stratum) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
metadata_strata

inlaFstrat <- inla(log10(Number+1) ~ cYear:Stratum + Stratum + 
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                   data=completeData)

load("E:/inlaFstrat.RData")
stratSlope<- inlaFstrat$summary.fixed[9:14,]
vars<-data.frame(do.call(rbind, strsplit(rownames(stratSlope), split = ":")))
stratSlope<-cbind(stratSlope, vars)
stratSlope$X1<-gsub("Stratum", "", stratSlope$X1)
stratSlope<- merge(stratSlope, metadata_strata, by.x = "X1", by.y = "Stratum")
stratSlope$text = paste0("(", stratSlope$Datasources, " / ", stratSlope$Plots, ")")

# reorder for graph
rownames(stratSlope)<-stratSlope$X1
stratSlope$X1<- ordered(stratSlope$X1, levels = c("Water", "Underground" , "Soil surface", "Herb layer", "Trees", "Air" ))

ggplot(data.frame(stratSlope))+
  geom_crossbar(aes(x=X1,y=mean, fill = X1,
                    ymin=X0.025quant,ymax=X0.975quant),position="dodge", width = 0.7, fill = "grey70")+
  #scale_fill_manual(values = col.scheme.strat, guide=FALSE)+
  coord_flip()+
  ylim(-0.015, 0.025)+
  xlab ("")+ ylab ("Trend slope")+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_text(aes(x = X1 , y = 0.022, label = text), size = 3) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"))




# continents #####


inlaFcont <- inla(log10(Number+1) ~ cYear: Realm:Continent + Realm + Continent + 
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE,waic=TRUE),
                  data=completeData)

load("E:/inlaFcont.RData")
metadata_cont<-  completeData %>% 
  group_by(Continent, Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
metadata_cont

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
  xlab ("")+ ylab("Trend slope")+
  geom_text(aes(x = Continent , y = 0.028, label = text, fill = Realm),  
            position = position_dodge(width = 1), size = 3, color = 1) +
    coord_flip()+
  scale_y_continuous(breaks = c(-0.02, -0.01, 0,0.01, 0.02)) +
ylim(-0.025, 0.03)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
theme(legend.key=element_blank())
  

# politics:  east vs west europe
eur<- subset(completeData, Continent == "Europe")
eur$E_W[eur$Country == "Russia" | eur$Country == "Belarus"  | eur$Country == "Ukraine" ] <-"E"
eur$E_W[eur$Country != "Russia" & eur$Country != "Belarus"  & eur$Country != "Ukraine" ] <-"W"
eur$EMW<- "W"
eur$EMW[eur$Country == "Russia" | eur$Country == "Belarus"  | eur$Country == "Ukraine" ] <-"E"
eur$EMW[eur$Country == "Hungary" | eur$Country == "Czech Republic"  | eur$Country == "Slovakia" ] <-"FEB"

unique(eur[, c("Country", "E_W")])
unique(eur[, c("Country", "EMW")])


inlaFeur <- inla(log10(Number+1) ~ cYear: Realm: E_W + Realm + E_W + 
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE,waic=TRUE),
                  data=eur)

eurSlope<- inlaFeur$summary.fixed[4:7,]
ggplot(data.frame(eurSlope))+
  geom_crossbar(aes(x=rownames(eurSlope),   y=mean,
                    ymin=X0.025quant,ymax=X0.975quant),position="dodge")+
  #scale_fill_manual(values = col.scheme.realm)+
  coord_flip()+
  geom_hline(yintercept=0,linetype="dashed")+
  #ylim(-0.03, 0.03)+  xlab ("")+ ylab ("Slope")+
  #geom_text(aes(x = Biome , y = 0.027, fill = Realm,  label = text), position = position_dodge(width = 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


# using country as additional random intercept 

completeData$Country4INLA<- interaction(completeData$Datasource_ID, completeData$Country)
completeData$Country4INLA <- as.numeric(factor(completeData$Country4INLA))   

inlaFcont2.0 <- inla(log10(Number+1) ~ cYear: Realm:Continent + Realm + Continent + 
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Country4INLA,model='iid')+   # new coutnrly level random slope 
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE,waic=TRUE),
                  data=completeData)

# index for random slopes
completeData$Country4INLAR<- completeData$Country4INLA+max(completeData$Country4INLA)
inlaFcont2.1 <- inla(log10(Number+1) ~ cYear: Realm:Continent + Realm + Continent + 
                       f(Period_4INLA,model='iid')+
                       f(Location_4INLA,model='iid')+
                       f(Plot_ID_4INLA,model='iid')+
                       f(Datasource_ID_4INLA,model='iid')+
                       f(Country4INLA,model='iid')+   # new country level random slope 
                       f(Plot_ID_4INLAR,iYear,model='iid')+
                       f(Datasource_ID_4INLAR,iYear,model='iid')+
                       f(Country4INLAR,iYear,model='iid')+
                       f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                     control.compute = list(dic=TRUE,waic=TRUE),
                     data=completeData)




# biomes Coarse#####
#(tropical/ drylands / temperate / boreal-alpine )
inlaFbiom <- inla(log10(Number+1) ~ cYear: Realm:BiomeCoarse + Realm + BiomeCoarse + 
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE,waic=TRUE),
                  data=completeData)


metadata_biom<-  completeData %>% 
  group_by(BiomeCoarse, Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
metadata_biom



load("E:/inlaFbiom.RData")
biomSlope<- inlaFbiom$summary.fixed[6:13,]
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
    xlab ("")+ ylab ("Trend slope")+geom_hline(yintercept=0,linetype="dashed")+
  coord_flip()+
  scale_y_continuous(breaks = c(-0.02, -0.01, 0,0.01, 0.02)) +
  ylim(-0.03, 0.032)+  
  geom_text(aes(x = Biome , y = 0.027, fill = Realm,  label = text), position = position_dodge(width = 1), size = 3, color = 1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black") , 
        legend.key=element_blank())




# biomes original wwf biomes
selectedData<- subset(completeData, biome != "Tropical & Subtropical Dry Broadleaf Forests" & biome != "Tropical & Subtropical Grasslands, Savannas & Shrublands")
selectedData$biome<- droplevels(selectedData$biome)

inlaFbiom2 <- inla(log10(Number+1) ~ cYear: Realm:biome + Realm + biome + 
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE,waic=TRUE),
                  data=selectedData)

biom2Slope<- inlaFbiom2$summary.fixed[c(13:26, 31:34),]
vars<-data.frame(do.call(rbind, strsplit(rownames(biom2Slope), split = ":")))
biom2Slope<-cbind(biom2Slope, vars)
biom2Slope$X1<-gsub("Realm", "", biom2Slope$X1);  biom2Slope$X2<-gsub("biome", "", biom2Slope$X2)

ggplot(data.frame(biom2Slope))+
  geom_crossbar(aes(x=X2,   y=mean, fill = X1,
                    ymin=X0.025quant,ymax=X0.975quant),position="dodge")+
  scale_fill_manual(values = col.scheme.realm)+
  coord_flip()+
  geom_hline(yintercept=0,linetype="dashed")
# does not provide much that we didn;t already know 








###########################################################
#for SOM: Include a Table for with Standard deviations for each random effect

#below code shows how to convert precision to standard deviation

#pull out variance
tauPlot <-inlaF$marginals.hyperpar$`Precision for Plot_ID_4INLA`
tauDatasource <-inlaF$marginals.hyperpar$`Precision for Datasource_ID_4INLA`
tauPeriod <-inlaF$marginals.hyperpar$`Precision for Period_4INLA`
tauLocation <-inlaF$marginals.hyperpar$`Precision for Location_4INLA`
tauPlotR <-inlaF$marginals.hyperpar$`Precision for Plot_ID_4INLAR`
tauDatasourceR <-inlaF$marginals.hyperpar$`Precision for Datasource_ID_4INLAR`

#convert to standard deviations
myfun <- function(x){1/sqrt(x)}
sigmaPlot <- inla.emarginal(myfun,tauPlot)
sigmaDatasource <- inla.emarginal(myfun,tauDatasource)
sigmaPeriod <- inla.emarginal(myfun,tauPeriod)
sigmaLocation <- inla.emarginal(myfun,tauLocation)
sigmaPlotR <- inla.emarginal(myfun,tauPlotR)
sigmaDatasourceR <- inla.emarginal(myfun,tauDatasourceR)

sigmaPlot;sigmaDatasource;sigmaLocation;sigmaPeriod;sigmaPlotR;sigmaDatasourceR








#(3) moving 15-year window model #####

# 10 or 15 year slices 
  

window = 10

windowFits10 <- ddply(subset(completeData,Year>1959),
                    .(Realm,Continent),
                    function(df){
                      
                      
        ldply(min(df$Year):(max(df$Year)-window),
                       
                            
                                 function(x){
                              
                              myData <- subset(df, Year>=as.numeric(x) & 
                                                 Year <= as.numeric(x+window)) 
                             
           # build dataframe exluding plots of less than half the period  in duration  
              selectionDF<-myData %>% group_by(Plot_ID) %>%
                summarize( length.ok = max(Year) - min(Year) > 0.5*window)          
                myData<- merge(myData, selectionDF, by = "Plot_ID")
                myData<- subset(myData, length.ok == T )            
                            
                              
                              #write model formula
                              
                              #basic model 
                              formula="log10(Number+1)~cYear"
                              
                              #if multiple datasets
                              if(length(unique(myData$Datasource_ID))>1){
                                formula=paste(formula,"f(Datasource_ID_4INLA,model='iid')+
                                              f(as.numeric(Datasource_ID_4INLAR), iYear, model='iid')",sep="+")
                              }
                              
                              #to this model, add a location effect if plots within a dataset are nested in sites 
                              #add only random location intercept 
                              if(length(unique(myData$Location))>1){
                                formula=paste(formula,"f(Location_4INLA,model='iid')" ,sep="+")
                              }
                              
                              #to this model, add a plot effect if a dataset sampled multiple plots
                              #add both a random plot intercept and random plot slope
                              #(if a dataset only sampled one plot, no plot effects are added)
                              if(length(unique(myData$Plot_ID))>1){
                                formula=paste(formula,"f(Plot_ID_4INLA,model='iid') +
                                              f(Plot_ID_4INLAR,iYear,model='iid')",sep="+")
                              }
                              
                              #to this model, add a period effect if there multiple periods in a dataset
                              #(if only one period is sampled, no period term is added)
                              if(length(unique(myData$Period))>1){
                                formula=paste(formula,"f(Period_4INLA,model='iid')",sep="+")
                              }
                              
                              #to this model, if model doesnt crash with ar1 term, then include it in the model formula, 
                              #if INLA crashes, then dont add the ar1 term
                              formula_ar1 <- paste(formula,"f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA))",sep="+")
                              out <- try(inla(as.formula(formula_ar1),data=myData,silent="2L"),TRUE)
                              if(class(out)=="try-error"){
                                
                                formula_ar2 <- paste(formula,"f(iYear,model='ar1')",sep="+")
                                out <- try(inla(as.formula(formula_ar1),data=myData,silent="2L"),TRUE)
                                
                                if(class(out)=="try-error"){
                                  formula = formula
                                  
                                }else{
                                  formula = formula_ar2
                                }
                                
                              }else{
                                formula = formula_ar1
                              }
                              
                              #finally fit this formula using INLA to the dataset
                              library(INLA)
                              inla1 <- inla(as.formula(formula),data=myData)
                              
                              #return model summary
                              return(cbind(Year = x+window,trend = inla1$summary.fixed[2,]))
                              })
                            })

save(windowFits10,file="windowFits.RData") 



load("windowFits15.RData")
load("windowFits10.RData")
windowFits10$Continent<- ordered(windowFits10$Continent, levels = c("Europe", "North America" , "Asia", "Latin America", "Australia", "Africa" ))
windowFits15$Continent<- ordered(windowFits15$Continent, levels = c("Europe", "North America" , "Asia", "Latin America", "Australia", "Africa" ))


ggplot(windowFits15)+
  geom_line(aes(x=Year-15,y=trend.mean,colour=Realm))+
  geom_ribbon(aes(x=Year-15,ymin=trend.0.025quant,ymax=trend.0.975quant,fill=Realm),alpha=0.5)+
  scale_color_manual(values = col.scheme.realm)+
  facet_wrap(~Continent)+
  scale_fill_manual(values = col.scheme.realm)+
  theme_bw()+  labs(y = "15 year slope", x = "First year of time window") +
  geom_hline(yintercept=0,linetype="dashed") + 
  scale_y_continuous(limits = c(-0.1, 0.1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.spacing = unit(2, "lines"))
  
ggplot(windowFits10)+
  geom_line(aes(x=Year-10,y=trend.mean,colour=Realm))+
  geom_ribbon(aes(x=Year-10,ymin=trend.0.025quant,ymax=trend.0.975quant,fill=Realm),alpha=0.5)+
  scale_color_manual(values = col.scheme.realm)+
  facet_wrap(~Continent)+
  scale_fill_manual(values = col.scheme.realm)+
  theme_bw()+  labs(y = "10 year slope", x = "First year of time window") +
  geom_hline(yintercept=0,linetype="dashed") + 
  scale_y_continuous(limits = c(-0.15, 0.15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.spacing = unit(2, "lines"))



#################################################################################################
# Random walk model #####

#continent

randomFits <- ddply(subset(completeData, Year < 2016 ),.
                        (Realm),
                        function(myData){
                          
                        #fit model
                          inlaRW <- inla(
                            log10(Number+1)~f(iYear,model='rw1') + 
                              f(Datasource_ID_4INLAR,iYear,model='iid')+
                              f(Location_4INLA,model='iid')+
                              f(Plot_ID,model='iid')+
                              f(Period_4INLA,model='iid'),
                            data=myData)
                          
                          #return model summary
                          return(data.frame(
                            Year=sort(unique(myData$Year)),
                            RW=inlaRW$summary.random$iYear, 
                            intercept = as.numeric(inlaRW$summary.fixed[1]))
                          )
                        }); beep(1)

save(randomFits,file="randomFitsFullPer.RData") 

load("randomFitsFullPer.RData")

randomFits$abun<- randomFits$RW.mean+ randomFits$intercept
ggplot(randomFits)+
  geom_line(aes(x=Year,y=abun, colour=Realm))+
  scale_colour_manual(values = col.scheme.realm)+
  geom_ribbon(aes(x=Year, ymin = intercept + RW.0.025quant,  ymax = intercept+RW.0.975quant, fill=Realm),alpha=0.5)+
  scale_fill_manual (values = col.scheme.realm)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(y = "Arthropod abundance")

# plot on top of lines: 

# first make script to get predicted lines
load("RandEfDataset.RData")



p<-ggplot(data = completeData, aes(x= Year, y=(Number+1))) + 
  scale_y_log10() +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_point(colour = "grey", size = 1) + 
  labs(x = "", y = "Abundance / Biomass") #+
p+
  geom_line(data=pframe.all, aes(x= Year, y=unlogMin1, group = Datasource,  colour = Realm), size =1, alpha = 0.8)+
  scale_colour_manual(values = col.scheme.realm)   



#random fits per continent 

randomFitsCont <- ddply(subset(completeData,Year>1960 & Year < 2016 ),.
                    (Realm, Continent),
                    function(myData){
                    
                      
                      #write model formula
                       #basic model 
                      formula="log10(Number+1)~f(iYear,model='rw1') + f(Datasource_ID_4INLAR,iYear,model='iid')"#  random slope at dataset level
                      # f(Datasource_ID_4INLA,model='iid')"
                      
                      #add period effect if multiple periods
                      if(length(unique(myData$Period))>1){
                        formula=paste(formula,"f(Period_4INLA,model='iid')",sep="+") # only random int
                      }
                      
                      # add a layer for location s
                      if(length(unique(myData$Location))>1){
                        formula=paste(formula,"f(Location_4INLA,model='iid')",sep="+") # only random int
                      }
                      
                      
                      #add plot effect if multiple plots
                      if(length(unique(myData$Plot_ID))>1){
                        formula=paste(formula,"f(Plot_ID,model='iid')",sep="+")   #+
                            #          f(Plot_ID_4INLAR,iYear,model='iid')  # here slope at plot level 
                      }
                      
                      #fit model
                      inla1 <- inla(as.formula(formula),data=myData)
                      
                      #return model summary
                      return(data.frame(
                        Year=sort(unique(myData$Year)),
                        RW=inla1$summary.random$iYear))
                      
                    }); beep(1)

randomFitsCont$Continent<- ordered(randomFitsCont$Continent, levels = c("Europe", "North America" , "Asia", "Latin America", "Australia", "Africa" ))
save(randomFitsCont, file = "randomFitsCont.RData")

ggplot(randomFitsCont)+
  geom_line(aes(x=Year,y=RW.mean,colour=Realm))+
  scale_colour_manual(values = col.scheme.realm)+
  geom_ribbon(aes(x=Year, ymin = RW.0.025quant,ymax = RW.0.975quant,fill=Realm),alpha=0.5)+
  scale_fill_manual (values = col.scheme.realm)+
  geom_hline(yintercept = 0, linetype="dashed") +  theme_bw()+
  facet_wrap(~Continent)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(y = "Standardized insect abundance")



# 

# DRiveRS #####

# protected areas
metadata_pa<-  completeData %>% 
  group_by( Realm,PA) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)))
metadata_pa


PAmodels<- data.frame(modelName=(character()),
                     fixedEffects=character(), 
                     DIC=numeric(),
                     WAIC=numeric(),
                     stringsAsFactors=FALSE) 


inlaFpaInt <- inla(log10(Number+1) ~ cYear: PA:Realm + PA + Realm +
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                   data=completeData, verbose = T, num.threads = 2)

  PAmodels[1,1]<- "3way"   
  PAmodels[1,2]<- "Year: PA:Realm"
  PAmodels[1,3]<- summary(inlaFpaInt)$dic$dic
  PAmodels[1,4]<- summary(inlaFpaInt)$waic$waic
   

inlaFpa <- inla(log10(Number+1) ~ cYear: PA + cYear:Realm + 
                  f(Period_4INLA,model='iid')+
                  f(Location_4INLA,model='iid')+
                  f(Plot_ID_4INLA,model='iid')+
                  f(Datasource_ID_4INLA,model='iid')+
                  f(Plot_ID_4INLAR,iYear,model='iid')+
                  f(Datasource_ID_4INLAR,iYear,model='iid')+
                  f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                control.compute = list(dic=TRUE,waic=TRUE),
                data=completeData, verbose = T, num.threads = 2)

PAmodels[2,1]<- "2way"   
PAmodels[2,2]<- "Year: PA + PA:Realm"
PAmodels[2,3]<- summary(inlaFpa)$dic$dic
PAmodels[2,4]<- summary(inlaFpa)$waic$waic


inlaFpa2 <- inla(log10(Number+1) ~ cYear:Realm +PA + 
                  f(Period_4INLA,model='iid')+
                  f(Location_4INLA,model='iid')+
                  f(Plot_ID_4INLA,model='iid')+
                  f(Datasource_ID_4INLA,model='iid')+
                  f(Plot_ID_4INLAR,iYear,model='iid')+
                  f(Datasource_ID_4INLAR,iYear,model='iid')+
                  f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                control.compute = list(dic=TRUE,waic=TRUE),
                data=completeData, verbose = F, num.threads = 2)

PAmodels[3,1]<- "additive"   
PAmodels[3,2]<- "Year: Realm + PA"
PAmodels[3,3]<- summary(inlaFpa2)$dic$dic
PAmodels[3,4]<- summary(inlaFpa2)$waic$waic

PAmodels[4,1]<- "none"   
PAmodels[4,2]<- "Year: Realm "
PAmodels[4,3]<- summary(inlaF)$dic$dic
PAmodels[4,4]<- summary(inlaF)$waic$waic







completeDataTemp<-subset(completeData, BiomeCoarse == "Temperate")
inlaFpaIntTemp <- inla(log10(Number+1) ~ cYear: PA:Realm + PA + Realm +
                         f(Period_4INLA,model='iid')+
                         f(Location_4INLA,model='iid')+
                         f(Plot_ID_4INLA,model='iid')+
                         f(Datasource_ID_4INLA,model='iid')+
                         f(Plot_ID_4INLAR,iYear,model='iid')+
                         f(Datasource_ID_4INLAR,iYear,model='iid')+
                         f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                       control.compute = list(dic=TRUE,waic=TRUE),
                       data=completeDataTemp, verbose = T, num.threads = 2)






completeData$REP_AREA[is.na(completeData$REP_AREA)] <- "0"
completeData$REP_AREA<- as.numeric(completeData$REP_AREA)
inlaFpaSize <- inla(log10(Number+1) ~ cYear* log10(REP_AREA) *Realm  + 
                      f(Period_4INLA,model='iid')+
                      f(Location_4INLA,model='iid')+
                      f(Plot_ID_4INLA,model='iid')+
                      f(Datasource_ID_4INLA,model='iid')+
                      f(Plot_ID_4INLAR,iYear,model='iid')+
                      f(Datasource_ID_4INLAR,iYear,model='iid')+
                      f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                    control.compute = list(dic=TRUE,waic=TRUE),
                    data=completeData, verbose = F, num.threads = 2)

inlaFpaSizeSEL <- inla(log10(Number+1) ~ cYear* log10(REP_AREA) *Realm  + 
                         f(Period_4INLA,model='iid')+
                         f(Location_4INLA,model='iid')+
                         f(Plot_ID_4INLA,model='iid')+
                         f(Datasource_ID_4INLA,model='iid')+
                         f(Plot_ID_4INLAR,iYear,model='iid')+
                         f(Datasource_ID_4INLAR,iYear,model='iid')+
                         f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                       control.compute = list(dic=TRUE,waic=TRUE),
                       data= subset(completeData, REP_AREA>0), 
                       verbose = F, num.threads = 2)






inlaFpaSize1 <- inla(log10(Number+1) ~ cYear* log10(REP_AREA)  + log10(REP_AREA) *Realm  + 
                       f(Period_4INLA,model='iid')+
                       f(Location_4INLA,model='iid')+
                       f(Plot_ID_4INLA,model='iid')+
                       f(Datasource_ID_4INLA,model='iid')+
                       f(Plot_ID_4INLAR,iYear,model='iid')+
                       f(Datasource_ID_4INLAR,iYear,model='iid')+
                       f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                     control.compute = list(dic=TRUE,waic=TRUE),
                     data=completeData, verbose = F, num.threads = 2)

# compare to inlaF



#  Land use at end of timeseries #####

#land-use model:
#log(Number+1)~Year*Driver + Year*Realm 

plotData<-unique(completeData[, c( "Plot_ID", "Realm", "Continent", "Datasource_ID", "Stratum", "Location",            
                                   "Datasource_name", "biome", "BiomeCoarse",   "End_forestArea",      
                                   "End_cropArea" , "End_pastureArea" ,  "End_urbanArea"   ) ]) 



 ggplot(plotData, aes(End_urbanArea)) + 
  geom_histogram( aes(x = End_urbanArea,   y = ..density..), fill="blue") +
   geom_histogram( aes(x = End_cropArea, y = -..density..), fill= "green")+
     facet_wrap(~Realm)
# is a bit skewed....   
 
 ggplot(plotData, aes(End_urbanArea)) + 
   geom_histogram( aes(x = sqrt(End_urbanArea),   y = ..density..), fill="blue") +
   geom_histogram( aes(x = sqrt(End_cropArea), y = -..density..), fill= "green")+
   facet_wrap(~Realm)
 # is a bit better
 
 ggplot(plotData, aes(End_pastureArea)) + 
   geom_histogram( aes(x = (End_pastureArea),   y = ..density..), fill="blue") +
   geom_histogram( aes(x = (End_forestArea), y = -..density..), fill= "green")+
   facet_wrap(~Realm)
 
 ggplot(plotData, aes(End_pastureArea)) + 
   geom_histogram( aes(x = sqrt(End_pastureArea),   y = ..density..), fill="blue") +
   geom_histogram( aes(x = sqrt(End_forestArea), y = -..density..), fill= "green")+
   facet_wrap(~Realm) # a bit better
 
 
 
 
 
################################################
 #Urban area #####

 urbanModels <- data.frame(modelName=(character()),
                          fixedEffects=character(), 
                          DIC=numeric(),
                          WAIC=numeric(),
                          stringsAsFactors=FALSE) 
 
inlaFurban<- inla(log10(Number+1) ~ cYear + Realm + sqrt(End_urbanArea) + cYear* Realm * sqrt(End_urbanArea) +
          f(Period_4INLA,model='iid')+
          f(Location_4INLA,model='iid')+
          f(Plot_ID_4INLA,model='iid')+
          f(Datasource_ID_4INLA,model='iid')+
          f(Plot_ID_4INLAR,iYear,model='iid')+
          f(Datasource_ID_4INLAR,iYear,model='iid')+
          f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
        control.compute = list(dic=TRUE,waic=TRUE),
        data=completeData, verbose = T, num.threads = 2)

urbanModels[1,1] <-"3way"
urbanModels[1,2] <- "Realm*cYear*urbanArea"
urbanModels[1,3] <-summary(inlaFurban)$dic$dic
urbanModels[1,4] <-summary(inlaFurban)$waic$waic


inlaFurban1<- inla(log10(Number+1) ~ Realm * cYear + cYear* sqrt(End_urbanArea) +
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE,waic=TRUE),
                  data=completeData, verbose = T, num.threads = 2)

urbanModels[2,1] <-"2way"
urbanModels[2,2] <- "Year*UrbanCover + Realm*Year"
urbanModels[2,3] <-summary(inlaFurban)$dic$dic
urbanModels[2,4] <-summary(inlaFurban)$waic$waic


inlaFurban2<- inla(log10(Number+1) ~ Realm * cYear + sqrt(End_urbanArea)+
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                   data=completeData, verbose = T, num.threads = 2)

urbanModels[3,1] <-"additive"
urbanModels[3,2] <- "Realm*Year +UrbanCover"
urbanModels[3,3] <-summary(inlaFurban)$dic$dic
urbanModels[3,4] <-summary(inlaFurban)$waic$waic

urbanModels[4,1] <-"none"
urbanModels[4,2] <- "Year* Realm "
urbanModels[4,3] <-summary(inlaF)$dic$dic
urbanModels[4,4] <-summary(inlaF)$waic$waic




inlaFurbanT<- inla(log10(Number+1) ~  cYear* sqrt(End_urbanArea) +
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE,waic=TRUE),
                  data= subset(completeData, Realm == "Terrestrial"), 
                  verbose = T, num.threads = 2)

inlaFurbanT2<- inla(log10(Number+1) ~  cYear + sqrt(End_urbanArea) +
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                   data= subset(completeData, Realm == "Terrestrial"), 
                   verbose = T, num.threads = 2)



inlaFurbanFW<- inla(log10(Number+1) ~  cYear* sqrt(End_urbanArea) +
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                   data= subset(completeData, Realm == "Freshwater"), 
                    num.threads = 2)#verbose = T,

inlaFurbanFW2<- inla(log10(Number+1) ~  cYear+ sqrt(End_urbanArea) +
                      f(Period_4INLA,model='iid')+
                      f(Location_4INLA,model='iid')+
                      f(Plot_ID_4INLA,model='iid')+
                      f(Datasource_ID_4INLA,model='iid')+
                      f(Plot_ID_4INLAR,iYear,model='iid')+
                      f(Datasource_ID_4INLAR,iYear,model='iid')+
                      f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                    control.compute = list(dic=TRUE,waic=TRUE),
                    data= subset(completeData, Realm == "Freshwater"), 
                    num.threads = 2)#verbose = T,


###############################################################################
# crop area #####

cropModels <- data.frame(modelName=(character()),
                 fixedEffects=character(), 
                 DIC=numeric(),
                 WAIC=numeric(),
                 stringsAsFactors=FALSE)


inlaFcrop<- inla(log10(Number+1) ~   cYear* Realm* sqrt(End_cropArea)  +   #cYear + Realm + End_cropArea +
                   f(Period_4INLA,model='iid')+
                   f(Location_4INLA,model='iid')+
                   f(Plot_ID_4INLA,model='iid')+
                   f(Datasource_ID_4INLA,model='iid')+
                   f(Plot_ID_4INLAR,iYear,model='iid')+
                   f(Datasource_ID_4INLAR,iYear,model='iid')+
                   f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                 control.compute = list(dic=TRUE,waic=TRUE),
                 data=completeData,   num.threads = 2) #verbose = T,

cropModels[1,1] <-"3way"
cropModels[1,2] <- "Realm*cYear*cropArea"
cropModels[1,3] <-summary(inlaFcrop)$dic$dic
cropModels[1,4] <-summary(inlaFcrop)$waic$waic



inlaFcrop1<- inla(log10(Number+1) ~   cYear* Realm + cYear* sqrt(End_cropArea)  +   #cYear + Realm + End_cropArea +
                   f(Period_4INLA,model='iid')+
                   f(Location_4INLA,model='iid')+
                   f(Plot_ID_4INLA,model='iid')+
                   f(Datasource_ID_4INLA,model='iid')+
                   f(Plot_ID_4INLAR,iYear,model='iid')+
                   f(Datasource_ID_4INLAR,iYear,model='iid')+
                   f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                 control.compute = list(dic=TRUE,waic=TRUE),
                 data=completeData,   num.threads = 2) #verbose = T,

cropModels[2,1] <-"2way"
cropModels[2,2] <- "cYear* Realm + cYear*cropArea"
cropModels[2,3] <-summary(inlaFcrop1)$dic$dic
cropModels[2,4] <-summary(inlaFcrop1)$waic$waic

inlaFcrop2<- inla(log10(Number+1) ~   cYear* Realm +  sqrt(End_cropArea)  +   #cYear + Realm + End_cropArea +
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE,waic=TRUE),
                  data=completeData,   num.threads = 2) #verbose = T,
cropModels[3,1] <-"additive"
cropModels[3,2] <- "Year* Realm + cropCover"
cropModels[3,3] <-summary(inlaFcrop2)$dic$dic
cropModels[3,4] <-summary(inlaFcrop2)$waic$waic

cropModels[4,1] <-"none"
cropModels[4,2] <- "Year* Realm "
cropModels[4,3] <-summary(inlaF)$dic$dic
cropModels[4,4] <-summary(inlaF)$waic$waic





inlaFcropT<- inla(log10(Number+1) ~  cYear* sqrt(End_cropArea) +
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                   data= subset(completeData, Realm == "Terrestrial"), 
                    num.threads = 2) #verbose = T,

inlaFcropT1<- inla(log10(Number+1) ~  cYear+ sqrt(End_cropArea) +
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE,waic=TRUE),
                  data= subset(completeData, Realm == "Terrestrial"), 
                  num.threads = 2) #verbose = T,

inlaFcropFW<- inla(log10(Number+1) ~  cYear* sqrt(End_cropArea) +
                      f(Period_4INLA,model='iid')+
                      f(Location_4INLA,model='iid')+
                      f(Plot_ID_4INLA,model='iid')+
                      f(Datasource_ID_4INLA,model='iid')+
                      f(Plot_ID_4INLAR,iYear,model='iid')+
                      f(Datasource_ID_4INLAR,iYear,model='iid')+
                      f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                    control.compute = list(dic=TRUE,waic=TRUE),
                    data= subset(completeData, Realm == "Freshwater"), 
                    num.threads = 2)#verbose = T,

inlaFcropFW1<- inla(log10(Number+1) ~  cYear+ sqrt(End_cropArea) +
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                   data= subset(completeData, Realm == "Freshwater"), 
                   num.threads = 2)#verbose = T,









inlaFpasture<- inla(log10(Number+1) ~ cYear + Realm + sqrt(End_pastureArea) + cYear*Realm * sqrt(End_pastureArea)  +
                   f(Period_4INLA,model='iid')+
                   f(Location_4INLA,model='iid')+
                   f(Plot_ID_4INLA,model='iid')+
                   f(Datasource_ID_4INLA,model='iid')+
                   f(Plot_ID_4INLAR,iYear,model='iid')+
                   f(Datasource_ID_4INLAR,iYear,model='iid')+
                   f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                 control.compute = list(dic=TRUE,waic=TRUE),
                 data=completeData, verbose = T, num.threads = 2)

# surprisingly there might be an effect here in terrestrial

inlaFpastureT<- inla(log10(Number+1) ~  cYear: sqrt(End_pastureArea)  +
                       f(Period_4INLA,model='iid')+
                       f(Location_4INLA,model='iid')+
                       f(Plot_ID_4INLA,model='iid')+
                       f(Datasource_ID_4INLA,model='iid')+
                       f(Plot_ID_4INLAR,iYear,model='iid')+
                       f(Datasource_ID_4INLAR,iYear,model='iid')+
                       f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                     control.compute = list(dic=TRUE,waic=TRUE),
                     data=subset(completeData, Realm == "Terrestrial")  , verbose = F, num.threads = 2)

inlaFpastureT1<- inla(log10(Number+1) ~  cYear+ sqrt(End_pastureArea)  +
                        f(Period_4INLA,model='iid')+
                        f(Location_4INLA,model='iid')+
                        f(Plot_ID_4INLA,model='iid')+
                        f(Datasource_ID_4INLA,model='iid')+
                        f(Plot_ID_4INLAR,iYear,model='iid')+
                        f(Datasource_ID_4INLAR,iYear,model='iid')+
                        f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                      control.compute = list(dic=TRUE,waic=TRUE),
                      data=subset(completeData, Realm == "Terrestrial")  , verbose = F, num.threads = 2)




inlaFforest<- inla(log10(Number+1) ~ cYear + Realm + sqrt(End_forestArea) + cYear* Realm * sqrt(End_forestArea)  +
                      f(Period_4INLA,model='iid')+
                      f(Location_4INLA,model='iid')+
                      f(Plot_ID_4INLA,model='iid')+
                      f(Datasource_ID_4INLA,model='iid')+
                      f(Plot_ID_4INLAR,iYear,model='iid')+
                      f(Datasource_ID_4INLAR,iYear,model='iid')+
                      f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                    control.compute = list(dic=TRUE,waic=TRUE),
                    data=completeData, verbose = T, num.threads = 2)





#where Driver is Urban, crop, or CC, or Protected Area
#Explain continental effects with drivers
#Maybe include continental effects as a random intercept and slopes



load("TmeanSlopesWholePeriod.Rdata")

test<- merge(completeData, TmeanSlopesWholePeriod, by.x = "Plot_ID", by.y = "plt") 



load("TmeanSlopes.Rdata")
completeDataClim<- merge(completeData, TmeanSlopes )
# what's missing 
unique(completeData$Plot_ID)[! unique(completeData$Plot_ID) %in% unique(completeDataClim$Plot_ID)]
# ould plots

meanTModels <- data.frame(modelName=(character()),
                         fixedEffects=character(), 
                         DIC=numeric(),
                         WAIC=numeric(),
                         stringsAsFactors=FALSE)

inlaFmeanT<- inla(log10(Number+1) ~ cYear + Realm +  cYear* Realm * relDeltaTmean  +
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                   data=completeData, verbose = F, num.threads = 2)

meanTModels[1,1] <-"3way"
meanTModels[1,2] <- "Year*Realm*relDeltaTmean"
meanTModels[1,3] <-summary(inlaFmeanT)$dic$dic
meanTModels[1,4] <-summary(inlaFmeanT)$waic$waic



inlaFmeanT1<- inla(log10(Number+1) ~ cYear * relDeltaTmean +   cYear* Realm   +
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                       control.compute = list(dic=TRUE,waic=TRUE),
                  data=completeData, verbose = F, num.threads = 2)

meanTModels[2,1] <-"2way"
meanTModels[2,2] <- "Year*Realm*relDeltaTmean"
meanTModels[2,3] <-summary(inlaFmeanT1)$dic$dic
meanTModels[2,4] <-summary(inlaFmeanT1)$waic$waic


inlaFmeanT2<- inla(log10(Number+1) ~ relDeltaTmean +   cYear* Realm   +
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                   data=completeData, verbose = F, num.threads = 2)

meanTModels[3,1] <-"additive"
meanTModels[3,2] <- "relDeltaTmean+cYear*Realm "
meanTModels[3,3] <-summary(inlaFmeanT2)$dic$dic
meanTModels[3,4] <-summary(inlaFmeanT2)$waic$waic

# dic and waic for simplest model
meanTModels[4,1] <-"none"
meanTModels[4,2] <- "Year* Realm "
meanTModels[4,3] <-summary(inlaF)$dic$dic
meanTModels[4,4] <-summary(inlaF)$waic$waic


# there might be stng there

#split terr and FW
inlaFmeanTT<- inla(log10(Number+1) ~ cYear * relDeltaTmean    +
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                   data= subset(completeDataClim, Realm == "Terrestrial")   , 
                   verbose = F, num.threads = 2)

inlaFmeanTT1<- inla(log10(Number+1) ~ cYear + relDeltaTmean    +
                      f(Period_4INLA,model='iid')+
                      f(Location_4INLA,model='iid')+
                      f(Plot_ID_4INLA,model='iid')+
                      f(Datasource_ID_4INLA,model='iid')+
                      f(Plot_ID_4INLAR,iYear,model='iid')+
                      f(Datasource_ID_4INLAR,iYear,model='iid')+
                      f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                    control.compute = list(dic=TRUE,waic=TRUE),
                    data= subset(completeDataClim, Realm == "Terrestrial")   , 
                    verbose = F, num.threads = 2)

inlaFmeanTFW<- inla(log10(Number+1) ~ cYear * relDeltaTmean    +
                      f(Period_4INLA,model='iid')+
                      f(Location_4INLA,model='iid')+
                      f(Plot_ID_4INLA,model='iid')+
                      f(Datasource_ID_4INLA,model='iid')+
                      f(Plot_ID_4INLAR,iYear,model='iid')+
                      f(Datasource_ID_4INLAR,iYear,model='iid')+
                      f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                    control.compute = list(dic=TRUE,waic=TRUE),
                    data= subset(completeDataClim, Realm == "Freshwater")   , 
                    verbose = F, num.threads = 2)

inlaFmeanTFW1<- inla(log10(Number+1) ~ cYear + relDeltaTmean    +
                       f(Period_4INLA,model='iid')+
                       f(Location_4INLA,model='iid')+
                       f(Plot_ID_4INLA,model='iid')+
                       f(Datasource_ID_4INLA,model='iid')+
                       f(Plot_ID_4INLAR,iYear,model='iid')+
                       f(Datasource_ID_4INLAR,iYear,model='iid')+
                       f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                     control.compute = list(dic=TRUE,waic=TRUE),
                     data= subset(completeDataClim, Realm == "Freshwater")   , 
                     verbose = F, num.threads = 2)









##############################################################################################
# precipitation #####
load("PrecSlopes.RData")
completeDataClimPrec<- merge(completeDataClim, PrecSlopes, by = "Plot_ID")
dim(completeDataClimPrec) # lost a bit there
unique(completeDataClim$Plot_ID)[! unique(completeDataClim$Plot_ID) %in% unique(completeDataClimPrec$Plot_ID)]
save(completeDataClimPrec, file = "completeDataClimPrec.RData")
# old plots and israel are missing 


inlaFmeanP<- inla(log10(Number+1) ~ cYear + Realm +  cYear* Realm * relDeltaPrec +
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE,waic=TRUE),
                  data=completeDataClimPrec, verbose = F, num.threads = 2)
save(inlaFmeanP, file = "inlaFmeanP.RData")

inlaFmeanP1<- inla(log10(Number+1) ~ cYear * Realm +  cYear* relDeltaPrec  +
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                   data=completeDataClimPrec, verbose = F, num.threads = 2)
save(inlaFmeanP1, file = "inlaFmeanP1.RData")

inlaFmeanP2<- inla(log10(Number+1) ~ cYear * Realm +  relDeltaPrec  +
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                   data=completeDataClimPrec, verbose = F, num.threads = 2)
save(inlaFmeanP2, file = "inlaFmeanP2.RData")












#adding a test line
