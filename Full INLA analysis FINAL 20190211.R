rm(list=ls()) 

library(INLA)
library(ggplot2)
library(ggnewscale)
library(tidyverse)



setwd("C:/Users/roelv/Dropbox/Insect Biomass Trends/csvs") # home
setwd("C:\\Dropbox\\Dropbox\\Insect Biomass Trends/csvs") # work
setwd("~/Dropbox/Insect Biomass Trends/csvs")#diana mac


load("completeData.Rdata")
load("completeDataArth.RData")
load("E:/inla1.RData")
load("all.results.RData")
load("metadata_per_dataset.RData")
#completeData$Stratum[completeData$Stratum == "air"]<-"Air"
#completeData$Stratum[completeData$Plot_ID == 930 ]<- "Soil surface"

theme_clean<- theme_grey() + theme(panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 panel.background = element_blank(), 
                                 axis.line = element_line(colour = "black") , 
                                 legend.key=element_blank())

col.scheme.cont<-c( "Europe"="green3", "Latin America"= "magenta", "North America"= "orange","Asia" = "purple3", 
                    "Africa" = "blue", "Australia" = "red")
col.scheme.realm<-c(  "Freshwater"  = "dodgerblue2", "Terrestrial" = "peru")
col.scheme.realm<- c(  "Freshwater"  = "dodgerblue2", "Terrestrial" = "chocolate4")  #coral4

col.scheme.strat<-c( "Air" = "peru", "Herb layer" = "peru", "Soil surface" = "peru", "Trees" = "peru", 
                     "Underground" = "peru"  ,"Water" = "dodgerblue2")
col.scheme.realm2<- c(  "Freshwater"  = "blue", "Terrestrial" = "")
col.scheme.PA <- c(  "yes"  = "darkgreen", "no" = "white")


col.scheme.global<- c(  "Global"  = "grey10", "Observed" = "grey70")  #
col.scheme.black<- c(  "Global"  = "black", "Observed" = "black")  #




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
save(inla1.2RS, file = "inla1.2RS.RData")


inla1.3RS <- inla(log10(Number+1) ~ cYear+
                    f(Period_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                      f(Plot_ID_4INLAR,iYear,model='iid')+
                      f(Location_4INLAR,iYear,model='iid')                      +
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
                    f(Country_State_4INLA) +
                      f(Plot_ID_4INLAR,iYear,model='iid')+
                      f(Location_4INLAR,iYear,model='iid')                      +
                      f(Datasource_ID_4INLAR,iYear,model='iid')+
                      f(Country_State_4INLAR,iYear,model='iid')  +
                    f(iYear,model='ar1',replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE,waic=TRUE), verbose = T,
                  data=completeData); beep(2)
save("inla1.4RS.RData")
# breaks down also on cluster

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
                f(Datasource_ID_4INLAR,iYear,copy='Datasource_ID_4INLA')+ # what is this for? 
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
                f(Plot_ID_4INLA,model='iid')+
                f(Location_4INLA,model='iid')+
                f(Datasource_ID_4INLA,model='iid')+
                 f(Plot_ID_4INLAR,iYear,model='iid')+
                 f(Location_4INLAR,iYear,model='iid')                      +
                 f(Datasource_ID_4INLAR,iYear,model='iid')+
                f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
              control.compute = list(dic=TRUE,waic=TRUE),
              data=completeData) # has lower WAIC and DIC than inlaF2
save(inla1, file = "/data/Roel/inla1.RData")
all.results<- c(all.results, Empty_model_inla1 =  list(inla1$summary.fixed))


# Check for confounding factors####

inla1.1 <- inla(log10(Number+1) ~ cYear: cStartYear + cYear: cDuration + cYear:Country_state
                f(Period_4INLA,model='iid')+
                  f(Plot_ID_4INLA,model='iid')+
                  f(Location_4INLA,model='iid')+
                  f(Datasource_ID_4INLA,model='iid')+
                   f(Plot_ID_4INLAR,iYear,model='iid')+
                   f(Location_4INLAR,iYear,model='iid')                      +
                   f(Datasource_ID_4INLAR,iYear,model='iid')+
                  f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
              control.compute = list(dic=TRUE,waic=TRUE),
              data=completeData) # 
# not working 
save(inla1.1, file = "/data/Roel/inla1.1.RData")
load("H:/inla1.1.1.RData")
load("H:/inla1.1.2.RData")


all.results<-c(all.results, confoundersStartYear = list(inla1.1.1$summary.fixed))
all.results<-c(all.results, confoundersDuration = list(inla1.1.2$summary.fixed))




############################################################
#Pull out the random effects and slopes from the grand model
load("E:/inla1.RData")
#get list of unique plots and datasourceID
summary_df <- unique(completeData[,c("Plot_ID","Datasource_ID",
                                     "Plot_ID_4INLA","Datasource_ID_4INLA",
                                     "Plot_ID_4INLAR","Datasource_ID_4INLAR")])

RandEfDataset<-unique(completeData[,c("Datasource_ID", "Datasource_ID_4INLA", "Datasource_ID_4INLAR")])
RandEfPlot<-unique(completeData[,c("Datasource_ID", "Datasource_ID_4INLA", "Datasource_ID_4INLAR",
                                      "Location",       "Location_4INLA",      "Location_4INLAR", 
                                      "Plot_ID",        "Plot_ID_4INLA",       "Plot_ID_4INLAR" )])
#pull out random intercepts and slopes:

#data source ID
intercepts     <- inla1$summary.random$Datasource_ID_4INLA
slopes         <- inla1$summary.random$Datasource_ID_4INLAR
slopes_Location<-inla1$summary.random$Location_4INLAR
slopes_plot    <-inla1$summary.random$Plot_ID_4INLAR
names(intercepts)[2:8] <- paste("DataID_Intercept_", names(intercepts)[2:8]) # names for dataset intercepts
names(slopes)[2:8] <- paste("DataID_Slope_", names(slopes)[2:8])             # names for dataset slopes
names(slopes_Location)[2:8] <-paste("Loc_slp_", names(slopes_Location)[2:8]) # names for Location slopes
names(slopes_plot)[2:8] <-paste("Plot_slp_", names(slopes_plot)[2:8])        # names for plot slopes

# datasource level slopes for Fig 1
RandEfDataset <- merge(RandEfDataset,intercepts, by.x="Datasource_ID_4INLA", by.y="ID")
RandEfDataset <- merge(RandEfDataset,slopes, by.x="Datasource_ID_4INLAR", by.y="ID")

# add up fixed slope and random slopes
load("metadata_per_dataset.RData")
RandEfDataset<- merge(RandEfDataset, metadata_per_dataset, by = "Datasource_ID")
RandEfDataset$fixedSlp<- inla1$summary.fixed$mean[2]
RandEfDataset$fixedIntercept<- inla1$summary.fixed$mean[1]
RandEfDataset$slope <- RandEfDataset$'DataID_Slope_ mean'+ RandEfDataset$fixedSlp # sum of fixed and random slopes  
RandEfDataset$intercept <- RandEfDataset$'DataID_Intercept_ mean'+ RandEfDataset$fixedIntercept # sum of fixed and random slopes  
save(RandEfDataset, file = "RandEfDataset.RData")


# plot level random effects for Fig 4: merge together all elements
RandEfPlot <- merge(RandEfPlot,intercepts, by.x="Datasource_ID_4INLA", by.y="ID") # not really needed here
RandEfPlot <- merge(RandEfPlot,slopes,          by.x="Datasource_ID_4INLAR", by.y="ID")
RandEfPlot <- merge(RandEfPlot,slopes_Location, by.x="Location_4INLAR", by.y="ID")
RandEfPlot <- merge(RandEfPlot,slopes_plot, by.x="Plot_ID_4INLAR", by.y="ID")
RandEfPlot <- merge(metadata_per_plot, RandEfPlot )
# add up fixed slope, dataset random + location Random, + plot random 
RandEfPlot$fixedSlp<- inla1$summary.fixed$mean[2]
RandEfPlot$slope <- RandEfPlot$fixedSlp +  RandEfPlot$'DataID_Slope_ mean'  + RandEfPlot$'Plot_slp_ mean' +RandEfPlot$'Loc_slp_ mean' 
save(RandEfPlot, file = "RandEfPlot.RData")

# plot spagetti plot (Dornelas)

load("RandEfDataset.RData") # random slopes and intercepts
load("metadata_per_dataset.RData") # 
source("pframe.R") # predict lines

randomFits$Realm<- factor(randomFits$Realm, levels=c('Terrestrial','Freshwater'))
pframe$Realm <- factor(pframe$Realm, levels=c('Terrestrial','Freshwater'))

ggplot(data = pframe, aes(x= Year, y=abun,   colour = Realm)) + 
  scale_y_log10() +  
  labs(x = "", y = "Insect abundance / biomass") +
  geom_line(data=pframe, aes(x= Year, y=unlog, group = Datasource_ID,  colour = Realm), size =0.6, alpha = 0.8)+
  scale_colour_manual(values = col.scheme.realm, name = "Dataset trends")+
  theme_clean +
  facet_grid(Realm~.)


load("randomFitsFullPer.RData") # wiggly line
randomFits$abun<- 10^ (randomFits$RW.mean+ randomFits$intercept)
randomFits$c0.025quant<- randomFits$RW.0.025quant + randomFits$intercept
randomFits$c0.975quant<- randomFits$RW.0.975quant+ randomFits$intercept

  new_scale_color()+
   geom_line(data = randomFits,    aes(x=Year,  y=abun, colour=Realm2), size = 1.2)+
  scale_colour_manual(values = col.scheme.realm2, name = "Random walk model") +
  geom_ribbon(data = randomFits, aes(x=Year, ymin = 10^(intercept + RW.0.025quant),  
                                    ymax = 10^(intercept+RW.0.975quant), fill=Realm2),alpha=0.4, color = NA)+
  scale_fill_manual (values = col.scheme.realm, name = "Random walk model")+
  facet_grid(Realm~.)
 




# Fig 1B D plot map #####
  
load("RandEfDataset.RData")

  library(rgdal)
  library(sp)
  library(broom)

pts.wgs <- RandEfDataset
pts.wgs$slope<- pts.wgs$slope
pts.wgs$slope.scal<-scale(pts.wgs$slope) # rescale slopes
pts.wgs <- SpatialPointsDataFrame(coords = data.frame(lon = pts.wgs$mean_long,
                                                      lat = pts.wgs$mean_lat),
                                  proj4string = CRS(WGS84),
                                  data = pts.wgs)

setwd("C:\\Dropbox\\Dropbox\\Insect Biomass Trends/csvs") # work
source("map_preparation.R")
# scale slopes 
pts.wgs$slope.scal[pts.wgs$slope.scal<(-0.02)]<- -0.02 # 
pts.wgs$slope.scal[pts.wgs$slope.scal>(0.02)]<- 0.02

# plot on map  by Datasource # scale is set to be symmetrical over most extreme value
fw.wgs<- p.wgs+
  geom_point(data = subset(pts.rob, Realm =="Freshwater")@data ,  size = 1.8, #color = "grey30",pch = 21,
             aes(x = x,   y = y,  color = slope, group = NULL), 
             position=position_jitter(h=1, w=1)) +  #
  scale_color_viridis_c(space = "Lab" , option = "plasma" ,
                        limits = c(min(pts.rob$slope), max(pts.rob$slope)), name = 'Trend \nslope') +# "PuBuGn"
  ggtitle("Freshwater fauna") 

png("map plasma fw.png", width=4400, height=2000, res = 360)
fw.wgs
dev.off()


# terrestrial
terr.wgs<-p.wgs+
 geom_point(data = subset(pts.rob, Realm =="Terrestrial")@data, size = 1.8, #pch = 21,color = "grey30" ,
            aes(x = x,   y = y,  color = slope, group = NULL) , 
            position=position_jitter(h=1, w=1)) +
  scale_color_viridis_c(space = "Lab" , option = "plasma",
                       limits = c(min(pts.rob$slope), max(pts.rob$slope)), name = 'Trend \nslope') +# "PuBuGn"
  ggtitle("Terrestrial fauna") 

png("map plasma terr.png", width=4400, height=2000, res = 360)
terr.wgs
dev.off()


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





##########################################################################################
# descriptive statistics #####
median(metadata_per_plot$Duration) #16yrs median duration plot level 
median(metadata_per_dataset$Duration) #19 yrs median duration dataset level 
median(metadata_per_plot$Start_year) #1996 median start year plots
median(metadata_per_dataset$Start) #1987 median start year datasets

max(metadata_per_dataset$NUMBER_OF_PLOTS) #264
max(metadata_per_plot$Duration) #80 


sum(metadata_per_dataset$Realm == "Terrestrial") # 100
sum(metadata_per_dataset$Realm == "Freshwater") # 57
sum(metadata_per_plot$Realm == "Terrestrial") # 1000
sum(metadata_per_plot$Realm == "Freshwater") # 533

# percentage plots in protected Areas
sum(metadata_per_plot$PA == "yes")/nrow(metadata_per_plot)

# of full dataset
median(completeData$Year ) #2002 median year of all data
hist(completeData$Year, las = 1)

# slope trends: 
sum(RandEfDataset$`DataID_Slope_ mean`>0) / 157 #51% positive
sum(RandEfDataset$`DataID_Slope_ mean`<0) / 157 #49% negative

sum(RandEfDataset$`DataID_Slope_ 0.025quant`>0)/157 # 10 datasets 6.5% positive 
RandEfDataset[RandEfDataset$`DataID_Slope_ 0.025quant`>0, c(18:21,25, 30)]

sum(RandEfDataset$`DataID_Slope_ 0.975quant`<0)/157 # 15datasets,  9.5% positive 
RandEfDataset[RandEfDataset$`DataID_Slope_ 0.975quant`<0, c(18:21,25, 30)]


   
###################################################################################################################################################






#FINAL model for realms (no correlation slope and interceps)
inlaF <- inla(log10(Number+1) ~ cYear:Realm+ Realm + 
                f(Period_4INLA,model='iid')+
                f(Location_4INLA,model='iid')+
                f(Plot_ID_4INLA,model='iid')+
                f(Datasource_ID_4INLA,model='iid')+
                f(Plot_ID_4INLAR,iYear,model='iid')+
                f(Location_4INLAR,iYear,model='iid')                      +
                f(Datasource_ID_4INLAR,iYear,model='iid')+
                f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
              control.compute = list(dic=TRUE,waic=TRUE),
              data=completeData) # has lower WAIC and DIC than inlaF2

load("E:/inlaF.RData")

all.results<-c(all.results, Realm_model_InlaF =list(inlaF$summary.fixed))

summary(inlaF)
# percentage change per year and per decade
 data.frame(
   var =   c("FW 1 yr" ,"Terr 1 yr", "FW 10 yr", "Terr 10 yr"), 
   CI2.5 =  c((10^(inlaF$summary.fixed  [3:4,3] )-1 ) *100, (10^(inlaF$summary.fixed  [3:4,3] *10)-1)  *100),#0.025 CI
   mean =   c((10^(inlaF$summary.fixed  [3:4,1] )-1)  *100, (10^(inlaF$summary.fixed  [3:4,1] *10)-1)  *100), # proportional changes per year
   CI97.5 = c((10^(inlaF$summary.fixed  [3:4,5] )-1 ) *100, (10^(inlaF$summary.fixed  [3:4,5] *10)-1)  *100)# 0.975
   )

 (10^(inlaF$summary.fixed  [4,1] *50)-1)  *100 # -33% in 50 years in terrestrial 



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
                     f(Location_4INLAR,iYear,model='iid')                      +
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                   data=completeData)

load("E:/inlaFstrat.RData")
all.results<-c(all.results, Strata_model = list(inlaFstrat$summary.fixed))

stratSlope<- inlaFstrat$summary.fixed[7:12,]
vars<-data.frame(do.call(rbind, strsplit(rownames(stratSlope), split = ":")))
stratSlope<-cbind(stratSlope, vars)
stratSlope$X1<-gsub("Stratum", "", stratSlope$X1)
stratSlope<- merge(stratSlope, metadata_strata, by.x = "X1", by.y = "Stratum")
stratSlope$text = paste0("(", stratSlope$Datasources, " | ", stratSlope$Plots, ")")

# reorder for graph
rownames(stratSlope)<-stratSlope$X1
stratSlope$X1<- ordered(stratSlope$X1, levels = c("Water", "Underground" , "Soil surface", "Herb layer", "Trees", "Air" ))

brks<- c(-0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
e<- c("","","","","","","")


ggplot(data.frame(stratSlope))+
  geom_crossbar(aes(x=X1,y=mean, fill = X1,
                    ymin=X0.025quant,ymax=X0.975quant),position="dodge", width = 0.7, fill = "grey70")+
  coord_flip()+
  xlab ("")+ ylab("Trend slope  \n % change per year")+ #
  geom_hline(yintercept=0,linetype="dashed")+
  geom_text(aes(x = X1 , y = 0.028, label = text), size = 3) +
scale_y_continuous(breaks = brks,labels = l, limits=c(-0.015,0.032))+
  theme_clean
  



# continents #####

inlaFcont <- inla(log10(Number+1) ~ cYear: Realm:Continent + Realm + Continent + 
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Location_4INLAR,iYear,model='iid')                      +
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE,waic=TRUE),
                  data=completeData)

load("E:/inlaFcont.RData")
all.results<-c(all.results, Continents_model = list(inlaFcont$summary.fixed))


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
  geom_text(aes(x = 5.3 , y = -0.025, label = "A"),  
            size = 6, color = 1) 

#presentations version: 
contPlot<- ggplot(data.frame(subset(contSlope, Continent != "Africa"   )))+ # exclude africa,bc it has too wide CI's 
  geom_crossbar(aes(x=Continent,   y=mean, fill = Realm, 
                    ymin=X0.025quant,ymax=X0.975quant),position="dodge",alpha=0.8 ,  width =0.7)+
  scale_fill_manual(values = col.scheme.realm)+
  geom_hline(yintercept=0,linetype="dashed") +
  geom_text(aes(x = Continent , y = 0.032, label = text, fill = Realm),  
            position = position_dodge(width = 0.7), size = 3, color = 1) +
  coord_flip()+
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.028,0.036))+
  xlab ("")+ ylab("Trend slope  \n % change per year")+
  theme_clean +
  theme(legend.key=element_blank(), 
        legend.position="bottom") 

png("continent plot.png", width=2000, height=1500, res = 360)
contPlot
dev.off()


# Geographic Regions ##### 

inlaFregions <- inla(log10(Number+1) ~ cYear: Realm:Region + Realm + Region + 
                       f(Period_4INLA,model='iid')+
                       f(Location_4INLA,model='iid')+
                       f(Plot_ID_4INLA,model='iid')+
                       f(Datasource_ID_4INLA,model='iid')+
                        f(Plot_ID_4INLAR,iYear,model='iid')+
                        f(Location_4INLAR,model='iid')+   
                        f(Datasource_ID_4INLAR,iYear,model='iid')+
                       f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                     control.compute = list(dic=TRUE,waic=TRUE),
                     data=completeData)
save(inlaFregions,  file = "/data/Roel/inlaFregions.RData")
all.results<-c(all.results, Regions_model = list(inlaFregions$summary.fixed))

load("E:/inlaFregionsSMALL.RData")
metadata_region<-  completeData %>% 
  group_by(Region, Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    #Protected = dim((unique(Plot_ID, PA)))[2],
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
print(metadata_region, n = Inf)

regionSlope<- inlaFregions$summary.fixed[29:82,]
vars<-data.frame(do.call(rbind, strsplit(rownames(regionSlope), split = ":")))
regionSlope<-cbind(regionSlope, vars)
regionSlope$Realm<-gsub("Realm", "", regionSlope$X1);  regionSlope$Region<-gsub("Region", "", regionSlope$X2)
regionSlope<- merge(regionSlope, metadata_region)
regionSlope$text = paste0("(", regionSlope$Datasources, " | ", regionSlope$Plots, ")")
regionSlope$Region<- ordered(regionSlope$Region, 
          levels = rev(c("United Kingdom", "Germany" , "Europe rest West",
                         "Sweden", "Russia Northwest","Europe rest North", #
                         "Russia Central & Volga",   "Europe rest East ", 
                         "Europe rest South", "Asia East", 
                         "USA West", "USA Midwest"  , "USA Northeast","USA South", 
                         "Central America",
                         "Australia","New Zealand" ,
                          "Asia Southeast"  ,   "Asia Central", "Africa", 
                         "Canada" , "High Arctic" , "Middle east", "South America",    "Russia Ural & Siberia" )))
regionSlope$plots.ok<- regionSlope$Plots > 20
regionSlope$dataset.ok<- regionSlope$Datasources >4
regionSlope$ok <- regionSlope$plots.ok + regionSlope$dataset.ok 

brks<- c(-0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")


allVars <-expand.grid(Realm = unique(regionSlope$Realm), Region = unique(regionSlope$Region))
regionSlope<- merge(allVars, regionSlope, all.x = T)
test<-NULL
for (i in (1: length(unique(regionSlope$Region)))){
regs<-  unique(regionSlope$Region)
df<-subset(regionSlope, Region == regs[i])

if (any(is.na (df$ok))) {
  df$ok[is.na(df$ok)]<-   df$ok[!is.na(df$ok)]}
if (sum(df$ok ==0 ) ==1) {
  df[df$ok == 0 , c(3:9, 17)] <- NA     # but values need to be NA, or else they'll be included
df$ok[df$ok == 0 ] <-   df$ok[df$ok != 0 ]} # needs number >0 to be included 
 test<- rbind(test, df) 
}
regionSlope<- test

ggplot(data.frame(subset(test, ok >0 )))+ # only use >20plots or >4 datasets 
  geom_crossbar(aes(x=Region,   y=mean, fill = Realm, 
                    ymin=X0.025quant,ymax=X0.975quant),position="dodge",alpha=0.8 ,  width =0.7)+
  scale_fill_manual(values = col.scheme.realm)+
  geom_hline(yintercept=0,linetype="dashed") +
  xlab ("")+ ylab("Trend slope  \n % change per year")+ #
  geom_text(aes(x = Region , y = 0.045, label = text, fill = Realm),  
            position = position_dodge(width = 0.7), size = 3, color = 1) +
  coord_flip()+
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.03,0.050))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key=element_blank())






# biomes #####
#(tropical/ drylands / temperate / boreal-alpine )
inlaFbiom <- inla(log10(Number+1) ~ cYear: Realm:BiomeCoarse + Realm + BiomeCoarse + 
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Location_4INLAR,iYear,model='iid')                      +
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE,waic=TRUE),
                  data=completeData)
all.results<-c(all.results, Biome_model = list(inlaFbiom$summary.fixed))
load("E:/inlaFbiom.RData")

metadata_biom<-  completeData %>% 
  group_by(BiomeCoarse, Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
metadata_biom



biomSlope<- inlaFbiom$summary.fixed[6:13,]
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
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.028,0.036))+
  xlab ("")+ ylab("Trend slope  \n % change per year")+
  theme_clean +
  theme(legend.key=element_blank(), 
        legend.position="bottom")  + 
  geom_text(aes(x = 4.3 , y = -0.025, label = "B"),  
          size = 6, color = 1) 
  
png("biome plot.png", width=2000, height=1300, res = 360)
biomPlot
dev.off()


library(gridExtra)
grid.arrange(contPlot, biomPlot, nrow = 2, heights = c(4,5) )








###########################################################
#for SOM: Include a Table for with Standard deviations for each random effect

#below code shows how to convert precision to standard deviation

#pull out variance
tauPlot <-inla1$marginals.hyperpar$`Precision for Plot_ID_4INLA`
tauDatasource <-inla1$marginals.hyperpar$`Precision for Datasource_ID_4INLA`
tauPeriod <-inla1$marginals.hyperpar$`Precision for Period_4INLA`
tauLocation <-inla1$marginals.hyperpar$`Precision for Location_4INLA`
tauPlotR <-inla1$marginals.hyperpar$`Precision for Plot_ID_4INLAR`
tauLocationR<-inla1$marginals.hyperpar$`Precision for Location_4INLAR`
tauDatasourceR <-inla1$marginals.hyperpar$`Precision for Datasource_ID_4INLAR`

#convert to standard deviations
myfun <- function(x){1/sqrt(x)}
sigmaPlot <- inla.emarginal(myfun,tauPlot)
sigmaDatasource <- inla.emarginal(myfun,tauDatasource)
sigmaPeriod <- inla.emarginal(myfun,tauPeriod)
sigmaLocation <- inla.emarginal(myfun,tauLocation)
sigmaPlotR <- inla.emarginal(myfun,tauPlotR)
sigmaLocationR <- inla.emarginal(myfun,tauLocationR)
sigmaDatasourceR <- inla.emarginal(myfun,tauDatasourceR)

data.frame(Plot_intercept = sigmaPlot, 
           Dataset_intercept = sigmaDatasource,
           Location_intercept = sigmaLocation,
           Period_intercept = sigmaPeriod,
           Plot_slope = sigmaPlotR,
           Location_slope = sigmaLocationR,
           Dataset_slope = sigmaDatasourceR)









#################################################################################################
# Random walk model #####
compDat4RW<- completeData
compDat4RW$Terrestrial<- as.numeric(compDat4RW$Realm == "Terrestrial")
compDat4RW$Freshwater<- as.numeric(compDat4RW$Realm == "Freshwater")
compDat4RW$iYear2<- compDat4RW$iYear
compDat4RW$iYear3<- compDat4RW$iYear
compDat4RW$iYear4<- compDat4RW$iYear
compDat4RW$iYear5<- compDat4RW$iYear
compDat4RW$iYear6<- compDat4RW$iYear


inlaRW <- inla( log10(Number+1) ~ Realm +
                  f(iYear, Terrestrial, model='rw1') +
                  f(iYear2, Freshwater, model='rw1') +
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID,model='iid')+
                    f(Period_4INLA,model='iid') , 
                control.compute = list(dic=TRUE,waic=TRUE),     
                data=compDat4RW)
save(inlaRW, file =  "E:/inlaRW.RData")
# check what this does 
inlaRW2 <- inla( log10(Number+1) ~ Realm +
                   f(iYear, Terrestrial, model='rw1') +
                   f(iYear2, Freshwater, model='rw1') +
                   
                   f(Datasource_ID_4INLA,model='iid')+
                   f(Location_4INLA,model='iid')+
                   f(Plot_ID,model='iid')+
                   f(Period_4INLA,model='iid')+ 
                   f(Plot_ID_4INLAR,iYear3,model='iid')+
                   f(Location_4INLAR,iYear4,model='iid')                      +
                   f(Datasource_ID_4INLAR,iYear5,model='iid'),
                 # f(iYear6,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                 
                 control.compute = list(dic=TRUE,waic=TRUE),     
                 data=compDat4RW)
# exaclty the same
load("E:/inlaRW.RData")
# cut off above max and below mean for each realm 
terMax<- max(metadata_per_dataset$End[metadata_per_dataset$Realm == "Terrestrial"] )
fwMax <- max(metadata_per_dataset$End[metadata_per_dataset$Realm == "Freshwater"] )
terMin<- min(metadata_per_dataset$Start[metadata_per_dataset$Realm == "Terrestrial"] )
fwMin <- min(metadata_per_dataset$Start[metadata_per_dataset$Realm == "Freshwater"] )


rwTerr<- inlaRW$summary.random$iYear
rwTerr$Year<-  sort(unique(completeData$Year))
rwTerr$Realm <- "Terrestrial"
rwTerr$fixedInt<- inlaRW$summary.fixed[1,1] + inlaRW$summary.fixed[1,2]
rwTerr<- subset(rwTerr, Year >= terMin & Year <=terMax )

rwFW  <- inlaRW$summary.random$iYear2
rwFW$Year<- sort(unique(completeData$Year))
rwFW$Realm <- "Freshwater"
rwFW$fixedInt<- inlaRW$summary.fixed[1,1]
rwFW<- subset(rwFW, Year >= fwMin & Year <=fwMax )

RW<- rbind(rwTerr, rwFW)


ggplot(RW )+
  geom_line(aes(x=Year,y=TerrMn, color = "Terrestrial")) +
  geom_line(aes(x=Year,y=FwMn, color = "Freshwater")) +
  geom_ribbon(aes(x=Year, ymin = FwMin,  ymax = FwMax, fill="Freshwater"),alpha=0.5)+
  geom_ribbon(aes(x=Year, ymin = TerMin,  ymax = TerMax, fill= "Terrestrial"),alpha=0.5)+
  scale_fill_manual(values = col.scheme.realm)   +
  scale_color_manual(values = col.scheme.realm)   +
    labs(y = "Arthropod abundance") + 
    theme_clean




# FULL FIG 1 #####
RW$Realm<- factor(RW$Realm, levels = c("Terrestrial", "Freshwater")) # make sure that TErr is ordered first 
labs<- data.frame(
  x = c(1930, 1930), 
  y = c(50000, 50000), 
  Realm = c("Terrestrial", "Freshwater"), 
  labs = c("A", "C"))

# overal slopes 



fig1AC<-ggplot(data = RW, 
       aes(x= Year, y=10^ (RW$mean+ RW$fixedInt),   colour = Realm)) + 
  scale_y_log10() +  
  labs(x = "", y = "Insect abundance / biomass") +
  geom_line(data = RW,    aes(x=Year,  y= 10^ (RW$mean+ RW$fixedInt), colour=Realm), size = 1)+
  geom_ribbon(data = RW, aes(x=Year, ymin = 10^(fixedInt + `0.025quant`),  
                                     ymax = 10^(fixedInt + `0.975quant`), fill=Realm),alpha=0.4, color = NA)+
  scale_colour_manual(values = col.scheme.realm, name = "Random walk model") +
  scale_fill_manual (values = col.scheme.realm, name = "Random walk model")+
  theme_clean +
  geom_text(aes(x, y, label=labs, group=NULL),data=labs,  
            size = 6, color = 1) +
  
new_scale_color ()+
  geom_line(data=pframe, aes(x= Year, y=unlog, group = Datasource_ID,  colour = Realm), size =0.2, alpha = 0.7)+
  scale_colour_manual(values = col.scheme.realm, name = "Dataset trends")+
facet_grid(Realm~.)+
  theme(strip.text.y = element_blank())

ggsave(filename = "fig 1AC test5.pdf",
       plot = print(fig1AC),
      device = "pdf", 
      colormodel = "cmyk", 
      useDingbats = F)
png("Fig 1AC.png", width=2000, height=2500, res = 360)
fig1AC
dev.off()



# fig 3 random fits per continent #####
compDat4RW<- completeData
compDat4RW$EuropeTerr<- as.numeric(compDat4RW$Continent == "Europe" & compDat4RW$Realm == "Terrestrial")
compDat4RW$EuropeFw<-   as.numeric(compDat4RW$Continent == "Europe" & compDat4RW$Realm == "Freshwater")
compDat4RW$NATerr<- as.numeric(compDat4RW$Continent == "North America" & compDat4RW$Realm == "Terrestrial")
compDat4RW$NAFw<-   as.numeric(compDat4RW$Continent == "North America" & compDat4RW$Realm == "Freshwater")
compDat4RW$AsiaTerr<- as.numeric(compDat4RW$Continent == "Asia" & compDat4RW$Realm == "Terrestrial")
compDat4RW$AsiaFw<-   as.numeric(compDat4RW$Continent == "Asia" & compDat4RW$Realm == "Freshwater")
compDat4RW$LATerr<- as.numeric(compDat4RW$Continent == "Latin America" & compDat4RW$Realm == "Terrestrial")
compDat4RW$LAFw<-   as.numeric(compDat4RW$Continent == "Latin America" & compDat4RW$Realm == "Freshwater")
compDat4RW$AusTerr<- as.numeric(compDat4RW$Continent == "Australia" & compDat4RW$Realm == "Terrestrial")
compDat4RW$AusFw<-   as.numeric(compDat4RW$Continent == "Australia" & compDat4RW$Realm == "Freshwater")
compDat4RW$AfricaTerr<- as.numeric(compDat4RW$Continent == "Africa" & compDat4RW$Realm == "Terrestrial")
compDat4RW$AfricaFw<-   as.numeric(compDat4RW$Continent == "Africa" & compDat4RW$Realm == "Freshwater")

compDat4RW$iYear2<- compDat4RW$iYear
compDat4RW$iYear3<- compDat4RW$iYear
compDat4RW$iYear4<- compDat4RW$iYear
compDat4RW$iYear5<- compDat4RW$iYear
compDat4RW$iYear6<- compDat4RW$iYear
compDat4RW$iYear7<- compDat4RW$iYear
compDat4RW$iYear8<- compDat4RW$iYear
compDat4RW$iYear9<- compDat4RW$iYear
compDat4RW$iYear10<- compDat4RW$iYear
compDat4RW$iYear11<- compDat4RW$iYear
compDat4RW$iYear12<- compDat4RW$iYear


inlaRWcont <- inla( log10(Number+1) ~ Realm +
                  f(iYear,  EuropeTerr   , model='rw1') +
                  f(iYear2, EuropeFw     , model='rw1') +
                  f(iYear3, NATerr, model='rw1') +
                  f(iYear4, NAFw, model='rw1') +
                  f(iYear5, AsiaTerr, model='rw1') +
                  f(iYear6, AsiaFw, model='rw1') +
                  f(iYear7, LATerr, model='rw1') +
                  f(iYear8, LAFw, model='rw1') +
                  f(iYear9, AusTerr, model='rw1') +
                  f(iYear10, AusFw, model='rw1') +
                  f(iYear11, AfricaTerr, model='rw1') +
                  f(iYear12, AfricaFw, model='rw1') +
                  
                  f(Datasource_ID_4INLAR,iYear,model='iid')+
                  f(Location_4INLA,model='iid')+
                  f(Plot_ID,model='iid')+
                  f(Period_4INLA,model='iid') , 
                control.compute = list(dic=TRUE,waic=TRUE),     
                data=compDat4RW)

save(inlaRWcont, file = "inlaRWcont.RData")


load("E:/inlaRWcont.RData")
# grab all year random effects and realm and continent into 1 df
Year<-rep(1925:2018, 12)
Continent <- rep(c("Europe", "North America", "Asia", "Latin America", "Australia", "Africa" ), each = 2*94)
Realm <-  rep(c("Terrestrial", "Freshwater"), each = 94) 
RWs<-rbind(inlaRWcont$summary.random$iYear, inlaRWcont$summary.random$iYear2, inlaRWcont$summary.random$iYear3, inlaRWcont$summary.random$iYear4, inlaRWcont$summary.random$iYear5,
           inlaRWcont$summary.random$iYear6, inlaRWcont$summary.random$iYear7,inlaRWcont$summary.random$iYear8, inlaRWcont$summary.random$iYear9, inlaRWcont$summary.random$iYear10,
           inlaRWcont$summary.random$iYear11, inlaRWcont$summary.random$iYear12)
RWcont<-cbind(Year, Continent, Realm, RWs)
RWcont$Continent<- ordered(RWcont$Continent, levels = c("Europe", "North America" , "Asia", "Latin America", "Australia", "Africa" ))


metadata_cont<-  completeData %>% # get start and end years  
  group_by(Continent, Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
  metadata_cont$value_startYear<-NA
for (i in 1: nrow(metadata_cont)){
year  <-  metadata_cont$Start_year[i] 
if(year <1960){year = 1960}
      metadata_cont$value_startYear[i]<-
    RWcont$mean[RWcont$Continent == as.character(metadata_cont$Continent[i]) &
                RWcont$Realm ==  as.character(metadata_cont$Realm[i]) &
                RWcont$Year ==  year ]
  }

RWcont<- merge(RWcont, metadata_cont)
RWcont$goodData<- RWcont$Year >= RWcont$Start_year &RWcont$Year <= RWcont$End_year # only select years with actual data 
#Fig 3####
ggplot(subset(RWcont, goodData == T ))+
  geom_line(aes(x=Year,y=mean - value_startYear, color = Realm ))+
  scale_colour_manual(values = col.scheme.realm)+
  geom_ribbon(aes(x=Year, ymin = `0.025quant`- value_startYear ,ymax = `0.975quant`- value_startYear, fill=Realm), alpha=0.5)+
  scale_fill_manual (values = col.scheme.realm)+
  xlim (1960, 2018)+
  geom_hline(yintercept = 0, linetype="dashed") +  theme_bw()+
  facet_wrap(~Continent )+ #, scales = "free"
  labs(y = "Standardized insect abundance") +
  theme_clean +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+
  theme(strip.background =element_rect(fill="white"), 
        strip.text.x = element_text(angle = 0, hjust = 0), 
        axis.text.x  = element_text(angle=45, vjust=1, hjust = 1))

png("Fig 3.png", width=4000, height=2300, res = 360)
fig1AC
dev.off()


## # # # # # # # # # # # # # # ########################################################################## # 

# DRiveRS #####

# protected areas #####
# how many pa's changes status during the sampling period? and how many have missing data?
dim(subset(metadata_per_plot, PA == "yes"))
subset(metadata_per_plot, PAsince == 0) # 14, incl a couple that will be hard to find have mostly been fixed
subset(metadata_per_plot, PA == "yes" & is.na(PAsince)) # have been fixed

subset(metadata_per_plot, PAsince > Start_year & PAsince < End_year)[, c("Plot_ID", "Datasource_ID"  , 
                                     "Datasource_name",  "Duration", "Start_year", "End_year", "PAsince", "PAname")]
# 143 sites chenged protection status in sapling period
subset(metadata_per_plot, PAsince > End_year) [, c("Plot_ID", "Datasource_ID"  , 
                                                                         "Datasource_name",  "Duration", "Start_year", "End_year", "PAsince", "PAname")]
# 63 attained status after sampling period
metadata_pa<-  completeData %>% 
  group_by( Realm,PA) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)))
metadata_pa


inlaFpaInt <- inla(log10(Number+1) ~ cYear: PA:Realm + PA + Realm +
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Location_4INLAR,iYear,model='iid')                      +
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                   data=completeData, verbose = F, num.threads = 2)
all.results<-c(all.results, PA_model = list(inlaFpaInt$summary.fixed)) # save fixed effects
save(inlaFpaInt, file = "/data/Roel/inlaFpaInt.RData")


load("E:/inlaFpaInt.RData")
data.frame(
var =   rownames(inlaFpaInt$summary.fixed)[4:7], 
mean = (10^(inlaFpaInt$summary.fixed[4:7,1] )-1)  *100, # proportional changes per year
CI2.5 = (10^(inlaFpaInt$summary.fixed[4:7,3] )-1 ) *100,#0.025 CI
CI97.5 = (10^(inlaFpaInt$summary.fixed[4:7,5] )-1 ) *100# 0.975
)
10^(inlaFpaInt$summary.fixed[4:7,1] *10)-1 # proportional changes per decade



paSlope<- inlaFpaInt$summary.fixed[4:7,]
vars<-data.frame(do.call(rbind, strsplit(rownames(paSlope), split = ":")))
paSlope<-cbind(paSlope, vars)
paSlope$Realm<-gsub("Realm", "", paSlope$X2);  paSlope$PA<-gsub("PA", "", paSlope$X1)
paSlope$PA <-paSlope$PA
paSlope<- merge(paSlope, metadata_pa)
paSlope$text = paste0("(", paSlope$Datasources, " | ", paSlope$Plots, ") ")


brks<- c(-0.010, -0.005, 0, 0.005, 0.01, 0.015)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
e<- c("","","","","","","")


PAplot<- ggplot(data.frame(paSlope))+
  geom_crossbar(aes(x=Realm,   y=mean, fill = PA,
                    ymin=X0.025quant,ymax=X0.975quant),position="dodge",alpha=0.8 ,  width =0.7)+
  geom_hline(yintercept=0,linetype="dashed")+
  coord_flip()+
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.01, 0.015))+
  xlab ("")+ ylab("Trend slope  \n % change per year")+
  geom_text(aes(x = Realm , y = 0.014, fill = PA,  label = text), position = position_dodge(width = 1), size = 3, color = 1) +
  scale_fill_manual(name="Protection\nstatus",
                    breaks=c("no", "yes"),
                    labels=c("Unprotected", "Protected"), 
                    values = col.scheme.PA) + 
  theme_clean

png("PA plot.png", width=2000, height=700, res = 360)
PAplot
dev.off()

  






completeData$REP_AREA[is.na(completeData$REP_AREA)] <- "0"
completeData$REP_AREA<- as.numeric(completeData$REP_AREA)
inlaFpaSize <- inla(log10(Number+1) ~ cYear* log10(REP_AREA) *Realm  + 
                      f(Period_4INLA,model='iid')+
                      f(Location_4INLA,model='iid')+
                      f(Plot_ID_4INLA,model='iid')+
                      f(Datasource_ID_4INLA,model='iid')+
                      f(Plot_ID_4INLAR,iYear,model='iid')+
                      f(Location_4INLAR,iYear,model='iid')                      +
                      f(Datasource_ID_4INLAR,iYear,model='iid')+
                      f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                    control.compute = list(dic=TRUE,waic=TRUE),
                    data=completeData, verbose = F, num.threads = 2)

#excluding not-PA's 
inlaFpaSizeSEL <- inla(log10(Number+1) ~ cYear* log10(REP_AREA) *Realm  + 
                         f(Period_4INLA,model='iid')+
                         f(Location_4INLA,model='iid')+
                         f(Plot_ID_4INLA,model='iid')+
                         f(Datasource_ID_4INLA,model='iid')+
                         f(Plot_ID_4INLAR,iYear,model='iid')+
                         f(Location_4INLAR,iYear,model='iid')                      +
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
                       f(Location_4INLAR,iYear,model='iid')                      +
                       f(Datasource_ID_4INLAR,iYear,model='iid')+
                       f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                     control.compute = list(dic=TRUE,waic=TRUE),
                     data=completeData, verbose = F, num.threads = 2)

# compare to inlaF





#  LAND USE  #####

#  USing LUH2: cover of Urban and cropland in the surrounding of the sites
# resolution : 25km


# How are the values distributed?
plotData<-unique(completeData[, c( "Plot_ID", "Realm", "Continent", "Datasource_ID", "Stratum", "Location",            
                                   "Datasource_name", "biome", "BiomeCoarse",   "End_forestArea",      
                                   "End_cropArea" , "End_pastureArea" ,  "End_urbanArea", "urbanization", 
                                   "cropification", "frcCrop900m", "frcUrban900m"   ) ]) 


# crop and urban
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
 
 # urbanization and cropification
 ggplot(plotData, aes(urbanization)) + 
   geom_histogram( aes(x = urbanization,   y = ..density..), fill="blue") +
   geom_histogram( aes(x = urbanization, y = -..density..), fill= "green")+
   facet_wrap(~Realm)
 

 
 
 
# 1) land use at end of sampling period:  
############################################### #
 #Changes in land use in 25km around sites #####


inlaFlanduseT<- inla(log10(Number+1) ~  cYear* sqrt(End_cropArea)+ cYear* sqrt(End_urbanArea) +
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Location_4INLAR,iYear,model='iid')                      +
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                   data= subset(completeData, Realm == "Terrestrial"), 
                   num.threads = 2) #verbose = T,
 save(inlaFlanduseT, file = "inlaFlanduseT.RData")
 load("E:/inlaFlanduseT.RData")
 all.results<-c(all.results, Landuse_LUH2_Terrestrial = list(inlaFlanduseT$summary.fixed)) # save fixed effects
 
 inlaFlanduseFW<- inla(log10(Number+1) ~  cYear* sqrt(End_cropArea)+ cYear* sqrt(End_urbanArea) +
                        f(Period_4INLA,model='iid')+
                        f(Location_4INLA,model='iid')+
                        f(Plot_ID_4INLA,model='iid')+
                        f(Datasource_ID_4INLA,model='iid')+
                        f(Plot_ID_4INLAR,iYear,model='iid')+
                        f(Location_4INLAR,iYear,model='iid')                      +
                        f(Datasource_ID_4INLAR,iYear,model='iid')+
                        f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                      control.compute = list(dic=TRUE,waic=TRUE),
                      data= subset(completeData, Realm == "Freshwater"), 
                      num.threads = 2) #verbose = T,
 save(inlaFlanduseFW, file = "inlaFlanduseFW.RData")
 load("E:/inlaFlanduseFW.RData")
 all.results$Landuse_LUH2_FW <- (inlaFlanduseFW$summary.fixed) # save fixed effects
 
 
landusePlots<- merge(RandEfPlot,  LU )
landusePlots$Realm<- relevel(landusePlots$Realm, ref = "Terrestrial")
 
urbanPlot<- ggplot(landusePlots, aes(x=(End_urbanArea), y = slope))+  #`Plot_slp_ mean`
  geom_point (aes(color = Realm), size = 0.5 )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_x_sqrt()+
  xlab (bquote('% Urban cover per 25'~ km^2 )) + ylab ("Trend slope")+
  geom_hline(yintercept=0,linetype="dashed")+
  ylim(-0.04, 0.03)+
  facet_wrap(~Realm ) +#, scales = "free"
  theme_clean + 
  theme( strip.background = element_blank(),
         strip.text.x = element_blank())


cropPlot<-ggplot(landusePlots, aes(x=(End_cropArea), y = slope))+  #`Plot_slp_ mean`
  geom_point (aes(color = Realm), size = 0.5 )+
  scale_x_sqrt()+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  xlab (bquote('% Cropland cover per 25'~ km^2)) + ylab ("Trend slope")+
  geom_hline(yintercept=0,linetype="dashed")+
  ylim(-0.04, 0.03)+
  facet_wrap(~Realm , scales = "free") +
  theme_clean + 
  theme( strip.background = element_blank(),
         strip.text.x = element_blank())






#where Driver is Urban, crop, or CC, or Protected Area
#Explain continental effects with drivers
#Maybe include continental effects as a random intercept and slopes


# LAND USE CHANGE #####
#models (only use LUH2  = LANDSCAPE change) 
inlaFChangesTerr<- inla(log10(Number+1) ~ cYear + cYear *  urbanization + cYear * cropification +# cYear*PA +
                          f(Period_4INLA,model='iid')+
                          f(Location_4INLA,model='iid')+
                          f(Plot_ID_4INLA,model='iid')+
                          f(Datasource_ID_4INLA,model='iid')+
                          f(Plot_ID_4INLAR,iYear,model='iid')+
                          f(Location_4INLAR,iYear,model='iid')                      +
                          f(Datasource_ID_4INLAR,iYear,model='iid')+
                          f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                        control.compute = list(dic=TRUE,waic=TRUE),
                        control.inla = list(tolerance = 1e-10),
                        data=subset(completeData, Realm == "Terrestrial"), verbose = T, num.threads = 2)
save(inlaFChangesTerr, file = "/data/Roel/inlaFChangesTerr.RData")
load("E:/inlaFChangesTerr.RData")
all.results$Changes_LUH2_Terr <- (inlaFChangesTerr$summary.fixed) # negatve Urbanization effect! 


inlaFChangesFW<- inla(log10(Number+1) ~ cYear + cYear *  urbanization + cYear * cropification + #cYear:PA +
                        f(Period_4INLA,model='iid')+
                        f(Location_4INLA,model='iid')+
                        f(Plot_ID_4INLA,model='iid')+
                        f(Datasource_ID_4INLA,model='iid')+
                        f(Plot_ID_4INLAR,iYear,model='iid')+
                        f(Location_4INLAR,iYear,model='iid')                      +
                        f(Datasource_ID_4INLAR,iYear,model='iid')+
                        f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                      control.compute = list(dic=TRUE,waic=TRUE),
                      control.inla = list(tolerance = 1e-10),
                      data=subset(completeData, Realm == "Freshwater"), verbose = F, num.threads = 2)
save(inlaFChangesFW, file = "/data/Roel/inlaFChangesFW.RData")
load("G:/work/2017 idiv/inlaFChangesFW.RData")
all.results$Changes_LUH2_FW <- (inlaFChangesFW$summary.fixed) # save fixed effects





urbanizationPlot<- ggplot(landusePlots, aes(x=urbanization*100, y = slope))+  #`Plot_slp_ mean`
  geom_point (aes(color = Realm), size = 1 )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  xlab (bquote('Change in % urban cover per 25'~ km^2)) + ylab ("Trend slope")+
  geom_hline(yintercept=0,linetype="dashed")+
  facet_wrap(~Realm ) + #, scales = "free"
  theme_clean + 
  theme( strip.background = element_blank(),
         strip.text.x = element_blank())

cropificationPlot<- ggplot(landusePlots, aes(x=cropification*100, y = slope))+  #`Plot_slp_ mean`
  geom_point (aes(color = Realm) , size = 0.5)+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  xlab (bquote('Change in % cropland per 25'~ km^2)) + ylab ("Trend slope")+
  geom_hline(yintercept=0,linetype="dashed")+
  ylim(-0.04, 0.03)+
  facet_wrap(~Realm ) + #, scales = "free"
  theme_clean + 
  theme( strip.background = element_blank(),
         strip.text.x = element_blank())

library(gridExtra)
grid.arrange(urbanizationPlot, cropificationPlot, urbanPlot,cropPlot, nrow = 2)

  






# ESA CCI#####
load("percCover900m.RData")
# small scale land use models: 900 * 900 m 
landusePlots900<- merge(RandEfPlot,  percCover900m )
landusePlots900<- merge(landusePlots900, metadata_per_plot)
landusePlots900$Realm<- relevel(landusePlots900$Realm, ref = "Terrestrial")

inlaFlanduseESA<- inla(log10(Number+1) ~  cYear* Realm* frcCrop900m + cYear* Realm* frcUrban900m +
                      f(Period_4INLA,model='iid')+
                      f(Location_4INLA,model='iid')+
                      f(Plot_ID_4INLA,model='iid')+
                      f(Datasource_ID_4INLA,model='iid')+
                       f(Plot_ID_4INLAR,iYear,model='iid')+
                       f(Location_4INLAR,iYear,model='iid')                      +
                       f(Datasource_ID_4INLAR,iYear,model='iid')+
                      f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                    control.compute = list(dic=TRUE,waic=TRUE),
                    data= subset(completeData, !is.na(completeData$frcCrop900m)), 
                    num.threads = 2)#verbose = T,
save(inlaFlanduseESA, file = "/data/Roel/inlaFlanduseESA.RData")

inlaFlanduseESAterr<- inla(log10(Number+1) ~  cYear* frcCrop900m + cYear* frcUrban900m
                           f(Period_4INLA,model='iid')+
                             f(Location_4INLA,model='iid')+
                             f(Plot_ID_4INLA,model='iid')+
                             f(Datasource_ID_4INLA,model='iid')+
                             f(Plot_ID_4INLAR,iYear,model='iid')+
                             f(Location_4INLAR,iYear,model='iid')                      +
                             f(Datasource_ID_4INLAR,iYear,model='iid')+
                             f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                           control.compute = list(dic=TRUE,waic=TRUE),
                           data= subset(completeData, !is.na(completeData$frcCrop900m) & Realm == "Terrestrial"), 
                           num.threads = 2)#verbose = T,
save(inlaFlanduseESAterr, file = "/data/Roel/inlaFlanduseESAterr.RData")

inlaFlanduseESAfw<- inla(log10(Number+1) ~   cYear* frcCrop900m + cYear* frcUrban900m
                         f(Period_4INLA,model='iid')+
                           f(Location_4INLA,model='iid')+
                           f(Plot_ID_4INLA,model='iid')+
                           f(Datasource_ID_4INLA,model='iid')+
                           f(Plot_ID_4INLAR,iYear,model='iid')+
                           f(Location_4INLAR,iYear,model='iid')                      +
                           f(Datasource_ID_4INLAR,iYear,model='iid')+
                           f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                         control.compute = list(dic=TRUE,waic=TRUE),
                         data= subset(completeData, !is.na(completeData$frcCrop900m) & Realm == "Freshwater"), 
                         num.threads = 2)#verbose = T,
save(inlaFlanduseESAfw, file = "/data/Roel/inlaFlanduseESAfw.RData")

#plots
crop900mPlot<- ggplot(landusePlots900, aes(x=frcCrop900m*100, y = slope))+  #`Plot_slp_ mean`
  geom_point (aes(color = Realm), size = 0.5 )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  xlab (bquote('% Cropland per 0.81'~ km^2)) + ylab ("Trend slope")+
  geom_hline(yintercept=0,linetype="dashed")+
  ylim(-0.04, 0.03)+
  facet_wrap(~Realm , scales = "free") +
  theme_clean +
  theme( strip.background = element_blank(),
  strip.text.x = element_blank())

urban900mPlot<- ggplot(landusePlots900, aes(x=frcUrban900m*100, y = slope))+  #`Plot_slp_ mean`
  geom_point (aes(color = Realm), size = 0.5 )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+ #
  xlab (bquote('% Urban cover per 0.81'~ km^2)) + ylab ("Trend slope")+
    geom_hline(yintercept=0,linetype="dashed")+
  ylim(-0.04, 0.03)+
  facet_wrap(~Realm , scales = "free") +
  theme_clean +
  theme( strip.background = element_blank(),
         strip.text.x = element_blank()    )   #legend.position="bottom",


library(gridExtra)
grid.arrange(urbanizationPlot, cropificationPlot, 
             urbanPlot,cropPlot, 
             urban900mPlot, crop900mPlot,
             nrow = 3)













# Climate change models ######
#: CRU (whole period, low resolustion) & CHELSA 1979-2013 high resolution
# test for delta Tmean and delta Prec AND for RELATIVE delta Tmean and Delta Prec 

# CRU #####
load("CRUtpSlopes.RData")
RandEfPlot <- merge(metadata_per_plot, RandEfPlot )
CCplots<- merge(RandEfPlot, CRUtpSlopes)
CCplots$Realm<- relevel(CCplots$Realm, ref = "Terrestrial")

# relative changes in climate mean T and precipitation 
inlaFClimChangesTerr<- inla(log10(Number+1) ~ cYear + cYear *  relDeltaTmean + cYear * relDeltaPrec +# cYear*PA +
                              f(Period_4INLA,model='iid')+
                              f(Location_4INLA,model='iid')+
                              f(Plot_ID_4INLA,model='iid')+
                              f(Datasource_ID_4INLA,model='iid')+
                              f(Plot_ID_4INLAR,iYear,model='iid')+
                              f(Location_4INLAR,iYear,model='iid')                      +
                              f(Datasource_ID_4INLAR,iYear,model='iid')+
                              f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                            control.compute = list(dic=TRUE,waic=TRUE),
                            control.inla = list(tolerance = 1e-10),
                            data=subset(completeData, Realm == "Terrestrial"), verbose = T, num.threads = 2)
save(inlaFClimChangesTerr, file = "/data/Roel/inlaFClimChangesTerr.RData")
load("E:/inlaFClimChangesTerr.RData")
all.results<-c(all.results, relClimate_Change_CRU_Terr = list(inlaFClimChangesTerr$summary.fixed)) # save fixed effects


inlaFClimChangesFW<- inla(log10(Number+1) ~ cYear + cYear *  relDeltaTmean + cYear * relDeltaPrec + #cYear:PA +
                            f(Period_4INLA,model='iid')+
                            f(Location_4INLA,model='iid')+
                            f(Plot_ID_4INLA,model='iid')+
                            f(Datasource_ID_4INLA,model='iid')+
                            f(Plot_ID_4INLAR,iYear,model='iid')+
                            f(Location_4INLAR,iYear,model='iid')                      +
                            f(Datasource_ID_4INLAR,iYear,model='iid')+
                            f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                          control.compute = list(dic=TRUE,waic=TRUE),
                          control.inla = list(tolerance = 1e-10),
                          data=subset(completeData, Realm == "Freshwater"), verbose = F, num.threads = 2)
save(inlaFClimChangesFW, file = "/data/Roel/inlaFClimChangesFW.RData")
load("E:/inlaFClimChangesFW.RData")
all.results<-c(all.results, relClimate_Change_CRU_FW = list(inlaFClimChangesFW$summary.fixed)) # save fixed effects


inlaFClimChangesTerrABS<- inla(log10(Number+1) ~ cYear + cYear *  DeltaTmean + cYear * DeltaPrec +# cYear*PA +
                              f(Period_4INLA,model='iid')+
                              f(Location_4INLA,model='iid')+
                              f(Plot_ID_4INLA,model='iid')+
                              f(Datasource_ID_4INLA,model='iid')+
                              f(Plot_ID_4INLAR,iYear,model='iid')+
                              f(Location_4INLAR,iYear,model='iid')                      +
                              f(Datasource_ID_4INLAR,iYear,model='iid')+
                              f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                            control.compute = list(dic=TRUE,waic=TRUE),
                            control.inla = list(tolerance = 1e-10),
                            data=subset(completeData, Realm == "Terrestrial"), verbose = T, num.threads = 2)
save(inlaFClimChangesTerrABS, file = "/data/Roel/inlaFClimChangesTerrABS.RData")
load("E:/inlaFClimChangesTerrABS.RData")
all.results<-c(all.results, absClimate_Change_CRU_Terr = list(inlaFClimChangesTerrABS$summary.fixed)) # save fixed effects


inlaFClimChangesFWabs<- inla(log10(Number+1) ~ cYear + cYear *  DeltaTmean + cYear * DeltaPrec + #cYear:PA +
                            f(Period_4INLA,model='iid')+
                            f(Location_4INLA,model='iid')+
                            f(Plot_ID_4INLA,model='iid')+
                            f(Datasource_ID_4INLA,model='iid')+
                            f(Plot_ID_4INLAR,iYear,model='iid')+
                            f(Location_4INLAR,iYear,model='iid')                      +
                            f(Datasource_ID_4INLAR,iYear,model='iid')+
                            f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                          control.compute = list(dic=TRUE,waic=TRUE),
                          control.inla = list(tolerance = 1e-10),
                          data=subset(completeData, Realm == "Freshwater"), verbose = F, num.threads = 2)
save(inlaFClimChangesFWabs, file = "/data/Roel/inlaFClimChangesFWabs.RData")
load("E:/inlaFClimChangesFWabs.RData")
all.results<-c(all.results, absClimate_Change_CRU_FW = list(inlaFClimChangesFWabs$summary.fixed)) # save fixed effects


sz<- 1
brks<- c(-0.002,  0.002, 0.006)
TrelPlot<- ggplot(CCplots, aes(x=relDeltaTmean, y = slope))+  #`Plot_slp_ mean`
  geom_point (aes(color = Realm), size =sz )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_x_continuous(breaks = brks)+
  xlab( expression(atop("Relative change in mean temperature", '('*Delta*'T / ' *mu*'T (K))')))+
  ylab ("Trend slope")+ #
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  ylim(-0.04, 0.03)+
  facet_wrap(~Realm ) + #, scales = "free"
  theme_clean +
theme( strip.background = element_blank(),
       strip.text.x = element_blank(),panel.spacing.x = unit(4, "mm")   )   #legend.position="bottom",

PrelPlot<- ggplot(CCplots, aes(x=relDeltaPrec, y = slope))+  #`Plot_slp_ mean`
  geom_point (aes(color = Realm) , size = sz)+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  xlab( expression(atop("Relative change in precipitation", '('*Delta*'Prec / ' *mu*'Prec (mm))')))+
  ylab ("")+ #
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  ylim(-0.04, 0.03)+
  facet_wrap(~Realm ) + #, scales = "free"
  theme_clean +
  theme( strip.background = element_blank(),
         strip.text.x = element_blank() ,panel.spacing.x = unit(4, "mm")   )   #legend.position="bottom",
 
  
TmeanPlot<- ggplot(CCplots, aes(x=deltaTmean, y = slope))+  #`Plot_slp_ mean`
  geom_point (aes(color = Realm) , size = sz)+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  xlab ("Change in mean temperature per decade")+ ylab ("Trend slope")+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  ylim(-0.04, 0.03)+
  facet_wrap(~Realm , scales = "free") +
  theme_clean +
  theme( strip.background = element_blank(),
         strip.text.x = element_blank() ,panel.spacing.x = unit(4, "mm")  )   #legend.position="bottom",

PPlot<- ggplot(CCplots, aes(x=deltaPrec, y = slope))+  #`Plot_slp_ mean`
  geom_point (aes(color = Realm), size = sz )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  xlab ("Change in precipitation per decade (mm)")+ ylab ("Trend slope")+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  ylim(-0.04, 0.03)+
  facet_wrap(~Realm , scales = "free") +
  theme_clean +
  theme( strip.background = element_blank(),
         strip.text.x = element_blank() ,panel.spacing.x = unit(4, "mm")   )   #legend.position="bottom",



library(gridExtra)
grid.arrange(TmeanPlot,PPlot,  TrelPlot,  PrelPlot, nrow = 2)








# CHELSA #####

inlaFCHELSAChangesTerr<- inla(log10(Number+1) ~ cYear + cYear *  CHELSArelDeltaTmean + cYear * CHELSArelDeltaPrec +# cYear*PA +
                                f(Period_4INLA,model='iid')+
                                f(Location_4INLA,model='iid')+
                                f(Plot_ID_4INLA,model='iid')+
                                f(Datasource_ID_4INLA,model='iid')+
                                f(Plot_ID_4INLAR,iYear,model='iid')+
                                f(Location_4INLAR,iYear,model='iid')                      +
                                f(Datasource_ID_4INLAR,iYear,model='iid')+
                                f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                              control.compute = list(dic=TRUE,waic=TRUE),
                              control.inla = list(tolerance = 1e-10),
                              data=subset(completeData, Realm == "Terrestrial" & !is.na(CHELSArelDeltaTmean) ), 
                              verbose = F, num.threads = 2)
save(inlaFCHELSAChangesTerr, file = "/data/Roel/inlaFCHELSAChangesTerr.RData")
load( "E:/inlaFCHELSAChangesTerr.RData")
all.results<-c(all.results, relClimate_Change_CHELSA_Terr = list(inlaFCHELSAChangesTerr$summary.fixed)) # save fixed effects

inlaFCHELSAChangesFW<- inla(log10(Number+1) ~ cYear + cYear *  CHELSArelDeltaTmean + cYear * CHELSArelDeltaPrec + #cYear:PA +
                              f(Period_4INLA,model='iid')+
                              f(Location_4INLA,model='iid')+
                              f(Plot_ID_4INLA,model='iid')+
                              f(Datasource_ID_4INLA,model='iid')+
                              f(Plot_ID_4INLAR,iYear,model='iid')+
                              f(Location_4INLAR,iYear,model='iid')                      +
                              f(Datasource_ID_4INLAR,iYear,model='iid')+
                              f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                            control.compute = list(dic=TRUE,waic=TRUE),
                            control.inla = list(tolerance = 1e-10),
                            data=subset(completeData, Realm == "Freshwater" & !is.na(CHELSArelDeltaTmean)),
                            verbose = F, num.threads = 2)
save(inlaFCHELSAChangesFW, file = "/data/Roel/inlaFCHELSAChangesFW.RData")
load( "E:/inlaFCHELSAChangesFW.RData")
all.results<-c(all.results, relClimate_Change_CHELSA_FW = list(inlaFCHELSAChangesFW$summary.fixed)) # save fixed effects
# positive effect of Tmean , # almost positibve effect of prec increase

ABSinlaFCHELSAChangesTerr<- inla(log10(Number+1) ~ cYear + cYear *  CHELSAdeltaTmean + cYear * CHELSAdeltaPrec +# cYear*PA +
                                f(Period_4INLA,model='iid')+
                                f(Location_4INLA,model='iid')+
                                f(Plot_ID_4INLA,model='iid')+
                                f(Datasource_ID_4INLA,model='iid')+
                                f(Plot_ID_4INLAR,iYear,model='iid')+
                                f(Location_4INLAR,iYear,model='iid')                      +
                                f(Datasource_ID_4INLAR,iYear,model='iid')+
                                f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                              control.compute = list(dic=TRUE,waic=TRUE),
                              control.inla = list(tolerance = 1e-10),
                              data=subset(completeData, Realm == "Terrestrial" & !is.na(CHELSAdeltaTmean) ), 
                              verbose = F, num.threads = 2)
save(ABSinlaFCHELSAChangesTerr, file = "/data/Roel/ABSinlaFCHELSAChangesTerr.RData")
load( "E:/ABSinlaFCHELSAChangesTerr.RData")
all.results<-c(all.results, absClimate_Change_CHELSA_Terr = list(ABSinlaFCHELSAChangesTerr$summary.fixed)) # save fixed effects

ABSinlaFCHELSAChangesFW<- inla(log10(Number+1) ~ cYear + cYear *  CHELSAdeltaTmean + cYear * CHELSAdeltaPrec + #cYear:PA +
                              f(Period_4INLA,model='iid')+
                              f(Location_4INLA,model='iid')+
                              f(Plot_ID_4INLA,model='iid')+
                              f(Datasource_ID_4INLA,model='iid')+
                              f(Plot_ID_4INLAR,iYear,model='iid')+
                              f(Location_4INLAR,iYear,model='iid')                      +
                              f(Datasource_ID_4INLAR,iYear,model='iid')+
                              f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                            control.compute = list(dic=TRUE,waic=TRUE),
                            control.inla = list(tolerance = 1e-10),
                            data=subset(completeData, Realm == "Freshwater" & !is.na(CHELSAdeltaTmean)),
                            verbose = F, num.threads = 2)
save(ABSinlaFCHELSAChangesFW, file = "/data/Roel/ABSinlaFCHELSAChangesFW.RData")
load( "E:/ABSinlaFCHELSAChangesFW.RData")
all.results<-c(all.results, absClimate_Change_CHELSA_FW = list(ABSinlaFCHELSAChangesFW$summary.fixed)) # save fixed effects




# graphs (SI)
load("CHELSATmeanSlopes.RData")
load("CHELSAPrecSlopes.Rdata")
CHELSA<- merge(CHELSATmeanSlopes[, c(1:3, 8,9) ], CHELSAPrecSlopes[, c(1:2, 7,8) ])

CHELSA<- merge(RandEfPlot,  CHELSA )
CHELSAplots<- merge(CHELSA, metadata_per_plot)
CHELSAplots$Realm <- relevel(CHELSAplots$Realm, ref = "Terrestrial")


CHELSApPlot<- ggplot(CHELSAplots, aes(x=CHELSAdeltaPrec, y = slope))+  #`Plot_slp_ mean`
  geom_point (aes(color = Realm) , size = 1)+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  xlab ("Absolute change in precipitation per decade (mm)")+ ylab ("Trend slope")+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  ylim(-0.04, 0.03)+
  facet_wrap(~Realm , scales = "free") +
  theme_clean +theme( strip.background = element_blank(),
                      strip.text.x = element_blank()    )   #legend.position="bottom",




CHELSATmeanPlot<- ggplot(CHELSAplots, aes(x=CHELSAdeltaTmean, y = slope))+  #`Plot_slp_ mean`
  geom_point (aes(color = Realm) , size = 1)+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  xlab ("Absolute change in mean Temperature")+ ylab ("")+ #Trend slope
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  ylim(-0.04, 0.03)+
  facet_wrap(~Realm , scales = "free") +
  theme_clean +
  theme( strip.background = element_blank(),
         strip.text.x = element_blank()    )   #legend.position="bottom",


CHELSAPrelPlot<- ggplot(CHELSAplots, aes(x=CHELSArelDeltaPrec, y = slope))+  #`Plot_slp_ mean`
  geom_point (aes(color = Realm), size = 1 )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  ylab ("")+ #Trend slope
  xlab( expression(atop("Relative change in precipitation per decade", '('*Delta*'Prec / ' *mu*'Prec (mm))')))+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  ylim(-0.04, 0.03)+
  facet_wrap(~Realm ) +
  theme_clean +
  theme( strip.background = element_blank(),
         strip.text.x = element_blank() , panel.spacing.x = unit(4, "mm")   )   #legend.position="bottom",



brks<- c(0.00,  0.001, 0.002)
CHELSATrelPlot<- ggplot(CHELSAplots, aes(x=CHELSArelDeltaTmean, y = slope))+  #`Plot_slp_ mean`
  geom_point (aes(color = Realm), size = 1)+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_x_continuous(breaks = brks)+
  xlab( expression(atop("Relative change in mean temperature", '('*Delta*'T / ' *mu*'T (K))')))+
  ylab ("Trend slope")+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  ylim(-0.04, 0.03)+
  facet_wrap(~Realm ) +
  theme_clean +
  theme( strip.background = element_blank(),
         strip.text.x = element_blank()  ,
         panel.spacing.x = unit(4, "mm"))   #legend.position="bottom",

  


library(gridExtra)
grid.arrange(TrelPlot,  PrelPlot,  CHELSATrelPlot,  CHELSAPrelPlot, nrow = 2)






# absolute CC models? 



















































# trash #####

#REDUNDANT 
# models
CHELSAinlaFmeanTrel<- inla(log10(Number+1) ~ cYear + Realm +  cYear* Realm * CHELSArelDeltaTmean  +
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Location_4INLAR,iYear,model='iid')                      +
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                   data=subset(completeData, !is.na(CHELSAmnC) ), 
                   verbose = F, num.threads = 2)
CHELSAinlaFmeanTabs<- inla(log10(Number+1) ~ cYear + Realm +  cYear* Realm * CHELSAdeltaTmean  +
                             f(Period_4INLA,model='iid')+
                             f(Location_4INLA,model='iid')+
                             f(Plot_ID_4INLA,model='iid')+
                             f(Datasource_ID_4INLA,model='iid')+
                             f(Plot_ID_4INLAR,iYear,model='iid')+
                             f(Location_4INLAR,iYear,model='iid')                      +
                             f(Datasource_ID_4INLAR,iYear,model='iid')+
                             f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                           control.compute = list(dic=TRUE,waic=TRUE),
                           data=subset(completeData, !is.na(CHELSAmnC) ), 
                           verbose = F, num.threads = 2)
#split terr and FW
CHELSAinlaFmeanTT<- inla(log10(Number+1) ~ cYear * CHELSArelDeltaTmean    +
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Location_4INLAR,iYear,model='iid')                      +
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                   data= subset(completeData, Realm == "Terrestrial" & !is.na(CHELSAmnC))   , 
                   verbose = F, num.threads = 2)
CHELSAinlaFmeanTFW<- inla(log10(Number+1) ~ cYear * CHELSArelDeltaTmean    +
                      f(Period_4INLA,model='iid')+
                      f(Location_4INLA,model='iid')+
                      f(Plot_ID_4INLA,model='iid')+
                      f(Datasource_ID_4INLA,model='iid')+
                      f(Plot_ID_4INLAR,iYear,model='iid')+
                      f(Location_4INLAR,iYear,model='iid')                      +
                      f(Datasource_ID_4INLAR,iYear,model='iid')+
                      f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                    control.compute = list(dic=TRUE,waic=TRUE),
                    data= subset(completeData, Realm == "Freshwater" & !is.na(CHELSAmnC))   , 
                    verbose = F, num.threads = 2)
CHELSAinlaFPrel<- inla(log10(Number+1) ~ cYear + Realm +  cYear* Realm * CHELSArelDeltaPrec +
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Location_4INLAR,iYear,model='iid')                      +
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE,waic=TRUE),
                  data=subset(completeData, !is.na(CHELSAmnC) ),
                  verbose = F, num.threads = 2)
save(CHELSAinlaFPrel, file = "CHELSAinlaFPrel.RData")
CHELSAinlaFPabs<- inla(log10(Number+1) ~ cYear + Realm +  cYear* Realm * CHELSAdeltaPrec +
                             f(Period_4INLA,model='iid')+
                             f(Location_4INLA,model='iid')+
                             f(Plot_ID_4INLA,model='iid')+
                             f(Datasource_ID_4INLA,model='iid')+
                             f(Plot_ID_4INLAR,iYear,model='iid')+
                             f(Location_4INLAR,iYear,model='iid')                      +
                             f(Datasource_ID_4INLAR,iYear,model='iid')+
                             f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                           control.compute = list(dic=TRUE,waic=TRUE),
                           data=subset(completeData, !is.na(CHELSAmnC) ),
                           verbose = F, num.threads = 2)
save(CHELSAinlaFPabs, file = "CHELSAinlaFPabs.RData")
CHELSAinlaFPrelTerr<- inla(log10(Number+1) ~  cYear*  CHELSArelDeltaPrec +
                         f(Period_4INLA,model='iid')+
                         f(Location_4INLA,model='iid')+
                         f(Plot_ID_4INLA,model='iid')+
                         f(Datasource_ID_4INLA,model='iid')+
                         f(Plot_ID_4INLAR,iYear,model='iid')+
                         f(Location_4INLAR,iYear,model='iid')                      +
                         f(Datasource_ID_4INLAR,iYear,model='iid')+
                         f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                       control.compute = list(dic=TRUE,waic=TRUE),
                       data=subset(Realm == "Terrestrial" & completeData, !is.na(CHELSAmnC) ),
                       verbose = F, num.threads = 2)
save(CHELSAinlaFPrelTerr, file = "CHELSAinlaFPrelTerr.RData")
CHELSAinlaFPrelFW<- inla(log10(Number+1) ~  cYear*  CHELSArelDeltaPrec +
                             f(Period_4INLA,model='iid')+
                             f(Location_4INLA,model='iid')+
                             f(Plot_ID_4INLA,model='iid')+
                             f(Datasource_ID_4INLA,model='iid')+
                             f(Plot_ID_4INLAR,iYear,model='iid')+
                             f(Location_4INLAR,iYear,model='iid')                      +
                             f(Datasource_ID_4INLAR,iYear,model='iid')+
                             f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                           control.compute = list(dic=TRUE,waic=TRUE),
                           data=subset(Realm == "Freshwater" & completeData, !is.na(CHELSAmnC) ),
                           verbose = F, num.threads = 2)
save(CHELSAinlaFPrelFW, file = "CHELSAinlaFPrelFW.RData")

# not used: forest and passture land 
ggplot(plotData, aes(End_pastureArea)) + 
  geom_histogram( aes(x = (End_pastureArea),   y = ..density..), fill="blue") +
  geom_histogram( aes(x = (End_forestArea), y = -..density..), fill= "green")+
  facet_wrap(~Realm)

ggplot(plotData, aes(End_pastureArea)) + 
  geom_histogram( aes(x = sqrt(End_pastureArea),   y = ..density..), fill="blue") +
  geom_histogram( aes(x = sqrt(End_forestArea), y = -..density..), fill= "green")+
  facet_wrap(~Realm) # a bit better





inlaFpasture<- inla(log10(Number+1) ~ cYear + Realm + sqrt(End_pastureArea) + cYear*Realm * sqrt(End_pastureArea)  +
                      f(Period_4INLA,model='iid')+
                      f(Location_4INLA,model='iid')+
                      f(Plot_ID_4INLA,model='iid')+
                      f(Datasource_ID_4INLA,model='iid')+
                      f(Plot_ID_4INLAR,iYear,model='iid')+
                      f(Location_4INLAR,iYear,model='iid')                      +
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
                       f(Location_4INLAR,iYear,model='iid')                      +
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
                        f(Location_4INLAR,iYear,model='iid')                      +
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
                     f(Location_4INLAR,iYear,model='iid')                      +
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                   data=completeData, verbose = T, num.threads = 2)

# OBSOLETE
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





LU_per_dataset<- LU %>%    group_by(Datasource_ID) %>%
  summarise(mnUrbanization = mean(urbanization), 
            sdUrbanization = sd(urbanization))
urbanizationPlot2<-merge(RandEfDataset,  LU_per_dataset )
urbanizationPlot2$sdUrbanization[is.na(urbanizationPlot2$sdUrbanization)] <-0

ggplot(urbanizationPlot2, aes(x=mnUrbanization, y = `DataID_Slope_ mean`))+
  geom_point ()+
  geom_errorbar(aes(ymin =`DataID_Slope_ 0.025quant`  , ymax = `DataID_Slope_ 0.975quant`  ), alpha = 0.3) +
  geom_errorbarh(aes(xmin=mnUrbanization-sdUrbanization, xmax=mnUrbanization+sdUrbanization), alpha = 0.3)+
  facet_wrap(~Realm)+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black") , 
        legend.key=element_blank() )  






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
                  f(Location_4INLAR,iYear,model='iid')                      +
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
                   f(Location_4INLAR,iYear,model='iid')                      +
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








####################
# THRASH #####





# biomes original wwf biomes
selectedData<- subset(completeData, biome != "Tropical & Subtropical Dry Broadleaf Forests" & biome != "Tropical & Subtropical Grasslands, Savannas & Shrublands")
selectedData$biome<- droplevels(selectedData$biome)

inlaFbiom2 <- inla(log10(Number+1) ~ cYear: Realm:biome + Realm + biome + 
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Location_4INLAR,iYear,model='iid')                      +
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
                   f(Location_4INLAR,iYear,model='iid')                      +
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
                       #f(Country4INLA,model='iid')+   # new coutnrly level random slope ???
                       f(Plot_ID_4INLAR,iYear,model='iid')+
                       f(Location_4INLAR,iYear,model='iid')                      +
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



# Protected areas in temperate zone

completeDataTemp<-subset(completeData, BiomeCoarse == "Temperate")
inlaFpaIntTemp <- inla(log10(Number+1) ~ cYear: PA:Realm + PA + Realm +
                         f(Period_4INLA,model='iid')+
                         f(Location_4INLA,model='iid')+
                         f(Plot_ID_4INLA,model='iid')+
                         f(Datasource_ID_4INLA,model='iid')+
                         f(Plot_ID_4INLAR,iYear,model='iid')+
                         f(Location_4INLAR,iYear,model='iid')                      +
                         f(Datasource_ID_4INLAR,iYear,model='iid')+
                         f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                       control.compute = list(dic=TRUE,waic=TRUE),
                       data=completeDataTemp, verbose = T, num.threads = 2)
load("/data/Roel/inlaFpaIntTemp.RData")




inlaFurban1<- inla(log10(Number+1) ~ Realm * cYear + cYear* sqrt(End_urbanArea) +
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Location_4INLAR,iYear,model='iid')                      +
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
                     f(Location_4INLAR,iYear,model='iid')                      +
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
                    f(Location_4INLAR,iYear,model='iid')                      +
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
                    f(Location_4INLAR,iYear,model='iid')                      +
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

# REDUNDANT 
inlaFurban<- inla(log10(Number+1) ~ cYear + Realm + sqrt(End_urbanArea) + cYear* Realm * sqrt(End_urbanArea) +
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Location_4INLAR,iYear,model='iid')                      +
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE,waic=TRUE),
                  data=completeData, verbose = T, num.threads = 2)

all.results<-c(all.results, Urban_cover_3way = list(inlaFurban$summary.fixed)) # save fixed effects
#REDUNDANT 
inlaFurbanT<- inla(log10(Number+1) ~  cYear* sqrt(End_urbanArea) +
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Location_4INLAR,iYear,model='iid')                      +
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                   data= subset(completeData, Realm == "Terrestrial"), 
                   verbose = T, num.threads = 2)

#redundnat
inlaFurbanFW<- inla(log10(Number+1) ~  cYear* sqrt(End_urbanArea) +
                      f(Period_4INLA,model='iid')+
                      f(Location_4INLA,model='iid')+
                      f(Plot_ID_4INLA,model='iid')+
                      f(Datasource_ID_4INLA,model='iid')+
                      f(Plot_ID_4INLAR,iYear,model='iid')+
                      f(Location_4INLAR,iYear,model='iid')                      +
                      f(Datasource_ID_4INLAR,iYear,model='iid')+
                      f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                    control.compute = list(dic=TRUE,waic=TRUE),
                    data= subset(completeData, Realm == "Freshwater"), 
                    num.threads = 2)#verbose = T,
load("E:/inlaFurbanFW.RData")
all.results<-c(all.results, Urban_cover_FW = list(inlaFurbanFW$summary.fixed)) # save fixed effects




############################################################################## #
# crop area #####

#REDUNDANT 
inlaFcrop<- inla(log10(Number+1) ~   cYear* Realm* sqrt(End_cropArea)  +   #cYear + Realm + End_cropArea +
                   f(Period_4INLA,model='iid')+
                   f(Location_4INLA,model='iid')+
                   f(Plot_ID_4INLA,model='iid')+
                   f(Datasource_ID_4INLA,model='iid')+
                   f(Plot_ID_4INLAR,iYear,model='iid')+
                   f(Location_4INLAR,iYear,model='iid')                      +
                   f(Datasource_ID_4INLAR,iYear,model='iid')+
                   f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                 control.compute = list(dic=TRUE,waic=TRUE),
                 data=completeData,   num.threads = 2) #verbose = T,

load("E:/inlaFcrop.RData")
all.results<-c(all.results, Urban_cover_FW = list(inlaFurbanFW$summary.fixed)) # save fixed effects




# Redundant 
inlaFcropT<- inla(log10(Number+1) ~  cYear* sqrt(End_cropArea) +
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Location_4INLAR,iYear,model='iid')                      +
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE,waic=TRUE),
                  data= subset(completeData, Realm == "Terrestrial"), 
                  num.threads = 2) #verbose = T,

# rdundant 
inlaFcropFW<- inla(log10(Number+1) ~  cYear* sqrt(End_cropArea) +
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Location_4INLAR,iYear,model='iid')                      +
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE,waic=TRUE),
                   data= subset(completeData, Realm == "Freshwater"), 
                   num.threads = 2)#verbose = T,


# REDUNDANT 
inlaFurbanChange<- inla(log10(Number+1) ~ cYear + Realm + urbanization + cYear* Realm * urbanization +
                          f(Period_4INLA,model='iid')+
                          f(Location_4INLA,model='iid')+
                          f(Plot_ID_4INLA,model='iid')+
                          f(Datasource_ID_4INLA,model='iid')+
                          f(Plot_ID_4INLAR,iYear,model='iid')+
                          f(Location_4INLAR,iYear,model='iid')                      +
                          f(Datasource_ID_4INLAR,iYear,model='iid')+
                          f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                        control.compute = list(dic=TRUE,waic=TRUE),
                        data=completeData, verbose = F, num.threads = 2)
save(inlaFurbanChange, file = "/data/Roel/inlaFurbanChange.RData")

inlaFurbanChangeTerr<- inla(log10(Number+1) ~ cYear + urbanization + cYear * urbanization +
                              f(Period_4INLA,model='iid')+
                              f(Location_4INLA,model='iid')+
                              f(Plot_ID_4INLA,model='iid')+
                              f(Datasource_ID_4INLA,model='iid')+
                              f(Plot_ID_4INLAR,iYear,model='iid')+
                              f(Location_4INLAR,iYear,model='iid')                      +
                              f(Datasource_ID_4INLAR,iYear,model='iid')+
                              f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                            control.compute = list(dic=TRUE,waic=TRUE),
                            data=subset(completeData, Realm == "Terrestrial"), verbose = F, num.threads = 2)
save(inlaFurbanChangeTerr, file = "/data/Roel/inlaFurbanChangeTerr.RData")
# redundant 
inlaFurbanChangeFW<- inla(log10(Number+1) ~ cYear +  urbanization + cYear * urbanization +
                            f(Period_4INLA,model='iid')+
                            f(Location_4INLA,model='iid')+
                            f(Plot_ID_4INLA,model='iid')+
                            f(Datasource_ID_4INLA,model='iid')+
                            f(Plot_ID_4INLAR,iYear,model='iid')+
                            f(Location_4INLAR,iYear,model='iid')                      +
                            f(Datasource_ID_4INLAR,iYear,model='iid')+
                            f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                          control.compute = list(dic=TRUE,waic=TRUE),
                          data=subset(completeData, Realm == "Freshwater"), verbose = F, num.threads = 2)
save(inlaFurbanChangeFW, file = "/data/Roel/inlaFurbanChange.RData")

# redundant 
inlaFcropChange<- inla(log10(Number+1) ~ cYear + Realm + cropification + cYear* Realm * cropification +
                         f(Period_4INLA,model='iid')+
                         f(Location_4INLA,model='iid')+
                         f(Plot_ID_4INLA,model='iid')+
                         f(Datasource_ID_4INLA,model='iid')+
                         f(Plot_ID_4INLAR,iYear,model='iid')+
                         f(Location_4INLAR,iYear,model='iid')                      +
                         f(Datasource_ID_4INLAR,iYear,model='iid')+
                         f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                       control.compute = list(dic=TRUE,waic=TRUE),
                       data=completeData, verbose = F, num.threads = 2)
save(inlaFcropChange, file = "/data/Roel/inlaFcropChange.RData")

inlaFcropChangeTerr<- inla(log10(Number+1) ~ cYear + cropification + cYear * cropification +
                             f(Period_4INLA,model='iid')+
                             f(Location_4INLA,model='iid')+
                             f(Plot_ID_4INLA,model='iid')+
                             f(Datasource_ID_4INLA,model='iid')+
                             f(Plot_ID_4INLAR,iYear,model='iid')+
                             f(Location_4INLAR,iYear,model='iid')                      +
                             f(Datasource_ID_4INLAR,iYear,model='iid')+
                             f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                           control.compute = list(dic=TRUE,waic=TRUE),
                           data=subset(completeData, Realm == "Terrestrial"), verbose = F, num.threads = 2)
save(inlaFcropChangeTerr, file = "/data/Roel/inlaFcropChangeTerr.RData")

inlaFcropChangeFW<- inla(log10(Number+1) ~ cYear +  cropification + cYear * cropification +
                           f(Period_4INLA,model='iid')+
                           f(Location_4INLA,model='iid')+
                           f(Plot_ID_4INLA,model='iid')+
                           f(Datasource_ID_4INLA,model='iid')+
                           f(Plot_ID_4INLAR,iYear,model='iid')+
                           f(Location_4INLAR,iYear,model='iid')                      +
                           f(Datasource_ID_4INLAR,iYear,model='iid')+
                           f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                         control.compute = list(dic=TRUE,waic=TRUE),
                         data=subset(completeData, Realm == "Freshwater"), verbose = F, num.threads = 2)
save(inlaFurbanChangeFW, file = "/data/Roel/inlaFurbanChange.RData")


#REDUNDANT 
# absolute change in T
inlaFcruT<- inla(log10(Number+1) ~ cYear + Realm +  cYear* Realm * deltaTmean  + 
                   f(Period_4INLA,model='iid')+
                   f(Location_4INLA,model='iid')+
                   f(Plot_ID_4INLA,model='iid')+
                   f(Datasource_ID_4INLA,model='iid')+
                   f(Plot_ID_4INLAR,iYear,model='iid')+
                   f(Location_4INLAR,iYear,model='iid')                      +
                   f(Datasource_ID_4INLAR,iYear,model='iid')+
                   f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                 control.compute = list(dic=TRUE,waic=TRUE),
                 data=completeData, verbose = F, num.threads = 2)
inlaFcruTrel<- inla(log10(Number+1) ~ cYear + Realm +  cYear* Realm * relDeltaTmean  + cYear* Realm * relDeltaPrec
                    f(Period_4INLA,model='iid')+
                      f(Location_4INLA,model='iid')+
                      f(Plot_ID_4INLA,model='iid')+
                      f(Datasource_ID_4INLA,model='iid')+
                      f(Plot_ID_4INLAR,iYear,model='iid')+
                      f(Location_4INLAR,iYear,model='iid')                      +
                      f(Datasource_ID_4INLAR,iYear,model='iid')+
                      f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                    control.compute = list(dic=TRUE,waic=TRUE),
                    data=completeData, verbose = F, num.threads = 2)
inlaFcruTrelTerr<- inla(log10(Number+1) ~ cYear + Realm +  cYear* Realm * relDeltaTmean  +
                          f(Period_4INLA,model='iid')+
                          f(Location_4INLA,model='iid')+
                          f(Plot_ID_4INLA,model='iid')+
                          f(Datasource_ID_4INLA,model='iid')+
                          f(Plot_ID_4INLAR,iYear,model='iid')+
                          f(Location_4INLAR,iYear,model='iid')                      +
                          f(Datasource_ID_4INLAR,iYear,model='iid')+
                          f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                        control.compute = list(dic=TRUE,waic=TRUE),
                        data=subset(completeData, Realm == "Terrestrial") , verbose = F, num.threads = 2)
inlaFcruTrelFW<- inla(log10(Number+1) ~ cYear + Realm +  cYear* Realm * relDeltaTmean  +
                        f(Period_4INLA,model='iid')+
                        f(Location_4INLA,model='iid')+
                        f(Plot_ID_4INLA,model='iid')+
                        f(Datasource_ID_4INLA,model='iid')+
                        f(Plot_ID_4INLAR,iYear,model='iid')+
                        f(Location_4INLAR,iYear,model='iid')                      +
                        f(Datasource_ID_4INLAR,iYear,model='iid')+
                        f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                      control.compute = list(dic=TRUE,waic=TRUE),
                      data=subset(completeData, Realm == "Freshwater") , verbose = F, num.threads = 2)
inlaFmeanP<- inla(log10(Number+1) ~ cYear + Realm +  cYear* Realm * DeltaPrec  +
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Location_4INLAR,iYear,model='iid')                      +
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE,waic=TRUE),
                  data=completeData, verbose = F, num.threads = 2)
inlaFmeanPrel<- inla(log10(Number+1) ~ cYear + Realm +  cYear* Realm * relDeltaPrec  +
                       f(Period_4INLA,model='iid')+
                       f(Location_4INLA,model='iid')+
                       f(Plot_ID_4INLA,model='iid')+
                       f(Datasource_ID_4INLA,model='iid')+
                       f(Plot_ID_4INLAR,iYear,model='iid')+
                       f(Location_4INLAR,iYear,model='iid')                      +
                       f(Datasource_ID_4INLAR,iYear,model='iid')+
                       f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                     control.compute = list(dic=TRUE,waic=TRUE),
                     data=completeData, verbose = F, num.threads = 2)




# REDUNDANT
inlaFurbanESA<- inla(log10(Number+1) ~  cYear* Realm* frcUrban900m + 
                       f(Period_4INLA,model='iid')+
                       f(Location_4INLA,model='iid')+
                       f(Plot_ID_4INLA,model='iid')+
                       f(Datasource_ID_4INLA,model='iid')+
                       f(Plot_ID_4INLAR,iYear,model='iid')+
                       f(Location_4INLAR,iYear,model='iid')                      +
                       f(Datasource_ID_4INLAR,iYear,model='iid')+
                       f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                     control.compute = list(dic=TRUE,waic=TRUE),
                     data= subset(completeData, !is.na(completeData$frcCrop900m)), 
                     num.threads = 2)#verbose = T,
inlaFurbanESAterr<- inla(log10(Number+1) ~  cYear+ frcUrban900m + cYear: frcUrban900m
                         f(Period_4INLA,model='iid')+
                           f(Location_4INLA,model='iid')+
                           f(Plot_ID_4INLA,model='iid')+
                           f(Datasource_ID_4INLA,model='iid')+
                           f(Plot_ID_4INLAR,iYear,model='iid')+
                           f(Location_4INLAR,iYear,model='iid')                      +
                           f(Datasource_ID_4INLAR,iYear,model='iid')+
                           f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                         control.compute = list(dic=TRUE,waic=TRUE),
                         data= subset(completeData, !is.na(completeData$frcCrop900m) & Realm == "Terrestrial"), 
                         num.threads = 2)#verbose = T,
inlaFurbanESAfw<- inla(log10(Number+1) ~  cYear+ frcUrban900m + cYear: frcUrban900m
                       f(Period_4INLA,model='iid')+
                         f(Location_4INLA,model='iid')+
                         f(Plot_ID_4INLA,model='iid')+
                         f(Datasource_ID_4INLA,model='iid')+
                         f(Plot_ID_4INLAR,iYear,model='iid')+
                         f(Location_4INLAR,iYear,model='iid')                      +
                         f(Datasource_ID_4INLAR,iYear,model='iid')+
                         f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                       control.compute = list(dic=TRUE,waic=TRUE),
                       data= subset(completeData, !is.na(completeData$frcCrop900m) & Realm == "Freshwater"), 
                       num.threads = 2)#verbose = T,
