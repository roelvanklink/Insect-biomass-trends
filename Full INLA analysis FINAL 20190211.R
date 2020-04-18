rm(list=ls()) 

library(INLA)
library(brinla)
library(ggplot2)
library(ggnewscale)
library(tidyverse)
library(reshape2)



setwd("C:/Users/roelv/Dropbox/Insect Biomass Trends/csvs") # home
setwd("C:\\Dropbox\\Insect Biomass Trends/csvs") # work
setwd("~/Dropbox/Insect Biomass Trends/csvs")#diana mac


load("completeData.Rdata")
levels(completeData$Realm)[levels(completeData$Realm) == "Terrestrial "]<- "Terrestrial"
levels(completeData$Region)[levels(completeData$Region) == "Europe rest East "]<- "Europe rest East"
completeData$Region[completeData$Region == "Sweden" & completeData$Realm == "Terrestrial"]<- "Europe rest North"
load("completeDataArth.RData")
load("completeDataAB.RData")
#inlaRealm<- readRDS("InlaRealm5TEST.rds")
#load("all.results.RData")
load("metadata_per_dataset.RData")
load("metadata_per_plot.RData")


# include Spanish data or not? 
completeData<- subset(completeData, Datasource_ID != "1514")
completeDataArth<- subset(completeDataArth, Datasource_ID != "1514")
completeDataAB<- subset(completeDataAB, Datasource_ID != "1514")




theme_clean<- theme_grey() + theme(panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 panel.background = element_blank(), 
                                 axis.line = element_line(colour = "black") , 
                                 legend.key=element_blank())

col.scheme.cont<-c( "Europe"="green3", "Latin America"= "magenta", "North America"= "orange","Asia" = "purple3", 
                    "Africa" = "blue", "Australia" = "red")
col.scheme.biom <-c( "Tropical"="green3",  "Drylands"= "orange", 
                     "Boreal/Alpine" = "blue", "Temperate" = "red")
col.scheme.realm<-c(  "Freshwater"  = "dodgerblue2", "Terrestrial" = "peru")
col.scheme.realm<- c(  "Freshwater"  = "dodgerblue2", "Terrestrial" = "chocolate4", "Both realms" = "grey50")  #coral4
shps<- c("Freshwater" = 24, "Terrestrial" = 21, "Both realms" = 22)
col.scheme.strat<-c( "Air" = "peru", "Herb layer" = "peru", "Soil surface" = "peru", "Trees" = "peru", 
                     "Underground" = "peru"  ,"Water" = "dodgerblue2")
col.scheme.realm2<- c(  "Freshwater"  = "blue", "Terrestrial" = "")
col.scheme.PA <- c(  "yes"  = "darkgrey", "no" = "lightgrey")
col.scheme.AB <- c("biomass" = "red", "abundance" = "blue")

col.scheme.global<- c(  "Global"  = "grey10", "Observed" = "grey70")  #
col.scheme.black<- c(  "Global"  = "black", "Observed" = "black")  #

sz = 0.5
brks<- c(-0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
e<- c("","","","","","","")
lims<- c(-0.03,0.036)

mypalett<- colorRampPalette  (c("#CC0000", "#FF6666", "cornsilk2", "dodgerblue2", "dodgerblue4"), space = "rgb")


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




# test simpler structure (no location slope or intercept), because the ABrealm model keeps crashing 

load("inla1.2r.RData")
summary(inla1.2r)
# dic:  64501.57
# waic: 63924.14

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


# Check for confounding factors####


inlaConfoundEndYr  <- inla(log10(Number+1) ~  cYear+   cEndYear +  cYear: cEndYear +  #cYear: cDuration   +   cDuration  + #cYear: cStartYear + cStartYear +    
                           f(Plot_ID_4INLA,model='iid')+
                           f(Location_4INLA,model='iid')+
                           f(Datasource_ID_4INLA,model='iid')+
                           f(Plot_ID_4INLAR,iYear,model='iid')+
                           f(Location_4INLAR,iYear,model='iid')                      +
                           f(Datasource_ID_4INLAR,iYear,model='iid')+
                           f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                         control.compute = list(dic=TRUE,waic=TRUE),
                         control.inla = list(h = 16.1713), 
                         data=completeData) # 

inlaConfounDuration  <- inla(log10(Number+1) ~ cYear+    cYear: cDuration   +   cDuration +  #cEndYear +  cYear: cEndYear + + #cYear: cStartYear + cStartYear +    
                             f(Plot_ID_4INLA,model='iid')+
                             f(Location_4INLA,model='iid')+
                             f(Datasource_ID_4INLA,model='iid')+
                             f(Plot_ID_4INLAR,iYear,model='iid')+
                             f(Location_4INLAR,iYear,model='iid')                      +
                             f(Datasource_ID_4INLAR,iYear,model='iid')+
                             f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                           control.compute = list(dic=TRUE,waic=TRUE),
                           control.inla = list(h = 16.1713), 
                           data=completeData) # 

inlaConfoundStartYr  <- inla(log10(Number+1) ~  cYear+  cYear: cStartYear + cStartYear + #cEndYear +  cYear: cEndYear +  #cYear: cDuration   +   cDuration  + #    
                             f(Plot_ID_4INLA,model='iid')+
                             f(Location_4INLA,model='iid')+
                             f(Datasource_ID_4INLA,model='iid')+
                             f(Plot_ID_4INLAR,iYear,model='iid')+
                             f(Location_4INLAR,iYear,model='iid')                      +
                             f(Datasource_ID_4INLAR,iYear,model='iid')+
                             f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                           control.compute = list(dic=TRUE,waic=TRUE),
                           control.inla = list(h = 16.1713), 
                           data=completeData) # 


# 

read_rds("inlaConfoundEndSUMMARY.rds") # no effect 
read_rds("inlaConfoundStartSUMMARY.rds")# no effect 
read_rds("inlaConfoundDurSUMMARY.rds") # no effect

confEnd<- read_rds("inlaConfoundEndTEST.rds")
inla.pmarginal(0, confEnd$marginals.fixed[[4]]) # for end year: 0.81

confStart<- read_rds("inlaConfoundStartTEST.rds")
inla.pmarginal(0, confStart$marginals.fixed[[4]]) # for start: 0.177

confDur<- read_rds("inlaConfoundDurTEST.rds")
inla.pmarginal(0, confDur$marginals.fixed[[4]]) # HESSIAN PROBLEM  on 21-2-20 p = 0.59

rm(confDur)
rm(confStart)
rm(confEnd)



############################################################
#Pull out the random effects and slopes from the grand model


# Master model get random effects #####
inla1<- readRDS("inla1TEST.rds")
inla1sum<- inla1$summary.fixed

inla.pmarginal(0, inla1$marginals.fixed[[2]])
data.frame(
     var =   c("1 yr" ,"Terr 10 yr"), 
     CI2.5 =  c((10^(inla1$summary.fixed [2,4] )-1 )  *100, (10^(inla1$summary.fixed [2,4] *10)-1)  *100),#0.025 CI
     mean =   c((10^(inla1$summary.fixed [2,1] )-1)   *100, (10^(inla1$summary.fixed [2,1] *10)-1)  *100), # proportional changes per year
     CI97.5 = c((10^(inla1$summary.fixed [2,12] )-1 ) *100, (10^(inla1$summary.fixed [2,12] *10)-1)  *100)# 0.975
   )
#get list of unique plots and datasourceID
summary_df <- unique(completeData[,c("Plot_ID","Datasource_ID",
                                     "Plot_ID_4INLA","Datasource_ID_4INLA",
                                     "Plot_ID_4INLAR","Datasource_ID_4INLAR")])

RandEfDataset<-unique(completeData[,c("Datasource_ID", "Datasource_ID_4INLA", "Datasource_ID_4INLAR")])
#pull out random intercepts and slopes:

#data source ID
intercepts     <- inla1$summary.random$Datasource_ID_4INLA
slopes         <- inla1$summary.random$Datasource_ID_4INLAR
slopes_Location<-inla1$summary.random$Location_4INLAR
slopes_plot    <-inla1$summary.random$Plot_ID_4INLAR
names(intercepts)[2:ncol(intercepts)]      <- paste("DataID_Intercept_", names(intercepts)[2:ncol(intercepts)]) # names for dataset intercepts
names(slopes)[2:ncol(intercepts)]          <- paste("DataID_Slope_", names(slopes)[2:ncol(intercepts)])             # names for dataset slopes
names(slopes_Location)[2:ncol(intercepts)] <- paste("Loc_slp_", names(slopes_Location)[2:ncol(intercepts)]) # names for Location slopes
names(slopes_plot)[2:ncol(intercepts)]     <- paste("Plot_slp_", names(slopes_plot)[2:ncol(intercepts)])        # names for plot slopes

# datasource level slopes for Fig 1
RandEfDataset <- merge(RandEfDataset,intercepts, by.x="Datasource_ID_4INLA", by.y="ID")
RandEfDataset <- merge(RandEfDataset,slopes, by.x="Datasource_ID_4INLAR", by.y="ID")

# add up fixed slope and random slopes
load("metadata_per_dataset.RData")
RandEfDataset<- merge(RandEfDataset, metadata_per_dataset, by = "Datasource_ID")
fx<-data.frame( # df for fixed effects. because we use the moel with only 'year' no differences between realms
               fixedSlp = inla1$summary.fixed$mean[2], 
               fixedIntercept = (inla1$summary.fixed$mean[1]  ) )
RandEfDataset<- merge(RandEfDataset, fx )
RandEfDataset$slope <- RandEfDataset$'DataID_Slope_ mean'+ RandEfDataset$fixedSlp # sum of fixed and random slopes  
RandEfDataset$intercept <- RandEfDataset$'DataID_Intercept_ mean'+ RandEfDataset$fixedIntercept # sum of fixed and random slopes  
saveRDS(RandEfDataset, file = "RandEfDataset.rds")


# plot level random effects 
RandEfPlot<-unique(completeData[,c("Datasource_ID", "Datasource_ID_4INLA", "Datasource_ID_4INLAR",
                                   "Location",       "Location_4INLA",      "Location_4INLAR", 
                                   "Plot_ID",        "Plot_ID_4INLA",       "Plot_ID_4INLAR" )])
#RandEfPlot <- merge(RandEfPlot,intercepts, by.x="Datasource_ID_4INLA", by.y="ID") # not really needed here
RandEfPlot <- merge(RandEfPlot,slopes,          by.x="Datasource_ID_4INLAR", by.y="ID")
RandEfPlot <- merge(RandEfPlot,slopes_Location, by.x="Location_4INLAR", by.y="ID")
RandEfPlot <- merge(RandEfPlot,slopes_plot, by.x="Plot_ID_4INLAR", by.y="ID")
RandEfPlot <- merge(metadata_per_plot, RandEfPlot )
fx<-data.frame( # df for fixed effects
               fixedSlp = inla1$summary.fixed$mean[2], 
               fixedIntercept = c(inla1$summary.fixed$mean[1]) )
RandEfPlot<- merge(RandEfPlot, fx )
# add up fixed slope, dataset random + location Random, + plot random 
RandEfPlot$slope <- RandEfPlot$fixedSlp +  RandEfPlot$'DataID_Slope_ mean'  + RandEfPlot$'Plot_slp_ mean' +RandEfPlot$'Loc_slp_ mean' 
saveRDS(RandEfPlot, file = "RandEfPlot.rds")

# plot spagetti plot (Dornelas)

source("pframe.R") # predict lines

pframe$Realm <- factor(pframe$Realm, levels=c('Terrestrial','Freshwater'))

ggplot(data = pframe, aes(x= Year, y=abun,   colour = Realm)) + 
  scale_y_log10() +  
  labs(x = "", y = "Insect abundance / biomass") +
  geom_line(data=pframe, aes(x= Year, y=unlog, group = Datasource_ID,  colour = Realm), size =0.6, alpha = 0.8)+
  scale_colour_manual(values = col.scheme.realm, name = "Dataset trends")+
  theme_clean +
  facet_grid(Realm~.)


load("randomFitsFullPer.RData") # wiggly line  DEPRECATED!!!! The random walk model is no longer imlemented
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
RandEfDataset <-  readRDS(file = "RandEfDataset.rds")
  

  library(rgdal)
  library(sp)
  library(broom)

  
  
  #  ++++ RUN TWICE ####
pts.wgs <- RandEfDataset
pts.wgs$slope.lim<- pts.wgs$slope
pts.wgs$slope.scal<-scale(pts.wgs$slope) # rescale slopes

pts.wgs$slope.lim[pts.wgs$slope.lim<(-0.02)]<- -0.02 # 
pts.wgs$slope.lim[pts.wgs$slope.lim>(0.02)]<- 0.02
pts.wgs <- SpatialPointsDataFrame(coords = data.frame(lon = pts.wgs$mean_long,
                                                      lat = pts.wgs$mean_lat),
                                  proj4string = CRS(WGS84),
                                  data = pts.wgs)


setwd("C:\\Dropbox\\Insect Biomass Trends/csvs") # work
source("map_preparation.R")
####   ++++ RUN TWICE UNTIL HERE ++++ ####


# scale slopes 


# plot on map  by Datasource # scale is set to be symmetrical over most extreme value
fw.wgs <-
 p.wgs+
  geom_point(data = subset(pts.rob, Realm =="Freshwater")@data ,  size = 1.3, #color = "grey30",pch = 21,
             aes(x = x,   y = y,  color = slope.lim, group = NULL), 
             position=position_jitter(h=1, w=1)) +  #
  scale_colour_gradientn(colours = mypalett(100), limits=c(-0.02, 0.02))+
  theme(legend.position = "none")+
   ggtitle("b. Freshwater fauna") 

png("map fw.png", width=4400, height=1800, res = 360)
fw.wgs
dev.off()


# terrestrial
terr.wgs <-
  p.wgs+
 geom_point(data = subset(pts.rob, Realm =="Terrestrial")@data, size = 1.3, #pch = 21,color = "grey30" ,
            aes(x = x,   y = y,  color = slope.lim, group = NULL) , 
            position=position_jitter(h=1, w=1)) +
    scale_colour_gradientn(colours = mypalett(100), limits=c(-0.02, 0.02))+
  theme(legend.position = "none")+
  ggtitle("a. Terrestrial fauna") 

png("map terr.png", width=4400, height=2000, res = 360)
terr.wgs
dev.off()

library(gridExtra)
grid.arrange(terr.wgs,  fw.wgs, nrow = 2)
# Export ratio: 4.72 : 5.00


# for press release
mypalettPG<- colorRampPalette  (c("darkorchid4", "darkorchid2", "cornsilk2", "chartreuse2", "chartreuse4"), space = "rgb")
mypalettBR<- colorRampPalette  (c("#CC0000", "#FF6666", "cornsilk2", "dodgerblue2", "dodgerblue4"), space = "rgb")
mypalettPG<- colorRampPalette  (c("#762A83", "#9970AB",   "cornsilk2", "#5AAE61", "#1B7837"), space = "rgb") #"#C2A5CF","#E7D4E8","#D9F0D3","#A6DBA0",

datTer<- arrange(pts.rob@data, desc(slope.lim))
datFw<- arrange(pts.rob@data, (slope.lim))


FWtitleDutch <- "Zoetwaterinsecten"    ; TtitleDutch <- "Landinsecten"          ; negDutch  <- "Afname"  ;posDutch  <- "Toename"
FWtitleGerman <- "Süßwasserinsekten"  ; TtitleGerman <- "Landlebende Insekten" ; negGerman <- "Abnahme" ;posGerman <- "Zunahme" 
FWtitleEnglish <- "Freshwater insects" ; TtitleEnglish<- "Land-dwelling insects"; negEnglish<- "Decline" ;posEnglish<- "Increase" 
FWtitleRussian <- "Пресноводные насекомые"; TtitleRussian<- "Наземные насекомые"; negRussian<- "Снижение"; posRussian<- "Возрастание"

fw.wgs <-
  p.wgsLIGHT+
  geom_point(data = subset(datFw, Realm =="Freshwater") ,  size = 2, #color = "grey30",pch = 21,
             aes(x = x,   y = y,  color = slope.lim, group = NULL), 
             position=position_jitter(h=200000, w=200000)) +  #
  scale_colour_gradientn(colours = mypalettPG(100), limits=c(-0.02, 0.02), breaks = waiver(),
                         labels = c(negRussian, "", "", "", posRussian))+
  scale_x_continuous(limits = c(-12500000, 16000000 ))+
  scale_y_continuous(limits = c(-6000000, 8000000 ))+
  labs(color = "")+
  ggtitle(FWtitleRussian) + 
  theme(legend.position = "bottom")+
#  theme(legend.text = "none")+
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(), 
        plot.title = element_text(hjust = 0.5, vjust=-1), 
        plot.margin = unit(c(0, -0.5, 0, -0.5), "cm"))+
  ylab("")+xlab("")
#fw.wgs

terr.wgs <-
  p.wgsLIGHT+
  geom_point(data = subset(datTer, Realm =="Terrestrial"), size = 2, #pch = 21,color = "grey30" ,
             aes(x = x,   y = y,  color = slope.lim, group = NULL) , 
             position=position_jitter(h=200000, w=200000)) +
  scale_colour_gradientn(colours = mypalettPG(100), limits=c(-0.02, 0.02), breaks = waiver(),
                         labels = c(negRussian, "", "", "", posRussian))+
  scale_x_continuous(limits = c(-12500000, 16000000 ))+
  scale_y_continuous(limits = c(-6000000, 8000000 ))+
  labs(color = "")+
  ggtitle(TtitleRussian) +
  theme(legend.position = "bottom")+
  #  theme(legend.text = "none")+
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(), 
        plot.title = element_text(hjust = 0.5, vjust=-1), 
        plot.margin = unit(c(0, -0.5, 0, -0.5), "cm"))+ 
        
  ylab("")+xlab("") 
#terr.wgs

library(gridExtra)
grid.arrange(terr.wgs,  fw.wgs, nrow = 1)






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
median(metadata_per_plot$Duration) #15yrs median duration plot level 
median(metadata_per_dataset$Duration) #20 yrs median duration dataset level 
median(metadata_per_plot$Start_year) #1996 median start year plots
median(metadata_per_dataset$Start) #1987 median start year datasets

max(metadata_per_dataset$NUMBER_OF_PLOTS) #264
max(metadata_per_plot$Duration) #81 


sum(metadata_per_dataset$Realm == "Terrestrial") # 104
sum(metadata_per_dataset$Realm == "Freshwater") # 63
sum(metadata_per_plot$Realm == "Terrestrial") # 1066
sum(metadata_per_plot$Realm == "Freshwater") # 615

sum(studies$Abundance.Biomass == "A")
sum(studies$Abundance.Biomass == "B")
sum(studies$Abundance.Biomass == "AB")

# percentage plots in protected Areas
sum(metadata_per_plot$PA == "yes")/nrow(metadata_per_plot)

# of full dataset
median(completeData$Year ) #2003 median year of all data
hist(completeData$Year, las = 1)

# slope trends: 
sum(RandEfDataset$`DataID_Slope_ mean`>0) / 167 #51% positive
sum(RandEfDataset$`DataID_Slope_ mean`<0) / 167 #49% negative

sum(RandEfDataset$`DataID_Slope_ 0.025quant`>0)/167 # 10 datasets 6.5% positive 
RandEfDataset[RandEfDataset$`DataID_Slope_ 0.025quant`>0, c(18:21,25, 30)]

sum(RandEfDataset$`DataID_Slope_ 0.975quant`<0)/167 # 15datasets,  9.5% positive 
RandEfDataset[RandEfDataset$`DataID_Slope_ 0.975quant`<0, c(18:21,25, 30)]


#Histograms ####

# data availability
sums<- dcast(subset(completeData, !is.na(Number)), Realm + Datasource_ID + Year ~"SumNumber", value.var = "Number" , sum)

hist(sums$Year)
ggplot(sums, aes(x=Year)) + 
  geom_histogram(binwidth=1, color = "grey30", fill = "grey30") +
  ylab ("Number of\nDatasets")+
  xlim(1920, 2020)+
  facet_wrap(.~Realm, scales = "free") +
  theme_clean +
theme( strip.background = element_blank(), plot.margin=unit(c(5.5,12, 5.5, 5.5),"points"))


hists<- data.frame( level =  c(rep("Dataset", nrow(metadata_per_dataset)),rep("Plot", nrow(metadata_per_plot)) ),
  Realm = c(as.character(metadata_per_dataset$Realm), as.character(metadata_per_plot$Realm)),
                    Duration = c(metadata_per_dataset$Duration, metadata_per_plot$Duration),
                      Start = c(metadata_per_dataset$Start, metadata_per_plot$Start_year),
                      End = c(metadata_per_dataset$End, metadata_per_plot$End_year)) 


# Duration 
Ddur<- ggplot(metadata_per_dataset, aes(x= Duration)) +
         geom_histogram(aes(fill = (Realm)), alpha = 0.8, binwidth = 3) + 
  scale_fill_manual(values=col.scheme.realm)  +
         xlab ("")+ ylab("Number of studies")+  #Trend slope  \n % change per year
#ylim (0,25)+ #xlim(0,95)+
    theme_clean +
  theme(legend.key=element_blank(), 
        legend.position="none")

Pdur<- ggplot(hists, aes(x= Duration)) +
  geom_histogram(aes(fill = (Realm)), alpha = 0.8) + 
  scale_fill_manual(values=col.scheme.realm)  +
  xlab ("Study duration (years)")+ ylab("Number of plots")+  #Trend slope  \n % change per year
  facet_wrap(~level, nrow= 2, scales = "free_y" )+
#  ylim (0,600)+ xlim(5,85)+
  theme_clean +
  theme(legend.key=element_blank(), 
        legend.position="none")



Dstr<- ggplot(metadata_per_dataset, aes(x= Start)) +
  geom_histogram(aes(fill = (Realm)),  alpha = 0.8) + 
  xlab ("")+ ylab("")+  #Trend slope  \n % change per year
  scale_fill_manual(values=col.scheme.realm)  +
  ylim (0,25)+  theme_clean +
  theme(legend.key=element_blank(), 
        legend.position="none")
Pstr<- ggplot(hists, aes(x= Start)) +
  geom_histogram(aes(fill = (Realm)),  alpha = 0.8) + 
  xlab ("Start year")+ ylab("")+  #Trend slope  \n % change per year
  facet_wrap(~level, nrow= 2, scales = "free_y" )+
  scale_fill_manual(values=col.scheme.realm)  +
   theme_clean +
  theme(legend.key=element_blank(), 
        legend.position="none")


Dend<- ggplot(metadata_per_dataset, aes(x= End) ) +
  geom_histogram(aes(fill = (Realm)),  alpha = 0.8) + 
  xlab ("")+ ylab("")+  #Trend slope  \n % change per year
  scale_fill_manual(values=col.scheme.realm)  +
  ylim (0,25)+  theme_clean +
  theme(legend.key=element_blank(), 
        legend.position="none")
Pend<- ggplot(hists, aes(x= End) ) +
  geom_histogram(aes(fill = (Realm)),  alpha = 0.8) + 
  xlab ("End year")+ ylab("")+  #Trend slope  \n % change per year
  scale_fill_manual(values=col.scheme.realm)  +
  facet_wrap(~level, nrow= 2, scales = "free_y" )+
  theme_clean +
  theme(legend.key=element_blank(), 
        legend.position="none")


library(gridExtra)
grid.arrange( Pdur, Pstr, Pend, nrow = 1)

metadata_per_dataset$Continent<- factor(metadata_per_dataset$Continent, levels = c("Europe","North America", "Asia" ,  "Latin America" , "Australia","Africa" ))
metadata_per_dataset$Realm<- factor(metadata_per_dataset$Realm, levels = c("Terrestrial","Freshwater" ))

contDatasets<- ggplot(metadata_per_dataset,  aes(x=Continent, fill=Realm)) +
       geom_histogram( color="#e9ecef", alpha=1, stat = "count", position="dodge") +
       scale_fill_manual(values=col.scheme.realm)  +
  xlab( "") + ylab ("Number of datasets")     +
  labs(fill="")+
  theme_clean+
  theme(legend.position = c(0.8, 0.9), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


metadata_per_plot$Continent<- factor(metadata_per_plot$Continent, levels = c("Europe","North America", "Asia" ,  "Latin America" , "Australia","Africa" ))
metadata_per_plot$Realm<- factor(metadata_per_plot$Realm, levels = c("Terrestrial","Freshwater" ))
contPlots<- ggplot(metadata_per_plot,  aes(x=Continent, fill=Realm)) +
  geom_histogram( color="#e9ecef", alpha=1, stat = "count", position="dodge") +
  scale_fill_manual(values=col.scheme.realm)  +
  xlab( "") + ylab ("Number of plots")     +
  labs(fill="")+
  theme_clean+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

gA <- ggplotGrob(contDatasets)
gB <- ggplotGrob(contPlots)
gC <- ggplotGrob(taxaPlot)

grid::grid.newpage()
grid::grid.draw(rbind(gA, gB))

grid.arrange(
  grobs = list(gC, gA, gB),
  widths = c(1, 1.5),
  layout_matrix = rbind(c( 2,1),
                        c( 3,1))
)


ggplot(hists, aes (Duration)) + 
  geom_histogram(subset(hists, level == "Dataset"), aes(x = Duration, y = ..density..)) + 
  geom_histogram( aes(x = subset(hists, level == "Plot")$Duration, y = -..density..))


ggplot(metadata_per_dataset,  aes(x=Duration, fill=Realm) ) +
 geom_histogram( aes(x = Duration, y = ..density..)) + 
  scale_fill_manual(values=col.scheme.realm)  +
  geom_histogram( aes(x = Start, y = -..density..),  fill= "green")





###################################################################################################################################################
# Realm ####

#FINAL model for realms (no correlation slope and interceps)
InlaRealm <- inla(log10(Number+1) ~  cYear:Realm+ Realm + 
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Location_4INLAR,iYear,model='iid') +
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE,waic=TRUE),
                  control.predictor = list(link = 1),
                  data=completeData) # has lower WAIC and DIC than InlaRealm2

inlaRealmSum<- as.data.frame(readRDS("InlaRealmSUMMARY.rds"))
inlaRealm<- readRDS("InlaRealmTEST.rds")

# get probabilities of including 0 , one sided test
ps<- NULL
for(i in 3: nrow(inlaRealmSum)){
  p<-inla.pmarginal(0, inlaRealm$marginals.fixed[[i]])
  ps<- c(ps, p) };ps

# fitted vs predicted values (figure not in paper)  Looks good!  
completeData$preds <- inlaRealm$summary.fitted.values$mean  #need to have control.predictor = list(link = 1) in model
ggplot(completeData,aes(x=preds,y=log10(Number+1)))+
  geom_point (aes(color = Realm), size =sz)+
  geom_abline(slope=1,intercept=0)+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  xlab ("Predicted values") + ylab ("Observed values")+
  facet_wrap(.~Realm, scales = "free")+
 theme_clean + 
  theme( strip.background = element_blank())

metadata_realm<-  completeData %>% 
  group_by(Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID))) 

metadata_realm$Unit<- "Abundance &\n biomass"

Rslope<- inlaRealmSum[3:4,]
varsR<- data.frame(do.call(rbind, strsplit(rownames(Rslope), split = ":")))
Rslope<-cbind(Rslope, varsR)
Rslope$X3<- Rslope$X2
Rslope$Realm<-gsub("Realm", "", Rslope$X1)
Rslope$Unit<- "Abundance &\n biomass"
Rslope$AB <-Rslope$Unit
Rslope<- merge(Rslope, metadata_realm)
Rslope$P<- ps

# percentage change per year and per decade
data.frame(
  var =   c("FW 1 yr" ,"Terr 1 yr", "FW 10 yr", "Terr 10 yr"), 
  CI2.5 =  c((10^(inlaRealmSum  [3:4,4]  )-1 ) *100, (10^(inlaRealmSum  [3:4,4] *10)-1 )  *100),#0.025 CI
  mean =   c((10^(inlaRealmSum  [3:4,1]  )-1)  *100, (10^(inlaRealmSum  [3:4,1] *10)-1 )  *100), # proportional changes per year
  CI97.5 = c((10^(inlaRealmSum  [3:4,12] )-1 ) *100, (10^(inlaRealmSum  [3:4,12] *10)-1)  *100)# 0.975
)

(10^(inlaRealmSum  [4,1] *75)-1)  *100 # compare to Hallman 
(10^(-0.02161 *36)-1)  *100 # Lister = ~ -83.32 in 36 years
(10^(-0.02161)-1)  *100 # Lister -4.85 per year 

# Lister & garcia change per year 
(10^(x *36)-1)  *100 #= -84% (1/6)
(10^(x *36)-1) = -0.84
(10^(x *36)) = 0.16
log10(0.16667) = -0.778
-0.778/36 = -0.02161

table(inlaRealm$cpo$failure)
cpo = inlaRealm$cpo$cpo
ind <- 1:length(cpo)
df = data.frame(ind,cpo,
                realm=completeData$Realm,
                unit=completeData$Unit,
                dataset=completeData$Datasource_ID)

qplot(ind,cpo,data=subset(df,cpo<0.05))+
  geom_point(aes(colour=factor(dataset)))+
  facet_wrap(realm~unit) # some datasets have weird values

pit <- inlaRealm$cpo$pit
n = length(sort(pit))
samples <- (1:n)/(n+1)
plot(samples, sort(pit), xlab="uniform quantiles", ylab="Sorted PIT values")
abline(0,1)



#for SOM: Include a Table for with Standard deviations for each random effect

#below code shows how to convert precision to standard deviation

#pull out variance
inlaRealm<- readRDS("InlaRealmTEST.rds")
load("inlaPCcor0.r-5652192.RData")

inla1<-inlaRealm
inla1<- inlaFpcCor0
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
















# biomass vs abundance  #####
#data (uses dataframe with both Units for datasets that have both) #####
metadata_AB<-  completeDataAB %>% 
  group_by( Realm, Unit) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)))
metadata_AB


inlaABrealm <- inla(log10(Number+1) ~ cYear: Realm: Unit +  Unit +Realm +
                        f(Period_4INLA,model='iid')+
                        f(Plotunit_4INLA,model='iid')+
                        f(Locunit_4INLA,model='iid')+
                        f(DSunit_4INLA,model='iid')+
                        f(Plotunit_4INLAR,iYear,model='iid')+
                        f(Locunit_4INLAR,iYear,model='iid')                      +
                        f(DSunit_4INLAR,iYear,model='iid')+
                        f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                      control.compute = list(dic=TRUE,waic=TRUE),
                      data=completeDataAB) # 


inlaABrealm<-as.data.frame(readRDS("inlaABrealm-6235720inlaABrealmSUMMARY.rds"))
#names(inlaABrealm)<- make.names(names(inlaABrealm))
inlaABrealmTEST<-(readRDS("inlaABrealm-6235720inlaABrealmTEST.rds"))

ps<- NULL
for(i in 4: nrow(inlaABrealm)){
  p<-inla.pmarginal(0, inlaABrealmTEST$marginals.fixed[[i]])
  ps<- c(ps, p) }; ps


(inlaABrealm)
data.frame(
  var =   rownames(inlaABrealm)[4:7], 
  mean = (10^(inlaABrealm[4:7,1] )-1)  *100, # proportional changes per year
  CI2.5 = (10^(inlaABrealm[4:7,3] )-1 ) *100,#0.025 CI
  CI97.5 = (10^(inlaABrealm[4:7,5] )-1 ) *100# 0.975
)
10^(inlaABrealm$mean[4:7] *10)-1 # proportional changes per decade
10^(inlaABrealm$mean[4:7] *30)-1 # proportional changes per 30 yrs



ABSlope<- inlaABrealm[4:7,]
vars<-data.frame(do.call(rbind, strsplit(rownames(ABSlope), split = ":")))
ABSlope<-cbind(ABSlope, vars)
ABSlope$Realm<-gsub("Realm", "", ABSlope$X2)
ABSlope$Unit<-gsub("Unit", "", ABSlope$X1)
ABSlope$AB <-ABSlope$Unit
ABSlope$P<- ps
ABSlope<- merge(ABSlope, metadata_AB)

ABSlope<- rbind(Rslope, ABSlope)


ABSlope$text = paste0("(", ABSlope$Datasources, " | ", ABSlope$Plots, ") ")
ABSlope$Unit[ABSlope$Unit == "abundance"]<- "Abundance" ; ABSlope$Unit[ABSlope$Unit == "biomass"] <- "Biomass" 
ABSlope$Unit<- ordered(ABSlope$Unit, levels = rev(c("Abundance &\n biomass", "Abundance", "Biomass" ,  "Abundance &\n  biomass")))


inla1<- (readRDS("inla1TEST.rds")) # get results from null model: 
ABSlope<- rbind(ABSlope, 
  data.frame(Realm = "Both realms", 
             Unit = "Abundance &\n  biomass",
             inla1$summary.fixed[2,], 
             X1 = "RealmFreshwater",
             X2 = "cYear", 
             X3 = "cYear", 
             AB = "Abundance &\n  biomass",  
             Datasources = 166,   
             Plots = 1676, 
             text = "(166 | 1676)" , 
             P = inla.pmarginal(0, inla1$marginals.fixed[[2]])))
ABSlope$Realm<- ordered(as.factor(ABSlope$Realm), levels = rev(c("Both realms",  "Terrestrial", "Freshwater" )))

saveRDS(ABSlope, file = "ABslope.rds")
#brks<- c(-0.010, -0.005, 0, 0.005, 0.01, 0.015)
#perc<-(10^(brks )  *100) - 100
#l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
#e<- c("","","","","","","")

# Fig 2A ####
ABSlope<- read_rds("ABslope.rds")

ABplot<- ggplot(data.frame(ABSlope))+
  geom_errorbar(aes(x=Unit,ymin=X0.025quant,ymax= X0.975quant, color = Realm),alpha = 0.5,
                size = 1, width=0,   position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Unit,ymin=X0.05quant,ymax= X0.95quant, color = Realm),alpha = 0.75,
                size = 2, width = 0,  position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Unit,ymin=X0.1quant,ymax= X0.9quant, color = Realm), alpha = 1,
                size = 3, width=0, position=position_dodge(width= 0.7))+  
  geom_point(aes(x=Unit,   y=mean,  shape = Realm), 
             size = 2.5, position=  position_dodge(width = 0.7), color = "black", fill = "black",  alpha=1 )+
  scale_color_manual(values = col.scheme.realm)+
  scale_fill_manual(values = col.scheme.realm)+
  scale_shape_manual(values = shps)+
  geom_hline(yintercept=0,linetype="dashed")+
  coord_flip()+
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.030,  0.034))+
  xlab ("")+ ylab("")+  #Trend slope  \n % change per year
  geom_text(aes(x = Unit , y = 0.028, label = text, fill = Realm),  
            position = position_dodge(width = 0.7), size = 3, color = 1) +
  theme_clean +
  theme(legend.key=element_blank(), 
        legend.position="bottom", 
        axis.text.x=element_blank()) +
  geom_text(aes(x = 4.3 , y = -0.025, label = "A"),  
            size = 6, color = 1) 


png("./figures/AB plot.png", width=2000, height=700, res = 360)
ABplot
dev.off()





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

inlaFstrat<- read_rds("inlaFstratTEST.rds")
stratSlope<- as.data.frame(readRDS("inlaFstratSUMMARY.rds"))[7:12,]
ps<- NULL
for(i in 7: nrow(inlaFstrat$summary.fixed )){
  p<-inla.pmarginal(0, inlaFstrat$marginals.fixed[[i]])
  ps<- c(ps, p) } ; ps


vars<-data.frame(do.call(rbind, strsplit(rownames(stratSlope), split = ":")))
stratSlope<-cbind(stratSlope, vars)
stratSlope$X1<-gsub("Stratum", "", stratSlope$X1)

stratSlope$P <- round(ps, 3)
stratSlope$ptxt<- paste0("p = ", round(stratSlope$P, 3))
stratSlope<- merge(stratSlope, metadata_strata, by.x = "X1", by.y = "Stratum")
stratSlope$text = paste0("(", stratSlope$Datasources, " | ", stratSlope$Plots, ")")
  
# reorder for graph
stratSlope$X1[stratSlope$X1 == "Underground"]<- "Below ground"
stratSlope$X1<- ordered(stratSlope$X1, levels = c("Water", "Below ground" , "Soil surface", "Herb layer", "Trees", "Air" ))
rownames(stratSlope)<-stratSlope$X1

brks<- c(-0.02, -0.01, 0, 0.01, 0.02)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")

# Fig S2 strata #####
strataplot<- ggplot(data.frame(stratSlope))+
#  geom_errorbar(aes(x=X1 ,ymin=mean-sd, ymax=mean+sd), alpha = 0.60, 
#                size = 3, width=0, position=position_dodge(width= 0.7),color = "grey50")+  
  geom_errorbar(aes(x=X1,ymin=X0.025quant,ymax=X0.975quant), alpha = 0.5,
                size = 1, width=0, position=position_dodge(width= 0.7), color = "grey50")+  
  geom_errorbar(aes(x=X1,ymin=X0.05quant,ymax=X0.95quant), alpha = 0.75,
                size = 2,width=0, position=position_dodge(width= 0.7), color = "grey50")+  
  geom_errorbar(aes(x=X1,ymin=X0.1quant,ymax= X0.9quant),
                size = 3, width=0, position=position_dodge(width= 0.7), color = "grey50")+  
  geom_point(aes(x=X1,   y=mean), shape = 16, color = "black", fill = "black",  alpha=1,
             size = 2.5, position=  position_dodge(width = 0.7))+
  coord_flip()+
  xlab ("")+ ylab("Trend slope  \n % change per year")+ #
  geom_hline(yintercept=0,linetype="dashed")+
  geom_text(aes(x = X1 , y = 0.028, label = text), size = 3) +
  geom_text(aes(x = X1 , y = 0.037, label = ptxt), size = 2.5) +
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.030,  0.04))+
  theme_clean
  
png("./figures/Strata plot.png", width=2000, height=1500, res = 360)
strataplot
dev.off()

geom_errorbar(aes(x=Unit,ymin=X0.025quant,ymax= X0.975quant, color = Realm),alpha = 0.5,
              size = 1, width=0,   position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Unit,ymin=X0.05quant,ymax= X0.95quant, color = Realm),alpha = 0.75,
                size = 2, width = 0,  position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Unit,ymin=X0.1quant,ymax= X0.9quant, color = Realm), alpha = 1,
                size = 3, width=0, position=position_dodge(width= 0.7))+  
  geom_point(aes(x=Unit,   y=mean,  shape = Realm), 
             size = 2.5, position=  position_dodge(width = 0.7), color = "black", fill = "black",  alpha=1 )+
  

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

inlaFcont<- readRDS("inlaFcontTEST.rds")

completeData$preds <- inlaFcont$summary.fitted.values$mean  #need to have control.predictor = list(link = 1) in model
corplot<- ggplot(completeData,aes(x=preds,y=log10(Number+1)))+
  geom_point (aes(color = Realm), size =sz)+
  geom_abline(slope=1,intercept=0)+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  xlab ("Predicted values") + ylab ("Observed values")+
  facet_wrap(Continent~Realm, scales = "free")+
  theme_clean + 
  theme( strip.background = element_blank())


png("./figures/Fig S7 cor pred obs.png", width=6500, height=600, res = 360)
corplot
dev.off()


#model check: 
plot(inlaFcont$cpo$cpo, main = "CPO")

pit <- inlaFcont$cpo$pit
n = nrow(completeData)
samples <- (1:n)/(n+1)
plot(samples[1:length(sort(pit))], sort(pit), xlab="uniform quantiles", ylab="Sorted PIT values")
abline(0,1)



metadata_cont<-  completeData %>% 
  group_by(Continent, Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
metadata_cont


inlaFcont<- readRDS("inlaFcontTEST.rds")
ps<- NULL
for(i in 8: nrow(inlaFcont$summary.fixed )){
  p<-inla.pmarginal(0, inlaFcont$marginals.fixed[[i]])
  ps<- c(ps, p) }

contSlope<- as.data.frame(readRDS("inlaFcontSUMMARY.rds"))[8:19,]

# africa
data.frame(
  var =   c("FW 1 yr" ,"Terr 1 yr"), 
  CI2.5 =  c((10^(contSlope  [3:4,4]  )-1 ) *100),#0.025 CI
  mean =   c((10^(contSlope  [3:4,1]  )-1)  *100), # proportional changes per year
  CI97.5 = c((10^(contSlope  [3:4,12] )-1 ) *100)# 0.975
)


vars<-data.frame(do.call(rbind, strsplit(rownames(contSlope), split = ":")))
contSlope<-cbind(contSlope, vars)
contSlope$Realm<-gsub("Realm", "", contSlope$X1);  contSlope$Continent<-gsub("Continent", "", contSlope$X2)
contSlope$P <- ps
contSlope<- merge(contSlope, metadata_cont)
contSlope$text = paste0("(", contSlope$Datasources, " | ", contSlope$Plots, ")")
contSlope$Continent<- ordered(contSlope$Continent, levels = rev(c("Europe", "North America" , "Asia", "Latin America", "Australia", "Africa" )))

saveRDS(contSlope, file = "contSlope.rds")


contSlope<- read_rds( "contSlope.rds")
# Fig 2b continents #####
contPlot<- 
ggplot(data.frame(subset(contSlope, Continent != "Africa"   )))+ # exclude africa,bc it has too wide CI's 
#  geom_errorbar(aes(x=Continent,ymin=mean-sd, ymax=mean+sd, color = Realm), apl,
#                size = 2, width=0, position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Continent,ymin=X0.025quant,ymax= X0.975quant, color = Realm),alpha = 0.5,
                size = 1, width=0,   position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Continent ,ymin=X0.05quant,ymax= X0.95quant, color = Realm),alpha = 0.75,
                size = 2, width = 0,  position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Continent ,ymin=X0.1quant,ymax= X0.9quant, color = Realm), alpha = 1,
                size = 3, width=0, position=position_dodge(width= 0.7))+  
  geom_point(aes(x=Continent ,   y=mean,  shape = Realm), 
             size = 2.5, position=  position_dodge(width = 0.7), color = "black", fill = "black",  alpha=1 )+
  scale_color_manual(values = col.scheme.realm)+
  scale_fill_manual(values = col.scheme.realm)+
  scale_shape_manual(values = shps)+
  geom_hline(yintercept=0,linetype="dashed") +
  geom_text(aes(x = Continent , y = 0.03, label = text, fill = Realm),  
            position = position_dodge(width = 0.7), size = 3, color = 1) +
  coord_flip()+
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.03,0.034))+
  xlab ("")+ ylab("")+ #Trend slope | \n % change per year
  theme_clean +
  theme(legend.key=element_blank(),
        legend.position='none', 
        axis.text.x=element_blank()) +
  geom_text(aes(x = 5.3 , y = -0.025, label = "B"),  
            size = 6, color = 1) 
#
png("continent plot.png", width=2000, height=1500, res = 360)
contPlot
dev.off()





#presentations version: 
contPlot<- ggplot(data.frame(subset(contSlope, Continent != "Africa"   )))+ # exclude africa,bc it has too wide CI's 
  geom_errorbar(aes(x=Continent,ymin=X0.025quant,ymax= X0.975quant, color = Realm),alpha = 0.5,
                size = 1, width=0,   position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Continent ,ymin=X0.05quant,ymax= X0.95quant, color = Realm),alpha = 0.75,
                size = 2, width = 0,  position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Continent ,ymin=X0.1quant,ymax= X0.9quant, color = Realm), alpha = 1,
                size = 3, width=0, position=position_dodge(width= 0.7))+  
  geom_point(aes(x=Continent ,   y=mean,  shape = Realm), 
             size = 2.5, position=  position_dodge(width = 0.7), color = "black", fill = "black",  alpha=1 )+
  scale_color_manual(values = col.scheme.realm)+
  scale_fill_manual(values = col.scheme.realm)+
  scale_shape_manual(values = shps)+
  geom_hline(yintercept=0,linetype="dashed") +
  geom_text(aes(x = Continent , y = 0.030, label = text, fill = Realm),  
            position = position_dodge(width = 0.7), size = 3, color = 1) +
  coord_flip()+
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.03,0.034))+
  xlab ("")+ ylab("Trend slope  \n % change per year")+
  theme_clean +
  theme(legend.key=element_blank(), 
        legend.position="bottom") 

png("continent plot.png", width=2000, height=1500, res = 360)
contPlot
dev.off()


# Regions ##### 

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

metadata_region<-  completeData %>% 
  group_by(Region, Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    #Protected = dim((unique(Plot_ID, PA)))[2],
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
print(metadata_region, n = Inf)


inlaFregions<- readRDS("inlaFregionsTEST.rds")
ps<- NULL
for(i in 30: nrow(inlaFregions$summary.fixed )){
  p<-inla.pmarginal(0, inlaFregions$marginals.fixed[[i]])
  ps<- c(ps, p) }


regionSlope<- as.data.frame(readRDS("inlaFregionsSUMMARY.rds"))[30:79,]
vars<-data.frame(do.call(rbind, strsplit(rownames(regionSlope), split = ":")))
regionSlope<-cbind(regionSlope, vars)
regionSlope$Realm<-gsub("Realm", "", regionSlope$X1);  regionSlope$Region<-gsub("Region", "", regionSlope$X2)
regionSlope$P<- ps
regionSlope$ptxt<- paste0("p = ", format(round(regionSlope$P, 3), nsmall = 3))
regionSlope<- merge(regionSlope, metadata_region)
regionSlope$text = paste0("(", regionSlope$Datasources, " | ", regionSlope$Plots, ")")
regionSlope$Region<- ordered(regionSlope$Region, 
          levels = rev(c("United Kingdom", "Germany" , "Europe rest West",
                         "Sweden", "Russia Northwest","Europe rest North", #
                         "Russia Central & Volga",   "Europe rest East", 
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


# include NA for missing levels: 
allVars <-expand.grid(Realm = unique(regionSlope$Realm), Region = unique(regionSlope$Region))
regionSlope<- merge(allVars, regionSlope, all.x = T)
test<-NULL
for (i in (1: length(unique(regionSlope$Region)))){
regs<-  unique(regionSlope$Region)
df<-subset(regionSlope, Region == regs[i])

if (any(is.na (df$ok))) {
  df$ok[is.na(df$ok)]<-   df$ok[!is.na(df$ok)]}
if (sum(df$ok ==0 ) ==1) {
  df[df$ok == 0 , c(3:17, 22,  27)] <- NA     # but values need to be NA, or else they'll be included
df$ok[df$ok == 0 ] <-   df$ok[df$ok != 0 ]} # needs number >0 to be included 
 test<- rbind(test, df) 
}
regionSlope<- test

# Fig S1 Regions ####
ggplot(data.frame(subset(regionSlope, ok >0 )))+ # only use >20plots or >4 datasets 
  geom_errorbar(aes(x=Region,ymin=X0.025quant,ymax= X0.975quant, color = Realm),alpha = 0.5,
                size = 1, width=0,   position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Region ,ymin=X0.05quant,ymax= X0.95quant, color = Realm),alpha = 0.75,
                size = 2, width = 0,  position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Region ,ymin=X0.1quant,ymax= X0.9quant, color = Realm), alpha = 1,
                size = 3, width=0, position=position_dodge(width= 0.7))+  
  geom_point(aes(x=Region ,   y=mean,  shape = Realm), 
             size = 2.5, position=  position_dodge(width = 0.7), color = "black", fill = "black",  alpha=1 )+
  scale_color_manual(values = col.scheme.realm)+
  scale_fill_manual(values = col.scheme.realm)+
  scale_shape_manual(values = shps)+
  geom_hline(yintercept=0,linetype="dashed") +
  xlab ("")+ ylab("Trend slope  \n % change per year")+ #
  geom_text(aes(x = Region , y = 0.04, label = text, fill = Realm),  
            position = position_dodge(width = 0.7), size = 3, color = 1) +
  geom_text(aes(x = Region , y = 0.052, label = ptxt, fill = Realm),  
            position = position_dodge(width = 0.7), size = 2.5, color = 1) +
  coord_flip()+
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.03,0.055))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key=element_blank())
# ignre warning about 10 resp 4  missing values  

  



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

metadata_biom<-  completeData %>% 
  group_by(BiomeCoarse, Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
metadata_biom


inlaFbiom<- readRDS("inlaFbiomTEST.rds")
ps<- NULL
for(i in 6: nrow(inlaFbiom$summary.fixed )){
  p<-inla.pmarginal(0, inlaFbiom$marginals.fixed[[i]])
  ps<- c(ps, p) }

biomSlope<- as.data.frame(readRDS("inlaFbiomSUMMARY.rds"))[6:13,]
vars<-data.frame(do.call(rbind, strsplit(rownames(biomSlope), split = ":")))
biomSlope<-cbind(biomSlope, vars)
biomSlope$Realm<-gsub("Realm", "", biomSlope$X1);  biomSlope$BiomeCoarse<-gsub("BiomeCoarse", "", biomSlope$X2)
biomSlope$Biome <-biomSlope$BiomeCoarse
biomSlope$P<- ps
biomSlope<- merge(biomSlope, metadata_biom)
biomSlope$text = paste0("(", biomSlope$Datasources, " | ", biomSlope$Plots, ") ")

biomSlope$BiomeCoarse<- ordered(biomSlope$BiomeCoarse, levels = rev(c("Boreal/Alpine", "Temperate" , "Drylands", "Tropical"  )))

saveRDS(biomSlope, file = "biomSlope.rds")

biomSlope<- read_rds( "biomSlope.rds")
# Fig 2c biomes ####
biomPlot<- ggplot(data.frame(biomSlope))+
  geom_errorbar(aes(x=BiomeCoarse,ymin=X0.025quant,ymax= X0.975quant, color = Realm),alpha = 0.5,
                size = 1, width=0,   position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=BiomeCoarse ,ymin=X0.05quant,ymax= X0.95quant, color = Realm),alpha = 0.75,
                size = 2, width = 0,  position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=BiomeCoarse ,ymin=X0.1quant,ymax= X0.9quant, color = Realm), alpha = 1,
                size = 3, width=0, position=position_dodge(width= 0.7))+  
  geom_point(aes(x=BiomeCoarse ,   y=mean,  shape = Realm), 
             size = 2.5, position=  position_dodge(width = 0.7), color = "black", fill = "black",  alpha=1 )+
  scale_color_manual(values = col.scheme.realm)+
  scale_fill_manual(values = col.scheme.realm)+
  scale_shape_manual(values = shps)+
  geom_hline(yintercept=0,linetype="dashed") +
  geom_text(aes(x = Biome , y = 0.030, label = text, fill = Realm),  
            position = position_dodge(width = 0.7), size = 3, color = 1) +
  coord_flip()+
  #ylim(-0.025, 0.035)+
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.03,0.034))+ 
  xlab ("")+ ylab("Trend slope  \n % change per year")+
  theme_clean +
  theme(legend.key=element_blank(), 
        legend.position="none")  + 
  geom_text(aes(x = 4.3 , y = -0.025, label = "C"),  
          size = 6, color = 1) 
  
png("biome plot.png", width=2000, height=1300, res = 360)
biomPlot
dev.off()


library(gridExtra)


grid.arrange(ABplot, contPlot, biomPlot, nrow = 3, heights = c(5,5,5) )

gA <- ggplotGrob(ABplot)
gB <- ggplotGrob(contPlot)
gC <- ggplotGrob(biomPlot)

grid::grid.newpage()
grid::grid.draw(rbind(gA, gB, gC))

# dimension for storing 





###########################################################
# time slices #####
metadata_full<-  completeData %>%   group_by(Continent, Realm) %>%
  summarise(    Datasources = length(unique(Datasource_ID)),
                Plots =  length(unique(Plot_ID)), 
                slice = min(Year) )

# grab data after certain years, exclude datasets with less than 10 years data in said period


cD1960 <- subset(completeData, Year >1959); dim(cD1960) # lost 345 observations
dim(cD1960); dim(completeData) # 8000 difference
metadata_1960<-  subset(cD1960, !is.na(Number )) %>% 
  group_by( Plot_ID) %>%
  summarise(  Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1)
too.short<- subset(metadata_1960, Duration< 9)
cD1960<- cD1960[! cD1960$Plot_ID  %in% too.short$Plot_ID  , ]
length(unique(cD1960$Datasource_ID)) #165    # lost 2
length(unique(cD1960$Plot_ID)) # 1667 # lost 11
metadata_1960<-  cD1960 %>%   group_by(Continent, Realm) %>%
  summarise(    Datasources = length(unique(Datasource_ID)),
                Plots =  length(unique(Plot_ID)), 
                slice = min(Year) )



cD1970 <- subset(completeData, Year >1969)
dim(cD1970); dim(completeData) # 8000 difference
metadata_1970<-  subset(cD1970, !is.na(Number )) %>% 
  group_by( Plot_ID) %>%
  summarise(  Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1)
too.short<- subset(metadata_1970, Duration< 9)
cD1970<- cD1970[! cD1970$Plot_ID  %in% too.short$Plot_ID  , ]
length(unique(cD1970$Datasource_ID)) #160
length(unique(cD1970$Plot_ID)) #1599
metadata_1970<-  cD1970 %>%   group_by(Continent, Realm) %>%
  summarise(    Datasources = length(unique(Datasource_ID)),
                Plots =  length(unique(Plot_ID)), 
                slice = min(Year) )


cD1980 <- subset(completeData, Year >1979)
dim(cD1980); dim(completeData) # 8000 difference
metadata_1980<-  subset(cD1980, !is.na(Number )) %>% 
  group_by( Plot_ID) %>%
  summarise(  Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1)
too.short<- subset(metadata_1980, Duration< 9)
cD1980<- cD1980[! cD1980$Plot_ID  %in% too.short$Plot_ID  , ] ; dim(cD1980)
length(unique(cD1980$Datasource_ID)) #154
length(unique(cD1980$Plot_ID)) #1553
metadata_1980<-  cD1980 %>%   group_by(Continent, Realm) %>%
  summarise(    Datasources = length(unique(Datasource_ID)),
                Plots =  length(unique(Plot_ID)),
                slice = min(Year) )

cD1990 <- subset(completeData, Year >1989)
dim(cD1990); dim(completeData) # 10000 difference
metadata_1990<-  subset(cD1990, !is.na(Number )) %>% 
  group_by( Plot_ID) %>%
  summarise(  Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1)
too.short<- subset(metadata_1990, Duration< 9)
cD1990<- cD1990[! cD1990$Plot_ID  %in% too.short$Plot_ID  , ] ; dim(cD1990)
length(unique(cD1990$Datasource_ID)) #123
length(unique(cD1990$Plot_ID)) #1428
metadata_1990<-  cD1990 %>%   group_by(Continent, Realm) %>%
  summarise(    Datasources = length(unique(Datasource_ID)),
                Plots =  length(unique(Plot_ID)), 
                slice = min(Year) )

cD1995 <- subset(completeData, Year >1994)
dim(cD1995); dim(completeData) # 10000 difference
metadata_1995<-  subset(cD1995, !is.na(Number )) %>% 
  group_by( Plot_ID) %>%
  summarise(  Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1)
too.short<- subset(metadata_1995, Duration< 9)
cD1995<- cD1995[! cD1995$Plot_ID  %in% too.short$Plot_ID  , ] ; dim(cD1995)
length(unique(cD1995$Datasource_ID)) #123
length(unique(cD1995$Plot_ID)) #1428
metadata_1995<-  cD1995 %>%   group_by(Continent, Realm) %>%
  summarise(    Datasources = length(unique(Datasource_ID)),
                Plots =  length(unique(Plot_ID)), 
                slice = min(Year) )


cD2000 <- subset(completeData, Year >1999)
dim(cD2000)- dim(completeData) # 26625 difference
metadata_2000<-  subset(cD2000, !is.na(Number )) %>% 
  group_by( Plot_ID) %>%
  summarise(  Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1)
too.short<- subset(metadata_2000, Duration< 9)
cD2000<- cD2000[! cD2000$Plot_ID  %in% too.short$Plot_ID  , ] ; dim(cD2000)
length(unique(cD2000$Datasource_ID)) #73
length(unique(cD2000$Plot_ID)) #1054
metadata_2000<-  cD2000 %>%   group_by(Continent, Realm) %>%
  summarise(    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)), 
    slice = min(Year) )

cD2005 <- subset(completeData, Year >2004)
dim(cD2005)- dim(completeData) # 38519 difference
metadata_2005<-  subset(cD2005, !is.na(Number )) %>% 
  group_by( Plot_ID) %>%
  summarise(  Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1)
too.short<- subset(metadata_2005, Duration< 9)
cD2005<- cD2005[! cD2005$Plot_ID  %in% too.short$Plot_ID  , ] ; dim(cD2005)
length(unique(cD2005$Datasource_ID)) #44
length(unique(cD2005$Plot_ID)) #827
metadata_2005<-  cD2005 %>%   group_by(Continent, Realm) %>%
  summarise(    Datasources = length(unique(Datasource_ID)),
                Plots =  length(unique(Plot_ID)), 
                slice = min(Year) )


# load all RDS files 

# realm
inlaRealm<- as.data.frame(readRDS("InlaRealmSUMMARY.rds"))[3:4, ]
inlaRealm$slice<- "Full"
inlaRealm1960<- as.data.frame(readRDS("inlaRealm1960SUMMARY.rds"))[3:4, ]
inlaRealm1960$slice<- "1960"
inlaRealm1970<- as.data.frame(readRDS("inlaRealm1970SUMMARY.rds"))[3:4, ]
inlaRealm1970$slice<- "1970"
inlaRealm1980<- as.data.frame(readRDS("inlaRealm1980SUMMARY.rds"))[3:4, ]
inlaRealm1980$slice<- "1980"
inlaRealm1990<- as.data.frame(readRDS("inlaRealm1990SUMMARY.rds"))[3:4, ]
inlaRealm1990$slice<- "1990"
inlaRealm2000<- as.data.frame(readRDS("inlaRealm2000SUMMARY.rds"))[3:4, ]
inlaRealm2000$slice<- "2000"
inlaRealm2005<- as.data.frame(readRDS("inlaRealm2005SUMMARY.rds"))[3:4, ]
inlaRealm2005$slice<- "2005"


RealmSlices<- rbind(inlaRealm1960, inlaRealm1970, inlaRealm1980, inlaRealm1990, inlaRealm2000, inlaRealm2005)
RealmSlices$Realm<- c("Freshwater", "Terrestrial")
#realmSlices$slice<- ordered(realmSlices$slice, levels = (c( "1970" , "1980", "1990", "2000"  )))

#Continent     Realm       Datasources Plots slice
#<fct>         <fct>             <int> <int> <chr>
#  1 Europe        Freshwater           22   329 1960 

cds<- list(cD1960, cD1970, cD1980, cD1990, cD2000, cD2005)
metadata_realm <- NULL
for (i in 1: length(cds)) {
meta<-   as.data.frame(cds[i]) %>%   group_by( Realm) %>%
  summarise(    Datasources = length(unique(Datasource_ID)),
                Plots =  length(unique(Plot_ID)), 
                slice = min(Year)         )
  
metadata_realm <- rbind(metadata_realm, meta)
  
}
metadata_realm$Continent<- as.factor("Global")


metadata_1960$slice<- 1960
metadata_1970$slice<- 1970
metadata_1980$slice<- 1980
metadata_1990$slice<- 1990
metadata_1995$slice<- 1995
metadata_2000$slice<- 2000
metadata_2005$slice<- 2005

metadata<- bind_rows(metadata_1960, metadata_1970, metadata_1980, metadata_1990,
                       metadata_2000, metadata_2005, 
                      metadata_realm[, c("Continent", "Realm", "Datasources", "Plots", "slice") ])
# ignore warning


inlaCont1960<- as.data.frame(readRDS("inlaCont1960SUMMARY.rds"))[8:19, ]
inlaCont1970<- as.data.frame(readRDS("inlaCont1970SUMMARY.rds"))[8:19, ]
inlaCont1980<- as.data.frame(readRDS("inlaCont1980SUMMARY.rds"))[8:19, ]
inlaCont1990<- as.data.frame(readRDS("inlaCont1990SUMMARY.rds"))[8:19, ]
inlaCont1995<- as.data.frame(readRDS("inlaCont1995SUMMARY.rds"))[8:19, ]
inlaCont2000<- as.data.frame(readRDS("inlaCont2000SUMMARY.rds"))[8:19, ]
inlaCont2005<- as.data.frame(readRDS("inlaCont2005SUMMARY.rds"))[8:19, ]

inlaCont1960$slice<- "1960"
inlaCont1970$slice<- "1970"
inlaCont1980$slice<- "1980"
inlaCont1990$slice<- "1990"
inlaCont2000$slice<- "2000"
inlaCont2005$slice<- "2005"
ContSlices<- rbind(inlaCont1960,   inlaCont1970,  inlaCont1980,  inlaCont1990,   inlaCont2000,  inlaCont2005)
#ContSlices$slice<- ordered(ContSlices$slice, levels = (c(">1960", ">1970" , ">1980", ">1990", ">2000"  )))

vars<-data.frame(do.call(rbind, strsplit(rownames(ContSlices), split = ":")))
ContSlices<-cbind(ContSlices, vars)
ContSlices$Realm<-gsub("Realm", "", ContSlices$X1);  ContSlices$Continent<-gsub("Continent", "", ContSlices$X2)

varsR<- data.frame(do.call(rbind, strsplit(rownames(RealmSlices), split = ":")))
RealmSlices<-cbind(RealmSlices, varsR)
RealmSlices$X3<- RealmSlices$X2;  RealmSlices$Realm<-gsub("Realm", "", RealmSlices$X1);   RealmSlices$X2<-"Global"; RealmSlices$Continent<-"Global" 

slices<- rbind(RealmSlices, ContSlices)

slices<- merge(metadata, slices) ; dim(slices) #shoudl be 59 rows


#ContSlices$text = paste0("(", ContSlices$Datasources, " | ", ContSlices$Plots, ")")
#ContSlices$slice<- ordered(ContSlices$slice, levels = (c(">1960", ">1970" , ">1980", ">1990", ">2000"  )))
slices$minmax<- NA 
slices$minmax[slices$Continent == "Global"]       <- c(-0.01258, 0.02)
slices$minmax[slices$Continent == "Europe"]       <- c(-0.022, 0.035)
slices$minmax[slices$Continent == "North America"]<- c(-0.022, 0.035)
slices$minmax[slices$Continent == "Asia"]<- c(-0.015, 0.05)
slices$minmax[slices$Continent == "Latin America"]<- c(-0.035, 0.030)
slices$minmax[slices$Continent == "Australia"]<- c(-0.035, 0.030)     #ignore warnings
slices$minmax[slices$Continent == "Africa"]<- c(-0.025, 0.02)


slices$Continent<- ordered(slices$Continent, levels = (c("Global", "Europe", "North America" , "Asia", "Latin America", "Australia", "Africa" )))
slices$plots.ok<- slices$Plots > 20
slices$dataset.ok<- slices$Datasources >3
slices$ok <- slices$plots.ok + slices$dataset.ok 


slicePlot<- ggplot(subset(slices, ok >0 & sd < 1))+
  geom_hline(yintercept=0,linetype="dashed") +
  geom_errorbar(aes(x=as.factor(slice) ,ymin=X0.1quant, ymax=X0.9quant, color = Realm),
                size = 1, width=0,  position=position_dodge(width= 0.5))+  
  geom_errorbar(aes(x=as.factor(slice),ymin=X0.025quant,ymax=X0.975quant, color = Realm),
                width=0, alpha = 0.7, position=position_dodge(width= 0.5))+  
  geom_point(aes(x=as.factor(slice),   y=mean, shape = Realm),
             size = 1, position=  position_dodge(width = 0.5), alpha=1 ,  fill = "black", color = "black")+
  geom_blank(aes(y=minmax)) +
  scale_color_manual(values = col.scheme.realm)+
  scale_fill_manual(values = col.scheme.realm)+
  scale_shape_manual(values = shps)+
  xlab ("Start year") + ylab("Trend slope")+
facet_wrap(.~Continent, scales = "free_y")+  #)+#
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+
    theme_clean +
  theme(strip.background =element_rect(fill="white"), 
        axis.line=element_line() ,
        axis.text.x  = element_text(angle=45, vjust=1, hjust = 1), 
        legend.position = "bottom")   

# export dimensions: 6 * 4.5


inlaR60<- readRDS("inlaRealm1960TEST.rds")
inlaR70<- readRDS("inlaRealm1970TEST.rds")
inlaR80<- readRDS("inlaRealm1980TEST.rds")
inlaR90<- readRDS("inlaRealm1990TEST.rds")
inlaR00<- readRDS("inlaRealm2000TEST.rds")
inlaR05<- readRDS("inlaRealm2005TEST.rds")
inlaC60<- readRDS("inlaCont1960TEST.rds")
inlaC70<- readRDS("inlaCont1970TEST.rds")
inlaC80<- readRDS("inlaCont1980TEST.rds")
inlaC90<- readRDS("inlaCont1990TEST.rds")
inlaC00<- readRDS("inlaCont2000TEST.rds")
inlaC05<- readRDS("inlaCont2005TEST.rds")

timeslices<- list(inlaR60, inlaR70, inlaR80, inlaR90, inlaR00, inlaR05, 
                  inlaC60, inlaC70, inlaC80, inlaC90, inlaC00, inlaC05)
names(timeslices)<- list("inlaR60", "inlaR70", "inlaR80", "inlaR90", "inlaR00", "inlaR05", 
                        "inlaC60", "inlaC70", "inlaC80", "inlaC90", "inlaC00", "inlaC05")

timesliceResults<-NULL
for (j in 1:6){
  dat <- timeslices[[j]]
ps<- NULL
for(i in 3: nrow(dat$summary.fixed )){
  p<-inla.pmarginal(0, dat$marginals.fixed[[i]])
  ps<- c(ps, p) }
  decade<- cbind(names(timeslices[j]), rownames(dat$summary.fixed[3: nrow(dat$summary.fixed ), ]), ps)
  timesliceResults<- rbind(timesliceResults, decade) 
}

for (j in 7:12){
  dat <- timeslices[[j]]
  ps<- NULL
  for(i in 8: nrow(dat$summary.fixed )){
    p<-inla.pmarginal(0, dat$marginals.fixed[[i]])
    ps<- c(ps, p) }
  decade<- cbind(names(timeslices[j]), rownames(dat$summary.fixed[8: nrow(dat$summary.fixed ), ]), ps)
  timesliceResults<- rbind(timesliceResults, decade) 
}

 timesliceResults
saveRDS(timesliceResults, file = "timesliceResults.rds")
ts<- read_rds("timesliceResults.rds")






## # # # # # # # # # # # # # # ########################################################################## # 

# DRiveRS #####

# protected areas #####
# how many pa's changes status during the sampling period? and how many have missing data?
dim(subset(metadata_per_plot, PA == "yes"))
sum(metadata_per_plot$PA == "yes")/ nrow(metadata_per_plot)
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

inlaFpa<- as.data.frame(readRDS("inlaFpaSUMMARY.rds"))
inlaFpaTEST<- (readRDS("inlaFpaTEST.rds"))
ps<- NULL
for(i in 4: nrow(inlaFpaTEST$summary.fixed )){
  p<-inla.pmarginal(0, inlaFpaTEST$marginals.fixed[[i]])
  ps<- c(ps, p) }


data.frame(
var =   rownames(inlaFpa)[4:7], 
mean = (10^(inlaFpa[4:7,1] )-1)  *100, # proportional changes per year
CI2.5 = (10^(inlaFpa[4:7,4] )-1 ) *100,#0.025 CI
CI97.5 = (10^(inlaFpa[4:7,12] )-1 ) *100# 0.975
)
10^(inlaFpa[4:7,1] *10)-1 # proportional changes per decade



paSlope<- inlaFpa[4:7,]
vars<-data.frame(do.call(rbind, strsplit(rownames(paSlope), split = ":")))
paSlope<-cbind(paSlope, vars)
paSlope$Realm<-gsub("Realm", "", paSlope$X2);  paSlope$PA<-gsub("PA", "", paSlope$X1)
paSlope$factor4<-rownames(paSlope) 
paSlope$PA <-paSlope$PA
paSlope$P <- ps
paSlope<- merge(paSlope, metadata_pa)
paSlope$text = paste0("(", paSlope$Datasources, " | ", paSlope$Plots, ") ")
paSlope$PA<- factor(paSlope$PA, levels = rev(c( "no", "yes"))) 

brks<- c(-0.010, -0.005, 0, 0.005, 0.01)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
e<- c("","","","","","","")


PA.alph<- c(  "yes"  = 1, "no" = 0.7)
PA.col<- c("PAno:RealmFreshwater:cYear" = "dodgerblue4",  "PAyes:RealmFreshwater:cYear" = "dodgerblue2", 
           "PAno:RealmTerrestrial:cYear" =  "chocolate4", "PAyes:RealmTerrestrial:cYear" =  "chocolate3" )

saveRDS(paSlope, file = "paSlope.rds")

paSlope<- read_rds("paSlope.rds")

  
 
PAplot <- 
ggplot(data.frame(paSlope))+
  geom_errorbar(aes(x=Realm,ymin=X0.025quant,ymax= X0.975quant, color = factor4),alpha = 0.5,
              size = 1, width=0,   position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Realm ,ymin=X0.05quant,ymax= X0.95quant, color = factor4),alpha = 0.75,
                size = 2, width = 0,  position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Realm ,ymin=X0.1quant,ymax= X0.9quant, color = factor4), alpha = 1,
                size = 3, width=0, position=position_dodge(width= 0.7))+  
  geom_point(aes(x=Realm ,   y=mean,  shape = Realm, group  = factor4), 
             size = 2.5, position=  position_dodge(width = 0.7), color = "black", fill = "black",  alpha=1 )+
  geom_hline(yintercept=0,linetype="dashed")+
  coord_flip()+
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.01, 0.015))+
  xlab ("")+ ylab("Trend slope  \n % change per year")+
  geom_text(aes(x = Realm , y = 0.0125, group = PA,  label = text), 
            position = position_dodge(width = 0.7), size = 3, color = 1) +
  scale_fill_manual(values = PA.col)+
  scale_shape_manual(values = shps)+
  scale_color_manual(name="Protection status",
                    breaks=rev(c("PAno:RealmFreshwater:cYear", "PAyes:RealmFreshwater:cYear", 
                             "PAno:RealmTerrestrial:cYear", "PAyes:RealmTerrestrial:cYear")),
                    labels=rev(c("Unprotected", "Protected", "Unprotected", "Protected")), 
                    values = PA.col) + 
  guides(#fill = guide_legend(reverse = TRUE), 
        fill = FALSE,
  shape =  FALSE)+
  theme_clean +
  theme(legend.position="bottom") 
 

png("./figures/PA plot.png", width=2000, height=700, res = 360)
PAplot
dev.off()

  








#  LAND USE  #####

#  USing LUH2: cover of Urban and cropland in the surrounding of the sites
# resolution : 25km


# How are the values distributed?
plotData<-unique(completeData[, c( "Plot_ID", "Realm", "Continent", "Datasource_ID", "Stratum", "Location",            
                                   "Datasource_name", "biome", "BiomeCoarse",   "End_forestArea",      
                                   "End_cropArea" ,  "End_urbanArea", "urbanization", 
                                   "cropification", "frcCrop900m1992", "frcUrban900m1992"   ) ]) 


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
  
 res<- as.data.frame(readRDS("inlaFlanduseTSUMMARY.rds"))[5:6,]
 inlaFlanduseT<- readRDS("inlaFlanduseTTEST.rds") 
 
 ps<- NULL
 for(i in 5: nrow(inlaFlanduseT$summary.fixed )){
   p<-inla.pmarginal(0, inlaFlanduseT$marginals.fixed[[i]])
   ps<- c(ps, p) } ; ps # 0.7068763 0.8435788
 cover25kmTer   <- cbind(Realm = "Terrestrial", res, variable = rownames(res), ps)
 
 
 
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
 
 res<- as.data.frame(readRDS("inlaFlanduseFWSUMMARY.rds"))[5:6,] # ok
 inlaFlanduseFW<- readRDS("inlaFlanduseFWTEST.rds") 
 
 ps<- NULL
 for(i in 5: nrow(inlaFlanduseFW$summary.fixed )){
   p<-inla.pmarginal(0, inlaFlanduseFW$marginals.fixed[[i]])
   ps<- c(ps, p) } ; ps # 0.4820279 0.3905370
 cover25kmFW    <- cbind(Realm = "Freshwater",  res, variable = rownames(res), ps)
 
 
 


# LAND USE CHANGE #####
#models (only use LUH2  = LANDSCAPE change) 
inlaFChangesTerr<- inla(log10(Number+1) ~ cYear + urbanization + cropification + cYear *  urbanization + cYear * cropification  +  # cYear*PA +
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
 res<- as.data.frame(readRDS("inlaFchangesTerrSUMMARY.rds"))[5:6,] # fixed hessian
inlaFChangesTerr<- readRDS("inlaFchangesTerrTEST.rds")

ps<- NULL
for(i in 5: nrow(inlaFChangesTerr$summary.fixed )){
  p<-inla.pmarginal(0, inlaFChangesTerr$marginals.fixed[[i]])
  ps<- c(ps, p) } ; ps # 0.9679753 0.1021923  both are pretty much meaningful but there's still a problem with the hessian 
LUchangeTer    <- cbind(Realm = "Terrestrial", res  , variable = rownames(res), ps)



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
 
inlaFChangesFW<-    readRDS("inlaFchangesFWTEST.rds") 
res<- as.data.frame(readRDS("inlaFchangesFWSUMMARY.rds"))[5:6,] # fixed hessian

ps<- NULL
for(i in 5: nrow(inlaFChangesFW$summary.fixed )){
  p<-inla.pmarginal(0, inlaFChangesFW$marginals.fixed[[i]])
  ps<- c(ps, p) } ; ps # 0.2811565 0.1227597 

LUchangeFW     <- cbind(Realm = "Freshwater", res  , variable = rownames(res), ps)



# ESA CCI#####

inlaFlanduseESAterr<- inla(log10(Number+1) ~  cYear* frcCrop900m1992 + cYear* frcUrban900m1992 +# frcCrop900m + frcUrban900m +
                           f(Period_4INLA,model='iid')+
                             f(Location_4INLA,model='iid')+
                             f(Plot_ID_4INLA,model='iid')+
                             f(Datasource_ID_4INLA,model='iid')+
                             f(Plot_ID_4INLAR,iYear,model='iid')+
                             f(Location_4INLAR,iYear,model='iid')                      +
                             f(Datasource_ID_4INLAR,iYear,model='iid'), #+
                             f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                           control.compute = list(dic=TRUE,waic=TRUE),
                           data= testDat, # subset(completeData, !is.na(completeData$frcCrop900m1992) & Realm == "Terrestrial"), 
                           num.threads = 2)#verbose = T,
res<- as.data.frame(read_rds("inlaFlanduseESAterrSUMMARYhessian.rds"))[5:6,]
inlaFlanduseESAterr<- read_rds("inlaFlanduseESAterrTESThessian.rds")

ps<- NULL
for(i in 5: nrow(inlaFlanduseESAterr$summary.fixed )){
  p<-inla.pmarginal(0, inlaFlanduseESAterr$marginals.fixed[[i]])
  ps<- c(ps, p) } ; ps # 0.2811565 0.1227597   # with fixed hessian: 0.05427648 0.45935564

cover0.81Ter   <- cbind(Realm = "Terrestrial", res, variable = rownames(res), ps)



inlaFlanduseESAfw<- inla(log10(Number+1) ~   cYear* frcCrop900m1992 + cYear* frcUrban900m1992+
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

res<- as.data.frame(read_rds("inlaFlanduseESAfwSUMMARY.rds"))[5:6,]
inlaFlanduseESAfw <- readRDS("inlaFlanduseESAfwTEST.rds") 

ps<- NULL
for(i in 5: nrow(inlaFlanduseESAfw$summary.fixed )){
  p<-inla.pmarginal(0, inlaFlanduseESAfw$marginals.fixed[[i]])
  ps<- c(ps, p) } ; ps # 0.0806475 0.8180687

cover0.81FW    <- cbind(Realm = "Freshwater",  res, variable = rownames(res), ps)





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
res<- as.data.frame(readRDS("inlaFClimChangesTerrSUMMARY.rds"))[5:6,] 
inlaFClimChangesTerr<- readRDS("inlaFClimChangesTerrTEST.rds")
ps<- NULL
for(i in 5: nrow(inlaFClimChangesTerr$summary.fixed )){
  p<-inla.pmarginal(0, inlaFClimChangesTerr$marginals.fixed[[i]])
  ps<- c(ps, p) } ; ps #  0.1442737 0.1399039  prestty close to 0.1

deltaTerCRU    <- cbind(Realm = "Terrestrial", res, variable = rownames(res), ps)




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
res<- as.data.frame(readRDS("inlaFClimChangesFWSUMMARY.rds"))[5:6,] # ok
inlaFClimChangesFW<- readRDS("inlaFClimChangesFWTEST.rds")
ps<- NULL
for(i in 5: nrow(inlaFClimChangesFW$summary.fixed )){
  p<-inla.pmarginal(0, inlaFClimChangesFW$marginals.fixed[[i]])
  ps<- c(ps, p) } ; ps #  0.8260536 0.2490158
deltaFwCRU     <- cbind(Realm = "Freshwater",  res, variable = rownames(res),ps)








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
res<-as.data.frame(readRDS("inlaFCHELSAChangesTerrSUMMARY.RDS"))[5:6,] #ok
inlaFCHELSAChangesTerr<- readRDS("inlaFCHELSAChangesTerrTEST.rds")
ps<- NULL
for(i in 5: nrow(inlaFCHELSAChangesTerr$summary.fixed )){
  p<-inla.pmarginal(0, inlaFCHELSAChangesTerr$marginals.fixed[[i]])
  ps<- c(ps, p) } ; ps #   

deltaTerCHELSA <- cbind(Realm = "Terrestrial", res, variable = rownames(res), ps)


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
res<- as.data.frame(readRDS("inlaFCHELSAChangesFWSUMMARY.RDS"))[5:6,] # 
inlaFCHELSAChangesFW<- readRDS("inlaFCHELSAChangesFWTEST.rds")
ps<- NULL
for(i in 5: nrow(inlaFCHELSAChangesFW$summary.fixed )){
  p<-inla.pmarginal(0, inlaFCHELSAChangesFW$marginals.fixed[[i]])
  ps<- c(ps, p) } ; ps #   0.3283940 0.8353412

deltaFwCHELSA  <- cbind(Realm = "Freshwater",  res, variable = rownames(res), ps)

drivers<- rbind(LUchangeTer, LUchangeFW, cover25kmFW   , cover25kmTer, cover0.81Ter, cover0.81FW, 
                deltaTerCRU, deltaFwCRU, deltaTerCHELSA, deltaFwCHELSA)

saveRDS(drivers, file = "Results drivers.rds")


























#plts confounders #####

RandEfPlot<- readRDS("RandEfPlot.rds")



strt<- ggplot(RandEfPlot, aes(x= Start_year ,  y = slope))+  #`Plot_slp_ mean`
  geom_point (aes(color = Realm), size =sz)+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
#  xlab ("Change in % urban cover per 1/16 degree") + ylab ("")+
  geom_hline(yintercept=0,linetype="dashed")+
 # ylim(-0.041, 0.03)+
  facet_wrap(~Realm ) + #, scales = "free"
  theme_clean + 
  theme( strip.background = element_blank(),
         strip.text.x = element_blank())
cor( RandEfPlot$Start_year ,   RandEfPlot$slope, )



end<- ggplot(RandEfPlot, aes(x= End_year ,  y = slope))+  #`Plot_slp_ mean`
  geom_point (aes(color = Realm), size =sz)+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  #  xlab ("Change in % urban cover per 1/16 degree") + ylab ("")+
  geom_hline(yintercept=0,linetype="dashed")+
  # ylim(-0.041, 0.03)+
  facet_wrap(~Realm ) + #, scales = "free"
  theme_clean + 
  theme( strip.background = element_blank(),
         strip.text.x = element_blank())

cor( RandEfPlot$End_year ,   RandEfPlot$slope, )


dur<- ggplot(RandEfPlot, aes(x= Duration ,  y = slope))+  #`Plot_slp_ mean`
  geom_point (aes(color = Realm), size =sz)+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  #  xlab ("Change in % urban cover per 1/16 degree") + ylab ("")+
  geom_hline(yintercept=0,linetype="dashed")+
  # ylim(-0.041, 0.03)+
  facet_wrap(~Realm ) + #, scales = "free"
  theme_clean + 
  theme( strip.background = element_blank(),
         strip.text.x = element_blank())

cor( RandEfPlot$Duration ,   RandEfPlot$slope, )

library(gridExtra)
grid.arrange(strt,  end,  dur, nrow = 3)










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

contPlot<- ggplot(data.frame(subset(contSlope, Continent != "Africa"   )))+ # exclude africa,bc it has too wide CI's 
  geom_crossbar(aes(x=Continent,   y=mean, fill = Realm, 
                    ymin=X0.025quant,ymax=X0.975quant),position="dodge",alpha=0.8 ,  width =0.7)+
  scale_fill_manual(values = col.scheme.realm)+
  geom_hline(yintercept=0,linetype="dashed") +
  geom_text(aes(x = Continent , y = 0.032, label = text, fill = Realm),  
            position = position_dodge(width = 0.7), size = 3, color = 1) +
  coord_flip()+
  scale_y_continuous(breaks = brks,labels = l, limits=lims)+
  xlab ("")+ ylab("")+ #Trend slope | \n % change per year
  theme_clean +
  theme(legend.key=element_blank(),
        legend.position='none', 
        axis.text.x=element_blank()) +
  geom_text(aes(x = 5.3 , y = -0.025, label = "A"),  
            size = 6, color = 1) 





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
load("inlaRW.RData")
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
  # geom_line(aes(x=Year,y=TerrMn, color = "Terrestrial")) +
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
                      f(Plot_ID_4INLA,model='iid')+
                      f(Period_4INLA,model='iid') , 
                    control.compute = list(dic=TRUE,waic=TRUE),     
                    data=compDat4RW)

save(inlaRWcont, file = "inlaRWcont.RData")


load("inlaRWcont20191003.RData")
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

# create fake variable to force equal min max values 
minUpper<- min(subset(RWcont, Continent == "North America")$'0.025quant')
maxUpper
minLower
maxLower<- 
  RWcont$minmax<- NA 
RWcont$minmax[RWcont$Continent == "Europe"]<- c(-0.7, 0.85)
RWcont$minmax[RWcont$Continent == "North America"]<- c(-0.7, 0.85)
RWcont$minmax[RWcont$Continent == "Asia"]<- c(-0.7, 0.85)
RWcont$minmax[RWcont$Continent == "Latin America"]<- c(-1.5, 1.5)
RWcont$minmax[RWcont$Continent == "Australia"]<- c(-1.5, 1.5)
RWcont$minmax[RWcont$Continent == "Africa"]<- c(-1.5, 1.5)


#Fig 3####
ggplot(subset(RWcont, goodData == T ))+
  geom_line(aes(x=Year,y=mean - value_startYear, color = Realm ))+
  scale_colour_manual(values = col.scheme.realm)+
  geom_ribbon(aes(x=Year, ymin = `0.025quant`- value_startYear ,ymax = `0.975quant`- value_startYear, fill=Realm), alpha=0.5)+
  geom_blank(aes(y=minmax)) +
  scale_fill_manual (values = col.scheme.realm)+
  xlim (1960, 2018)+
  geom_hline(yintercept = 0, linetype="dashed") +  theme_bw()+
  facet_wrap(~Continent , scales = "free")+ #
  labs(y = "Standardized insect abundance", x = "") +
  theme_clean +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+
  theme(strip.background =element_rect(fill="white"), 
        strip.text.x = element_text(angle = 0, hjust = 0), 
        axis.text.x  = element_text(angle=45, vjust=1, hjust = 1), 
        legend.position = "bottom")


png("Fig 3.png", width=4000, height=2300, res = 360)
fig1AC
dev.off()



test<- inlaRWcont$summary.random$iYear
test$TrueMean<- test$mean+ 2.262 +0.037 # europe + terr
test$True025<- test$`0.025quant`  + 2.262 + 0.037
test$True975<- test$`0.975quant`  + 2.262 +0.037
test$TrueMean
#1980 = 55
10^test$TrueMean[55]
10^test$TrueMean[94]




#Fig 3####
ggplot(subset(RWcont, goodData == T ))+
  geom_line(aes(x=Year,y=mean - value_startYear, color = Realm ))+
  scale_colour_manual(values = col.scheme.realm)+
  geom_ribbon(aes(x=Year, ymin = `0.025quant`- value_startYear ,ymax = `0.975quant`- value_startYear, fill=Realm), alpha=0.5)+
  geom_blank(aes(y=minmax)) +
  scale_fill_manual (values = col.scheme.realm)+
  xlim (1960, 2018)+
  geom_hline(yintercept = 0, linetype="dashed") +  theme_bw()+
  facet_wrap(~Continent , scales = "free")+ #
  labs(y = "Standardized insect abundance", x = "") +
  theme_clean +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+
  