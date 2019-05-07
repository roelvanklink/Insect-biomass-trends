################################################################
### IPBES-SSPs
### October 2018
### 
### Map with regional plots for ES and BD
### 
### Ines S. Martins (ines.martins@idiv.de)
#####################################################################################

# clear workspace
rm(list=ls())

# import packages 

library(rgdal)
library(ggplot2)
library(rgeos)
library(plyr)

### Import data ###

setwd("~/Dropbox (iDiv)/IPBES/Activity 1/SSP Modelling/BES-MIP_Figures/For_Almut/IPBESsubregions_map_BD_ES/")

## BD and ES data
final_data<-read.csv("BD_ES_regional_data_CH4_2.csv",sep=",")

#levels(final_data$NCP_cat)

final_data$NCP_cat<-factor(final_data$NCP_cat, levels =  c("Pgamma LU", "Ialpha LU","Pgamma LUCC", "Ialpha LUCC","M","R"))
levels(final_data$NCP_cat)


## Shapefiles of IPBES regions
#Original map for the centroids
map.kt<- readOGR("/Users/ISM/Dropbox (iDiv)/IPBES/Activity 1/SSP Modelling/GIS files/", layer="IPBES_subregions")

#Simplify version of map for plot (smaller files)
map_simp.kt<- readOGR("/Users/ISM/Dropbox (iDiv)/IPBES/Activity 1/SSP Modelling/GIS files/", layer="IPBES_subregions_simp")

### Correct names in data ###
levels(final_data$Region) = c(levels(final_data$Region),"South-East Asia", "North-East Asia")
final_data$Region[final_data$Region=="South East Asia"]<-"South-East Asia"
final_data$Region[final_data$Region=="North East Asia"]<-"North-East Asia"
final_data$Region<-droplevels(final_data$Region)

#### Create maps ####
## plot base map ##
ssp<-c("SSP1","SSP3","SSP5")
ssp2<-c("SSP1xRCP2.6","SSP3xRCP6.0","SSP5xRCP8.5")

map.test.centroids <- gCentroid(map.kt, byid=T)
map.test.centroids <- as.data.frame(map.test.centroids)
#adjustments to certain coordinates
centroids_corr<- read.csv("/Users/ISM/Dropbox (iDiv)/IPBES/Activity 1/SSP Modelling/GIS files/IPBES_centroids_correction.csv",sep=";")
map.test.centroids<-map.test.centroids+centroids_corr
map.test.centroids$OBJECTID <- row.names(map.test.centroids)

map<-ggplot()+
  geom_polygon(data = map_simp.kt, aes(long, lat,group = group), 
               fill = "grey91", colour = "grey76",size=0.2)+
  theme(legend.position = "none", rect = element_blank(),
        line = element_blank(), text = element_blank())

#set plots limits to be comparable across maps
lim<-final_data
lim$sumsdplus<-final_data$sum_sd+final_data$sd
lim$sumsdminus<-final_data$sum_sd-final_data$sd


## Draws plots on map and exports ##
for (i in 1:3){
  print(ssp[i])
  
  ## 1 - Select model ##
  final_data1<-subset(final_data, final_data$Scenario==ssp[i] | final_data$Scenario==ssp2[i])
  
  ## make it in the same order that in the shapefile
  final_data1$Region<-factor(final_data1$Region, levels = map.kt$IPBES_sub)
  
  final_data1 <- arrange(final_data1,Region, factor(Region))

  print(Sys.time())
  #runs and saves individual plots
  bar.testplot_list <- 
    lapply(unique(final_data1$Region), function(i) { 
      
      gt_plot <- ggplotGrob(
        ggplot(final_data1[final_data1$Region == i,], aes(x=NCP_cat, y=sum_sd, fill=NCP_cat)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
          geom_hline(yintercept=0, color = "grey")+
          geom_bar(stat="identity")+
          geom_errorbar(aes(ymin=sum_sd-se, ymax=sum_sd+se),color="grey55", width=.1)+
          ylim(min(lim$sumsdminus,na.rm = TRUE)-0.5,max(lim$sumsdplus,na.rm = TRUE)+0.5) +
          labs(x = NULL, y = TRUE)+
           theme_minimal() +
           #theme(legend.position = "none", rect = element_blank(),
           #       line = element_blank(),
           #       aspect.ratio=4/1  # change to 1 when running for 3/4 variables
          theme(axis.text.y = element_text(size=4),
            legend.position = "none", rect = element_blank(),
                      line = element_blank(),
                      aspect.ratio=1/1  # change to 1 when running for 3/4 variables
          )
      )
      panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
      gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
    })
  
  #alocates plots to the map by listed coordinates
  bar_annotation_list <- lapply(1:length(map.kt$OBJECTID), function(i) 
    annotation_custom(bar.testplot_list[[i]], 
                      
                      xmin = map.test.centroids$x[i] - 20,
                      xmax = map.test.centroids$x[i] + 20,
                      ymin = map.test.centroids$y[i] - 20,
                      ymax = map.test.centroids$y[i] + 20) )
  
  
  result_plot <- Reduce(`+`, bar_annotation_list, map)
  
  result_plot
  
  # sets size of file being export
  savePlot <- function(myPlot, X) {
    pdf(X, width = 10.93, height = 4.97, paper="a4r")
    print(myPlot)
    dev.off()
  }
  
  #export data
  path <- paste(ssp[i], "_Regions_CHP4",".pdf",sep="")
 savePlot(result_plot,path)
}





ggplot(data=df, aes(x=Datasource_name, y=Duration, fill = Realm)) +
 geom_bar(stat="identity") +theme_clean +
  scale_fill_manual(values = col.scheme.realm)+
  ylim (0, 90)+
#  xlim(0,1) +
  labs(x = NULL, y = TRUE)+
  theme(legend.position = "none", rect = element_blank(),
                                 line = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
                                 aspect.ratio=1/1 )




  print(ssp[i])
  
  ## 1 - Select model ##
  final_data1<-subset(final_data, final_data$Scenario==ssp[i] | final_data$Scenario==ssp2[i])
  
  ## make it in the same order that in the shapefile
  final_data1$Region<-factor(final_data1$Region, levels = map.kt$IPBES_sub)
  
  final_data1 <- arrange(final_data1,Region, factor(Region))
  
  print(Sys.time())
  #runs and saves individual plots
  
  bar.testplot_list <- 
    lapply(metadata_per_dataset$Datasource_ID, function(i) { 
      
      gt_plot <- ggplotGrob(
        ggplot(data=metadata_per_dataset[i,], aes(x=Datasource_name, y=Duration, fill = Realm)) +
          geom_bar(stat="identity") +theme_clean +
          scale_fill_manual(values = col.scheme.realm)+
          ylim (0, 90)+
          theme_void()+
          #labs(x = "", y = "")+
         theme(   legend.position = "none", 
                  rect = element_blank(),
        #        line = element_blank(),
                aspect.ratio=15/1 #, 
        #        axis.title=element_blank(),
        #        axis.text=element_blank(),
        #        axis.ticks=element_blank()
                
                 ))
      panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
      gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
    })
  
  #alocates plots to the map by listed coordinates
  bar_annotation_list <- lapply(1:length(metadata_per_dataset$Datasource_ID), function(i) 
    annotation_custom(bar.testplot_list[[i]], 
                      
                      xmin = metadata_per_dataset$mean_long[i] - 20,
                      xmax = metadata_per_dataset$mean_long[i] + 20,
                      ymin = metadata_per_dataset$mean_lat[i] - 20,
                      ymax = metadata_per_dataset$mean_lat[i] + 20) )
  
  
  result_plot <- Reduce(`+`, bar_annotation_list, p.wgs)
  
  result_plot
  
  
  ##################################################################
  # tryouts
  pts.wgs <- metadata_per_dataset
  pts.wgs <- SpatialPointsDataFrame(coords = data.frame(lon = pts.wgs$mean_long,
                                                        lat = pts.wgs$mean_lat),
                                    proj4string = CRS(WGS84),
                                    data = pts.wgs)
  
  pts.rob <- spTransform(pts.wgs, CRSobj = ROBINSON)
  
  
  pts.rob@data <- data.frame(pts.rob@data, 
                              x = coordinates(pts.rob)[,1],
                              y = coordinates(pts.rob)[,2])
  
  
  # this a least works:
  p.wgs + 
    annotation_custom(gt_plot, 
                      xmin = pts.rob@data$x[i] -400000 ,
                      xmax = pts.rob@data$x[i] + 400000,
                      ymin = pts.rob@data$y[i] - 100000,
                      ymax = pts.rob@data$y[i] + 2000000)

   
  
  
  # sets size of file being export
  savePlot <- function(myPlot, X) {
    pdf(X, width = 10.93, height = 4.97, paper="a4r")
    print(myPlot)
    dev.off()
  }
  
  #export data
  path <- paste(ssp[i], "_Regions_CHP4",".pdf",sep="")
  savePlot(result_plot,path)




