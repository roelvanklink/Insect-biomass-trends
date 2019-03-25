##############################################################################################

setwd("~/Dropbox/Insect Biomass Trends")

#get new coord info
library(sp)
library(maptools)
mydata<-read.csv("metadata per plot 20190128.csv",sep=",")
mydata<-subset(mydata,!is.na(Longitude))
mydata<-subset(mydata,!is.na(Latitude))
coordinates(mydata)<-c("Longitude","Latitude")
proj4string(mydata)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#get shapefile
tfolder<-"~/Documents/sChange/driver_maps/WWF_ecoregions/official"
teco<-readShapePoly(paste(tfolder,"wwf_terr_ecos.shp",sep="/"))
proj4string(teco)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
wwfkey<-read.delim("/Users/dianabowler/Documents/sChange/driver_maps/WWF_ecoregions/WWF_biomes.txt")#get codes

#get biome per location
biome<-over(mydata,teco)
biome<-wwfkey$Label[match(biome$BIOME,wwfkey$Code)] 
mydata$biome <- biome
write.table(mydata,file="Biomes.txt",sep="\t")

##############################################################################################

#get protected area database
library(rgdal)
setwd("C:/Users/db40fysa/Nextcloud/sMon-Analyses/Spatial_data/WDPA_Jan2019-shapefile")
out <-readOGR(dsn=getwd(),layer="WDPA_Jan2019-shapefile-polygons")

#over with points
protectedArea<-over(mydata,out)
protectedArea$Plot_ID <-mydata@data$Plot_ID
protectedArea$Datasource_ID <-mydata@data$Datasource_ID
write.table(protectedArea,file="ProtectedAreas.txt",sep="\t")

##############################################################################################

#get land use data

#taken from here:
#http://luh.umd.edu/data.shtml
#version: LUH2 v2h
#file:"states.vc"

#forest
library(raster)
setwd("~/Documents/sChange/driver_maps/LUH/v2h")
#name of layer is year with layer 1 = 850

#annual forest area
pforest<-stack("states.nc",varname="primf")#primary forest
pforest<-subset(pforest,names(pforest)[1051:1166])#restrict to 1900-2015
sforest<-stack("states.nc",varname="secdf")#secondary forest
sforest<-subset(sforest,names(sforest)[1051:1166])#restrict to 1900-2015
#combine both
forestArea<-overlay(pforest,sforest,fun=function(x,y){
  return((x+y))
})
nlayers(pforest)
nlayers(forestArea)
plot(forestArea[[116]])#plot for the last year

#turn into an equal area raster
forestArea <- projectRaster(forestArea,crs="+proj=eck4")
plot(forestArea[[116]])#plot for the last year

#farmland
crop1<-stack("states.nc",varname="c3ann")
crop1<-subset(crop1,names(crop1)[1051:1166])
crop2<-stack("states.nc",varname="c4ann")
crop2<-subset(crop2,names(crop2)[1051:1166])
crop3<-stack("states.nc",varname="c3per")
crop3<-subset(crop3,names(crop3)[1051:1166])
crop4<-stack("states.nc",varname="c4per")
crop4<-subset(crop4,names(crop4)[1051:1166])
crop5<-stack("states.nc",varname="c3nfx")
crop5<-subset(crop5,names(crop5)[1051:1166])
#sum them
cropArea<-overlay(crop1,crop2,crop3,crop4,crop5,fun=function(a,b,c,d,e){
  return((a+b+c+d+e))
})
#turn into an equal area raster
cropArea <- projectRaster(cropArea,crs="+proj=eck4")
plot(cropArea[[116]])#plot for the last year

#pasture
pasture1<-stack("states.nc",varname="pastr")#pasture
pasture1<-subset(pasture1,names(pasture1)[1051:1166])
pasture2<-stack("states.nc",varname="range")#range land
pasture2<-subset(pasture2,names(pasture2)[1051:1166])
#sum them
pastureArea<-overlay(pasture1,pasture2,fun=function(a,b){
  return((a+b))
})
#turn into an equal area raster
pastureArea <- projectRaster(pastureArea,crs="+proj=eck4")
plot(pastureArea[[116]])#plot for the last year

#urban
urban<-stack("states.nc",varname="urban")
urbanArea<-subset(urban,names(urban)[1051:1166])
#turn into an equal area raster
urbanArea <- projectRaster(urbanArea,crs="+proj=eck4")
plot(urbanArea[[116]])#plot for the last year

##########################################################################################

#convert monitoring data coordinates into eck4 projection as well
mydata <- spTransform(mydata, crs(forestArea))

#convert years into raster layers
mydata@data$Start_year <- mydata@data$Start_year-1900+1
mydata@data$End_year <- mydata@data$End_year-1900+1

#any dates over 2015 cap at 2015 since that is the maximum data available
mydata@data$Start_year[which(mydata@data$Start_year>116)]<-116
mydata@data$End_year[which(mydata@data$End_year>116)]<-116

########################################################################################

#function to extract environmental data layer at each location
myfun <- function(i,mylayer="forestArea"){

#identify cell overlapping with coords
cellNu <- cellFromXY(get(mylayer), mydata@coords[i,])

#identify time period for coords and restrict raster to then
r <- subset(get(mylayer), c(mydata@data[i,"Start_year"],mydata@data[i,"End_year"]))

#extract information at cell for these times
out <- extract(x=r,y=cellNu)
colnames(out) <- c(paste0("Start_",mylayer),paste0("End_",mylayer))

#add to dataframe
output <- data.frame(cbind(mydata@data[i,],out))
return(output)

}

#run function on each row of data
library(plyr)
output <- ldply(1:nrow(mydata),function(x)myfun(i=as.numeric(x),mylayer="forestArea"))
output2 <- ldply(1:nrow(mydata),function(x)myfun(i=as.numeric(x),mylayer="cropArea"))
output3 <- ldply(1:nrow(mydata),function(x)myfun(i=as.numeric(x),mylayer="pastureArea"))
output4 <- ldply(1:nrow(mydata),function(x)myfun(i=as.numeric(x),mylayer="urbanArea"))

#combine all
output <- data.frame(cbind(mydata@data),output[,14:15],
                     output2[,14:15],output3[,14:15],
                     output4[,14:15])
write.table(output,file="landuseData.txt",sep="\t")

######################################################################################

coords<-read.delim("landuseData.txt") 
coords <- subset(coords,complete.cases(coords[,c(11,13,15,17)]))
coords2<- coords[,c(11,13,15,17)]

# K-Means Cluster Analysis
fit <- kmeans(coords, 5) # 5 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)

### Ward Hierarchical Clustering
d <- dist(coords, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
coords2$groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")

###################################################################

#get country data
coords2continent = function(points){ 
  library(sp)
  library(rworldmap)
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  indices = over(pointsSP, countriesSP)
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
  indices$ADMIN  #returns country name
  #indices$ISO3 # returns the ISO3 code 
}

mydata$country<-coords2continent(mydata@coords[,c("Longitude","Latitude")])
write.table(mydata@data,file="country.txt",sep="\t")

##################################################################

#test with europe data
load("completeData.RData")

df<-subset(completeData,Realm=="Terrestrial"&Continent=="Europe")

#quick and dirty trends
trends <- ddply(df,.(Datasource_ID,Plot_ID),function(x){
  lm1<-lm(log10(Number+1)~Year,data=x)
  summary(lm1)$coef[2,]
})

trends$Country <- mydata@data$country[match(trends$Plot_ID,mydata$Plot_ID)]

#overall histogram
ggplot(trends)+
  geom_histogram(aes(x=Estimate))

ggplot(trends)+
  geom_histogram(aes(x=Estimate))+
  facet_wrap(~Country,scales="free")+
  geom_vline(xintercept=0,col="red")


##################################################################
#get ESA CCI data
setwd("~/Documents/sChange/driver_maps/CCI ESA")
library(raster)
r<-stack("ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif")
out <- extract(r,mydata)
out<-data.frame(out)
names(out)<-1992:2015
out$Plot_ID<-mydata$Plot_ID
out$Datasource_ID<-mydata$Datasource_ID

write.table(out,file="LU_CCIESA_1992on.txt",sep="\t")

###################################################################