

setwd("C:\\Dropbox\\Dropbox\\Insect Biomass Trends/csvs") # work

load("metadata_per_plot.RData")


# land-use harmonization databse: 
LU<-read.table("landuseData (2).txt", header = T)
LUsept<- read.table("landuseData_newplots.txt", header = T)
dim(LU)
head(LU)
tail(LU)

#append old and new files 

cols<- intersect(names(LU), names(LUsept))

LU<- rbind(LU[, cols], LUsept[, cols])

LU$Datasource_ID[LU$Plot_ID  == 1739]<- 1527

# LUH2

#how much changed?
(sum(LU$End_forestArea) - sum(LU$Start_forestArea)) / sum(LU$Start_forestArea) # forest: +0.45
(sum(LU$End_cropArea) - sum(LU$Start_cropArea)) / sum(LU$Start_cropArea) # crop: -0.098
(sum(LU$End_pastureArea) - sum(LU$Start_pastureArea)) / sum(LU$Start_pastureArea) # pasture: -0.045
(sum(LU$End_urbanArea) - sum(LU$Start_urbanArea)) / sum(LU$Start_urbanArea) # urban: +0.19

# that's not insubstantial, potentially interesting

# how many locations experienced urbanization over the sampling period?
LU$urbanization<- (LU$End_urbanArea - LU$Start_urbanArea) 
hist((unique(LU$urbanization)), xlab = "urbanization")

LU$cropification<- LU$End_cropArea - LU$Start_cropArea
hist((LU$cropification))

save(LU, file = "LU.RData")









######################################################################

#Prep ESA data for analysis (% of each LU type in final year)



# ESA Land Cover State products
LU2<-read.table("LU_CCIESA_1992on (2).txt", header = T)
LU2nbrs<- read.table("LU_CCIESA_neighbours_1992on (2).txt", header = T)

LU2nw<-read.table("LU_CCIESA_1992on_newplots.txt", header = T)
LU2nbrsnw<- read.table("LU_CCIESA_neighbours_1992on_newplots.txt", header = T)

LU2<- rbind(LU2, LU2nw)
LU2nbrs<- rbind(LU2nbrs, LU2nbrsnw)

hist(LU2$X2015, breaks=seq(5,225,by=1)) # distribution of sites over different categories 
# were there any changes in landcover in this period? 
changed<- (LU2[(LU2$X1992 - LU2$X2015 != 0)   , c(1,24:26) ]) # 67 sites  have a  change = 4%
nrow(changed) / nrow (metadata_per_plot) # 5.3%
# which changes do we have 
unique(changed[,1:2])



#1) combine the file for main cell and neighbor cells

LU2nbrs<- LU2nbrs[, c(25,1:24)]
LU2IDs<-LU2[, c(1,26,27)]
LU2IDs$Datasource_ID[LU$Plot_ID  == 1739]<- 1527

LU2<- LU2[, 1:25]
LU2<- unique(LU2) # keep only 1 value for each mainCell

LU2<- rbind(LU2, LU2nbrs)
dim(LU2)/9  # 1023 unique cells
dim(LU2nbrs)

fullLU2<-merge(LU2, LU2IDs) # 
dim(fullLU2)/9  #= 1711 plots! correct 

changed<- (fullLU2[(fullLU2$X1992 - fullLU2$X2015 != 0)   , c(1,24:26) ])
nrow(changed)/ nrow(fullLU2)
# make matrix 

library(reshape2)
#TRy out for 2015 values
counts<- dcast(LU2, mainCell~X2015, length)
counts$frcCrop<- (counts$`10` / 9) + (counts$`11`/9)  + (counts$`12`/9) + (counts$`30`*0.75)/9 + (counts$`40`*0.25)/9  
counts$frcUrban<-counts$`190` / 9
hist((counts$frcCrop))
# works


# only take plots that are in completeData
fullLU2<- fullLU2[fullLU2$Plot_ID %in%  metadata_per_plot$Plot_ID, ]

valueLastYr<-NULL


for(i in 1: nrow(fullLU2)){
plt<- fullLU2$Plot_ID[i] 

year<- metadata_per_plot$End_year[metadata_per_plot$Plot_ID == plt]
true.yr<-year
if (length(year) == 0) {year <-0} # if plotID is missing, replce year  by 0
if(year>2015){  year<-2015} # for yrs after 2015 use 2015
if (year<1992  & year >1986){year <- 1992} # for yrs between 1987 and 1991 use 1992

yearx<- paste0( "X", year)
value <- fullLU2[i, yearx]

if(true.yr < 1987){value <-NA} # for yrs before 1987 use NA

valueLastYr[i]<- value
}

# calculate % cover for each plot
fullLU2$valueLastYr<-valueLastYr

counts<- dcast(na.omit(fullLU2), Plot_ID~valueLastYr, length)
dim(counts)  # 40 plots lost because too old
counts$frcCrop900m<- (counts$`10` / 9) + (counts$`11`/9)  + (counts$`12`/9) + (counts$`30`*0.75)/9 + (counts$`40`*0.25)/9  
counts$frcUrban900m<-counts$`190` / 9
hist((counts$frcCrop900m))
hist(counts$frcUrban900m)


percCover900m <-counts 
save(percCover900m, file = "percCover900m.RData")







































# junk


LU$EndTotalLandCover<- rowSums(LU[, c(13,15,17,19)])
LU$SCend_cropArea<- LU$End_cropArea/LU$EndTotalLandCover
LU$SCend_urbanArea<- LU$End_urbanArea/LU$EndTotalLandCover
LU$SCend_forestArea<- LU$End_forestArea/LU$EndTotalLandCover
LU$SCend_pastureArea<- LU$End_pastureArea/LU$EndTotalLandCover

load("RandEfPlot.RData")
test<- merge(RandEfPlot, LU, by = "Plot_ID")
test<- merge(metadata_per_plot, test, by = "Plot_ID")
dim(test)

save(LU, file = "LU")

# still many locations with practically no land use... 
dim(subset(LU, EndTotalLandCover < 0.2 )) #116
dim(subset(LU, EndTotalLandCover < 0.4 )) # 193

sample_n(subset(LU, EndTotalLandCover < 0.4 ), 10)


head(test)

library(gridExtra)
p1<-ggplot(test, aes(x= sqrt(End_urbanArea), y= slope, color = Realm)) + 
  geom_point()       + ggtitle("Urban area") + 
  scale_color_manual(values = col.scheme.realm)
p2<-ggplot(test, aes(x= sqrt(End_cropArea), y= slope, color = Realm)) + 
  geom_point()       + ggtitle( "Crop area")+ 
  scale_color_manual(values = col.scheme.realm)

grid.arrange(p1,p2, nrow = 2)


# only random plot slope (bc these are corrected for dataset effects? )
p1<-ggplot(test, aes(x= sqrt(End_urbanArea), y= test$`Plot_slp_ mean`, color = Realm)) + 
  geom_point()       + ggtitle("Urban area") + 
  scale_color_manual(values = col.scheme.realm)
p2<-ggplot(test, aes(x= sqrt(End_cropArea), y= `Plot_slp_ mean`, color = Realm)) + 
  geom_point()       + ggtitle( "Crop area")+ 
  scale_color_manual(values = col.scheme.realm)

grid.arrange(p1,p2, nrow = 2)






# corrected for missing land cover
p1<-ggplot(test, aes(x= sqrt(SCend_urbanArea), y= slope, color = Realm)) + 
  geom_point()       + ggtitle("Urban gebiet") + 
  scale_color_manual(values = col.scheme.realm)
p2<-ggplot(test, aes(x= sqrt(SCend_cropArea), y= slope, color = Realm)) + 
  geom_point()       + ggtitle( "Ackerflaeche")+ 
  scale_color_manual(values = col.scheme.realm)
p3<- ggplot(test, aes(x= sqrt(SCend_pastureArea), y= slope, color = Realm)) + 
  geom_point() + ggtitle( "Weidegebiet")       + 
  scale_color_manual(values = col.scheme.realm)
p4<- ggplot(test, aes(x= sqrt(SCend_forestArea), y= slope, color = Realm)) + 
  geom_point()  + ggtitle("Wald")     + 
  scale_color_manual(values = col.scheme.realm)

grid.arrange(p1,p2,p3,p4, nrow = 2)



ggplot(test, aes(x= End_cropArea, y= slope, color = Realm)) + 
  geom_point()  +     
  facet_wrap(~Continent)


