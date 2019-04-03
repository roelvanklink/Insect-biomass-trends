

setwd("C:\\Dropbox\\Dropbox\\Insect Biomass Trends/csvs") # work

# land-use harmonization databse: 
LU<-read.table("landuseData2.txt", header = T)
dim(LU)
head(LU)

# ESA Land Cover State products
LU2<-read.table("LU_CCIESA_1992on.txt", header = T)
LU2nbrs<- read.table("LU_CCIESA_neighbours_1992on.txt", header = T)
hist(LU2$X2015, breaks=seq(5,225,by=1)) # distribution of sites over different categories 
# were there any changes in landcover in this period? 
changed<- (LU2[(LU2$X1992 - LU2$X2015 != 0)   , c(1,24:26) ]) # 67 sites  have a  change = 4%
# which changes do we have 
unique(changed[,1:2])









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



######################################################################

#Prep ESA data for analysis (% of each LU type in final year)

#1) combine the file for main cell and neighbor cells

LU2nbrs<- LU2nbrs[, c(25,1:24)]
LU2IDs<-LU2[, c(1,26,27)]
LU2<- LU2[, 1:25]
LU2<- unique(LU2) # keep only 1 value for each mainCell

LU2<- rbind(LU2, LU2nbrs)
dim(LU2)/9  # 1023 unique cells
dim(LU2nbrs)

fullLU2<-merge(LU2, LU2IDs) # doesnt work because of duplicate values in both tables
dim(fullLU2)/9  #= 1661 plots! correct 
# make matrix 

#TRy out for 2015 values
counts<- dcast(LU2, mainCell~X2015, length)
counts$frcCrop<- (counts$`10` / 9) + (counts$`11`/9)  + (counts$`12`/9) + (counts$`30`*0.75)/9 + (counts$`40`*0.25)/9  
counts$frcUrban<-counts$`190` / 9
hist((counts$frcCrop))
# works



valueLastYr<-NULL


for(i in 1: nrow(fullLU2)){
plt<- fullLU2$Plot_ID[i]

year<- metadata_per_plot$End_year[metadata_per_plot$Plot_ID == plt]

if (length(year) == 0){year <-0} # if plotID is missing, replce year  by 0
if(year>2015){  year<-2015} # for yrs after 2015 use 2015
if (year<1992  & year >1986){year <- 1992} # for yrs between 1987 and 1991 use 1992

yearx<- paste0( "X", year)
value <- fullLU2[i, yearx]

if(year < 1987){value <-NA} # for yrs before 1987 use NA

valueLastYr[i]<- value
}

# calculate % cover for each plot
fullLU2$valueLastYr<-valueLastYr

counts<- dcast(fullLU2, Plot_ID~valueLastYr, length)
counts$frcCrop<- (counts$`10` / 9) + (counts$`11`/9)  + (counts$`12`/9) + (counts$`30`*0.75)/9 + (counts$`40`*0.25)/9  
counts$frcUrban<-counts$`190` / 9
hist((counts$frcCrop))

# 250 plots have no values
