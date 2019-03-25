

setwd("C:\\Dropbox\\Dropbox\\Insect Biomass Trends/csvs") # work

# land-use harmonization databse: 
LU<-read.table("landuseData2.txt", header = T)
dim(LU)
head(LU)

# ESA Land Cover State products
LU2<-read.table("LU_CCIESA_1992on.txt", header = T)
hist(LU2$X2015, breaks=seq(5,225,by=1)) # distribution of sites over different categories 
# were there any changes in landcover in this period? 
changed<- (LU2[(LU2$X1992 - LU2$X2015 != 0)   , c(1,24:26) ]) # 67 sites  have a  chage 
# which changes do we have 
unique(changed[,1:2])


LU$EndTotalLandCover<- rowSums(LU[, c(13,15,17,19)])
LU$SCend_cropArea<- LU$End_cropArea/LU$EndTotalLandCover
LU$SCend_urbanArea<- LU$End_urbanArea/LU$EndTotalLandCover
LU$SCend_forestArea<- LU$End_forestArea/LU$EndTotalLandCover
LU$SCend_pastureArea<- LU$End_pastureArea/LU$EndTotalLandCover


test<- merge(results.per.plot, LU, by = "Plot_ID")
dim(test)

save(LU, file = "LU")

# still many locations with practically no land use... 
dim(subset(LU, EndTotalLandCover < 0.2 )) #116
dim(subset(LU, EndTotalLandCover < 0.4 )) # 193

sample_n(subset(LU, EndTotalLandCover < 0.4 ), 10)


head(test)

library(gridExtra)
p1<-ggplot(test, aes(x= End_urbanArea, y= slope, color = Realm)) + 
  geom_point()       + ggtitle("Urban gebiet") + 
  scale_color_manual(values = col.scheme.realm)
p2<-ggplot(test, aes(x= End_cropArea, y= slope, color = Realm)) + 
  geom_point()       + ggtitle( "Ackerflaeche")+ 
  scale_color_manual(values = col.scheme.realm)
p3<- ggplot(test, aes(x= End_pastureArea, y= slope, color = Realm)) + 
  geom_point() + ggtitle( "Weidegebiet")       + 
  scale_color_manual(values = col.scheme.realm)
p4<- ggplot(test, aes(x= End_forestArea, y= slope, color = Realm)) + 
  geom_point()  + ggtitle("Wald")     + 
  scale_color_manual(values = col.scheme.realm)

grid.arrange(p1,p2,p3,p4, nrow = 2)



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









