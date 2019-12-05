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
prec.prior <- list(prec = list(prior = "loggamma", param = c(1, 0.5)))
inlaFprior0.5 <- inla(log10(Number+1) ~ cYear:Realm+ Realm + 
                        f(Period_4INLA,model='iid', hyper = prec.prior)+ # prior for random effects 
                        f(Location_4INLA,model='iid', hyper = prec.prior)+
                        f(Plot_ID_4INLA,model='iid', hyper = prec.prior)+
                        f(Datasource_ID_4INLA,model='iid', hyper = prec.prior)+
                          f(Plot_ID_4INLAR,iYear,model='iid', hyper = prec.prior)+
                          f(Location_4INLAR,iYear,model='iid', hyper = prec.prior) +
                          f(Datasource_ID_4INLAR,iYear,model='iid', hyper = prec.prior)+
                      f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                      control.family = list(
                        hyper = list(
                          prec = list( prior = "loggamma", param = c(1,0.5)))), # this is the prior for the error
                      control.predictor = list(link = 1) ,
                      control.compute = list(dic=TRUE,waic=TRUE),
                      data=completeData) 

load("inlaFprior0.5-5651182.RData")
summary(inlaFprior0.5)

prec.prior <- list(prec = list(prior = "loggamma", param = c(0.1, 0.1)))
inlaFprior0.1 <- inla(log10(Number+1) ~ cYear:Realm+ Realm + 
                        f(Period_4INLA,model='iid', hyper = prec.prior)+ # prior for random effects 
                        f(Location_4INLA,model='iid', hyper = prec.prior)+
                        f(Plot_ID_4INLA,model='iid', hyper = prec.prior)+
                        f(Datasource_ID_4INLA,model='iid', hyper = prec.prior)+
                        f(Plot_ID_4INLAR,iYear,model='iid', hyper = prec.prior)+
                        f(Location_4INLAR,iYear,model='iid', hyper = prec.prior) +
                        f(Datasource_ID_4INLAR,iYear,model='iid', hyper = prec.prior)+
                        f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                      control.family = list(
                        hyper = list(
                          prec = list( prior = "loggamma", param = c(0.1,0.1)))), # this is the prior for the error
                      control.predictor = list(link = 1) ,
                      control.compute = list(dic=TRUE,waic=TRUE),
                      data=completeData) 



load("E:/inlaFprior0.5-5434477.RData")
load("E:/inlaFprior0.1-5434916.RData")
load("E:/inlaF-5433204.RData") # load normal file
summary(inlaFprior0.5)# no effects left . prior is too narrow 
summary(inlaFprior0.1)
summary(inlaF)

# PC prior #####
sdres <- sd(log10(completeData$Number+1),na.rm=T)

pcpriorAR1_0.6 <- list(theta1 = list(prior="pc.prec", param = c(u=3*sdres,0.01)),
                       theta2 = list(prior="pc.cor1", param=c(0.6, 0.75)))#75% likely that the autocorrelation is bigger than 0.6
pcpriorAR1_0.5 <- list(theta1 = list(prior="pc.prec", param = c(u=3*sdres,0.01)),
                       theta2 = list(prior="pc.cor1", param=c(0.5, 0.75)))#75% likely that the autocorrelation is bigger than 0.5
pcpriorAR1_0.86 <- list(theta1 = list(prior="pc.prec", param = c(u=3*sdres,0.01)),
                        theta2 = list(prior="pc.cor1", param=c(u=sqrt(3)/2, 3/4)))
pcpriorAR0_0.5 <- list(theta1 = list(prior="pc.prec", param = c(3*sdres,0.01)),
                       theta2 = list(prior="pc.cor0", param=c(0.5, 0.75)))#
#PC prior on random effects
pcpriorIID <- list(prec = list(prior="pc.prec", param = c(3*sdres,0.01)))
#prior prior on family
pcpriorFamily <- list(prec = list(prior="pc.prec", param = c(3*sdres,0.01)))



formul<-as.formula(paste("" ))

model <- inla( log10(Number+1) ~ cYear: Realm:Continent + Realm + Continent+ 
                 f(Period_4INLA,model='iid',hyper=pcpriorIID)+
                 f(Location_4INLA,model='iid',hyper=pcpriorIID)+
                 f(Plot_ID_4INLA,model='iid',hyper=pcpriorIID)+
                 f(Datasource_ID_4INLA,model='iid',hyper=pcpriorIID)+
                 f(Plot_ID_4INLAR,iYear,model='iid',hyper=pcpriorIID)+
                 f(Location_4INLAR,iYear,model='iid',hyper=pcpriorIID)+
                 f(Datasource_ID_4INLAR,iYear,model='iid',hyper=pcpriorIID)+
                 f(iYear,model='ar1', replicate=as.numeric(completeData$Plot_ID_4INLA), hyper = pcpriorAR0_0.5),
               control.compute = list(dic=TRUE,waic=TRUE, cpo = TRUE), 
               control.predictor = list(link = 1) , verbose = F, 
               control.family = list(hyper = pcpriorFamily),
               data=completeData)


inlaContPC1_0.5<- as.data.frame(readRDS("./Inla PC priors/inlaContPC1_0.5SUMMARY.rds"))[8:19,]  
inlaCont<- as.data.frame(readRDS("inlaFcontSUMMARY.rds"))[8:19,]
plot(inlaContPC1_0.5$mean, inlaCont$mean) ;abline(a=0, b=1) # almost ideantical
plot(inlaContPC1_0.5$X0.025quant, inlaCont$X0.025quant) ;abline(a=0, b=1)
plot(inlaContPC1_0.5$X0.975quant, inlaCont$X0.975quant) ;abline(a=0, b=1)

inlaContPC1_0.86<- as.data.frame(readRDS("./Inla PC priors/inlaContPC1_0.86SUMMARY.rds"))[8:19,]
inlaCont<- as.data.frame(readRDS("inlaFcontSUMMARY.rds"))[8:19,]
plot(inlaContPC1_0.86$mean, inlaCont$mean) ;abline(a=0, b=1) # almost ideantical
plot(inlaContPC1_0.86$X0.025quant, inlaCont$X0.025quant) ;abline(a=0, b=1)
plot(inlaContPC1_0.86$X0.975quant, inlaCont$X0.975quant) ;abline(a=0, b=1)

inlaRealmPC1_0.5<- as.data.frame(readRDS("./Inla PC priors/inlaRealmPC1_0.5SUMMARY.rds"))[3:4,]  
inlaRealmPC1_0.6<- as.data.frame(readRDS("./Inla PC priors/inlaRealmPC1_0.6SUMMARY.rds"))[3:4,]  
inlaRealmPC1_0.86<- as.data.frame(readRDS("./Inla PC priors/inlaRealmPC1_0.86SUMMARY.rds"))[3:4,]
inlaRealm<- as.data.frame(readRDS("inlaRealmSUMMARY.rds"))[3:4,]
rownames(inlaRealmPC1_0.5)<- paste0(rownames(inlaRealmPC1_0.5), "PC1_0.5")
rownames(inlaRealmPC1_0.6)<- paste0(rownames(inlaRealmPC1_0.6), "PC1_0.6")
rownames(inlaRealmPC1_0.86)<- paste0(rownames(inlaRealmPC1_0.86), "PC1_0.86")


allPCmodels<- rbind(inlaRealm, inlaRealmPC1_0.5, inlaRealmPC1_0.6,inlaRealmPC1_0.86)
allPCmodels$varname<- rownames(allPCmodels)
arrange(allPCmodels, rownames(allPCmodels))








# differences between biomass and abundance data (uses dataframe with both Units for datasets that have both) #####
metadata_AB<-  completeDataAB %>% 
  group_by( Realm, Unit) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)))
metadata_AB



# abundance vs biomass (overall model) #####
inla1.AB <- inla(log10(Number+1) ~ cYear: Unit +  Unit +
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
save(inla1.AB, file = "/data/Roel/inla1.AB.RData")

as.data.frame(readRDS("inla1.AB2SUMMARY.rds"))
inla1.AB<- readRDS("inla1.AB2TEST.rds")
# biomass is actually positive! This is maybe due to the FW datasets (fw = 21 vs terr =11 datasets)

# abundance vs biomass and realm
inla1.ABrealm <- inla(log10(Number+1) ~ cYear: Realm: Unit +  Unit +Realm +
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
save(inla1.ABrealm, file = "/data/Roel/inla1.ABrealm.RData")

#graph
inla1.ABrealm<-as.data.frame(readRDS("inla1.ABrealmSUMMARY.rds"))
inla1.ABrealm<- readRDS("inla1.ABrealmTEST.rds")

(inla1.ABrealm)
data.frame(
  var =   rownames(inla1.ABrealm)[4:7], 
  mean = (10^(inla1.ABrealm[4:7,1] )-1)  *100, # proportional changes per year
  CI2.5 = (10^(inla1.ABrealm[4:7,3] )-1 ) *100,#0.025 CI
  CI97.5 = (10^(inla1.ABrealm[4:7,5] )-1 ) *100# 0.975
)
10^(inla1.ABrealm$mean[4:7] *10)-1 # proportional changes per decade
10^(inla1.ABrealm$mean[4:7] *30)-1 # proportional changes per 30 yrs



ABSlope<- inla1.ABrealm[4:7,]
vars<-data.frame(do.call(rbind, strsplit(rownames(ABSlope), split = ":")))
ABSlope<-cbind(ABSlope, vars)
ABSlope$Realm<-gsub("Realm", "", ABSlope$X2)
ABSlope$Unit<-gsub("Unit", "", ABSlope$X1)
ABSlope$AB <-ABSlope$Unit
ABSlope<- merge(ABSlope, metadata_AB)
ABSlope$text = paste0("(", ABSlope$Datasources, " | ", ABSlope$Plots, ") ")
ABSlope$Unit[ABSlope$Unit == "abundance"]<- "Abundance" ; ABSlope$Unit[ABSlope$Unit == "biomass"] <- "Biomass" 
ABSlope$Unit<- ordered(ABSlope$Unit, levels = rev(c("Abundance", "Biomass" )))


#brks<- c(-0.010, -0.005, 0, 0.005, 0.01, 0.015)
#perc<-(10^(brks )  *100) - 100
#l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
#e<- c("","","","","","","")


ABplot<- ggplot(data.frame(ABSlope))+
  geom_errorbar(aes(x=Unit,ymin=mean-sd, ymax=mean+sd, color = Realm),
                size = 2, width=0, position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Unit,ymin=X0.025quant,ymax= X0.975quant, color = Realm),
                width=0, position=position_dodge(width= 0.7))+  
  geom_point(aes(x=Unit,   y=mean, shape = Realm,  fill = Realm, color = Realm),
             size = 4, position=  position_dodge(width = 0.7), alpha=1 )+
  scale_color_manual(values = col.scheme.realm)+
  scale_fill_manual(values = col.scheme.realm)+
  scale_shape_manual(values = shps)+
  geom_hline(yintercept=0,linetype="dashed")+
  coord_flip()+
  scale_y_continuous(breaks = brks,labels = l, limits=lims)+
  xlab ("")+ ylab("")+  #Trend slope  \n % change per year
  geom_text(aes(x = Unit , y = 0.032, label = text, fill = Realm),  
            position = position_dodge(width = 0.7), size = 3, color = 1) +
  theme_clean +
  theme(legend.key=element_blank(),
        legend.position='none', 
        axis.text.x=element_blank()) +
  geom_text(aes(x = 2.3 , y = -0.025, label = "A"),  
            size = 6, color = 1) 

  
png("AB plot.png", width=2000, height=700, res = 360)
ABplot
dev.off()



#############
# test different subsets


# Excluding North America


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
                          data=subset(completeData, Continent != "North America"))

inlaFrealmExclNAm<-  as.data.frame(read_rds("inlaFrealmExclNAmSUMMARY.rds"))
data.frame(
  var =   c("FW 1 yr" ,"Terr 1 yr", "FW 10 yr", "Terr 10 yr"), 
  CI2.5 =  c((10^(inlaFrealmExclNAm  [3:4,3] )-1 ) *100, (10^(inlaFrealmExclNAm  [3:4,3] *10)-1)  *100),#0.025 CI
  mean =   c((10^(inlaFrealmExclNAm  [3:4,1] )-1)  *100, (10^(inlaFrealmExclNAm  [3:4,1] *10)-1)  *100), # proportional changes per year
  CI97.5 = c((10^(inlaFrealmExclNAm  [3:4,5] )-1 ) *100, (10^(inlaFrealmExclNAm  [3:4,5] *10)-1)  *100)# 0.975
)



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


# excl biomass#####
# relation estimates incl biomass and excluding biomass datasets 

contExclB<- as.data.frame(read_rds("inlaFcontExclBSUMMARY.rds"))
cont<- as.data.frame(read_rds("inlaFcontSUMMARY.rds"))

names(contExclB)<- paste0(names(contExclB), "exclB")
cont<- cbind(cont, contExclB)
cont<- cont[8:19,]
vars<-data.frame(do.call(rbind, strsplit(rownames(cont), split = ":")))
cont<-cbind(cont, vars)
cont$Realm<-gsub("Realm", "", cont$X1)
cont$Continent<-gsub("Continent", "", cont$X2)
ABSlope$AB <-ABSlope$Unit

cont.correlation <- ggplot(cont, aes(x = mean, y = meanexclB, shape = Realm , color = Continent)) + 
  geom_point( size = 2)+
#  geom_errorbar(aes(x=mean ,ymin=meanexclB-sdexclB, ymax=meanexclB+sdexclB))+
#  geom_errorbarh(aes(y=meanexclB ,xmin=mean-sd, xmax=mean+sd))+
  geom_abline(aes(intercept = 0, slope = 1))+
  scale_color_manual(values = col.scheme.cont)+
  xlab ("full model estimates") + ylab("esimates of model excluding biomass-only datasets") +
theme_clean

png("cont.correlation.png", width=2000, height=1500, res = 360)
cont.correlation
dev.off()




biomExclB<- as.data.frame(read_rds("inlaFbiomExclBSUMMARY.rds"))
biom<- as.data.frame(read_rds("inlaFbiomSUMMARY.rds"))

names(biomExclB)<- paste0(names(biomExclB), "exclB")
biom<- cbind(biom, biomExclB)
biom<- biom[6:13,]
vars<-data.frame(do.call(rbind, strsplit(rownames(biom), split = ":")))
biom<-cbind(biom, vars)
biom$Realm<-gsub("Realm", "", biom$X1)
biom$Climatic_zone<-gsub("BiomeCoarse", "", biom$X2)

biom.correlation <- ggplot(biom, aes(x = mean, y = meanexclB, shape = Realm , color = Climatic_zone)) + 
  geom_point( size = 2)+
#  geom_errorbar(aes(x=mean ,ymin=meanexclB-sdexclB, ymax=meanexclB+sdexclB))+
#  geom_errorbarh(aes(y=meanexclB ,xmin=mean-sd, xmax=mean+sd))+
  geom_abline(aes(intercept = 0, slope = 1))+
  scale_color_manual(values = col.scheme.biom)+
  xlab ("full model estimates") + ylab("esimates of model excluding biomass-only datasets") +
  theme_clean

png("biom.correlation.png", width=2000, height=1500, res = 360)
biom.correlation
dev.off()










# only europe and NA 



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





###############################

# outliers #####

load("randEfDataset.RData")
hist(RandEfDataset$slope)

# definition: values outside the 1.5* the interquantile distance above or below the 0.25 and 0.75 quantiles

lowOutl<-  RandEfDataset$Datasource_ID[RandEfDataset$slope < 
                        quantile(RandEfDataset$slope, 0.25)- 1.5*IQR(RandEfDataset$slope)]
hiOutl<-  RandEfDataset$Datasource_ID[RandEfDataset$slope >
                                         quantile(RandEfDataset$slope, 0.75)+ 1.5*IQR(RandEfDataset$slope)]
outliers<- c(lowOutl, hiOutl)
sort(outliers)
completeData5<- completeData[! completeData$Datasource_ID %in% outliers, ]


realmExclO<- as.data.frame(read_rds("./inla Excl Outliers/inlaFrealmExclOutlSUMMARY.rds"))[3:4,]
realm<- as.data.frame(read_rds("inlaRealmSUMMARY.rds"))[3:4,]
rownames(realmExclO)<- paste0(rownames(realmExclO), "exclO")
rbind(realm, realmExclO)


contExclO<- as.data.frame(read_rds("./inla Excl Outliers/inlaFcontExclOutlSUMMARY.rds"))
cont<- as.data.frame(read_rds("inlaFcontSUMMARY.rds"))

names(contExclO)<- paste0(names(contExclO), "exclO")
cont<- cbind(cont, contExclO)
cont<- cont[8:19,]
vars<-data.frame(do.call(rbind, strsplit(rownames(cont), split = ":")))
cont<-cbind(cont, vars)
cont$Realm<-gsub("Realm", "", cont$X1)
cont$Continent<-gsub("Continent", "", cont$X2)

cont.correlation <- ggplot(subset(cont, Continent != "Africa"), aes(x = mean, y = meanexclO, shape = Realm , color = Continent)) + 
  geom_point( size = 2)+
    geom_errorbar(aes(x=mean ,ymin=meanexclO-sdexclO, ymax=meanexclO+sdexclO))+
    geom_errorbarh(aes(y=meanexclO ,xmin=mean-sd, xmax=mean+sd))+
  geom_abline(aes(intercept = 0, slope = 1))+
  geom_hline(aes(yintercept = 0))+
  geom_vline(xintercept = 0,linetype="dashed")+
  scale_color_manual(values = col.scheme.cont)+
  xlab ("full model estimates") + ylab("esimates of model excluding outliers") +
  theme_clean

# Climatic zones
biomExclO<- as.data.frame(read_rds("./inla Excl Outliers/inlaFbiomExclOutlSUMMARY.rds"))
biom<- as.data.frame(read_rds("inlaFbiomSUMMARY.rds"))

names(biomExclO)<- paste0(names(biomExclO), "exclO")
biom<- cbind(biom, biomExclO)
biom<- biom[6:13,]
vars<-data.frame(do.call(rbind, strsplit(rownames(biom), split = ":")))
biom<-cbind(biom, vars)
biom$Realm<-gsub("Realm", "", biom$X1)
biom$biome<-gsub("BiomeCoarse", "", biom$X2)

biom.correlation <- ggplot(biom, aes(x = mean, y = meanexclO, shape = Realm , color = biome)) + 
  geom_point( size = 2)+
  geom_errorbar(aes(x=mean ,ymin=meanexclO-sdexclO, ymax=meanexclO+sdexclO))+
  geom_errorbarh(aes(y=meanexclO ,xmin=mean-sd, xmax=mean+sd))+
  geom_abline(aes(intercept = 0, slope = 1))+
  geom_hline(aes(yintercept = 0))+
  geom_vline(xintercept = 0,linetype="dashed")+
#  scale_color_manual(values = col.scheme.cont)+
  xlab ("full model estimates") + ylab("esimates of model excluding outliers") +
  theme_clean







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

# excluding biomass
inlaFcontExclB <- get(load("inlaFcontExclB.RData"))


metadata_cont<-  subset(completeData, Unit != "biomass") %>% 
  group_by(Continent, Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
metadata_cont

contSlope<- inlaFcontExclB$summary.fixed[8:19,]
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
  geom_text(aes(x = 5.3 , y = 0.025, label = "excl biomass"),  
            size = 6, color = 1) 



metadata_biom<-  subset(completeData, Unit != "biomass" ) %>% 
  group_by(BiomeCoarse, Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
metadata_biom



biomSlope<- as.data.frame(readRDS("inlaFbiomExclB12SUMMARY.rds"))[6:13,]
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
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.029,0.036))+
  xlab ("")+ ylab("Trend slope  \n % change per year")+
  theme_clean +
  theme(legend.key=element_blank(), 
        legend.position="bottom")  + 
  geom_text(aes(x = 4.3 , y = -0.025, label = "B"),  
            size = 6, color = 1) 



# R2 #####
inla0 <- inla(log10(Number+1)~ 1, data=completeData)
inla1<- readRDS("inla1TEST.rds")
inlaRealm<- readRDS("inlaRealmTEST.rds")
inlaBiom<- readRDS("inlaFbiomTEST.rds")
inlaCont<- readRDS("inlaFcontTEST.rds")

outFE<-inla0$model.matrix #just get the values of year and realm for each data row
coefFE<-as.matrix(inla0$summary.fixed[,1],ncol=1) #the fixed effect coefficient for the effects of year and realm
inla_dfFE<-outFE%*%coefFE #multiply them as written in the model equation to get the model predictions based on fixed effects
varF0<-var(as.numeric(inla_dfFE[,1])) #get the variance of these values
varF0 # variation explained by year-only  model 
varTotal<- bri.hyperpar.summary(inla0)[1]

outFE<-inla1$model.matrix #just get the values of year and realm for each data row
coefFE<-as.matrix(inla1$summary.fixed[,1],ncol=1) #the fixed effect coefficient for the effects of year and realm
inla_dfFE<-outFE%*%coefFE #multiply them as written in the model equation to get the model predictions based on fixed effects
varF<-var(as.numeric(inla_dfFE[,1])) #get the variance of these values
varF # variation explained by year-only  model 


outFE<-inlaRealm$model.matrix #just get the values of year and realm for each data row
coefFE<-as.matrix(inlaRealm$summary.fixed[,1],ncol=1) #the fixed effect coefficient for the effects of year and realm
inla_dfFE<-outFE%*%coefFE #multiply them as written in the model equation to get the model predictions based on fixed effects
varFrealm<-var(as.numeric(inla_dfFE[,1])) #get the variance of these values
varFrealm # variation explained by year+realm   model 

outFE<-inlaBiom$model.matrix #just get the values of year and realm for each data row
coefFE<-as.matrix(inlaBiom$summary.fixed[,1],ncol=1) #the fixed effect coefficient for the effects of year and realm
inla_dfFE<-outFE%*%coefFE #multiply them as written in the model equation to get the model predictions based on fixed effects
varFbiom<-var(as.numeric(inla_dfFE[,1])) #get the variance of these values
varFbiom # variation explained by year+realm   model 

outFE<-inlaCont$model.matrix #just get the values of year and realm for each data row
coefFE<-as.matrix(inlaCont$summary.fixed[,1],ncol=1) #the fixed effect coefficient for the effects of year and realm
inla_dfFE<-outFE%*%coefFE #multiply them as written in the model equation to get the model predictions based on fixed effects
varFcont<-var(as.numeric(inla_dfFE[,1])) #get the variance of these values
varFcont # variation explained by year+realm   model 

varFcont/varTotal # 0.061
varFbiom/varTotal # 0.027
varFrealm/varTotal # 0.01


inlaT0<-inla(log10(Number+1)~ 1, data=subset(completeData, Realm == "Terrestrial"))
inlaFW0<-inla(log10(Number+1)~ 1, data=subset(completeData, Realm == "Freshwater"))

inla1T<- readRDS("inla1TerrTEST.rds")
inla1FW<- readRDS("inla1FWTEST.rds")

inlaChangesT<- readRDS("inlaFChangesTerrTEST.rds")
inlaChangesFW<- readRDS("inlaFChangesFWTEST.rds")
inlaLUt<- readRDS("inlaFlanduseTTEST.rds")
inlaLUfw<- readRDS("inlaFlanduseFWTEST.rds")

varTotalTerr<- bri.hyperpar.summary(inlaT0)[1]
varTotalFW<- bri.hyperpar.summary(inlaFW0)[1]


outFE<-inlaChangesT$model.matrix #just get the values of year and realm for each data row
coefFE<-as.matrix(inlaChangesT$summary.fixed[,1],ncol=1) #the fixed effect coefficient for the effects of year and realm
inla_dfFE<-outFE%*%coefFE #multiply them as written in the model equation to get the model predictions based on fixed effects
varTchange<-var(as.numeric(inla_dfFE[,1])) #get the variance of these values
varTchange # variation explained by year-only  model 

varTchange/varTotalTerr

outFE<-inlaChangesT$model.matrix #just get the values of year and realm for each data row
coefFE<-as.matrix(inlaChangesT$summary.fixed[,1],ncol=1) #the fixed effect coefficient for the effects of year and realm
inla_dfFE<-outFE%*%coefFE #multiply them as written in the model equation to get the model predictions based on fixed effects
varTchange<-var(as.numeric(inla_dfFE[,1])) #get the variance of these values
varTchange # variation explained by year-only  model 

varTchange/varTotalTerr


outFE<-inla1T$model.matrix #just get the values of year and realm for each data row
coefFE<-as.matrix(inla1T$summary.fixed[,1],ncol=1) #the fixed effect coefficient for the effects of year and realm
inla_dfFE<-outFE%*%coefFE #multiply them as written in the model equation to get the model predictions based on fixed effects
varT1<-var(as.numeric(inla_dfFE[,1])) #get the variance of these values
varT1 # variation explained by year-only  model 

outFE<-inlaChangesFW$model.matrix #just get the values of year and realm for each data row
coefFE<-as.matrix(inlaChangesFW$summary.fixed[,1],ncol=1) #the fixed effect coefficient for the effects of year and realm
inla_dfFE<-outFE%*%coefFE #multiply them as written in the model equation to get the model predictions based on fixed effects
varFWchange<-var(as.numeric(inla_dfFE[,1])) #get the variance of these values
varFWchange # variation explained by year-only  model 

varFWchange/varTotalFW

outFE<-inlaLUt$model.matrix #just get the values of year and realm for each data row
coefFE<-as.matrix(inlaLUt$summary.fixed[,1],ncol=1) #the fixed effect coefficient for the effects of year and realm
inla_dfFE<-outFE%*%coefFE #multiply them as written in the model equation to get the model predictions based on fixed effects
varTlu<-var(as.numeric(inla_dfFE[,1])) #get the variance of these values
varTlu # variation explained by year-only  model 

varTlu/varTotalTerr







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

inlaFAstrat <- inla(log10(Number+1) ~ cYear:Stratum + Stratum  + 
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

# Fig S3 diver effect size  #####

setwd("C:/Dropbox/Insect Biomass Trends/csvs/")


LUchangeFW     <- cbind(Realm = "Freshwater",  as.data.frame(readRDS("inlaFchangesFWSUMMARY.rds"))[5:6,], variable = rownames(as.data.frame(readRDS("inlaFchangesFWSUMMARY.rds"))[5:6,]))
LUchangeTer    <- cbind(Realm = "Terrestrial", as.data.frame(readRDS("inlaFchangesTerrSUMMARY.rds"))[5:6,], variable = rownames(as.data.frame(readRDS("inlaFchangesTerrSUMMARY.rds"))[5:6,]))
cover25kmFW    <- cbind(Realm = "Freshwater",  as.data.frame(readRDS("inlaFlanduseFWSUMMARY.rds"))[5:6,], variable = rownames(as.data.frame(readRDS("inlaFlanduseFWSUMMARY.rds"))[5:6,]))
cover25kmTer   <- cbind(Realm = "Terrestrial", as.data.frame(readRDS("inlaFlanduseTSUMMARY.rds"))[5:6,], variable = rownames(as.data.frame(readRDS("inlaFlanduseTSUMMARY.rds"))[5:6,]))
cover0.81FW    <- cbind(Realm = "Freshwater",  as.data.frame(readRDS("inlaFlanduseESAfwSUMMARY.rds"))[5:6,], variable = rownames(as.data.frame(readRDS("inlaFlanduseESAfwSUMMARY.rds"))[5:6,]))
cover0.81Ter   <- cbind(Realm = "Terrestrial", as.data.frame(readRDS("inlaFlanduseESAterrSUMMARY.rds"))[5:6,], variable = rownames(as.data.frame(readRDS("inlaFlanduseESAterrSUMMARY.rds"))[5:6,]))
deltaFwCRU     <- cbind(Realm = "Freshwater",  as.data.frame(readRDS("inlaFClimChangesFWSUMMARY.rds"))[5:6,], variable = rownames(as.data.frame(readRDS("inlaFClimChangesFWSUMMARY.rds"))[5:6,]))
deltaTerCRU    <- cbind(Realm = "Terrestrial", as.data.frame(readRDS("inlaFClimChangesTerrSUMMARY.rds"))[5:6,], variable = rownames(as.data.frame(readRDS("inlaFClimChangesTerrSUMMARY.rds"))[5:6,]))
deltaFwCHELSA  <- cbind(Realm = "Freshwater",  as.data.frame(readRDS("inlaFCHELSAChangesFWSUMMARY.RDS"))[5:6,], variable = rownames(as.data.frame(readRDS("inlaFCHELSAChangesFWSUMMARY.RDS"))[5:6,]))
deltaTerCHELSA <- cbind(Realm = "Terrestrial", as.data.frame(readRDS("inlaFCHELSAChangesTerrSUMMARY.RDS"))[5:6,], variable = rownames(as.data.frame(readRDS("inlaFCHELSAChangesTerrSUMMARY.RDS"))[5:6,]))


   
drivers<- rbind(LUchangeTer, LUchangeFW, cover25kmFW   , cover25kmTer, cover0.81Ter, cover0.81FW, 
             deltaTerCRU, deltaFwCRU, deltaTerCHELSA, deltaFwCHELSA)
drivers$scale <- "mid"
drivers$scale [ drivers$X0.975quant-drivers$X0.025quant  <0.08]<-"small"
drivers$scale [drivers$X0.975quant-drivers$X0.025quant  >1]<-"large"
drivers$scale [drivers$variable == "cYear:cropification" ]<-"mid"



# rename variables 

drivers$name <- NA
drivers$name[drivers$variable == "cYear:urbanization"]        <- "Change in urban cover (landscape scale)"
drivers$name[drivers$variable == "cYear:cropification"]       <- "                         Change in cropland cover (landscape scale)"
drivers$name[drivers$variable == "cYear:cEnd_cropArea"]  <- "sqrt fraction Cropland cover at end (landscape scale)"
drivers$name[drivers$variable == "cYear:cEnd_urbanArea"] <- "sqrt fraction Urban cover at end (landscape scale)"
drivers$name[drivers$variable == "cYear:cFrcCrop900m1992"]     <- "fraction Cropland cover at end (0.81 km2)"
drivers$name[drivers$variable == "cYear:cFrcUrban900m1992"]    <- "fraction Urban cover per at end (0.81 km2)"
drivers$name[drivers$variable == "cYear:cRelDeltaTmean"]       <- "    Relative change in mean temperature (landscape scale)"
drivers$name[drivers$variable == "cYear:cRelDeltaPrec"]        <- "Relative change in monthly precipitation (landscape scale)"
drivers$name[drivers$variable == "cYear:cCHELSArelDeltaTmean"] <- "Relative change in mean temperature (1 km2)"
drivers$name[drivers$variable == "cYear:cCHELSArelDeltaPrec"]  <- "Relative change in monthly precipitation (1 km2)"

drivers$name<- factor(drivers$name, levels = rev(c( "Change in urban cover (landscape scale)", 
                                                   "                         Change in cropland cover (landscape scale)", 
                                                   "sqrt fraction Cropland cover at end (landscape scale)", 
                                                   "sqrt fraction Urban cover at end (landscape scale)"   ,  
                                                   
                                                   "fraction Cropland cover at end (0.81 km2)",
                                                   "fraction Urban cover per at end (0.81 km2)",
                                                   "    Relative change in mean temperature (landscape scale)",
                                                   "Relative change in monthly precipitation (landscape scale)",
                                                   "Relative change in mean temperature (1 km2)" ,
                                                   "Relative change in monthly precipitation (1 km2)")))                                              



sml<- ggplot( subset(drivers, scale =="small")   ) + 
  xlab ("")+ ylab ("")+geom_hline(yintercept=0,linetype="dashed")+
  geom_errorbar(aes(x=name,ymin=mean-sd, ymax=mean+sd, color = Realm),
                size = 2, width=0, alpha = 0.7, position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=name, ymin=X0.025quant,ymax= X0.975quant, color = Realm),
                width=0, position=position_dodge(width= 0.7))+  
  geom_point(aes(x=name,   y=mean, shape = Realm,  color = Realm, fill = Realm),
             size = 4, position=  position_dodge(width = 0.7), alpha=1 )+
 # scale_x_discrete("", labels = parse(text = name))+
  #  geom_crossbar(aes(x=name,   y=mean, fill = Realm,
#                    ymin=X0.025quant ,ymax= X0.975quant ), position="dodge",alpha=1 ,  width =0.7)+
 scale_color_manual(values = col.scheme.realm)+
  coord_flip()+
   theme_clean + 
  theme(legend.position = "", 
        strip.text = element_blank(), 
        strip.background = element_blank())

md<- ggplot( subset(drivers, scale =="mid")   ) + 
  xlab ("")+ ylab ("")+geom_hline(yintercept=0,linetype="dashed")+
  geom_errorbar(aes(x=name,ymin=mean-sd, ymax=mean+sd, color = Realm),
                size = 2, width=0, alpha = 0.7, position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=name, ymin=X0.025quant,ymax= X0.975quant, color = Realm),
                width=0, position=position_dodge(width= 0.7))+  
  geom_point(aes(x=name,   y=mean, shape = Realm,  color = Realm, fill = Realm),
             size = 4, position=  position_dodge(width = 0.7), alpha=1 )+
  scale_color_manual(values = col.scheme.realm)+
  coord_flip()+
  theme_clean + 
  theme(legend.position = "", 
        strip.text = element_blank(), 
        strip.background = element_blank())

lrg<- ggplot( subset(drivers, scale =="large")   ) + 
  xlab ("")+ ylab ("Model estimate")+geom_hline(yintercept=0,linetype="dashed")+
  geom_errorbar(aes(x=name,ymin=mean-sd, ymax=mean+sd, color = Realm),
                size = 2, width=0, alpha = 0.7, position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=name, ymin=X0.025quant,ymax= X0.975quant, color = Realm),
                width=0, position=position_dodge(width= 0.7))+  
  geom_point(aes(x=name,   y=mean, shape = Realm,  color = Realm, fill = Realm),
             size = 4, position=  position_dodge(width = 0.7), alpha=1 )+
  scale_color_manual(values = col.scheme.realm)+
  coord_flip()+
  theme_clean + 
  theme(legend.position = "", 
        strip.text = element_blank(), 
        strip.background = element_blank())+
  facet_wrap(.~scale, scales = "free", nrow = 3)


library(gridExtra)
grid.arrange(md,  sml, lrg,  nrow = 3,  heights = c(2.5, 7,2.5) )#

#









saveRDS(completeData, file =  "completeData20191026.RDS")












