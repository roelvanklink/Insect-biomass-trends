setwd("C:/Users/db40fysa/Dropbox/Insect Biomass Trends/csvs")

#libraries we need
library(INLA)
#library(devtools)
#install_github("julianfaraway/brinla")

load("completeData.RData")
completeData <- completeData[sample(1:nrow(completeData),5000),]

#original model
inlaF <- inla(log10(Number+1) ~ cYear + cYear:Realm +
                f(Period_4INLA,model='iid')+
                #f(Location_4INLA,model='iid')+
                f(Plot_ID_4INLA,model='iid')+
                f(Datasource_ID_4INLA,model='iid')+
                f(Plot_ID_4INLAR,iYear,model='iid')+
                #f(Location_4INLAR,iYear,model='iid')                      
                f(Datasource_ID_4INLAR,iYear,model='iid')+
                f(iYear,model='ar1'),#replicate=as.numeric(Plot_ID_4INLA)
              control.predictor = list(link = 1),
              data=completeData) 


#compare between fits and obs
completeData$preds <- inlaF$summary.fitted.values$mean#need to have control.predictor = list(link = 1) in model
library(ggplot2)
ggplot(completeData,aes(x=preds,y=log10(Number+1)))+
  geom_point()+
  geom_abline(slope=1,intercept=0)+
  facet_wrap(Continent~Realm)

#Adding PC priors
sdres <- sd(log10(completeData$Number+1),na.rm=T)

#PC priors on ar1
#theta1  inla.doc("pc.prec")
#theta2  inla.doc("pc.cor1")
pcpriorAR1 <- list(theta1 = list(prior="pc.prec", param = c(3*sdres,0.01)),
               theta2 = list(prior="pc.cor1", param=c(0.5, 0.75)))#75% likely that the autocorrelation is bigger than 0.5

#PC prior on random effects
pcpriorIID <- list(prec = list(prior="pc.prec", param = c(3*sdres,0.01)))
#e.g. f(Location_4INLAR,iYear,model='iid',hyper=pcpriorIID)

#prior prior on family
pcpriorFamily <- list(prec = list(prior="pc.prec", param = c(3*sdres,0.01)))
#e.g.,control.family = list(hyper = pcpriorFamily)

#put on full model
inlaF <- inla(log10(Number+1) ~ cYear:Realm+ Realm + 
                f(Period_4INLA,model='iid',hyper=pcpriorIID)+
                #f(Location_4INLA,model='iid',hyper=pcpriorIID)+
                f(Plot_ID_4INLA,model='iid',hyper=pcpriorIID)+
                f(Datasource_ID_4INLA,model='iid',hyper=pcpriorIID)+
                f(Plot_ID_4INLAR,iYear,model='iid',hyper=pcpriorIID)+
                #f(Location_4INLAR,iYear,model='iid',hyper=pcpriorIID)+
                f(Datasource_ID_4INLAR,iYear,model='iid',hyper=pcpriorIID)+
                f(iYear,model='ar1',hyper=pcpriorAR1),#replicate=as.numeric(Plot_ID_4INLA)
              data=completeData) 

#get all random effects on scale of sd
library(brinla)
summary(inlaF)
bri.hyperpar.summary(inlaF)

##################################################################
