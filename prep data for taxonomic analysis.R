
rm(list=ls()) 


library(reshape2)
library(tidyverse)

# load sheets of original database
setwd("C:/Users/roelv/Dropbox/Insect Biomass Trends/csvs") # home
setwd("C:\\Dropbox\\Dropbox\\Insect Biomass Trends/csvs") # work


beetles<- "grey50"
aq <- "dodgerblue4"
rest <- "red"
color.scheme.taxa<- c( "Acari" = rest, "Opiliones" = rest ,"Collembola" = rest   ,"Myriapoda" = rest  ,
                       "Terrestrial Isopods" = rest ,"Thysanoptera"  = rest      , "Neuroptera"  = rest ,  "Orthoptera" = rest, 
                       "Aquatic Coleoptera" = aq ,"Aquatic Diptera" = aq ,"Chironomidae" = aq ,"Aquatic Crustaceans" = aq ,
                       "Ephemeroptera" = aq, "Megaloptera" = aq, "Odonata" = aq, "Plecoptera" = aq, "Trichoptera" = aq,
                       "Araneae" = rest, "Other Hymenoptera" = rest,"Syrphidae"   = rest  , "Other Diptera" = rest , 
                       "Other Coleoptera" = beetles  ,"Staphylinidae" = beetles  , "Coccinellidae"  = beetles ,"Curculionidae" =  beetles , "Carabidae" = beetles  , 
                       "Heteroptera" = "green" ,  "Auchenorrhyncha"  = "green","Sternorrhyncha" = "green" , 
                       "Moths" = "blue"   , "Butterflies"  = "red")




load("all.selectedIns.RData")
load("all.selectedArth.RData")
load("allSelectedArthHigherTax.RData")

all.arth<-allSelectedArthHigherTax

dim(all.selectedIns)
dim(all.arth)
names(all.selectedIns)

sort(unique(all.arth$Order))


# coarse taxa (order or subclass)
all.arth$GroupCoarse <- as.character(all.arth$Order)

all.arth$GroupCoarse[all.arth$Class == "Chilopoda"] <- "Myriapoda" # chilopda (class)
all.arth$GroupCoarse[all.arth$Class == "Diplopoda"] <- "Myriapoda" # diplopoda (class)

all.arth$GroupCoarse[all.arth$Subclass == "Collembola"] <- "Collembola"  # # Collembola (subclass)
all.arth$GroupCoarse[all.arth$Subclass == "Acari"] <- "Acari"  # # Collembola (subclass)

all.arth$GroupCoarse[all.arth$Class == "Malacostraca"] <- "Crustacea" # 
all.arth$GroupCoarse[all.arth$Class == "Ostracoda"] <- "Crustacea" # 
all.arth$GroupCoarse[all.arth$Class == "Branchiopoda"] <- "Crustacea" # 
all.arth$GroupCoarse[all.arth$Class == "Maxillopoda"] <- "Crustacea" # 
all.arth$GroupCoarse[all.arth$Class == "Hexanauplia"] <- "Crustacea" # 

sort(unique(all.arth$GroupCoarse))


metadata_TaxCoarse<-  all.arth %>% 
  group_by(GroupCoarse) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    Datapoints = length(Number),
    Datapointsabove0 = sum(Number>0),
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
print(metadata_TaxCoarse, n=Inf)




# finer grouping into commonly assessed groups
all.arth$Group <- NA

all.arth$Group[all.arth$Order == "Araneae"] <- "Araneae" #spiders (order) (exclude fw)
all.arth$Group[all.arth$Subclass == "Acari"] <- "Acari"  # mites (subclass) fw and terr
all.arth$Group[all.arth$Order == "Opiliones"] <- "Opiliones" # harvestmen  (exclude fw)
# scorpions and other weird arachnids? 
archnds<-(subset(all.arth, Class == "Arachnida" & Order != "Araneae" & Subclass != "Acari" & Order != "Opiliones" & Order != "Pseudoscorpiones"))
unique(archnds$Datasource_name) # 2 datasets - not include

# Myriapoda: 
all.arth$Group[all.arth$Class == "Chilopoda"] <- "Myriapoda" # chilopda (class)
all.arth$Group[all.arth$Class == "Diplopoda"] <- "Myriapoda" # diplopoda (class)

# crustacea
AC<- "Aquatic Crustaceans"
all.arth$Group[all.arth$Order == "Isopoda" & all.arth$Realm == "Terrestrial"] <- "Terrestrial Isopods"
all.arth$Group[all.arth$Order == "Isopoda" & all.arth$Realm == "Freshwater"] <- AC
all.arth$Group[all.arth$Order == "Decapoda" & all.arth$Realm == "Freshwater"] <-  AC
all.arth$Group[all.arth$Order == "Amphipoda" & all.arth$Realm == "Freshwater"] <- AC
all.arth$Group[all.arth$Order == "Harpacticoida" & all.arth$Realm == "Freshwater"] <- AC
all.arth$Group[all.arth$Order == "Mysida" & all.arth$Realm == "Freshwater"]     <- AC
all.arth$Group[all.arth$Order == "Notostraca" & all.arth$Realm == "Freshwater"] <- AC 
all.arth$Group[all.arth$Order == "Tanaidacea" & all.arth$Realm == "Freshwater"] <- AC
all.arth$Group[all.arth$Order == "Ostracoda" & all.arth$Realm == "Freshwater"] <- AC
all.arth$Group[all.arth$Order == "Cyclopoida" & all.arth$Realm == "Freshwater"] <- AC
all.arth$Group[all.arth$Order == "Cladocera" & all.arth$Realm == "Freshwater"] <- AC


all.arth$Group[all.arth$Subclass == "Collembola"] <- "Collembola"  # # Collembola (subclass)


all.arth$Group[all.arth$Order == "Orthoptera"]    <- "Orthoptera" # (order)
all.arth$Group[all.arth$Order == "Plecoptera"]    <- "Plecoptera" #(order) exclude terr
all.arth$Group[all.arth$Order == "Ephemeroptera"] <- "Ephemeroptera" #(order) exclude terr
all.arth$Group[all.arth$Order == "Odonata"]       <- "Odonata" #(order)
all.arth$Group[all.arth$Order == "Megaloptera"]   <- "Megaloptera"# (Order)
all.arth$Group[all.arth$Order == "Thysanoptera"]  <-   "Thysanoptera" #(Order)
all.arth$Group[all.arth$Order == "Blattodea" ]    <- "Blattodea"
all.arth$Group[all.arth$Order == "Psocoptera" ]    <- "Psocoptera"


#Hemiptera: 
all.arth$Group[all.arth$Suborder == "Auchenorrhyncha"] <- "Auchenorrhyncha" #  (suborder)
all.arth$Group[all.arth$Suborder == "Heteroptera"] <- "Heteroptera" #  Heteroptera (suborder)(suborder)   aquatic Heteroptera (several families in suborder)
all.arth$Group[all.arth$Suborder == "Sternorrhyncha"] <- "Sternorrhyncha" #  # Sternorrhyncha?  (suborder)
# 



# HOLOMETABOLA
all.arth$Group[all.arth$Order == "Neuroptera"] <- "Neuroptera" # neuroptera (order)



# Lepidoptera: butterfies (some families + more coarse classifications 'butteflies', day-active etc)
sort(unique(all.arth[all.arth$Order == "Lepidoptera", ]$Family))
bt<- "Butterflies"
all.arth$Group[all.arth$Family == "Nymphalidae" ] <- bt
all.arth$Group[all.arth$Family == "Hesperiidae" ] <- bt
all.arth$Group[all.arth$Family == "Pieridae" ]    <- bt
all.arth$Group[all.arth$Family == "Papilionidae" ] <- bt
all.arth$Group[all.arth$Family == "Riodinidae" ] <- bt
all.arth$Group[all.arth$Family == "Lycaenidae" ] <- bt
all.arth$Group[all.arth$Note == "dayactive" ] <- bt


# Lepidoptera: moths (rest lepis, exclude fw)
m<- "Moths"
all.arth$Group[all.arth$Order == "Lepidoptera" &  is.na(all.arth$Group) ] <- m

all.arth$Group[all.arth$Order == "Trichoptera"] <- "Trichoptera" # Trichoptera (Order, fw) exclude terr

# Diptera
sort(unique(all.arth[all.arth$Order == "Diptera", ]$Family))
all.arth$Group[all.arth$Family == "Syrphidae" ] <- "Syrphidae"  #Diptera: Syrphidae (Fam terr)
all.arth$Group[all.arth$Family == "Chironomidae" ] <- "Chironomidae"  # Diptera : Chironomidae (fam fw)
all.arth$Group[all.arth$Order == "Diptera" &  all.arth$Realm == "Freshwater" & all.arth$Family != "Chironomidae" ] <- "Aquatic Diptera"  # other fw diptera 
all.arth$Group[all.arth$Order == "Diptera" &  is.na(all.arth$Group) ]         <- "Other Diptera"  #  terrestrial diptera




#Coleoptera: 
sort(unique(all.arth[all.arth$Order == "Coleoptera", ]$Family))
# aquatic coleoptera as group (several families)
all.arth$Group[all.arth$Family == "Carabidae" ] <- "Carabidae" # carabidae (family)
all.arth$Group[all.arth$Family == "Staphylinidae" ] <- "Staphylinidae" # Staphylinidae (family)
# # scarabaeidae??? 
all.arth$Group[all.arth$Family == "Curculionidae" ] <- "Curculionidae" # Staphylinidae (family)
all.arth$Group[all.arth$Family == "Coccinellidae"] <- "Coccinellidae" # Coccinellidae
# rest coleoptera
all.arth$Group[all.arth$Order == "Coleoptera" &  all.arth$Realm == "Freshwater" ] <- "Aquatic Coleoptera"
all.arth$Group[all.arth$Order == "Coleoptera" &  is.na(all.arth$Group) ] <- "Other Coleoptera"


#Hymenoptera - rest 
unique(all.arth[all.arth$Order == "Hymenoptera", ]$Family)

b<- "Bees"# Hymen - bees (several families)
all.arth$Group[all.arth$Family == "Andrenidae" ]    <- b # 
all.arth$Group[all.arth$Family == "Halictidae" ]    <- b # 
all.arth$Group[all.arth$Family == "Megachilidae" ]  <- b # 
all.arth$Group[all.arth$Family == "Colletidae" ]    <- b # 
all.arth$Group[all.arth$Family == "Apidae" ]        <- b # 
      
all.arth$Group[all.arth$Family == "Vespidae" ]    <- "Vespidae" # social wasps
all.arth$Group[all.arth$Suborder == "Symphyta" ]  <- "Symphyta" # Symphyta: Tenthredinidae Xiphydriidae   Argidae        Cephidae # 
all.arth$Group[all.arth$Family == "Formicidae" ]    <- "Formicidae"# Ants
 # rest wasps (mostly parasitioids)
all.arth$Group[all.arth$Order == "Hymenoptera" &  is.na(all.arth$Group) ] <- "Other Hymenoptera"


dim(subset(all.arth, is.na(Group)))
sample_n(subset(all.arth, is.na(Group)), 50)$Order
unique(subset(all.arth, is.na(Group))$Order)



# how many datasets for each group? 
orders<- dcast(all.arth, GroupCoarse ~ "noDatasets", value.var =  "Datasource_name", function(x){length(unique (x))})
orders[order(-orders$noDatasets), ]

#
# how many datasets for each family? 
families<- dcast(all.arth, Group ~ "noDatasets", value.var =  "Datasource_name", function(x){length(unique (x))})
families[order(-families$noDatasets), ]



#######################################################################################################################################################

# aggregate per group
full.pivot.order<- dcast( all.arth,  Datasource_ID + Datasource_name+ Location + Stratum  + Plot_ID + Plot_name +  Unit+
                           Year + Period + Date  + Realm + Country_State +  Region + 
                            Country+ Continent~ GroupCoarse,    value.var = "Number", sum, na.rm = TRUE) 
dim(full.pivot.order)
sample_n(full.pivot.order, 5)

GroupCoarse.aggr<- dcast( all.arth,  Datasource_ID + Datasource_name+ Location + Stratum  + Plot_ID + Plot_name +  Unit+
                            Year + Period + Date  + Realm + Country_State +  Region + 
                            Country+ Continent+ GroupCoarse ~ "Number",    value.var = "Number", sum, na.rm = TRUE) 
dim(GroupCoarse.aggr)

Group.aggr<- dcast( all.arth,  Datasource_ID + Datasource_name+ Location + Stratum  + Plot_ID + Plot_name +  Unit+
                            Year + Period + Date  + Realm + Country_State +  Region + 
                            Country+ Continent+ Group ~ "Number",    value.var = "Number", sum, na.rm = TRUE) 
dim(Group.aggr)



################

# prep for inla 

source("G:/work/2017 iDiv/2018 insect biomass/Insect-biomass-trends/Inla prep.R")


##############




load("completeData.TaxCoarse.RData")


metadata_TaxCoarse<-  subset(completeData.TaxCoarse, !is.na(Number))   %>% 
  group_by(GroupCoarse) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    Datapoints = length(Number),
    Datapointsabove0 = sum(Number>0),
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
print(metadata_TaxCoarse, n=Inf)


smp<-sample(unique(completeData.TaxCoarse$Plot_ID), 100)
testdata<- completeData.TaxCoarse[completeData.TaxCoarse$Plot_ID %in% smp , ]


inlaFtaxCoarse <- inla(log10(Number+1) ~ cYear:GroupCoarse + GroupCoarse + 
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




taxSlope<- inlaFtaxCoarse$summary.fixed[7:12,]
vars<-data.frame(do.call(rbind, strsplit(rownames(taxSlope), split = ":")))
taxSlope<-cbind(taxSlope, vars)
taxSlope$X1<-gsub("TaxCoarse", "", taxSlope$X1)
taxSlope<- merge(taxSlope, metadata_TaxCoarse, by.x = "X1", by.y = "TaxCoarse")
taxSlope$text = paste0("(", taxSlope$Datasources, " | ", taxSlope$Plots, ")")

# reorder for graph
rownames(taxSlope)<-taxSlope$X1
#taxSlope$X1<- ordered(taxSlope$X1, levels = c("Water", "Underground" , "Soil surface", "Herb layer", "Trees", "Air" ))

brks<- c(-0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
e<- c("","","","","","","")


ggplot(data.frame(taxSlope))+
  geom_crossbar(aes(x=X1,y=mean, fill = X1,
                    ymin=X0.025quant,ymax=X0.975quant),position="dodge", width = 0.7, fill = "grey70")+
  coord_flip()+
  xlab ("")+ ylab("Trend slope  \n % change per year")+ #
  geom_hline(yintercept=0,linetype="dashed")+
  geom_text(aes(x = X1 , y = 0.028, label = text), size = 3) +
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.015,0.032))+
  theme_clean









load("completeData.TaxFine.RData")


metadata_TaxFine<-  subset(completeData.TaxFine, !is.na(Number))   %>% 
  group_by(Group) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    Datapoints = length(Number),
    Datapointsabove0 = sum(Number>0),
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
print(metadata_TaxFine, n=Inf)


smp<-sample(unique(completeData.TaxFine$Plot_ID), 100)
testdata<- completeData.TaxFine[completeData.TaxFine$Plot_ID %in% smp , ]




inlaFtaxFine <- inla(log10(Number+1) ~ cYear:Group + Group + 
                         f(Period_4INLA,model='iid')+
                         f(Location_4INLA,model='iid')+
                         f(Plot_ID_4INLA,model='iid')+
                         f(Datasource_ID_4INLA,model='iid')+
                         f(Plot_ID_4INLAR,iYear,model='iid')+
                         f(Location_4INLAR,iYear,model='iid')                      +
                         f(Datasource_ID_4INLAR,iYear,model='iid')+
                         f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                       control.compute = list(dic=TRUE,waic=TRUE),
                       data=completeData.TaxFine)



summary(inlaFtaxFine)
load("G:/work/2017 iDiv/2018 insect biomass/inlaFtaxFine.RData")

taxSlope<- inlaFtaxFine$summary.fixed[32:62,]
vars<-data.frame(do.call(rbind, strsplit(rownames(taxSlope), split = ":")))
taxSlope<-cbind(taxSlope, vars)
taxSlope$X1<-gsub("Group", "", taxSlope$X1)
taxSlope<- merge(taxSlope, metadata_TaxFine, by.x = "X1", by.y = "Group")
taxSlope$text = paste0("(", taxSlope$Datasources, " | ", taxSlope$Plots, ")")

# reorder for graph
rownames(taxSlope)<-taxSlope$X1
taxSlope$X1<- ordered(taxSlope$X1, levels = c("Acari" , "Opiliones" ,"Collembola"   ,"Myriapoda"  ,"Terrestrial Isopods" ,
                                              "Thysanoptera"       , "Neuroptera"  ,  "Orthoptera", 
                                              
                                              "Aquatic Coleoptera" ,"Aquatic Diptera" ,"Chironomidae" ,"Aquatic Crustaceans" ,
                                              "Ephemeroptera", "Megaloptera", "Odonata", "Plecoptera", "Trichoptera",
                                              "Araneae", "Other Hymenoptera","Syrphidae"    , "Other Diptera" , 
                                              "Other Coleoptera"   ,"Staphylinidae" , "Coccinellidae"  ,"Curculionidae" , "Carabidae"  , 
                                              "Heteroptera" ,  "Auchenorrhyncha" ,"Sternorrhyncha" , 
                                              
                                              "Moths"    , "Butterflies"))

#  taxSlope$X1 <- factor(taxSlope$X1, levels = taxSlope$X1[order(taxSlope$Datasources)])                                         




brks<- c(-0.1, -0.08, -0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
e<- c("","","","","","","")


ggplot(data.frame(subset(taxSlope, Datasources >2 )))+
  geom_crossbar(aes(x=X1,y=mean, fill = X1,
                    ymin=X0.025quant,ymax=X0.975quant),position="dodge", width = 0.7)+
  coord_flip()+
  xlab ("")+ ylab("Trend slope  \n % change per year")+ #
  scale_fill_manual(values = color.scheme.taxa)+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_text(aes(x = X1 , y = 0.085, label = text), size = 3) +
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.060,0.10)) +
  theme_clean + 
  theme(legend.key=element_blank(),
        legend.position='none')



