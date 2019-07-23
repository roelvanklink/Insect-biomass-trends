# load all datasets ##### 
rm(list=ls()) 
 

library(reshape2)
library(tidyverse)
library(beepr)

# load sheets of original database
setwd("C:/Users/roelv/Dropbox/Insect Biomass Trends/csvs") # home
setwd("C:\\Dropbox\\Dropbox\\Insect Biomass Trends/csvs") # work

taxa<-read.csv( file = "taxa 3.0.csv"); dim(taxa)
plots<-read.csv( file = "Plots.csv"); dim(plots)
samples <-read.csv( file = "Sample_Info.csv"); dim(samples)
database <-read.csv( file = "Data.csv"); dim(database)
database<- subset(database, Note != "remove");dim(database)
studies<-read.csv(file = "studies.csv", header = T); dim(studies)
#studies1 <-read.table( file = "clipboard", header = T, sep = "\t"); dim(studies) 
#write.csv(studies, file = "studies.csv")

#Add taxonomic level to Taxon table
taxa$Level<- NA
taxa$Level[taxa$Phylum!= ""]<- "Phylum"
taxa$Level[taxa$Class!= ""]<- "Class"
taxa$Level[taxa$Subclass!= ""]<- "Subclass"
taxa$Level[taxa$Order!= ""]<- "Order"
taxa$Level[taxa$Suborder!= ""]<- "Suborder"
taxa$Level[taxa$Family!= ""]<- "Family"
taxa$Level[taxa$Subfamily!= ""]<- "Subfamily"
taxa$Level[taxa$Genus!= ""]<- "Genus"
taxa$Level[taxa$Species!= ""]<- "Species"
taxa$Level <- factor(taxa$Level, ordered = TRUE, 
                    levels = c("Phylum",  "Class", "Subclass", "Order","Suborder",  "Family",
                               "Subfamily","Genus" ,"Species" ))
taxa$Rank<-as.numeric(taxa$Level)        


# some changes to groupings
levels(studies$Continent)[levels(studies$Continent) == "South America"]  <- "Latin America"
levels(studies$Continent)[levels(studies$Continent) == "Central America"]  <- "Latin America"
levels(studies$Region)[levels(studies$Region) == "Russia Volga"]  <- "Russia Central & Volga"
levels(studies$Region)[levels(studies$Region) == "Russia Central"]  <- "Russia Central & Volga"
levels(studies$Region)[levels(studies$Region) == "Russia Ural"]  <- "Russia Ural & Siberia"
levels(studies$Region)[levels(studies$Region) == "Russia Siberia"]  <- "Russia Ural & Siberia"
levels(studies$Region)[levels(studies$Region) == "Russia Far East"]  <- "Asia East"

# manual groupings of some datasets
studies$Region[(studies$Region == "Germany" & studies$Realm == "Freshwater" ) ] <- "Europe rest West"
studies$Region[(studies$Region == "United Kingdom" & studies$Realm == "Freshwater" ) ] <- "Europe rest West"
studies$Region[(studies$Region == "Russia Northwest" & studies$Realm == "Terrestrial" ) ] <- "Europe rest North"



# remove repeated column names
names(studies) # no redundancy
names(database) # remove redundant columns later
names(samples) # remove redundant comuns
samples<- samples[, c("Sample_ID", "Datasource_ID", "Data_source", "Extraction_method", "Sampling_method", "Stratum", 
                  "Sample_area", "Ref.to.methods", "Number_of_replicates", "Aggregation_of_replicates", "Taxon_in_Data",            
                  "Taxon_redundancy", "Original_unit", "Calculations", "Unit",  "Richness_precision", "Error_unit" )   ]
names(plots) # remove redundant columns
plots<- plots[, c("Plot_ID", "Datasource_ID", "Location", "Plot_name", "Details.plots",  "Experimental_Treatment", "Details_expt_trt",
                 "Process_of_change", "notes_change", "invasives", "Coord_system", "Original_Latitude", "Original_Longitude", "Latitude",
                 "Longitude", "Elevation", "Source_geogr_data")]
names(taxa) # no redundancy
taxa<-taxa[, c("ID","Phylum", "Class", "Subclass", "Suborder",  "Order", "Family","Subfamily", "Genus",     "Species",   "Taxon", "Level", "Rank", "Note")]
  
# extra files to append to database file
Biotime <- read.csv( file = "BioTIME final 2019.csv");#head(Biotime)
Schuch <-read.csv( file = "sel.schuch.data.csv"); head(Schuch)
Schuch$Sample_ID[Schuch$Datasource_name == "Germany Marchand Schuch"] <-  318
Schuch$Sample_ID[Schuch$Datasource_name == "Germany Schiemenz Schuch"] <-  319
Swengel.temp <-read.csv(file = "Swengel temp aggregated.csv"); #head(Swengel.temp)
Hungary <- read.csv (file = "Valtonen_long.csv"); #head(Hungary)
Finland <- read.csv (file = "Kuusamon_long.csv"); #head(Finland)
Breitenbach <- read.csv(file = "Breitenbach.csv")
Krefeld <- read.csv( file = "Germany biomass Krefeld.csv", header = T) #head(Krefeld)
Wijster <- read.csv( file = "Netherlands Ground beetles.csv", header = T) #head(Wijster)
Lauwersmeer<- read.csv( file = "lauwersmeer final.csv", header = T)
Luquillo<- read.csv(file = "LTER Luquillo all canopy arthropods.csv", header = T)
levels(Luquillo$Taxon)[levels(Luquillo$Taxon) == "MELA"]<-"MELA1"      # is duplicate name in taxon list
levels(Luquillo$Taxon)[levels(Luquillo$Taxon) == "CHRYS"]<-"CHRYSOPID" # is duplicate name in taxon list
Luquillo<-subset(Luquillo, Taxon != "LAM") # remove 'Leaf Area Missing'
GPDD <-read.csv( file = "GPDD data.csv", header = T)
Greenland<- read.csv( file = "Greenland.csv", header = T); Greenland$Taxon<-gsub(" ", "_", Greenland$Taxon)
California <-read.csv( "Cali freshwater Resh.csv", header = T)
CC<- read.csv(file = "cedarcreekBEF.csv", header = T) ; CC$Number<-as.numeric(as.character(CC$Number))
AZ<- read.csv(file = "LTER Arizona Pitfalls NEW2019.csv", header = T);levels(AZ$Taxon)<-c(levels(AZ$Taxon), "NONE")  ; AZ$Taxon[is.na(AZ$Taxon)]<-"NONE"
AZ2<- read.csv(file = "sycamore creek formatted.csv", header = T)
sev.ants<- read.csv(file = "sev antnests formatted.csv", header = T) 
sev.gh  <- read.csv(file = "sev grasshoppers formatted.csv", header = T)
sev.pf  <- read.csv(file = "sev pitfalls formatted.csv", header = T)
ecn.but<- read.csv(file = "ECN butterflies final.csv", header = T)
levels(ecn.but$Taxon)[levels(ecn.but$Taxon) == "NONE" ] <- "Butterflies"
ecn.gb<- read.csv(file ="ECN ground beetles final nw.csv", header = T)

ecn.m2<-  read.csv(file ="ECN moths final_2.csv", header = T)
ecn.m<-  read.csv(file ="ECN moths final.csv", header = T)
Sweden<-read.csv("SEFW fnal 201907.csv", header = T) ; Sweden$Plot_name<- as.factor(Sweden$Plot_name)
NZ<- read.csv(file = "NZ river monitoring final.csv", header = T)
Kellogg<- read.csv(file = "Kellogg final.csv", header = T)
Ireland<- read.csv(file = "IRfinal1.csv", header = T)


# make alternative color schemes
col.scheme.cont<-c( "Europe"="green3", "Latin America"= "magenta", "North America"= "orange","Asia" = "purple3", 
                    "Africa" = "blue", "Australia" = "red")
col.scheme.realm<-c(  "Freshwater"  = "dodgerblue2", "Terrestrial" = "peru")


# patch up Swengel for merge. #####
#Still to do: original data before merge to homogeneous format, + Sample IDs, and sample file 
Datasource_name <- "US butterflies Swengel"
Taxon <-"Butterflies"
Unit<- "abundance"
Transformed_number<- NA;    Sex <- NA
Error <- NA;               
Sample_ID<-245

Swengel.workfile<-data.frame(Datasource_name, 
                             Plot_ID = Swengel.temp$Plot_ID, 
                             Plot_name = Swengel.temp$Plot_name, 
                             Sample_ID, 
                             Year = Swengel.temp$Year,
                             Period = Swengel.temp$Period,
                             Date = as.factor(Swengel.temp$Date),
                             Taxon, 
                             Sex, 
                             Unit, 
                             Original_number  = Swengel.temp$Lepidoptera, 
                             Transformed_number, 
                             Number = Swengel.temp$Lepidoptera/Swengel.temp$miles, 
                             Error
)
Swengel.workfile$Sample_ID[Swengel.workfile$Plot_ID>342 & Swengel.workfile$Plot_ID<390]<- 246
Swengel.workfile$Sample_ID[Swengel.workfile$Plot_ID>389]<- 247


#head(Swengel.workfile)
#sample_n (Swengel.workfile, 50) # looks good 





# metadata per dataset (aggregated over plots) - this can go into Studies file

descriptors<- plots %>% 
  group_by(Datasource_ID) %>%
  summarise(#Duration = (max(Year) - min(Year))+1, 
            mean_lat = mean(Latitude),
            mean_long = mean(Longitude),
            NUMBER_OF_PLOTS =  length(unique(Plot_ID)),
            NUMBER_OF_LOCATIONS = length(unique(Location))#,
          #  NUMBER_OF_SAMPLES = (),
         #   NUMBER_OF_YEARS = length(unique(Year))
           # NUMBER_OF_TAXA = length(unique(Taxon),
          #  N = sum(Number)
          )
(descriptors)
  save(descriptors, file = "Descriptors.RData")



test <- rbind(
   AZ[, -(1)], 
  AZ2[, -(1)], 
  Biotime[, -(1)],
  Breitenbach[, -(1)],
  California[, -(1)],
  CC[, -(1)], 
  ecn.gb[, -(1)],
  ecn.but[, -(1)],
  ecn.m[, -(1)],
  Finland[, -(1)],
  GPDD [, -(1:2)],
  Greenland[, -(1)],
  Ireland[, -(1)],
  Hungary[, -(1:2)] ,
  Kellogg[, -(1)],
  Krefeld[, -(1)],
  Lauwersmeer[, -1],
  Luquillo[, -(1)], 
  NZ[, -(1)],
  sev.ants[, -(1)], 
  sev.gh[, -(1)], 
  sev.pf[, -(1)], 
  Schuch[, -(1:2)],
  Sweden [, -(1)],  
  Swengel.workfile, 
  Wijster[, -(1)], 
  database[, -c(1,6, 17,18)]  )
dim(test)   # 815047    -> 678569 with standardized SE-FW data 
#






#################################################################################################################################
#Test all merges and links

# 1) do we have duplicate data?
 
 sum(duplicated(database)) #
 database[duplicated(database), ] # this needs t be checked thoroughly 
  # Owen 1984 wk 34 has been removed because many species were present 5 times. Remaining duplicate: 1 male, 1 female  
 # iceland have 0 
 # LTER NTL are nematodes
 
 
 
 sum(duplicated(test)) # = ok 947   
 # Accepted duplicates: 11 from database (all removed before analysis) 
 #+ 512 from Biotime (Konza prairie has some subplots that are not clear at the plot level. Assumed sampled consistently) 
 #+ 393 from Cedar creek (intertaxon different designations) 
 #+ 26 from gpdd (for each plot and year 2 values are entered, probably differnet seasons, but not specified for butterflies and moths) 
 # + 4 from greenland. These are true duplicates in the original data. All 0 duplicates have been removed. Low numbers 
 # 1 US butterflies Swengel, 
 
dups<- test[duplicated(test), ]
dups$Datasource_name<- droplevels(dups$Datasource_name)
table(dups$Datasource_name)
head(dups)

tail(dups, 15)


head( Biotime[duplicated(Biotime), ])
sum(duplicated(Biotime[, -(1:2)])) # 512 = correct
# most datasets had data per date instead of per year. is now fixed, except
# konza also has discrete dates, PLUS 512 duplicates that belong to different subplots. These are to be ignored. We assume same sampling effort over time. see column SAMPLE_DESC in biotime 

head(Ireland)
dim(Ireland)
sum(duplicated(Ireland[, -1])) # 608
# I removed duplicate records. I assume they were entered twice in some database. No idea how this can happen. Th eoccurrence records were unique, however 

head(ecn.m)
dim(ecn.m)
sum(duplicated(ecn.m[, -1])) #32147
# added date to df; problem solved


head(AZ)
sum(duplicated(AZ[, -1])) #13381
# traps were not aggregated. hope it's correctly done. the systematic nature of sampling is not clear to me. 
# also 20 dublicate obs fro the original df were removed

head(CC)
sum(duplicated(CC[, -1]))# 393= ok 
CC[duplicated(CC[, -1]), ]
# duplicates are caused by column 'Further.ID' - morphospecies description


head(GPDD)
sum(duplicated(GPDD[, -1]))
GPDD[ duplicated(GPDD[, -1]), ]



head(Greenland, 15)
sum(duplicated(Greenland[, -1]))
grdup <- Greenland[duplicated(Greenland[, -1]), ]
subset(grdup, Number>0)

head(Luquillo) # data for 2002-2004 are all double in dataset. have been removed, LTERLuquillo has been contacted 
sum(duplicated(Luquillo[, -1]))
tail(subset(dups, Datasource_name == "LTER Luquillo canopy1" ))


head(Sweden2) # ll duplicates ahve been removed
sum(duplicated(Sweden2[, -1])) #11256
SEdups<- Sweden2[duplicated(Sweden2[, -1]), ]

SEdups[c(seq(1,1001, by = 50)),]
#That's a lot of NA's 
test<- (subset(SEdups,  !is.na(SEdups$Number)))
tail(subset(SEdups, Plot_ID == 1235), 50)


# check for duplicate taxa
dim(test)

# formerly duplicated taxa
subset(test, Taxon == "DERM") # seems only in Tasmania, this is dermaptera
subset(Luquillo, Taxon == "DERM") # no dermestidae found
subset(test, Taxon == "MELA") # beetle in Luquillo and grasshopper in Sevilleta, fixed up  
subset(test, Taxon == "MELA1")




# check taxonfile
taxa[duplicated(taxa),]
taxa$Taxon[duplicated(taxa$Taxon)]

dim(test)
test1<-merge(test, taxa,  by = "Taxon") ; beep(1)#
dim(test1) #



# test if plot and study files correspond
studies[duplicated(studies$Datasource_ID), ] # only empty lines

dim(plots)


test2<-merge(plots, studies, by = "Datasource_ID") # 
dim(test2) # correct, because black rock forest is not in studies

plots[!plots$Datasource_ID %in% studies$Datasource_ID, ] # black rock forest was disqualified 


# 




# observations and plots
test2<-(merge(test1, plots, by = "Plot_ID"))
dim(test2) # all there

# samples and observations
test3<-(merge(test2, samples, by = "Sample_ID"))
dim(test3) 

setdiff(samples$Sample_ID, unique(test2$Sample_ID)) # all checked and correct 24.1.19
#  # 39,175, 176, 177,  ar unused sampleIDs 257 is not used biomass converted AZ pitfalls, 
# 263 is blackrock forest (disqualified) 
# 325 spittlebug morphs (not standardized) , 349 doesn't exist yet 
sort(setdiff(unique(test2$Sample_ID), unique(test3$Sample_ID)))

unique(test2$Sample_ID) [!unique(test2$Sample_ID)  %in%  samples$Sample_ID] # 0
samples[duplicated(samples$Sample_ID)] #no duplicates


# make sure NA's in number are recognized as na's
is.numeric(test$Number) # is ok 
mf<- as.numeric(test$Number)
wrong<-(is.na(mf))
test[wrong, ]














#######################################################################################################
########################################################################################################


# Select data for analysis #####

# remove duplicate columns 
names(test)
names(test)[names(test) == "Sex"]<-"Withintaxon_group"
names(test)[names(test) == "Unit"]<-"Unit_in_data"
test<- test[, c("Plot_ID", "Sample_ID",  "Year", "Period", "Date", "Taxon", "Withintaxon_group",  
                "Unit_in_data",  "Original_number", "Transformed_number", "Number",  "Error")] 


# merge all tables into 1 big object 

# merge with taxon
dim(test)
merge1<-merge(test, taxa, by = "Taxon")
dim(merge1) # all there

# merge with samples
merge2<-(merge(merge1, samples, by = "Sample_ID"))
dim(merge2) # all there. 
length(unique(merge2$Datasource_ID)) #158

# merge with plot # mind that column 'Datasource ID is in both 
merge3<- merge(merge2, plots)#, by = "Plot_ID"
dim(merge3) # all there 
names(merge3)

# merge with studies 
merge4<- merge(merge3, studies)
dim(merge4)
names(merge4)[order(names(merge4))]
beep(2)
############################################################################################################################################

# check for complete data of all important variables 

sum(is.na(merge4$Taxon))
sum(is.na(merge4$Latitude))
sum(is.na(merge4$Longitude))
sum(is.na(merge4$Stratum))
sum(is.na(merge4$Continent))
sum(is.na(merge4$Biome))
sum(is.na(merge4$Sample_ID))
sum(is.na(merge4$Location))
sum(is.na(merge4$Datasource_ID))
sum(is.na(merge4$Period))
# replace missing period data
merge4$Period[is.na(merge4$Period)]<-1  # replace missing Period data with 1
merge4$Period[merge4$Period == ""]<-1  # replace missing Period data with 1

sum(is.na(merge4$Plot_ID))
sum(is.na(merge4$Number)) #4199 on 29-1-2019  4197 on 29-3-19
sum(is.na(merge4$Year ))











# Selection 1: remove plots that have less  9 years Plots of 9 yrs are accepted in datasets with >10 yrs. Only exceptions are Israel and Ukraine (9 yrs) 

metadata_per_plot<-  merge4 %>% 
  group_by(Plot_ID) %>%
  summarise(
    Plot_name = length(unique(Plot_name)), 
    Datasource_ID = unique(Datasource_ID),
    Datasource_name = unique(Datasource_name), 
    Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T),
    Country_State = unique(Country_State),
    Country = unique(Country),
    Region = unique(Region),
    Realm = unique(Realm),
    #Stratum = length(unique(Stratum)),
    Longitude = unique(Longitude),
    Latitude = unique(Latitude),
    AbundanceBiomass = unique(Abundance.Biomass),
    NUMBER_OF_PLOTS =  length(unique(Plot_ID)), # should be 1
    NUMBER_OF_SAMPLES = length(unique(paste(Year, Period))),
    NUMBER_OF_YEARS = length(unique(Year)),
    NUMBER_OF_TAXA = length(unique(Taxon)),
    TOTAL_N = sum(Number, na.rm = T)
  )
dim(metadata_per_plot) # 1661   now 1566 on 5.4.19
newest<- as.data.frame(metadata_per_plot)
write.csv(metadata_per_plot, "metadata per plot 20190705.csv")
#save(metadata_per_plot, file ="metadata_per_plot.RData")


new<- read.csv("metadata per plot 20190405.csv")
old<-read.csv("metadata per plot 20190531.csv")   #1509
older<-read.csv("metadata per plot 20181123.csv") #1486









############################################################





# Selection 1:  Duration of timeseries: #####


# which plots and datasets  have less that 10 years data? 
# only acceptable for Donana (Spain butterflies) (1 plot) and Israel (all plots)  and Ukraine beetles (1 plot) 
load("metadata_per_plot.RData")
# which  datasets have short plots?
unique(subset(metadata_per_plot, Duration < 10)$Datasource_name) # 

subset(metadata_per_plot, Duration < 10) # 41 plots
subset(metadata_per_plot, Duration < 9) # 23 plots

# remove all plots < 9 years 
# bad plots: 
short.plots<- subset(metadata_per_plot, Duration < 9)$Plot_ID
# plots with 9 yrs  within datasets with >10 years are allowed to stay  (?)

merge4$duration.ok<- ! merge4$Plot_ID %in% short.plots # length = ok 

# only select plots that have sufficient duration
merge4.1<- subset(merge4, duration.ok == T)
dim(merge4.1)# 675919 on 5-4-19 -  still good on 31-5-19






#Selection  2: remove NA in Number  ####
nas<-subset(merge4.1, is.na(Number) ) ;dim(nas) #4199
merge4.2<-subset(merge4.1, !is.na(Number) ) # 
dim(merge4.2)
length(unique(merge4.2$Datasource_name)) # 158


# Selection 3: remove species richness #####
merge4.3<- subset(merge4.2, Unit != "richness")
dim(merge4.3)



full.dataset<- merge4.3
save(full.dataset, file = "full.dataset.RData") # 



# selection 4.4: deselect non arthropods ##### 
dim(merge4.3)
others<-subset(merge4.3, Phylum !=  "Arthropoda" & Phylum != "invertebrata" ); dim(others)
merge4.4<-subset(merge4.3, Phylum ==  "Arthropoda" | Phylum == "invertebrata" | Phylum == "Invertebrata")
dim(merge4.4)
length(unique(merge4.4$Datasource_name)) # 155
setdiff(merge4.3$Datasource_name, merge4.4$Datasource_name) # only Russia earthworms data disappeared

all.arthropods.dataset<- merge4.4
save(all.arthropods.dataset, file = "all.arthropods.dataset.RData")


# deselect  crustaceans
unique(merge4.4$Class)
merge4.5<-subset(merge4.4, Class !=  "Crustacea" )
merge4.5<-subset(merge4.5, Class !=  "Crustaceae" )
merge4.5<-subset(merge4.5, Class !=  "Malacostraca" )
merge4.5<-subset(merge4.5, Class !=  "Branchiopoda" )
merge4.5<-subset(merge4.5, Class !=  "Ostracoda" )
merge4.5<-subset(merge4.5, Class !=  "Maxillopoda" )
merge4.5<-subset(merge4.5, Class !=  "Hexanauplia" )



merge4.5<-subset(merge4.5, Class !=  "Chilopoda" )
merge4.5<-subset(merge4.5, Class !=  "Diplopoda" )
merge4.5<-subset(merge4.5, Class !=  "Symphyla" )

unique(merge4.5$Class)
dim(merge4.5) # 753143
length(unique(merge4.5$Datasource_name))

all.insects.dataset<- merge4.5

save(all.insects.dataset, file = "all.insects.dataset.RData")  # 









#selection 5: 
# deselect redundant taxa
redund<- subset(merge4.4, Taxon_redundancy != "");dim(redund) # some 22500
dim(subset(merge4.4, Taxon_redundancy == "higher_resolution"))
dim(subset(merge4.4, Taxon_redundancy == "lower_resolution"))


merge5.ins<-subset(merge4.5, Taxon_redundancy == "" | Taxon_redundancy == "lower_resolution") # deselect redundant data 
merge5.arth<-subset(merge4.4, Taxon_redundancy == "" |Taxon_redundancy == "lower_resolution") # deselect redundant data, adn take lowest traxonomic aggregation provided 
merge5.arth.higherTax<-subset(merge4.4, Taxon_redundancy == "" |Taxon_redundancy == "higher_resolution") # deselect redundant data, but take highest taxonomic res provided


dim(merge4.5)-dim(merge5.ins)
dim(merge4.4)- dim(merge5.arth)
dim(merge4.4)- dim(merge5.arth.higherTax)

length(unique(merge5.arth$Datasource_name))
length(unique(merge5.ins$Datasource_name))
length(unique(merge5.arth.higherTax$Datasource_name))


#anything missing? 
unique(merge4.4$Datasource_ID)[! unique(merge4.4$Datasource_ID) %in% unique(merge5.arth$Datasource_ID)]



all.selectedIns<-merge5.ins
all.selectedArth<- merge5.arth
allSelectedArthHigherTax<- merge5.arth.higherTax
save(all.selectedIns, file = "all.selectedIns.RData")
save(all.selectedArth, file = "all.selectedArth.RData")
save(allSelectedArthHigherTax, file = "allSelectedArthHigherTax.RData")




# make dataframe for Abundance/biomass trend comparison
ABcomparison.arthr<- subset(merge4.4, Abundance.Biomass == "AB")
ABcomparison.insect<- subset(merge4.5, Abundance.Biomass == "AB")
save(ABcomparison.insect, file = "ABcomparison.insect.RData")
save(ABcomparison.arthr, file = "ABcomparison.arthr.RData")







  

















# aggregate all data#####
#per sampling per timepoint per location 


load( "all.selectedIns.RData")
load( "all.selectedArth.RData")


all.aggr.insects<-dcast(all.selectedIns,  Datasource_ID + Datasource_name+ Location + Stratum  + Plot_ID + Plot_name +  Unit+
                   Year + Period + Date  + Realm + Country_State +  Region + Country+ Continent~ "Number",    value.var = "Number", sum, na.rm = TRUE)
 #  104096 # 54761 with ecn.m means   52390 with SE-FW stdzd  52340 on 5-7-19 CC fixed, taxonomy fixed
dim(all.aggr.insects)
length(unique(all.aggr.insects$Plot_ID)) # 1628 # now 1416 SE FW 1533

all.aggr.arth<-dcast(all.selectedArth,  Datasource_ID + Datasource_name+ Location + Stratum  + Plot_ID + Plot_name + Unit +  
                          Year + Period + Date  + Country + Region + Country_State + Realm +Continent~ "Number",    value.var = "Number", sum, na.rm = TRUE)
#  54781 # seems correct - 50.000 obs from UK ecn moths  # now 52736 with SE fW stdzd
dim(all.aggr.arth)
length(unique(all.aggr.arth$Plot_ID)) # 1628   now 1416 SEFW  52340 5-7-19



#compare to old version 
old<- as.data.frame(table(all.aggr$Datasource_name))
new<- as.data.frame(table(all.aggr.insects$Datasource_name))
arth<- as.data.frame(table(all.aggr.arth$Datasource_name))
m1<-merge(old, new, by = "Var1", all.y = T)
merge(m1, arth, by = "Var1", all.y = T)

save(all.aggr.insects, file = "all.aggr.insects.RData")
save(all.aggr.arth, file = "all.aggr.arth.RData")





# Prep for INLA: #####
# Add NA's for all missing years for INLA analysis #####

# Insects
#####and aggregated data 
load("all.aggr.insects.RData")
load("all.aggr.arth.RData")


# do these df's have different dims? 
dim(all.aggr.arth); dim(all.aggr.insects) # same
sum(all.aggr.arth$Number) ; sum(all.aggr.insects$Number) # 1.5mln difference is possible
anti_join(all.aggr.arth[,1:13], all.aggr.insects[,1:13])
# fixes: Utah FW: added 0 insects to three dates with no insects in 1994
# Russia FW2: zooplankton should have been removed: are now marked as taxonredundant
# Russia FW:  zooplankton should have been removed: are now marked as taxonredundant
# 20 sites in Sweden missing


# numbers before 29-3-19: before: 54761 insects ; after: 80314  ;  54781 arthropod values, after: 80359

#1533 plots 
completeData<- NULL
for(i in 1:length(unique(all.aggr.insects$Plot_ID))){
  
  plt<- unique(all.aggr.insects$Plot_ID)[i]
  myData<- all.aggr.insects[all.aggr.insects$Plot_ID == plt , ]
  
  #expand grid to include NAs  
  constantData <- unique(myData[,c("Plot_ID","Datasource_ID")])#these are defo unique
  allgrid <- expand.grid(Plot_ID = unique(myData$Plot_ID),
                         Year= min(myData$Year):max(myData$Year))
  allgrid <- merge(allgrid,constantData,by=c("Plot_ID"),all.x=T)
  
  #add observed data
  myData1 <- merge(allgrid,myData[,c("Year","Plot_ID", "Period", "Number")],  #"classes",
                   by=c("Year","Plot_ID"),all=T)
  # add descriptors
  myData <- merge(myData1, unique(myData[,c("Plot_ID",  "Location", "Datasource_name", "Realm",
                                            "Continent", "Country", "Country_State", "Region", "Stratum" )]),
                  by="Plot_ID",all=T)
  if(!all(is.na(myData$Period))){
    myData$Period[is.na(myData$Period)]<-sample(myData$Period[!is.na(myData$Period)],
                                                length(myData$Period[is.na(myData$Period)]),
                                                replace=T) }
  print(plt)
  completeData<-rbind (completeData,myData)
  
}
dim(completeData)





completeDataArth <- NULL
for(i in 1:length(unique(all.aggr.arth $Plot_ID))){
  
  plt<- unique(all.aggr.arth$Plot_ID)[i]
  myData<- all.aggr.arth[all.aggr.arth$Plot_ID == plt , ]
  
  #expand grid to include NAs  
  constantData <- unique(myData[,c("Plot_ID","Datasource_ID")])#these are defo unique
  allgrid <- expand.grid(Plot_ID = unique(myData$Plot_ID),
                         Year= min(myData$Year):max(myData$Year))
  allgrid <- merge(allgrid,constantData,by=c("Plot_ID"),all.x=T)
  
  #add observed data
  myData1 <- merge(allgrid,myData[,c("Year","Plot_ID", "Period", "Number")],  #"classes",
                   by=c("Year","Plot_ID"),all=T)
  # add descriptors
  myData <- merge(myData1, unique(myData[,c("Plot_ID",  "Location", "Datasource_name", "Realm",
                                            "Continent", "Country", "Country_State", "Region", "Stratum" )]),
                  by="Plot_ID",all=T)
  if(!all(is.na(myData$Period))){
    myData$Period[is.na(myData$Period)]<-sample(myData$Period[!is.na(myData$Period)],
                                                length(myData$Period[is.na(myData$Period)]),
                                                replace=T) }
  print(plt)
  completeDataArth<-rbind (completeDataArth,myData)
  
}
dim(completeDataArth)





#step 2 center Years and add indicies to the dataset for INLA
addIndicies <- function(myData){
  
  #year covariates
  myData$cYear <- myData$Year - median(myData$Year)
  myData$iYear <- myData$Year - min(myData$Year) + 1
  myData$rYear <- myData$iYear
  myData$rYear2 <- myData$iYear
  
  #random intercept indices (these are nested)
  myData$Period_4INLA <- interaction(myData$Datasource_ID,myData$Period)
  myData$Period_4INLA <- as.numeric(factor(myData$Period_4INLA))
  myData$Plot_ID_4INLA <- interaction(myData$Datasource_ID,myData$Plot_ID)
  myData$Plot_ID_4INLA <- as.numeric(factor(myData$Plot_ID_4INLA))   
  myData$Datasource_ID_4INLA <- as.numeric(factor(myData$Datasource_ID))
  myData$Country_State_4INLA <- as.numeric(factor(myData$Country_State))
  
  # This is now a crossed random effect: accounting for datasets that were collected at the same location
#  myData$Location[is.na(myData$Location)] <- 1#dummy value
 # myData$Location_4INLA <- interaction(myData$Datasource_ID,myData$Location) # this is not necessary anymore
  myData$Location_4INLA <- as.numeric(factor(myData$Location))
  
  #random slope indices
  myData$Plot_ID_4INLAR <- myData$Plot_ID_4INLA+max(myData$Plot_ID_4INLA)
  myData$Datasource_ID_4INLAR <- myData$Datasource_ID_4INLA+max(myData$Datasource_ID_4INLA)
  myData$Location_4INLAR <- myData$Location_4INLA+max(myData$Location_4INLA)
  myData$Country_State_4INLAR <- myData$Country_State_4INLA+max(myData$Country_State_4INLA)
  
  
  return(myData)
}

completeData <- addIndicies(completeData)
dim(completeData)
save(completeData, file = "completeData.RData")


#checks: 

sum(is.na(completeData$Continent))  # should be 0 
sum(is.na(completeData$Number)) # 25000 now 8895  8912 on 5jul2019
sum(is.na(completeData$Location))
sum(is.na(completeData$Datasource_ID))
sum(is.na(completeData$Stratum)) # looks good
unique(completeData$Stratum)




#step 2 add indicies to the dataset for INLA

completeDataArth <- addIndicies(completeDataArth)


dim(completeData) # old: 129203    new: 129649 # don;t know what's wrong with the old one 
dim(completeDataArth) # old: 129694  new 129694

sum(is.na(completeData$Number)) # 25155
sum(is.na(completeDataArth$Number)) # 25180

sum(is.na(completeData$Location)) # should be 0
sum(is.na(completeDataArth$Location)) # 
sum(is.na(completeData$biome)) # should be 0




# add possible confounding factors: start, end and duration: #####

confounders<- completeData %>% 
  group_by(Plot_ID) %>%
  summarise(
    Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T))
    
completeData<- merge(completeData, confounders, by= "Plot_ID")
completeDataArth<-merge(completeDataArth, confounders, by= "Plot_ID")

# center yrs for INLA
completeData$cStartYear <- completeData$Start_year - median(completeData$Start_year)
completeData$cDuration <- completeData$Duration - median(completeData$Duration)
completeDataArth$cStartYear <- completeDataArth$Start_year - median(completeDataArth$Start_year)
completeDataArth$cDuration <- completeDataArth$Duration - median(completeDataArth$Duration)


save(completeDataArth, file = "completeDataArth.RData")
save(completeData, file = "completeData.RData")



# merge in biome #####

load("completeData.RData")

biomes<- read.csv( "biomesEdited 2019.csv", header = T)
dim(completeData)
dim(completeDataArth) #31 cols

#check all plots are in the biomes file
unique(completeData$Plot_ID)[!unique(completeData$Plot_ID) %in%  unique(biomes$Plot_ID)] # all there
unique(completeDataArth$Plot_ID)[!unique(completeDataArth$Plot_ID) %in%  unique(biomes$Plot_ID)] # all there


#merge with biome data
completeData<- merge (completeData, biomes[, c("Plot_ID", "biome", "BiomeCoarse" )] , by = "Plot_ID")
dim(completeData) # 
completeDataArth<-merge (completeDataArth, biomes[, c("Plot_ID", "biome", "BiomeCoarse" )] , by = "Plot_ID")
dim(completeDataArth)


#set Europe as the reference level
completeData$Continent<- relevel(completeData$Continent, ref = "Europe")

save(completeData,file="completeData.RData") 
save(completeDataArth,file="completeData.RData") 






# protected areas #####
# only in insect data for now 
PA<- read.csv("ProtectedAreasEdited.csv", header = T)

dim(PA)
dim(completeData)
completeData<- merge(completeData, PA[, c("NAME", "ORIG_NAME", "DESIG_ENG", "STATUS_YR", "REP_AREA",  "GIS_AREA", "Plot_ID", "PA")], by = "Plot_ID")
completeDataArth<- merge(completeDataArth, PA[, c("NAME", "ORIG_NAME", "DESIG_ENG", "REP_AREA",  "GIS_AREA", "Plot_ID", "PA")], by = "Plot_ID")

dim(completeData)

save(completeData, file = "completeData.RData")



# land use data  ESA and LUH2 #####


 load("LU.RData") # LUH2

head(LU); tail(LU)

# all there? 
unique(completeData$Plot_ID) [!unique(completeData$Plot_ID) %in% LU$Plot_ID] # looks like it 



completeData <- merge(completeData, LU[, c(2, 19:28)], by = "Plot_ID"); dim(completeData)
completeDataArth <- merge(completeDataArth, LU[, c(2, 19:28)], by = "Plot_ID"); dim(completeDataArth)
head(completeData)

hist(completeData$End_cropArea) # somewhat biased
hist(subset(completeData, Realm == "Terrestrial")$End_urbanArea) # somewhat biased



# ESA CCI data 
load("percCover900m.RData")
 dim(percCover900m)
dim(completeData) ; length(unique(completeData$Plot_ID))

# which are missing? These are all old plots 
unique(completeData$Plot_ID) [!unique(completeData$Plot_ID) %in% percCover900m$Plot_ID]

completeData<- merge(completeData, percCover900m [, c(1,32,33) ], by = "Plot_ID", all.x=T)
completeDataArth<- merge(completeDataArth, percCover900m [, c(1,32,33) ], by = "Plot_ID", all.x=T)
dim(completeData)
sum(is.na(completeData$frcCrop900m) ) 
# 509 obs missing 


save(completeData, file = "completeData.RData")
save(completeDataArth, file = "completeDataArth.RData")



# climate data #####

# CHELSA 
load("CHELSATmeanSlopes.RData")
load("CHELSAPrecSlopes.Rdata")
CHELSA<- merge(CHELSATmeanSlopes[, c(1:3, 8,9) ], CHELSAPrecSlopes[, c(1:2, 7,8) ])
dim(CHELSA)
head(CHELSA)
completeData <- merge(completeData, CHELSA, all.x = T)



#load CRU
load( "CRUtpSlopes.RData")

dim(CRUtpSlopes)

completeData<- merge(completeData, CRUtpSlopes)

save(completeData, file = "completeData.RData")











# metadata for suppl material #####
load( "all.selectedIns.RData") # check version date


metadata_per_dataset<-  all.selectedIns %>% 
  group_by(Datasource_ID) %>%
  summarise(
    Datasource_name = unique(Datasource_name), 
    Place = unique(Country_State),
    Country = unique(Country),
    Taxon = unique(Invertebrate_group), 
    Start = min(Year, na.rm = T),
    End = max(Year, na.rm = T),
    Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
    NUMBER_OF_PLOTS =  length(unique(Plot_ID)), 
    NUMBER_OF_YEARS = length(unique(Year)), 
    mean_lat = mean(Latitude), 
    mean_long =   mean(Longitude), 
    Continent = unique(Continent), 
    Realm = unique(Realm)
    )
dim(metadata_per_dataset) #  should be 155


write.csv(metadata_per_dataset, "metadata per dataset.csv")
save(metadata_per_dataset, file = "metadata_per_dataset.RData")



# metadata per plot

metadata_per_plot<- completeData %>% 
  group_by(Plot_ID) %>%
  summarise(
    #Plot_name = length(unique(Plot_name)), 
    Datasource_ID = unique(Datasource_ID),
    Datasource_name = unique(Datasource_name), 
    Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T),
    Continent = unique(Continent), 
#    Latitude = unique(Latitude),
    Country_State = unique(Country_State),
    Country = unique(Country),
    Realm = unique(Realm),
    Stratum = length(unique(Stratum)),
#Longitude = unique(Longitude),
    NUMBER_OF_PLOTS =  length(unique(Plot_ID)), # should be 1
    NUMBER_OF_SAMPLES = length(unique(paste(Year, Period))),
    NUMBER_OF_YEARS = length(unique(Year)),
    #NUMBER_OF_TAXA = length(unique(Taxon)),
    TOTAL_N = sum(Number, na.rm = T),
    PA = unique(PA)#, 
#PAname = unique(NAME),    
#PAsince = unique(STATUS_YR)
  )
metadata_per_plot <- merge(metadata_per_plot, plots[, c(1, 7,8,9,10, 16,17, 18)]   , by = "Plot_ID")
save(metadata_per_plot, file = "metadata_per_plot.RData")







# select countries

summ<-metadata_per_dataset %>% 
  group_by(Country, Realm) %>%
  summarise( 
  datasets = length(unique(Datasource_ID)), plots = sum(NUMBER_OF_PLOTS))
summ %>% print(n = Inf)




# pesticide data load  # looks unreliable to me ####
pesticideData<-read.csv("FAO pesticides per country.csv", header = T)
head(pesticideData)
cropAreaData<-  read.csv("FAO crop cover per country.csv", header = T)

pesticideData<- subset(pesticideData, Item == "Insecticides")
cropAreaData<-  subset(cropAreaData, Item == "Cropland")

# china has differnet names in the two files  but also only has total pesticide use
pesticideData$Area[pesticideData$Area == "China, mainland"] <- "China"
# merge tem together
full.data<- merge (pesticideData, cropAreaData, by = c("Area", "Area.Code", "Year"), all.x = T) 
dim(full.data)
# calculate pesticide load per 1000ha
full.data$PesticidePer1000ha<- full.data$Value.x / full.data$Value.y

# which countries for which we have data are not in this list? 
pestCountries<- unique(full.data$Area)
countriesInData<-unique(completeData$Country)
countriesInData[! countriesInData %in% pestCountries]

pestCountries[! pestCountries %in% cropCountries]


#Correct country names
completeData$Country2<- as.character(completeData$Country)
completeData$Country2[completeData$Country2 == "Kazachstan"] <- "Kazakhstan"
completeData$Country2[completeData$Country2 == "Vietnam"] <- "Viet Nam"
completeData$Country2[completeData$Country2 == "Czech republic"] <- "Czechia"
completeData$Country2[completeData$Country2 == "Russia"] <- "Russian Federation"
completeData$Country2[completeData$Country2 == "Taiwan"] <- "China, Taiwan Province of"
completeData$Country2[completeData$Country2 == "South Korea"] <- "Republic of Korea"
completeData$Country2[completeData$Country2 == "USA"] <- "United States of America"
completeData$Country2[completeData$Country2 == "West Africa"] <- "Ghana"
completeData$Country2[completeData$Country2 == "China"] <- "China, mainland"

countriesInData<-unique(completeData$Country2) # new list 


test<- subset(full.data, Area %in% countriesInData)
library(ggplot2)
qplot(Year, PesticidePer1000ha, data=test)+
  facet_wrap(~Area, scales="free")



summ.data<- test %>% group_by (Area) %>%
  summarize( mean = mean(PesticidePer1000ha),
  sd = sd(PesticidePer1000ha) )
  
ggplot(summ.data, aes(x=Area, y=mean)) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
  coord_flip()+  geom_line() +  geom_point()  

summ.data<- test %>% group_by (Area) %>%
  summarize( mean = mean(Value.x),
             sd = sd(Value.x) )
ggplot(summ.data, aes(x=Area, y=mean)) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
  coord_flip()+  geom_line() +  geom_point()  



# merge the pesticide data into the completedata

completeDataPest<- merge(completeData, full.data, by.x = "")








#
ggplot(pframe, aes(x = Year)) + 
  geom_histogram( data = subset(pframe, Realm == "Terrestrial"), binwidth = 1, fill="black", alpha = 1) +
  geom_histogram( data = subset(pframe, Realm == "Freshwater"), binwidth = 1, fill="dodgerblue", alpha = 1) +
facet_wrap(~ Realm, ncol = 1)  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),  strip.text = element_blank()) +
  scale_y_continuous(position = "right")+
  ylab ("Number of studies")

sum(metadata_per_plot$Realm == "Terrestrial")/nrow(metadata_per_plot) # 61%
sum(metadata_per_plot$Realm == "Freshwater")/nrow(metadata_per_plot) # 38%

table(metadata_per_plot$Realm)
table(metadata_per_dataset$Realm)
table(metadata_per_plot$PA)
table(metadata_per_dataset$Realm, metadata_per_dataset$Continent)
table(metadata_per_plot$Realm, metadata_per_plot$Continent)
table(metadata_per_plot$PA, metadata_per_plot$Continent)


#
sum(metadata_per_plot$PA == "yes")/nrow(metadata_per_plot) # 34% from PA's current estimates are under 15% of the terrestrial area is protected
sum(metadata_per_plot$PA == "no")






# which countries to test separately? 

load("metadata_per_plot.RData")



metadata_per_country<- metadata_per_plot %>% 
  group_by(Country_State, Realm) %>%
  summarise(
    Country = unique(Country),
    NumberPlots = length(unique(Plot_ID)), 
    NumberStudies = length(unique(Datasource_ID)))

subset(metadata_per_country, NumberStudies >2)


#countrywide datasets:
# Sweden Freshwater 
# new zealand freshwater
# Japan Ground beetles 

# pooled: 
# Germany Terrestrial
# Russia Freshwater  
# Russia Terrestrial 15 
# UK terrestrial 
# USA west FW
# USA West Terr
# USA Midwest terr
# (USA midwest FW)
# USA South FW

# us regions:
             # Terr                #FW
#West:        AZ MT    NM           AZ, CA , CO ID UT
#Midwest       KS  MI  MN   WI          MI WI
#USA_Northeast     MS NH                NY, Ontario, Penn
#USA_south:           GA                KY  AR, GA NC TN

metadata_per_plot$region<- metadata_per_plot$Country
levels(metadata_per_plot$region) <- c(levels(metadata_per_plot$region), "USA West", "USA Midwest", "USA Northeast", "USA South",
                                      "Russia Northwest", "Russia Volga", "Russia Siberia", "Russia Ural", "Russia Central", "Russia Far East", 
                                      "Russia Central & Volga", "Russia Siberia & Ural")
metadata_per_plot$region[metadata_per_plot$Country_State == "Arizona"]<-"USA West"
metadata_per_plot$region[metadata_per_plot$Country_State == "Montana"]<-"USA West"
metadata_per_plot$region[metadata_per_plot$Country_State == "New Mexico"]<-"USA West"
metadata_per_plot$region[metadata_per_plot$Country_State == "California"]<-"USA West"
metadata_per_plot$region[metadata_per_plot$Country_State == "Colorado"]<-"USA West"
metadata_per_plot$region[metadata_per_plot$Country_State == "Idaho"]<-"USA West"
metadata_per_plot$region[metadata_per_plot$Country_State == "Utah"]<-"USA West"
metadata_per_plot$region[metadata_per_plot$Country_State == "Kansas"]<-"USA Midwest"
metadata_per_plot$region[metadata_per_plot$Country_State == "Michigan"]<-"USA Midwest"
metadata_per_plot$region[metadata_per_plot$Country_State == "Minnesota"]<-"USA Midwest"
metadata_per_plot$region[metadata_per_plot$Country_State == "Wisconsin"]<-"USA Midwest"
metadata_per_plot$region[metadata_per_plot$Country_State == "Massachusetts"]<-"USA Northeast"
metadata_per_plot$region[metadata_per_plot$Country_State == "Ontario"]<-"USA Northeast"
metadata_per_plot$region[metadata_per_plot$Country_State == "Pennsylvania"]<-"USA Northeast"
metadata_per_plot$region[metadata_per_plot$Country_State == "New York"]<-"USA Northeast"
metadata_per_plot$region[metadata_per_plot$Country_State == "New Hampshire"]<-"USA Northeast"
metadata_per_plot$region[metadata_per_plot$Country_State == "Georgia"]<-"USA South"
metadata_per_plot$region[metadata_per_plot$Country_State == "Kentucky"]<-"USA South"
metadata_per_plot$region[metadata_per_plot$Country_State == "Arkansas"]<-"USA South"
metadata_per_plot$region[metadata_per_plot$Country_State == "North Carolina"]<-"USA South"
metadata_per_plot$region[metadata_per_plot$Country_State == "Tennessee"]<-"USA South"
metadata_per_plot$region[metadata_per_plot$Country_State == "Georgia"]<-"USA South"

#Regions in Russian Federation: 
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1391]<-"Russia Northwest"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1393]<- "Russia Central & Volga" #   "Russia Volga"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1400]<-"Russia Siberia & Ural"   #    "Russia Siberia"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1401]<-"Russia Central & Volga" #   "Russia Central"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1402]<-"Russia Siberia & Ural"   #    "Russia Siberia"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1403]<-"Russia Siberia & Ural"   #    "Russia Ural"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1406]<-"Russia Central & Volga" #   "Russia Volga"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1407]<-"Russia Central & Volga" #   "Russia Central"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1446]<-"Russia Central & Volga" #   "Russia Central"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1448]<-"Russia Siberia & Ural"   #    "Russia Ural"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1449]<-"Russia Siberia & Ural"   #    "Russia Northwest"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1451]<-"Russia Central & Volga" #   "Russia Volga"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1452]<-"Russia Central & Volga" #   "Russia Volga"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1453]<-"Russia Northwest"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1454]<-"Russia Northwest"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1455]<-"Russia Siberia & Ural"   #    "Russia Ural"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1456]<-"Russia Central & Volga" #   "Russia Volga"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1457]<-"Russia Central & Volga" #   "Russia Volga"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1458]<-"Russia Northwest"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1459]<-"Russia Central & Volga" #   "Russia Central"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1460]<-"Russia Northwest"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1461]<-"Russia Far East"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1462]<-"Russia Far East"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1507]<-"Russia Central & Volga" #   "Russia Volga"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1508]<-"Russia Northwest"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1509]<-"Russia Northwest"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1510]<-"Russia Northwest"
metadata_per_plot$region[metadata_per_plot$Datasource_ID == 1511]<-"Russia Siberia & Ural"   #    "Russia Siberia"












metadata_per_country<- metadata_per_plot %>% 
  group_by(region, Realm) %>%
  summarise(
    Country = unique(Country),
    NumberPlots = length(unique(Plot_ID)), 
    NumberStudies = length(unique(Datasource_ID)))

print(subset(metadata_per_country, NumberStudies >3 | NumberPlots >20), n = Inf)

























#thrash

#require(plyr)    OBSOLETE
completeData <- ddply(all.aggr.insects,.(Realm,Continent,Datasource_ID),
                      function(myData){
                        #expand grid to include NAs
                        constantData <- unique(myData[,c("Plot_ID","Datasource_ID")])#these are defo unique
                        allgrid <- expand.grid(Plot_ID = unique(myData$Plot_ID),
                                               Year= min(myData$Year):max(myData$Year))
                        allgrid <- merge(allgrid,constantData,by=c("Plot_ID"),all.x=T)
                        
                        #add observed data
                        myData1 <- merge(allgrid,myData[,c("Year","Plot_ID", "Period", "Number")],  #"classes",
                                         by=c("Year","Plot_ID"),all=T)
                        # add descriptors
                        myData <- merge(myData1, unique(myData[,c("Plot_ID",  "Location", "Datasource_name", "Country", "Country_State", "Region", "Stratum" )]),
                                        by="Plot_ID",all=T)
                        
                        #fit in missing values for period with random sample
                        if(!all(is.na(myData$Period))){
                          myData$Period[is.na(myData$Period)]<-sample(myData$Period[!is.na(myData$Period)],
                                                                      length(myData$Period[is.na(myData$Period)]),
                                                                      replace=T)
                        }
                        
                        #return dataset
                        return(myData)
                        
                      })
