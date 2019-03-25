

 setwd("C:\\Dropbox\\Dropbox\\Insect Biomass Trends/csvs") # work
 biomes<-read.table("Biomes 20181126.txt", header = T)
 #remove old luquillo data with wrong coordinates 
 biomes <- subset(biomes, Datasource_ID != 1485  )
 
 biomes2<- read.table("Biomes2.txt", header = T)
 dim(biomes)
 dim(biomes2)
 head(biomes)
head(biomes2)
unique(biomes2$Datasource_name)

# remove duplicate values from first df (these are updated in df2)
biomes<- (  biomes[!biomes$Plot_ID %in% biomes2$Plot_ID , ] ) #$ only plot_IDs that are not in df2


all.biom<- rbind(biomes, biomes2[,-3])
sort(unique(biomes$Datasource_ID))

#duplicates?
sum(duplicated(all.biom$Plot_ID))


setdiff(unique(all.aggr.insects$Datasource_ID), all.biom$Datasource_ID)






 # finland fw is missing , add manually for now 
 fnl<- data.frame(X = NA, 
                 Plot_ID = seq.int (156,178),    Datasource_ID = 1408,
                 Datasource_name = "Finland freshwater",
                 Duration = 14,           Start_year = 2000,     End_year = 2013,      Latitude = 66.5,  Longitude = 29,    NUMBER_OF_PLOTS = NA, 
                 NUMBER_OF_SAMPLES = 14,           NUMBER_OF_YEARS = 14,      TOTAL_N = NA,      biome = "Boreal Forests/Taiga",    optional = TRUE)
 nl<- data.frame(X = NA, 
                  Plot_ID = seq.int (1190,1193),    Datasource_ID = 1324,
                  Datasource_name = "Netherlands beetles spiders Lauwersmeer",
                  Duration = 40,     Start_year = 1969,     End_year = 2008,      Latitude = 53.350465	,  Longitude = 6.186429,    NUMBER_OF_PLOTS = 4, 
                  NUMBER_OF_SAMPLES = 39,           NUMBER_OF_YEARS = 39,      TOTAL_N = NA,      biome = "Temperate Broadleaf & Mixed Forests",    optional = TRUE)
 

 
 
 biomes<- rbind(all.biom, fnl, nl)
 dim(biomes)
 
biomes[duplicated(biomes$Plot_ID),] 
 
 
 
 
table(biomes$biome)

# check a few things

#check NA's
subset(biomes, is.na(biome))



subset(biomes, biome == "Lakes")
subset(biomes, biome == "Montane Grasslands & Shrublands" ) # looks ok
subset(biomes, biome == "Mediterranean Forests, Woodlands & Scrub" ) # looks ok
unique(subset(biomes, biome == "Tundra")$Datasource_name) # looks good
unique(subset(biomes, biome == "Deserts & Xeric Shrublands")$Datasource_name) # looks good
unique(subset(biomes, biome == "Temperate Conifer Forests")$Datasource_name) # contains a lot of alpine stuff as well 
unique(subset(biomes, biome == "Boreal Forests/Taiga")$Datasource_name) # contains a lot of alpine stuff as well 



# reclassify: temperate , tropical, boreal

biomes$BiomeCoarse<- NA 



# whch datasets are classified how? 
biomes$BiomeCoarse[biomes$biome== "Boreal Forests/Taiga"] <- "Boreal/Alpine"                                    
biomes$BiomeCoarse[biomes$biome== "Tundra"] <- "Boreal/Alpine"                                    
biomes$BiomeCoarse[biomes$biome== "Montane Grasslands & Shrublands"] <- "Boreal/Alpine"                                    


#Drylands
biomes$BiomeCoarse[biomes$biome== "Mediterranean Forests, Woodlands & Scrub"]    <- "Drylands"                   
biomes$BiomeCoarse[biomes$biome== "Deserts & Xeric Shrublands"]         <- "Drylands"                                    


#temperate
biomes$BiomeCoarse[biomes$biome== "Temperate Broadleaf & Mixed Forests"]         <- "Temperate"                                    
biomes$BiomeCoarse[biomes$biome== "Temperate Conifer Forests"]                   <- "Temperate"                                    
biomes$BiomeCoarse[biomes$biome== "Temperate Grasslands, Savannas & Shrublands"] <- "Temperate"                                    

#tropical
biomes$BiomeCoarse[biomes$biome== "Tropical & Subtropical Dry Broadleaf Forests"]                   <- "Tropical"       
biomes$BiomeCoarse[biomes$biome== "Tropical & Subtropical Grasslands, Savannas & Shrublands"]       <- "Tropical"       
biomes$BiomeCoarse[biomes$biome== "Tropical & Subtropical Moist Broadleaf Forests" ]                <- "Tropical"       


# fix some msising data and lakes
"Lakes"           # this is a mix of things                                         
subset(biomes, biome == "Lakes")
sample_n(subset(biomes, Datasource_name == "Sweden freshwater"), 10)

biomes$BiomeCoarse[biomes$biome== "Lakes" & biomes$Datasource_name == "US butterflies Swengel" ]         <- "Temperate"       
biomes$BiomeCoarse[biomes$biome== "Lakes" & biomes$Datasource_name == "Wisconsin Mayflies" ]             <- "Temperate"       
biomes$BiomeCoarse[biomes$biome== "Lakes" & biomes$Datasource_name == "Pennsylvania blackflies" ]        <- "Temperate"       
biomes$BiomeCoarse[biomes$biome== "Lakes" & biomes$Datasource_name == "Kazachstan freshwater" ]          <- "Temperate"       
biomes$BiomeCoarse[biomes$biome== "Lakes" & biomes$Datasource_name == "Lake Ontario Freshwater" ]          <- "Temperate"       
biomes$BiomeCoarse[biomes$biome== "Lakes" & biomes$Datasource_name == "Winnipeg freshwater" ]          <- "Boreal/Alpine"       
biomes$BiomeCoarse[biomes$biome== "Lakes" & biomes$Datasource_name == "Sweden freshwater" ]          <- "Temperate"       
# all large lakes in sweden are south of the line Gaevle - Frederikstad (NO)

biomes$biome[biomes$biome== "Lakes" & biomes$Datasource_name == "US butterflies Swengel" ]         <- "Temperate Broadleaf & Mixed Forests"       
biomes$biome[biomes$biome== "Lakes" & biomes$Datasource_name == "Wisconsin Mayflies" ]             <- "Temperate Broadleaf & Mixed Forests" # not completely sure       
biomes$biome[biomes$biome== "Lakes" & biomes$Datasource_name == "Pennsylvania blackflies" ]        <- "Temperate Broadleaf & Mixed Forests"       
biomes$biome[biomes$biome== "Lakes" & biomes$Datasource_name == "Kazachstan freshwater" ]          <- "Deserts & Xeric Shrublands"       
biomes$biome[biomes$biome== "Lakes" & biomes$Datasource_name == "Lake Ontario Freshwater" ]          <- "Temperate Broadleaf & Mixed Forests"       
biomes$biome[biomes$biome== "Lakes" & biomes$Datasource_name == "Winnipeg freshwater" ]          <- "Boreal Forests/Taiga"       
biomes$biome[biomes$biome== "Lakes" & biomes$Datasource_name == "Sweden freshwater" ]          <- "Temperate Broadleaf & Mixed Forests" # not completely sure        






subset(biomes, is.na(biomes$biome))# several have no NA - all coastal
biomes$BiomeCoarse[is.na(biomes$biome) & biomes$Datasource_name == "New Zealand freshwater monitoring" ] <- "Temperate"       
biomes$BiomeCoarse[is.na(biomes$biome) & biomes$Datasource_name == "Sweden freshwater" ] <- "Temperate"       
biomes$BiomeCoarse[is.na(biomes$biome) & biomes$Datasource_name == "UK suction biomass" ] <- "Temperate"       
biomes$BiomeCoarse[is.na(biomes$biome) & biomes$Datasource_name == "LTER Georgia coast" ] <- "Temperate"       
biomes$BiomeCoarse[is.na(biomes$biome) & biomes$Datasource_name == "Luquillo resampling" ] <- "Tropical"       
biomes$BiomeCoarse[is.na(biomes$biome) & biomes$Datasource_name == "Ireland butterflies" ] <- "Temperate"       
biomes$BiomeCoarse[is.na(biomes$biome) & biomes$Datasource_name == "Finland oak herbivores" ] <- "Temperate"       
biomes$BiomeCoarse[is.na(biomes$biome) & biomes$Datasource_name == "Parque Dunar" ] <- "Drylands"       

# original classification
biomes$biome[is.na(biomes$biome) & biomes$Datasource_name == "New Zealand freshwater monitoring" ] <- "Temperate Broadleaf & Mixed Forests" # is a guess.      
biomes$biome[is.na(biomes$biome) & biomes$Datasource_name == "Sweden freshwater" ]                 <- "Temperate Broadleaf & Mixed Forests" # south of stockholm at the coast       
biomes$biome[is.na(biomes$biome) & biomes$Datasource_name == "UK suction biomass" ]                <- "Temperate Broadleaf & Mixed Forests"       
biomes$biome[is.na(biomes$biome) & biomes$Datasource_name == "LTER Georgia coast" ] <- "Temperate Conifer Forests"   # temperate grassland would be more appropriate
biomes$biome[is.na(biomes$biome) & biomes$Datasource_name == "Luquillo resampling" ] <- "Tropical & Subtropical Moist Broadleaf Forests"       
biomes$biome[is.na(biomes$biome) & biomes$Datasource_name == "Ireland butterflies" ] <- "Temperate Broadleaf & Mixed Forests"       
biomes$biome[is.na(biomes$biome) & biomes$Datasource_name == "Finland oak herbivores" ] <- "Temperate Broadleaf & Mixed Forests"       
biomes$biome[is.na(biomes$biome) & biomes$Datasource_name == "Parque Dunar" ] <- "Mediterranean Forests, Woodlands & Scrub"       






sum(is.na(biomes$biome))

write.csv(biomes, "biomesEdited 2019.csv")




# broader classification
unique(biomes$classes)
biomes$classes2<- biomes$classes

biomes$classes2[biomes$classes2 == "Drylands"]<- "Temperate"


# even coarser
biomes$classes3<- NA
biomes$classes3[biomes$Latitude>66.5]<- "Polar"
biomes$classes3[biomes$Latitude<66.5 & biomes$Latitude> -66.5]<- "Temperate"
biomes$classes3[biomes$Latitude<30 & biomes$Latitude> -30]<- "Tropical"

write.table(biomes , "biomes edited.txt")


