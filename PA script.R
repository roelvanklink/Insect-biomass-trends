# prep data of protected areas
setwd("C:/Users/roelv/Dropbox/Insect Biomass Trends/csvs") # home
setwd("C:\\Dropbox\\Dropbox\\Insect Biomass Trends/csvs") # work

PA<- read.table("ProtectedAreas.txt", header = T)
tail(PA)

# assign the final variable that will go into analyses
PA$PA<- "yes"
PA$PA[is.na(PA$ORIG_NAME)]<- "no"

sum(PA$PA == "yes")
sum(PA$PA == "no")

#manual corrections
PA$PA[PA$Plot_ID == 1719]<- "yes" # donana  ES
PA$PA[PA$Plot_ID == 632]<- "yes" # virginia coast  US
PA$PA[PA$Plot_ID == 155]<- "yes" # Aggtelek National Park HU
PA$PA[PA$Plot_ID >155 & PA$Plot_ID < 180]<- "yes" # Oulanka National Park FI


#Add area in km2 for these missing areas 

PA$REP_AREA[PA$Plot_ID == 1719]<- PA$REP_AREA[PA$Plot_ID == 1717] # donana  ES
PA$REP_AREA[PA$Plot_ID == 632]<- PA$REP_AREA[PA$Plot_ID == 633] # virginia coast  US
PA$REP_AREA[PA$Plot_ID == 155]<- 198.92 # Aggtelek National Park HU
PA$REP_AREA[PA$Plot_ID >155 & PA$Plot_ID < 180]<- 270 # Oulanka National Park FI




write.csv(PA, "ProtectedAreasEdited.csv")

