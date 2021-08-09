#This script is to develop a method to test the correlations in trends of taxa at the same sites

#From Roel:
# Toy data for the taxon comparisons are also in the same folder now. The file is called ‘Toy data taxon comparisons 20210806.rds’ 
# You should use the column ‘Order’ to select which taxa you want to compare. 
# Dataset 1429 looks like a decent one to start with..
# I still need to add zeroes to the years where one taxon was absent, but that shouldn’t really affect your try-outs. 

#libraries
library(tidyverse)  

#get species data
mydata <- readRDS("C:/Users/db40fysa/Dropbox/Insect Biomass Trends/csvs/Toy data taxon comparisons 20210806.rds")

#select columns we need
mydata_select <- mydata %>%
  select(c(Datasource_ID,Plot_ID,Year,Order,Number)) %>%
  filter(!is.na(Order)) %>%
  filter(Order!="")%>%
  filter(!is.na(Number))

#identify rarely sampled order
rareOrder <- mydata_select %>%
  group_by(Order) %>%
  summarise(nuDatasets = length(unique(Datasource_ID))) %>%
  filter(nuDatasets==1)

#aggregate (across species) to order and remove rarely sampled orders
mydata_aggregated <- mydata_select %>%
                      filter(!Order %in% rareOrder$Order) %>%
                      group_by(Datasource_ID,Plot_ID,Year,Order) %>%
                      summarise(Number = sum(Number)) 

#check number of times each order co-occurs within the same dataset
V <- crossprod(table(mydata_aggregated[,c("Datasource_ID","Order")]))
diag(V) <- 0
V_long <- V %>%
          as_tibble() %>%
          add_column(Base = row.names(V)) %>%
          pivot_longer(!Base) %>%
          rename(Order = name) %>%
          mutate(log_value = log(value+1))

#plot co-occurrence (need to shade out upper half since it is repeated)
ggplot(V_long)+
  geom_tile(aes(x=Base,y=Order,fill=log_value))+
  scale_fill_viridis_c()+
  theme(axis.text.x = element_text(angle=90))


#expand orders into different columns
mydata_wide <- mydata_aggregated %>%
  pivot_wider(.,names_from="Order",
              values_from="Number")

#choose 2 groups to compare
ggplot(mydata_wide)+
  geom_line(aes(x=Year, y = Diptera, group=Plot_ID),color="red")+
  geom_line(aes(x=Year, y = Ephemeroptera, group=Plot_ID),color="blue")+
  facet_wrap(~Datasource_ID,scales="free")

ggplot(mydata_wide)+
  geom_line(aes(x=Year, y = Trichoptera, group=Plot_ID),color="red")+
  geom_line(aes(x=Year, y = Ephemeroptera, group=Plot_ID),color="blue")+
  facet_wrap(~Datasource_ID,scales="free")


#get simple correlations at the plot-level
spCors <- mydata_wide %>%
          group_by(Datasource_ID,Plot_ID) %>%
          summarise(CorrelationTE = cor(Trichoptera,Ephemeroptera),
                    CorrelationDE = cor(Diptera,Ephemeroptera),
                    CorrelationDT = cor(Diptera,Trichoptera))

par(mfrow=c(3,1))
hist(spCors$CorrelationTE)
hist(spCors$CorrelationDE)
hist(spCors$CorrelationDT)

#fit multivariate model
#https://cran.r-project.org/web/packages/brms/vignettes/brms_multivariate.html
library(brms)

mydata_taxasubset <-mydata_wide %>%
  filter(.,!is.na(Trichoptera) & !is.na(Ephemeroptera)) %>%
  mutate(log_T = log10(Trichoptera+1), log_E = log10(Ephemeroptera+1))  

#correlated plot-level intercepts
fit1 <- brm(
  mvbind(log_T, log_E) ~ Year + (1|p|Plot_ID),
  data = mydata_taxasubset, 
  chains = 3)
summary(fit1)

#correlated plot-level intercepts and slopes
fit1 <- brm(
  mvbind(log_T, log_E) ~ Year + (1 + Year|p|Plot_ID),
  data = mydata_taxasubset, 
  chains = 3)
summary(fit1)

#or slightly simpler
fit1 <- brm(
  mvbind(log_T, log_E) ~ Year + (1|p|Plot_ID) + (0 + Year|q|Plot_ID) ,
  data = mydata_taxasubset, 
  chains = 3)
