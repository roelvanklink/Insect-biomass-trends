# SAD_analysis
# From Roel:
# The file is called ‘Density values temp 20210805rds’  and is located in the dropbox under: Dropbox\Insect Biomass Trends\csvs
# 
# There are 4 types of densities: 
#   1)	Raw: densityMode = relative & correction = F 
#   2)	Corrected raw: added the part of the curve below 0 to the positive side of the curve:   densityMode = relative & correction = F
#   3)	Absolute raw: like Raw, but multiplied by the number of species. This is however strongly correlated to the richness trend, so in my opinion rather useless:   densityMode = absolute & correction = F
#   4)	Absolute corrected: Raw multiplied by the number of species + corrected for part below 0. Probably also highly correlated with  richness trend: densityMode = absolute & correction = F
#   
#   I’d start with the raw type, as it makes most sense to me. 
  
#libraries
library(tidyverse)
library(lme4)

#data - check this is the right one
mydata <- readRDS("C:/Users/db40fysa/Dropbox/Insect Biomass Trends/csvs/Toy data Density values 20210805.rds")

#pivot data set
mydata_long <- mydata %>%
                select(!X) %>%
                filter(densityMode == "relative" & correction == FALSE) %>%
                pivot_longer(cols = starts_with("p", ignore.case = FALSE),
                              names_to = "percentile",
                              values_to  ="value") %>%
                mutate(percentile = parse_number(percentile)) %>%
                as.data.frame()
              

#plot a couple of examples
ggplot(subset(mydata_long, Plot_ID == 117))+
  geom_line(aes(x = percentile, y = value, group = Year, color = Year), size=1.5)+
  scale_color_viridis_c()+
  theme_classic()

ggplot(subset(mydata_long, Plot_ID == 2087))+
  geom_line(aes(x = percentile, y = value, group=Year, color = Year), size=1.5)+
  scale_color_viridis_c()+
  theme_classic()

#look at the distribution of the data
summary(mydata_long$value)
hist(mydata_long$value)
hist(log10(mydata_long$value+0.1))
hist(log(mydata_long$value+0.1))

#how do we have examples above zero??
mydata_long_above0 <- subset(mydata_long, value>1)
head(mydata_long_above0)
length(unique(mydata_long_above0$Plot_ID))#237!!

#look at couple of examples when the density goes above zero
ggplot(subset(mydata_long, Plot_ID == 905))+
  geom_line(aes(x = percentile, y = value, group=Year, color = Year), size=1.5)+
  scale_color_viridis_c()+
  theme_classic()

ggplot(subset(mydata_long, Plot_ID == 2397))+
  geom_line(aes(x = percentile, y = value, group=Year, color = Year), size=1.5)+
  scale_color_viridis_c()+
  theme_classic()
#seems to be in cases when distribution was v skewed

#quick and dirty analysis

#trend at each percentile and data set
allTrends <- mydata_long %>% 
  group_by(percentile, Plot_ID) %>%
  nest_by() %>% 
  mutate(model = list(lm(log10(value+0.1) ~ Year, data = data))) %>% 
  summarise(broom::tidy(model)) %>%
  unnest(c(percentile, Plot_ID)) %>%
  filter(.,term == "Year")

ggplot(allTrends) +
  geom_boxplot(aes(x=as.factor(percentile),y=estimate))

ggplot(allTrends) +
  geom_boxplot(aes(x=as.factor(percentile),y=estimate),outlier.color = NA)+
  ylim(-0.03,0.03)+
  geom_hline(yintercept = 0, colour = "red")+
  theme_classic()

#using lmer
allTrends <- mydata_long %>% 
  group_by(percentile) %>%
  nest_by() %>% 
  mutate(model = list(lmer(log10(value+0.1) ~ Year + (1+Year|Plot_ID), data = data))) %>% 
  summarise(data.frame(t(summary(model)$coef[2,]))) %>%
  unnest(percentile) %>%
  ungroup() %>%
  mutate(upper = Estimate + (Std..Error*2),
         lower = Estimate - (Std..Error*2))
         
ggplot(allTrends) +
  geom_path(aes(x=percentile,y=Estimate),color = "black")+
  geom_path(aes(x=percentile,y=upper),color = "black", linetype="dashed")+
  geom_path(aes(x=percentile,y=lower),color = "black", linetype="dashed")+
  geom_hline(yintercept = 0, colour = "red")+
  theme_classic()+
  ylab("Trend")+xlab("SAD percentile")

  
#or plotting SAD at each time point
allTrends <- mydata_long %>% 
  group_by(percentile) %>%
  nest_by() %>% 
  mutate(model = list(lmer(log10(value+0.1) ~ -1 + factor(Year) + (1|Plot_ID), data = data))) %>% 
  summarise(data.frame(summary(model)$coef)) %>%
  unnest(percentile) %>%
  ungroup() %>%
  mutate(Estimate = (10^Estimate)-0.1) %>%
  add_column(Year = rep(sort(unique(mydata_long$Year)),length(unique(mydata_long$percentile))))


ggplot(subset(allTrends,Year>=1980)) +
  geom_path(aes(x=percentile,y=Estimate,group = Year, color = Year),size=1.5)+
  scale_color_viridis_c(option='A')+
  theme_classic()+
  xlab("SAD percentile")+ylab("Density")

