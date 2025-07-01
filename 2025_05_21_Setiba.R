#January 02, 2025

#################################################
#R Scrip made by Patrick Derviche and Rodrigo Ferreira Bastos
#################################################

#################################################################################### 
#Two-decade after the first assessment of the reef fish and benthos assemblages of 
#the Guarapari Islands (Southwestern Atlantic): Temporal changes and present diversity differences between islands.

#Patrick Derviche, Rodrigo Ferreira Bastos, João Luiz Gasparini, 
#Alexandre Aschenbrenner, Francielly Furlani, Marcelo Soeth, Ronaldo Ruy Oliveira-Filho, 
#Ivan Costa Santos, Sandra Ribeiro, Maurício Hostim-Silva
#################################################################################### 


#################################################
#1. Balancing N samples
#################################################

###############
#1.1 Rarefaction to define the smallest sampling effort 
###############

#packages
library("iNEXT")
library(ggplot2)
library(gridExtra)
library(grid)
library(ggpubr)
library(dplyr)
library(tidyr)

#Clean R environment 
rm(list = ls())

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Data analysis")

#Read dataset
data_2023_fish <-read.csv2("data_2023_fish_abun_per_trans.csv",head = TRUE)
data_2023_fish <- data_2023_fish[,c(1,3,16:134)]
head(data_2023_fish)

#Separate datasets 
data_tre <- data_2023_fish %>% filter(Island == "TRE")
data_esc <- data_2023_fish %>% filter(Island == "ESC")
data_ras <- data_2023_fish %>% filter(Island == "RAS")

###
#Columns to rows
###

#TRE

data_tre <- data_tre %>%
  pivot_longer(
    cols = -c(Island, ID),
    names_to = "Species",  
    values_to = "Abundance")
head(data_tre)

#Pivot wider using 'ID' to create multiple columns
data_tre <- data_tre %>%
  pivot_wider(
    names_from = ID,  
    values_from = Abundance )
data_tre <- as.data.frame(data_tre)
head(data_tre)

#ESC

data_esc <- data_esc %>%
  pivot_longer(
    cols = -c(Island, ID),
    names_to = "Species",  
    values_to = "Abundance")
head(data_esc)

#Pivot wider using 'ID' to create multiple columns
data_esc <- data_esc %>%
  pivot_wider(
    names_from = ID,  
    values_from = Abundance )
data_esc <- as.data.frame(data_esc)
head(data_esc)

#RAS

data_ras <- data_ras %>%
  pivot_longer(
    cols = -c(Island, ID),
    names_to = "Species",  
    values_to = "Abundance")
head(data_ras)

#Pivot wider using 'ID' to create multiple columns
data_ras <- data_ras %>%
  pivot_wider(
    names_from = ID,  
    values_from = Abundance )
data_ras <- as.data.frame(data_ras)
head(data_ras)

###
#Matrix
###

tre_matrix <- as.matrix(apply(data_tre[,-c(1,2)],2,as.integer))
row.names(tre_matrix) <- data_tre[,1]

esc_matrix <- as.matrix(apply(data_esc[,-c(1,2)],2,as.integer))
row.names(esc_matrix) <- data_esc[,1]

ras_matrix <- as.matrix(apply(data_ras[,-c(1,2)],2,as.integer))
row.names(ras_matrix) <- data_ras[,1]

#list
setiba = list(ESC = esc_matrix, RAS = ras_matrix, TRE = tre_matrix)
show(setiba)

colours <- c("#440154", "#21918C","#D8B100")

######
# Rarefaction per unit
######
fig_unit <- iNEXT(setiba, datatype = "incidence_raw")
fig_unit
ggiNEXT(fig_unit)  +
  theme_bw(base_size = 20) + 
  labs(x="Number of UVCs", y="Species richness") +
  theme(legend.title=element_blank(),
        text=element_text(size=20))  +
  scale_shape_manual(values=c(3,3,3))+
  scale_fill_manual(values=colours) +
  scale_colour_manual (values=colours) +
  scale_linetype_manual(values=c(1,3)) +
  theme(legend.position="None")+
  ylim(0, 125)

fig_unit$iNextEst$size_based
options(max.print = 1000000)

#600 x 550

#Using a sample coverage (SC) threshold of ≥ 95%, the minimum sampling effort required 
#was estimated at 41 samples for TRE, 37 for ESC, and 35 for RAS

######
# Rarefaction per individual
######

head(data_2023_fish)

# Transform the dataset to long format
data_individuals <- data_2023_fish %>%
  pivot_longer(
    cols = -c(ID, Island), 
    names_to = "Species",  
    values_to = "Abundance")

# Summarize by calculating the sum of each species per island
data_individuals <- data_individuals %>%
  group_by(Species, Island) %>%
  summarize(Total_Abundance = sum(Abundance), .groups = "drop")

# Pivot back to a wide format with columns for each Island
individuals <- data_individuals %>%
  pivot_wider(
    names_from = Island,         
    values_from = Total_Abundance)

individuals <- as.data.frame(individuals)
head(individuals)

individuals$Species<-NULL

fig_ind <- iNEXT(individuals, datatype="abundance")

ggiNEXT(fig_ind, type=1) +
  theme_bw(base_size = 20) + 
  labs(x="Number of individuals", y="Species richness") +
  theme(legend.title=element_blank(),
        text=element_text(size=20))  +
  scale_shape_manual(values=c(3,3,3))+
  scale_fill_manual(values=colours) +
  scale_colour_manual (values=colours) +
  scale_linetype_manual(values=c(1,3)) +
  theme(legend.position="None")+
  ylim(0, 125)

#600 x 550








###############
#1.2 Stats (complete dataset n = 424)
###############

#Clean R environment 
rm(list = ls())

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Data analysis")

#Read dataset
data_2023_fish <-read.csv2("data_2023_fish_abun_per_trans.csv",head = TRUE)
head(data_2023_fish)

#As numeric
data_2023_fish <- mutate_at(data_2023_fish, vars(c(ABUSAX, ACABAH, ACACHI, ACACOE, ACAPOL, ACAQUA, AHLEGM, ALPAFE, 
                               AMBPIN, ANISUR, ANIVIR, AULSTR, AZUMUL, BALVET, BODPUL, BODRUF, CALPEN, CANFIG, CANMAC, CANPUL, CARBAR, CARCRY, CARLAT, 
                               CARRUB, CEPFUL, CHAFAB, CHASTR, CHASED, CHIRET, CHISPI, CHRJUB, CLEBRA, CORSPP, CORGLA, CORTHR, CRYROS, CTESAE, 
                               DACVOL, DERINE, DIPARG, DIPRAD, DIOHOL, DIOHYS, DULAUR, ELAFIG, FISSPP, GOBKAL, GRABRA, GYMFUN, GYMMIL, GYMMOR, GYMVIC, HAEAUR, 
                               HAEATL, HAEPAR, HAEPLU, HALBRA, HALDIM, HALPEN, HALPOE, HALSPP, HETCRU, HOLADS, HOLCIL, HOLTRI, HYPFIS, KYPSPP, LABCRI, 
                               LABNUC, LUTALE, LUTJOC, MALDEL, MALMAC, MALZAL, MUGSPP, MICCHR, MYRJAC, OCYCHR, ODODEN, OPHTRI, OPIAUR, ORTRUB, PAGPAG, 
                               PARFUR, PARLIN, PARMAR, PARMOR, PARPIL, PEMSCH, PSEPER, POMARC, POMPAR, PRIARE, PSEMAC, PTERAN, RYPSAP, SCATRI, 
                               SCAZEL, SCOBRA, SCOIST, SCOPLU, SELSET, SERBAL, SERFLA, SPASPP, SPAAMP, SPAAXI, SPAFRO, SPATUI, SPHSPE, SPHSPP, STEFUS,
                               Temperature, Visibility)), as.numeric)

#####
#Temperature and visbility
#####

summary(data_2023_fish)
sd(data_2023_fish$Temperature, na.rm = TRUE)
sd(data_2023_fish$Visibility, na.rm = TRUE)

#The water temperature averaged 22.7 ± 1.2 °C (mean ± SD), with values ranging from 19 to 25 °C,
#while water visibility had a mean of 6.6 ± 3.8 m during our UVCs samplings


#####
#Number of individuals
#####

data_2023_fish_abundance <- data_2023_fish[,c(1,3,16:134)]
data_2023_fish_abundance
data_2023_fish_abundance <- data_2023_fish_abundance %>%
  pivot_longer(
    cols = -c(ID, Island), 
    names_to = "Species",  
    values_to = "Abundance")
data_2023_fish_abundance <- as.data.frame(data_2023_fish_abundance)
data_2023_fish_abundance <- data_2023_fish_abundance %>%
  filter(Abundance != 0)

data_2023_fish_abundance <- data_2023_fish_abundance %>%
  group_by(Species) %>%
  summarise(Abundance = sum(Abundance, na.rm = TRUE))

data_2023_fish_abundance <- as.data.frame(data_2023_fish_abundance)
head(data_2023_fish_abundance)


#Number of individuals
sum(data_2023_fish_abundance$Abundance) 
#23250 individuals

#Add info to the data
info <- read.csv("data_2023_fish_info.csv", header= TRUE, sep=";", dec=".")
head(info)

data_2023_fish_abundance <- left_join(data_2023_fish_abundance, info %>%
                                           select(Species, Family, Genus, spp), by = "Species")

data_2023_fish_abundance <- data_2023_fish_abundance %>%
  arrange(Family, Species)

#Number of taxa
head(data_2023_fish_abundance)
#118 taxa

#Number of species
length((unique(data_2023_fish_abundance$Species)))-sum(is.na(data_2023_fish_abundance$spp))
#111 species

#Number of families
length((unique(data_2023_fish_abundance$Family)))
#42 families

#We recorded 23,250 fish individuals, belonging to 111 species identified, 
#within 119 taxa, and 42 families across TRE, ESC, and RAS islands 





###
#Fish species (taxa) richness recorded in each island
###

data_2023_fish_abundance <- data_2023_fish[,c(1,3,16:134)]
data_2023_fish_abundance
data_2023_fish_abundance <- data_2023_fish_abundance %>%
  pivot_longer(
    cols = -c(ID, Island), 
    names_to = "Species",  
    values_to = "Abundance")
data_2023_fish_abundance <- as.data.frame(data_2023_fish_abundance)
data_2023_fish_abundance <- data_2023_fish_abundance %>%
  filter(Abundance != 0)

data_2023_fish_abundance <- data_2023_fish_abundance %>%
  group_by(Island, Species) %>%
  summarise(Abundance = sum(Abundance, na.rm = TRUE))

data_2023_fish_abundance <- as.data.frame(data_2023_fish_abundance)
head(data_2023_fish_abundance)

# Summarize species richness for each Island
data_2023_fish_abundance %>%
  group_by(Island) %>%
  summarise(Number_of_Species = n_distinct(Species))

#Island Number_of_Species
#1 ESC  90
#2 RAS  95
#3 TRE  86

#Fish species (taxa) richness recorded in TRE, ESC, and RAS was 86, 90, and 95, respectively

###
#Fish species (taxa) richness recorded per UVC
###


data_2023_fish_abundance <- data_2023_fish[,c(1,3,16:134)]
data_2023_fish_abundance
data_2023_fish_abundance <- data_2023_fish_abundance %>%
  pivot_longer(
    cols = -c(ID, Island), 
    names_to = "Species",  
    values_to = "Abundance")
data_2023_fish_abundance <- as.data.frame(data_2023_fish_abundance)
data_2023_fish_abundance <- data_2023_fish_abundance %>%
  filter(Abundance != 0)

data_2023_fish_abundance <- data_2023_fish_abundance %>%
  mutate(Abundance = ifelse(Abundance > 1, 1, Abundance))
data_2023_fish_abundance <- as.data.frame(data_2023_fish_abundance)
head(data_2023_fish_abundance)

# Sum the abundance for each ID
abundance_sum <- data_2023_fish_abundance %>%
  group_by(ID, Island) %>%
  summarise(Abundance = sum(Abundance))

abundance_sum <- as.data.frame(abundance_sum)
head(abundance_sum)

# Summarize species richness per transect for each Island
abundance_sum %>%
  group_by(Island) %>%
  summarise(Mean = mean(Abundance),
            SD = sd(Abundance),
            max = max(Abundance),
            min = min(Abundance))

#Island  Mean    SD   max   min
#1 ESC    11.9   4.19    23     3
#2 RAS    13.7   5.83    29     1
#3 TRE     9.47  3.39    19     

#In TRE, the species richness per census (mean ± SD) was 9.47 ± 3.39 species, 
#with a minimum of 2 and a maximum of 19 species. 
#In ESC, the mean species richness per census was 11.90 ± 4.19, with a minimum 
#of 3 and a maximum of 23 species. 
#In the RAS, the mean species richness per census was 13.70 ± 5.83, with a minimum of 1 and a maximum of 29 species. 


###
#Regional extinction threat 
###

head(info)
table(unique(info$INMA))

# Summarize the number of rows in each category of INMA
extinction_threat_inma <- info %>%
  group_by(INMA) %>%
  summarise(count = n())

extinction_threat_icmbio <- info %>%
  group_by(ICMBio) %>%
  summarise(count = n())

extinction_threat_iucn <- info %>%
  group_by(IUCN) %>%
  summarise(count = n())

extinction_threat_iucn <- extinction_threat_iucn %>%
  mutate(IUCN = factor(IUCN, levels = c("CR", "EN", "VU", "NT", "LC", "DD", "NE")) )
extinction_threat_icmbio <- extinction_threat_icmbio %>%
  mutate(ICMBio = factor(ICMBio, levels = c("CR", "EN", "VU", "NT", "LC", "DD", "NE")) )
extinction_threat_inma <- extinction_threat_inma %>%
  mutate(INMA = factor(INMA, levels = c("CR", "EN", "VU", "NT", "LC", "DD", "NE")) )

extinction_threat_iucn
extinction_threat_icmbio
extinction_threat_inma

#INMA  count
#1 CR        1
#2 EN        3
#3 VU        9
#4 NT        2
#5 DD       31
#6 NE       65
#7 NA        8





###############
#1.3 Filtering the data (dataset n = 260)
###############

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Data analysis")

#Packages
library(vegan)
library(dplyr)
library(ggplot2)

#Clean R environment 
rm(list = ls())

#Read our data
data <- read.csv("data_2023_fish_abun_per_trans.csv", header= TRUE, sep=";", dec=".")

###
#Select same months as Floeter et al. 2007 (January to April)
###

library(lubridate)

data$Date <- dmy(data$Date)
str(data)
data <- subset(data, year(Date) == 2023)
data <- subset(data, year(Date) == 2023 & Date < as.Date("2023-04-30"))

###
#Select same sites as Floeter et al. 2007
###

#Delete site "TRE Pedrinhas" and "TRE Guararema northeastern side"
length(unique(data$Site))
table(unique(data$Site))

data <- subset(data, !Site %in% c("TRE Pedrinhas", "TRE Guararema northeastern side"))
data <- data %>%  select(-MUGSPP) #It was not recorded in 2023

data <- mutate_at(data, vars(c(ABUSAX, ACABAH, ACACHI, ACACOE, ACAPOL, ACAQUA, AHLEGM, ALPAFE, 
                               AMBPIN, ANISUR, ANIVIR, AULSTR, AZUMUL, BALVET, BODPUL, BODRUF, CALPEN, CANFIG, CANMAC, CANPUL, CARBAR, CARCRY, CARLAT, 
                               CARRUB, CEPFUL, CHAFAB, CHASTR, CHASED, CHIRET, CHISPI, CHRJUB, CLEBRA, CORSPP, CORGLA, CORTHR, CRYROS, CTESAE, 
                               DACVOL, DERINE, DIPARG, DIPRAD, DIOHOL, DIOHYS, DULAUR, ELAFIG, FISSPP, GOBKAL, GRABRA, GYMFUN, GYMMIL, GYMMOR, GYMVIC, HAEAUR, 
                               HAEATL, HAEPAR, HAEPLU, HALBRA, HALDIM, HALPEN, HALPOE, HALSPP, HETCRU, HOLADS, HOLCIL, HOLTRI, HYPFIS, KYPSPP, LABCRI, 
                               LABNUC, LUTALE, LUTJOC, MALDEL, MALMAC, MALZAL, MICCHR, MYRJAC, OCYCHR, ODODEN, OPHTRI, OPIAUR, ORTRUB, PAGPAG, 
                               PARFUR, PARLIN, PARMAR, PARMOR, PARPIL, PEMSCH, PSEPER, POMARC, POMPAR, PRIARE, PSEMAC, PTERAN, RYPSAP, SCATRI, 
                               SCAZEL, SCOBRA, SCOIST, SCOPLU, SELSET, SERBAL, SERFLA, SPASPP, SPAAMP, SPAAXI, SPAFRO, SPATUI, SPHSPE, SPHSPP, STEFUS,
                               Temperature, Visibility)), as.numeric)
str(data)

#Select Islands
samples_TRE <- subset(data, Island %in% c("TRE"))
samples_ESC <- subset(data, Island %in% c("ESC"))
samples_RAS <- subset(data, Island %in% c("RAS"))

#Select depth ranges
samples_TRE_5 <- subset(samples_TRE, Depth_range %in% c("0 to 5.0"))
samples_TRE_10 <- subset(samples_TRE, Depth_range %in% c("5.1 to 9.9"))
samples_TRE_16 <- subset(samples_TRE, Depth_range %in% c("10.0 to 16.9"))

table(samples_TRE_5$Depth_range)
table(samples_TRE_10$Depth_range)
table(samples_TRE_16$Depth_range)

samples_ESC_5 <- subset(samples_ESC, Depth_range %in% c("0 to 5.0"))
samples_ESC_10 <- subset(samples_ESC, Depth_range %in% c("5.1 to 9.9"))
samples_ESC_16 <- subset(samples_ESC, Depth_range %in% c("10.0 to 16.9"))

table(samples_ESC_5$Depth_range)
table(samples_ESC_10$Depth_range)
table(samples_ESC_16$Depth_range)

samples_RAS_5 <- subset(samples_RAS, Depth_range %in% c("0 to 5.0"))
samples_RAS_10 <- subset(samples_RAS, Depth_range %in% c("5.1 to 9.9"))
samples_RAS_16 <- subset(samples_RAS, Depth_range %in% c("10.0 to 16.9"))

table(samples_RAS_5$Depth_range)
table(samples_RAS_10$Depth_range)
table(samples_RAS_16$Depth_range)

#Leave just fish data
samples_TRE_5_fish <- samples_TRE_5[,c(16:133)]
samples_TRE_10_fish <- samples_TRE_10[,c(16:133)]
samples_TRE_16_fish <- samples_TRE_16[,c(16:133)]

samples_ESC_5_fish <- samples_ESC_5[,c(16:133)]
samples_ESC_10_fish <- samples_ESC_10[,c(16:133)]
samples_ESC_16_fish <- samples_ESC_16[,c(16:133)]

samples_RAS_5_fish <- samples_RAS_5[,c(16:133)]
samples_RAS_10_fish <- samples_RAS_10[,c(16:133)]
samples_RAS_16_fish <- samples_RAS_16[,c(16:133)]


###############
#1.4 Resample the data randomly by bootstrapping  (bootstrapped dataset n = 182)
###############

set.seed(123)

#TRE: 77 (Floeter et al. 2007)
#2–5 m: 30
#6-9 m: 30
#10-16 m: 12

#Bootstrap
bootstrap_TRE_5 <- samples_TRE_5_fish[sample(1:nrow(samples_TRE_5_fish), size = 30, replace = TRUE), ]
bootstrap_TRE_10 <- samples_TRE_10_fish[sample(1:nrow(samples_TRE_10_fish), size = 30, replace = TRUE), ]
bootstrap_TRE_16 <- samples_TRE_16_fish[sample(1:nrow(samples_TRE_16_fish), size = 12, replace = TRUE), ]

#Compare raw and bootstrap data
samples_TRE_5_fish %>% summarise_at(vars(ACABAH, HAEAUR, STEFUS), 
                                          list(mean = ~ round(mean(., na.rm = TRUE),2), sd = ~ round(sd(., na.rm = TRUE),2)))
bootstrap_TRE_5 %>% summarise_at(vars(ACABAH, HAEAUR, STEFUS), 
                                          list(mean = ~ round(mean(., na.rm = TRUE),2), sd = ~ round(sd(., na.rm = TRUE),2)))
samples_TRE_10_fish %>% summarise_at(vars(ACABAH, HAEAUR, STEFUS), 
                                     list(mean = ~ round(mean(., na.rm = TRUE),2), sd = ~ round(sd(., na.rm = TRUE),2)))
bootstrap_TRE_10 %>% summarise_at(vars(ACABAH, HAEAUR, STEFUS), 
                                          list(mean = ~ round(mean(., na.rm = TRUE),2), sd = ~ round(sd(., na.rm = TRUE),2)))
samples_TRE_16_fish %>% summarise_at(vars(ACABAH, HAEAUR, STEFUS), 
                                     list(mean = ~ round(mean(., na.rm = TRUE),2), sd = ~ round(sd(., na.rm = TRUE),2)))
bootstrap_TRE_16 %>% summarise_at(vars(ACABAH, HAEAUR, STEFUS), 
                                          list(mean = ~ round(mean(., na.rm = TRUE),2), sd = ~ round(sd(., na.rm = TRUE),2)))


#ESC: 55 (Floeter et al. 2007)
#2–5 m: 8
#6-9 m: 22
#10-16 m: 25

#Bootstrap
bootstrap_ESC_5 <- samples_ESC_5_fish[sample(1:nrow(samples_ESC_5_fish), size = 8, replace = TRUE), ]
bootstrap_ESC_10 <- samples_ESC_10_fish[sample(1:nrow(samples_ESC_10_fish), size = 22, replace = TRUE), ]
bootstrap_ESC_16 <- samples_ESC_16_fish[sample(1:nrow(samples_ESC_16_fish), size = 25, replace = TRUE), ]

#Compare raw and bootstrap data
samples_ESC_5_fish %>% summarise_at(vars(ACABAH, HAEAUR, STEFUS), 
                                    list(mean = ~ round(mean(., na.rm = TRUE),2), sd = ~ round(sd(., na.rm = TRUE),2)))
bootstrap_ESC_5 %>% summarise_at(vars(ACABAH, HAEAUR, STEFUS), 
                                         list(mean = ~ round(mean(., na.rm = TRUE),2), sd = ~ round(sd(., na.rm = TRUE),2)))
samples_ESC_10_fish %>% summarise_at(vars(ACABAH, HAEAUR, STEFUS), 
                                     list(mean = ~ round(mean(., na.rm = TRUE),2), sd = ~ round(sd(., na.rm = TRUE),2)))
bootstrap_ESC_10 %>% summarise_at(vars(ACABAH, HAEAUR, STEFUS), 
                                          list(mean = ~ round(mean(., na.rm = TRUE),2), sd = ~ round(sd(., na.rm = TRUE),2)))
samples_ESC_16_fish %>% summarise_at(vars(ACABAH, HAEAUR, STEFUS), 
                                     list(mean = ~ round(mean(., na.rm = TRUE),2), sd = ~ round(sd(., na.rm = TRUE),2)))
bootstrap_ESC_16 %>% summarise_at(vars(ACABAH, HAEAUR, STEFUS), 
                                          list(mean = ~ round(mean(., na.rm = TRUE),2), sd = ~ round(sd(., na.rm = TRUE),2)))
#RAS
#Let's select same n size as ESC
#Bootstrap
bootstrap_RAS_5 <- samples_RAS_5_fish[sample(1:nrow(samples_RAS_5_fish), size = 8, replace = TRUE), ]
bootstrap_RAS_10 <- samples_RAS_10_fish[sample(1:nrow(samples_RAS_10_fish), size = 22, replace = TRUE), ]
bootstrap_RAS_16 <- samples_RAS_16_fish[sample(1:nrow(samples_RAS_16_fish), size = 25, replace = TRUE), ]

#Compare raw and bootstrap data
samples_RAS_5_fish %>% summarise_at(vars(ACABAH, HAEAUR, STEFUS), 
                                    list(mean = ~ round(mean(., na.rm = TRUE),2), sd = ~ round(sd(., na.rm = TRUE),2)))
bootstrap_RAS_5 %>% summarise_at(vars(ACABAH, HAEAUR, STEFUS), 
                                 list(mean = ~ round(mean(., na.rm = TRUE),2), sd = ~ round(sd(., na.rm = TRUE),2)))
samples_RAS_10_fish %>% summarise_at(vars(ACABAH, HAEAUR, STEFUS), 
                                     list(mean = ~ round(mean(., na.rm = TRUE),2), sd = ~ round(sd(., na.rm = TRUE),2)))
bootstrap_RAS_10 %>% summarise_at(vars(ACABAH, HAEAUR, STEFUS), 
                                  list(mean = ~ round(mean(., na.rm = TRUE),2), sd = ~ round(sd(., na.rm = TRUE),2)))
samples_RAS_16_fish %>% summarise_at(vars(ACABAH, HAEAUR, STEFUS), 
                                     list(mean = ~ round(mean(., na.rm = TRUE),2), sd = ~ round(sd(., na.rm = TRUE),2)))
bootstrap_RAS_16 %>% summarise_at(vars(ACABAH, HAEAUR, STEFUS), 
                                  list(mean = ~ round(mean(., na.rm = TRUE),2), sd = ~ round(sd(., na.rm = TRUE),2)))

###############
#1.4 Save dataset
###############

str(data)

bootstrap_TRE_5$Island <- c("TRE")
bootstrap_TRE_5$Depth_range <- c("0 to 5.0")
bootstrap_TRE_5 <- bootstrap_TRE_5 [, c(119:120,1:118)]

bootstrap_TRE_10$Island <- c("TRE")
bootstrap_TRE_10$Depth_range <- c("5.1 to 9.9")
bootstrap_TRE_10 <- bootstrap_TRE_10 [, c(119:120,1:118)]

bootstrap_TRE_16$Island <- c("TRE")
bootstrap_TRE_16$Depth_range <- c("10.0 to 16.9")
bootstrap_TRE_16 <- bootstrap_TRE_16 [, c(119:120,1:118)]

bootstrap_ESC_5$Island <- c("ESC")
bootstrap_ESC_5$Depth_range <- c("0 to 5.0")
bootstrap_ESC_5 <- bootstrap_ESC_5 [, c(119:120,1:118)]

bootstrap_ESC_10$Island <- c("ESC")
bootstrap_ESC_10$Depth_range <- c("5.1 to 9.9")
bootstrap_ESC_10 <- bootstrap_ESC_10 [, c(119:120,1:118)]

bootstrap_ESC_16$Island <- c("ESC")
bootstrap_ESC_16$Depth_range <- c("10.0 to 16.9")
bootstrap_ESC_16 <- bootstrap_ESC_16 [, c(119:120,1:118)]

bootstrap_RAS_5$Island <- c("RAS")
bootstrap_RAS_5$Depth_range <- c("0 to 5.0")
bootstrap_RAS_5 <- bootstrap_RAS_5 [, c(119:120,1:118)]

bootstrap_RAS_10$Island <- c("RAS")
bootstrap_RAS_10$Depth_range <- c("5.1 to 9.9")
bootstrap_RAS_10 <- bootstrap_RAS_10 [, c(119:120,1:118)]

bootstrap_RAS_16$Island <- c("RAS")
bootstrap_RAS_16$Depth_range <- c("10.0 to 16.9")
bootstrap_RAS_16 <- bootstrap_RAS_16 [, c(119:120,1:118)]

bootstrap_data <- bind_rows(
  bootstrap_TRE_5,
  bootstrap_TRE_10,
  bootstrap_TRE_16,
  bootstrap_ESC_5,
  bootstrap_ESC_10,
  bootstrap_ESC_16,
  bootstrap_RAS_5,
  bootstrap_RAS_10,
  bootstrap_RAS_16)

#write.table(bootstrap_data,"data_2023_fish_bootstrap.csv", sep=";", dec=".",row.names = F) #Month 1 correction (m1), Edge 3 otolith (m3)





#################################################
#2. Fish assemblages
#################################################


###############
#2.1 nMDS
###############

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Data analysis")

#packages
library(vegan)
library(dplyr)
library(ggplot2)

#clean R environment 
rm(list = ls())

bootstrap_data <- read.csv("data_2023_fish_bootstrap.csv", header= TRUE, sep=";", dec=".")
table(unique(bootstrap_data$Island))
bootstrap_data <- bootstrap_data %>%  select(-MUGSPP) #It was not recorded in 2023

bootstrap_data <- mutate_at(bootstrap_data, vars(c(ABUSAX, ACABAH, ACACHI, ACACOE, ACAPOL, ACAQUA, AHLEGM, ALPAFE, 
                                                   AMBPIN, ANISUR, ANIVIR, AULSTR, AZUMUL, BALVET, BODPUL, BODRUF, CALPEN, CANFIG, CANMAC, CANPUL, CARBAR, CARCRY, CARLAT, 
                                                   CARRUB, CEPFUL, CHAFAB, CHASTR, CHASED, CHIRET, CHISPI, CHRJUB, CLEBRA, CORSPP, CORGLA, CORTHR, CRYROS, CTESAE, 
                                                   DACVOL, DERINE, DIPARG, DIPRAD, DIOHOL, DIOHYS, DULAUR, ELAFIG, FISSPP, GOBKAL, GRABRA, GYMFUN, GYMMIL, GYMMOR, GYMVIC, HAEAUR, 
                                                   HAEATL, HAEPAR, HAEPLU, HALBRA, HALDIM, HALPEN, HALPOE, HALSPP, HETCRU, HOLADS, HOLCIL, HOLTRI, HYPFIS, KYPSPP, LABCRI, 
                                                   LABNUC, LUTALE, LUTJOC, MALDEL, MALMAC, MALZAL, MICCHR, MYRJAC, OCYCHR, ODODEN, OPHTRI, OPIAUR, ORTRUB, PAGPAG, 
                                                   PARFUR, PARLIN, PARMAR, PARMOR, PARPIL, PEMSCH, PSEPER, POMARC, POMPAR, PRIARE, PSEMAC, PTERAN, RYPSAP, SCATRI, 
                                                   SCAZEL, SCOBRA, SCOIST, SCOPLU, SELSET, SERBAL, SERFLA, SPASPP, SPAAMP, SPAAXI, SPAFRO, SPATUI, SPHSPE, SPHSPP, STEFUS)), as.numeric)
str(bootstrap_data)
summary(bootstrap_data)

data_var <- bootstrap_data[, c(3:120)] 
data_var <- sqrt(data_var)
data_var <- as.data.frame(data_var)
str(data_var)

#NMDS with 4 dimensions (k=4) and autotransforming into square root (function default)
set.seed(123)
nmds <- metaMDS(data_var, distance = "bray", k=4, autotransform = TRUE, trymax=100 )
nmds

#Call:
#metaMDS(comm = data_var, distance = "bray", k = 4, trymax = 100,      autotransform = TRUE) 
#global Multidimensional Scaling using monoMDS
#Data:     wisconsin(sqrt(data_var)) 
#Distance: bray 
#Dimensions: 4 
#Stress:     0.1616715 
#Stress type 1, weak ties
#Best solution was repeated 1 time in 20 tries
#The best solution was from try 0 (metric scaling or null solution)
#Scaling: centring, PC rotation, halfchange scaling 
#Species: expanded scores based on ‘wisconsin(sqrt(data_var))’

species<- envfit(nmds, data_var, permutations = 999, na.rm = TRUE)

species

#extract NMDS scores (x and y coordinates) for sites from newer versions of vegan package
data_scores <- as.data.frame(scores(nmds)$sites)

#extract coordinates from species
species_coord_cont <- as.data.frame(scores(species, "vectors")) * ordiArrowMul(species)
species_coord_cat <- as.data.frame(scores(species, "factors")) * ordiArrowMul(species)

#add columns to data frame
data_scores$Island = bootstrap_data$Island
data_scores$Depth_range = bootstrap_data$Depth_range

library(tibble)

species_coord_cont <- species_coord_cont %>%
  rownames_to_column("Species")

data_scores <- mutate_at(data_scores, vars(c(NMDS1, NMDS2, NMDS3, NMDS4)), as.numeric)

head(data_scores)
head(species_coord_cont)

#select species most relevant
selected_rows <- which(species_coord_cont$Species %in% c("ABUSAX","ACACHI","BODRUF","CANFIG","CANMAC",
                                                         "HALPOE","HOLTRI","MALZAL","SERFLA","STEFUS"))
species_coord_cont <- species_coord_cont[selected_rows, ]
species_coord_cont <- as.data.frame(species_coord_cont)
head(species_coord_cont)
str(species_coord_cont)
species_coord_cont <- mutate_at(species_coord_cont, vars(c(NMDS1, NMDS2)), as.numeric)
species <- species_coord_cont
str(species)
str(data_scores)

colours <- c("#440154", "#21918C","#D8B100")

fig_nmds_fish = ggplot(data_scores, aes(x = NMDS1, y = NMDS2, color=Island))+
  geom_point(size = 3, alpha=.5)+
  stat_ellipse(geom = "polygon",
               aes(color = Island, fill = Island),
               alpha=0.01,
               linetype=3,
               level=0.75,
               type="t")+
  geom_text(data = species, aes(x = NMDS1/2-0.1, y = NMDS2/2+0.1), colour = "grey30", 
            fontface = "bold", label = species$Species) +
  geom_segment(data = species, aes(x = 0, y = 0, xend = NMDS1/2, yend = NMDS2/2), 
               linewidth= 1.2, alpha = 0.5, colour = "grey30") +
  theme(axis.text.y = element_text(colour = "black"), 
        axis.text.x = element_text(colour = "black"), 
        legend.text = element_text(colour ="black"), 
        legend.position = "right", 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Island", y = "NMDS2", shape = "Island")+
  scale_color_manual(values =colours)+
  scale_fill_manual(values =colours) +
  scale_x_continuous(breaks = seq(-1.5, 1.5, by = 0.5))

fig_nmds_fish 


###############
#2.2 PERMANOVA and post hoc adonis
###############

#packages
library(pairwiseAdonis)
library(vegan)

# Distance matrix (replace `your_data` with your actual dataset)
str(bootstrap_data)
data_perm <- bootstrap_data[, c(3:120)]
dist_matrix <- vegdist(data_perm, method = "bray") 

# PERMANOVA
perm_test <- adonis2(dist_matrix ~ Island, data = bootstrap_data, 
                     permutations = 999, p.adjust.m ='bonferroni')
perm_test

# Perform pairwise PERMANOVA
pairwise_results <- pairwise.adonis2(dist_matrix ~ Island, data = bootstrap_data, 
                                     nperm = 999, p.adjust.m ='bonferroni')
pairwise_results

# PERMDISP
dispersion <- betadisper(dist_matrix, group = bootstrap_data$Island)
disp_test <- permutest(dispersion, permutations = 999)
disp_test

#Visualizations
plot(dispersion)
boxplot(dispersion, main = "Multivariate Dispersion (by Island)", ylab = "Distance to Centroid")


###############
#2.3 nicheROVER
###############

#packages
library(nicheROVER)
library(dplyr)
library(ellipse)
library(ggplot2)
library(ggtext)
library(here)
library(purrr)
library(patchwork)
library(readr)
library(stringr)
library(tidyr)

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Data analysis")

#Read dataset
my_data_fish <- data_scores

###
#Generate parameters
###

# generate parameter draws from the "default" posteriors of each my_data_fish
nsamples <- 1e3
system.time({
  my_data_fish_par <- tapply(1:nrow(my_data_fish), my_data_fish$Island ,
                        function(ii) niw.post(nsamples = nsamples, X = my_data_fish[ii,1:4]))
})

# various parameter plots
clrs <- c("#440154", "#21918C","#D8B100") # colors for each group

# mu1 
par(mar = c(4, 4, .5, .1)+.1, mfrow = c(1,3))
niche.par.plot(my_data_fish_par, col = clrs, plot.index = 1)
niche.par.plot(my_data_fish_par, col = clrs, plot.index = 2)
niche.par.plot(my_data_fish_par, col = clrs, plot.index = 1:2)
legend("topleft", legend = names(my_data_fish_par), fill = clrs)

# all mu 
niche.par.plot(my_data_fish_par, col = clrs, plot.mu = TRUE, plot.Sigma = FALSE)
legend("topleft", legend = names(my_data_fish_par), fill = clrs)

# all mu and Sigma
par(mar = c(4.2, 4.2, 2, 1)+.1)
niche.par.plot(my_data_fish_par, col = clrs, plot.mu = TRUE, plot.Sigma = TRUE)
legend("topright", legend = names(my_data_fish_par), fill = clrs)

###
# 2-d projections of 20 niche regions
###

nsamples <- 10
my_data_fish_par <- tapply(1:nrow(my_data_fish), my_data_fish$Island,
                      function(ii) niw.post(nsamples = nsamples, X = my_data_fish[ii,1:4]))

# format data for plotting function
my_data_fish.data <- tapply(1:nrow(my_data_fish), my_data_fish$Island , function(ii) X = my_data_fish[ii,1:4])

#####
# Figure S5 - Multivariate diversity space based on the four nMDS dimensions coordinates 
#####

niche.plot(niche.par = my_data_fish_par, niche.data = my_data_fish.data, pfrac = .6,
           iso.names = expression("NMDS1", "NMDS2", "NMDS3", "NMDS4"),
           col = clrs, xlab = expression("Multivariate diversity space"))

#750 x 650

# niche overlap plots for 95% niche region sizes
nsamples <- 1000
my_data_fish_par <- tapply(1:nrow(my_data_fish), my_data_fish$Island ,
                      function(ii) niw.post(nsamples = nsamples, X = my_data_fish[ii,1:4]))

# Overlap calculation.  use nsamples = nprob = 10000 (1e4) for higher accuracy.
# the variable over_stat can be supplied directly to the overlap.plot function

over_stat <- overlap(my_data_fish_par, nreps = nsamples, nprob = 1e3, alpha = 0.95)

over_stat_df <- over_stat %>% 
  as_tibble(rownames = "species_a") %>% 
  mutate(
    id = 1:nrow(.), 
    species_a = factor(species_a, 
                       level = c("ESC", "RAS", "TRE"))
  ) %>% 
  pivot_longer(cols = -c(id, species_a), 
               names_to = "species_b", 
               values_to = "mc_nr")  %>% 
  separate(species_b, into = c("species_c", "sample_number"), 
           sep = "\\.") %>% 
  select(-id) %>% 
  rename(species_b = species_c) %>% 
  mutate(
    species_b =  factor(species_b, 
                        level = c("ESC", "RAS", "TRE")
    ), 
    mc_nr_perc = mc_nr * 100
  )

#We then are going to take our newly made data frame and extract out 
#the mean percentage of similarities and the 2.5% and 97.5% quarantines. 
#We plot these as lines and dotted lines on our percent similarity density figure.

over_sum <- over_stat_df %>% 
  group_by(species_a, species_b) %>% 
  summarise(
    mean_mc_nr = round(mean(mc_nr_perc), digits = 2),
    qual_2.5 = round(quantile(mc_nr_perc, probs = 0.025, na.rm = TRUE), digits = 2), 
    qual_97.5 = round(quantile(mc_nr_perc, probs = 0.975, na.rm = TRUE), digits = 2)
  ) %>% 
  ungroup() %>% 
  pivot_longer(cols = -c(species_a, species_b, mean_mc_nr), 
               names_to = "percentage", 
               values_to = "mc_nr_qual") %>% 
  mutate(
    percentage = as.numeric(str_remove(percentage, "qual_"))
  ) 

#The mean overlap metrics calculated across iteratations for both niche 
#region sizes (alpha = .95 and alpha = .99) can be calculated and displayed in an array.

over_mean <- apply(over_stat, c(1:2,3), mean)*100
round(over_mean, 2)

over.cred <- apply(over_stat*100, c(1:2,3), quantile, prob = c(.025, .975), na.rm = TRUE)
round(over.cred[,,,1]) # display alpha = .95 niche region


#Overlap plot.Before you run this, make sure that you have chosen your 
#alpha level.

over_stat <- overlap(my_data_fish_par, nreps = nsamples, nprob = 1e3, alpha = .95)
over_stat





#####
#Figure 9 a - Histogram of the posterior distribution of overlap probabilities (%) 
#####

overlap.plot(over_stat, col = clrs, mean.cred.col = "black", equal.axis = TRUE,
             xlab = "Overlap Probability (%) in Multivariate diversity spaces (95%)")

#650 x 500

head(my_data_fish)
# posterior distribution of (mu, Sigma) for each Island 
nsamples <- 1000
my_data_fish_par <- tapply(1:nrow(my_data_fish), my_data_fish$Island ,
                      function(ii) niw.post(nsamples = nsamples, X = my_data_fish[ii,1:4]))

# posterior distribution of niche size by Island 
data_size_fish <- sapply(my_data_fish_par, function(spec) {
  apply(spec$Sigma, 3, niche.size, alpha = .95)
})

# point estimate and standard error
rbind(est = colMeans(data_size_fish),
      se = apply(data_size_fish, 2, sd))

#We then need to transform niche_size into a datafame 
#for visualization and summary statistics.
library(dplyr)
niche_size_df <- data_size_fish %>% 
  as_tibble() %>% 
  mutate(
    id = 1:nrow(.)
  ) %>% 
  pivot_longer(
    cols = -id, 
    names_to = "Islands", 
    values_to = "diversity_size"
  ) %>% 
  mutate(
    id = 1:nrow(.), 
    Islands = factor(Islands,
                     level = c("ESC", "RAS", 
                               "TRE"))
  )

#We can calculate the mean niche size, standard deviation, and standard error.
niche_size_mean <- niche_size_df %>% 
  group_by(Islands) %>% 
  summarise(
    mean_niche = round(mean(diversity_size), digits = 2), 
    sd_niche = round(sd(diversity_size), digits = 2), 
    sem_niche = round(sd(diversity_size) / sqrt(n()), digits = 2)
  )

# boxplots

boxplot(data_size_fish, col = clrs,
        ylab = "Multivariate diversity space size", xlab = "Island ")


summary(data_size_fish)
round(sd(data_size_fish[, 1], na.rm = TRUE),2)
round(sd(data_size_fish[, 2], na.rm = TRUE),2)
round(sd(data_size_fish[, 3], na.rm = TRUE),2)

#The multivariate diversity space (4D) for the reef fish assemblage was lower for TRE (4.67± 0.78), 
#followed by RAS (4.70 ± 0.89), and ESC (6.41± 1.23; Fig. 8a; bootstrapped dataset, n = 182)

head(data_size_fish)
size_fish <- as.data.frame(data_size_fish)

size_fish <- size_fish %>%
  pivot_longer(
    cols = everything(),
    names_to = "Islands",
    values_to = "Size")

size_fish <- as.data.frame(size_fish)
size_fish$Size <- as.numeric(size_fish$Size)

head(size_fish)
str(size_fish)

######
#Figure 8 a
######

library(ggstatsplot)

ggbetweenstats(data = size_fish,
                        x = Islands,
                        y = Size,
                        plot.type = "box",
                        xlab= "Islands",
                        ylab= "Multivariate diversity size",
                        type = "nonparametric", # ANOVA or Kruskal-Wallis
                        pairwise.comparisons = TRUE,
                        pairwise.display = "significant",
                        centrality.plotting = FALSE,
                        bf.message = FALSE)+
                        ggplot2::scale_color_manual(values = c("#440154", "#21918C","#D8B100"))


colours <- c("#440154", "#21918C","#D8B100")

Fig8a <- ggplot(data = size_fish, aes(x = Islands, y = Size, fill = Islands)) +
  theme_bw(base_size = 20) +
  geom_boxplot(alpha = 0.7, width = 0.3) +
  ylab(expression("Multivariate diversity space")) +
  xlab(expression("Islands")) +
  scale_fill_manual(values = colours) +
  theme(legend.position="none",
        axis.text = element_text(size = 20),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 20))

Fig8a

#550 x 400





################
# Table S3. The current total composition of fish assemblage 
################

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Data analysis")

#clean R environment 
rm(list = ls())

data_fish <- read.csv("data_2023_fish_abun_per_trans.csv", header= TRUE, sep=";", dec=".")
table(unique(data_fish$Island))

data_fish <- mutate_at(data_fish, vars(c(ABUSAX, ACABAH, ACACHI, ACACOE, ACAPOL, ACAQUA, AHLEGM, ALPAFE, 
                                                   AMBPIN, ANISUR, ANIVIR, AULSTR, AZUMUL, BALVET, BODPUL, BODRUF, CALPEN, CANFIG, CANMAC, CANPUL, CARBAR, CARCRY, CARLAT, 
                                                   CARRUB, CEPFUL, CHAFAB, CHASTR, CHASED, CHIRET, CHISPI, CHRJUB, CLEBRA, CORSPP, CORGLA, CORTHR, CRYROS, CTESAE, 
                                                   DACVOL, DERINE, DIPARG, DIPRAD, DIOHOL, DIOHYS, DULAUR, ELAFIG, FISSPP, GOBKAL, GRABRA, GYMFUN, GYMMIL, GYMMOR, GYMVIC, HAEAUR, 
                                                   HAEATL, HAEPAR, HAEPLU, HALBRA, HALDIM, HALPEN, HALPOE, HALSPP, HETCRU, HOLADS, HOLCIL, HOLTRI, HYPFIS, KYPSPP, LABCRI, 
                                                   LABNUC, LUTALE, LUTJOC, MALDEL, MALMAC, MALZAL, MICCHR, MYRJAC, MUGSPP, OCYCHR, ODODEN, OPHTRI, OPIAUR, ORTRUB, PAGPAG, 
                                                   PARFUR, PARLIN, PARMAR, PARMOR, PARPIL, PEMSCH, PSEPER, POMARC, POMPAR, PRIARE, PSEMAC, PTERAN, RYPSAP, SCATRI, 
                                                   SCAZEL, SCOBRA, SCOIST, SCOPLU, SELSET, SERBAL, SERFLA, SPASPP, SPAAMP, SPAAXI, SPAFRO, SPATUI, SPHSPE, SPHSPP, STEFUS)), as.numeric)
str(data_fish)
summary(data_fish)


#####
#Island TRE 2023
######

# Select just TRE
data_tre <- subset(data_fish, Island %in% c("TRE"))
data_tre <- data_tre[,c(16:134)]
str(data_tre)

#####
#Calculate the mean, SD
#####

tre_2023_mean <- round(colMeans(data_tre), 2)
tre_2023_mean

tre_2023_sd <- round(apply(data_tre, 2, sd, na.rm = TRUE), 2)
tre_2023_sd


#####
#Convert the data to a data frame and arrange it by column names
#####

tre_2023 <- data.frame(Species = names(tre_2023_mean), Mean = tre_2023_mean, SD = tre_2023_sd)
tre_2023$Island <- "TRE"

rownames(tre_2023) <- NULL
tre_2023 <- tre_2023 %>%
  arrange(Species)

#Rename
tre_2023 <- tre_2023 %>%
  rename(
    Mean_2023 = Mean,
    SD_2023 = SD)

tre_2023$Relative_abundance <- round((tre_2023$Mean_2023/sum(tre_2023$Mean_2023))*100,2)

head(tre_2023)

#####
#Island ESC 2023
######

# Select just ESC
data_esc <- subset(data_fish, Island %in% c("ESC"))
data_esc <- data_esc[,c(16:134)]
str(data_esc)

#####
#Calculate the mean, SD
#####

esc_2023_mean <- round(colMeans(data_esc), 2)
esc_2023_mean

esc_2023_sd <- round(apply(data_esc, 2, sd, na.rm = TRUE), 2)
esc_2023_sd


#####
#Convert the data to a data frame and arrange it by column names
#####

esc_2023 <- data.frame(Species = names(esc_2023_mean), Mean = esc_2023_mean, SD = esc_2023_sd)
esc_2023$Island <- "ESC"

rownames(esc_2023) <- NULL
esc_2023 <- esc_2023 %>%
  arrange(Species)

#Rename
esc_2023 <- esc_2023 %>%
  rename(
    Mean_2023 = Mean,
    SD_2023 = SD)

esc_2023$Relative_abundance <- round((esc_2023$Mean_2023/sum(esc_2023$Mean_2023))*100,2)

head(esc_2023)

#####
#Island RAS 2023
######

# Select just RAS
data_ras <- subset(data_fish, Island %in% c("RAS"))
data_ras <- data_ras[,c(16:134)]
str(data_ras)

#####
#Calculate the mean, SD
#####

ras_2023_mean <- round(colMeans(data_ras), 2)
ras_2023_mean

ras_2023_sd <- round(apply(data_ras, 2, sd, na.rm = TRUE), 2)
ras_2023_sd


#####
#Convert the data to a data frame and arrange it by column names
#####

ras_2023 <- data.frame(Species = names(ras_2023_mean), Mean = ras_2023_mean, SD = ras_2023_sd)
ras_2023$Island <- "RAS"

rownames(ras_2023) <- NULL
ras_2023 <- ras_2023 %>%
  arrange(Species)

#Rename
ras_2023 <- ras_2023 %>%
  rename(
    Mean_2023 = Mean,
    SD_2023 = SD)

ras_2023$Relative_abundance <- round((ras_2023$Mean_2023/sum(ras_2023$Mean_2023))*100,2)

head(ras_2023)


# Perform the row join
table_s2 <- bind_rows(
  tre_2023,
  esc_2023,
  ras_2023)

head(table_s2)

####
#Add info to the data
####

info <- read.csv("data_2023_fish_info.csv", header= TRUE, sep=";", dec=".")
head(info)


table_s2 <- left_join(table_s2, info %>%  select(Species, Family, Scientific_name, Group, IUCN, ICMBio, INMA), by = "Species")
head(table_s2)


#save Table S2
#write.table(table_s2, "table_s2.csv", sep=";", dec=".", row.names = F)

top <- table_s2 %>%
  group_by(Island) %>% 
  arrange(desc(Relative_abundance), .by_group = TRUE) %>% 
  slice_head(n = 10)

top










#####################
#2.4 t-test for all species
#####################

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Data analysis")

#clean R environment 
rm(list = ls())

bootstrap_data <- read.csv("data_2023_fish_bootstrap.csv", header= TRUE, sep=";", dec=".")
table(unique(bootstrap_data$Island))
bootstrap_data <- bootstrap_data %>%  select(-MUGSPP) #It was not recorded in 2023

bootstrap_data <- mutate_at(bootstrap_data, vars(c(ABUSAX, ACABAH, ACACHI, ACACOE, ACAPOL, ACAQUA, AHLEGM, ALPAFE, 
                                                   AMBPIN, ANISUR, ANIVIR, AULSTR, AZUMUL, BALVET, BODPUL, BODRUF, CALPEN, CANFIG, CANMAC, CANPUL, CARBAR, CARCRY, CARLAT, 
                                                   CARRUB, CEPFUL, CHAFAB, CHASTR, CHASED, CHIRET, CHISPI, CHRJUB, CLEBRA, CORSPP, CORGLA, CORTHR, CRYROS, CTESAE, 
                                                   DACVOL, DERINE, DIPARG, DIPRAD, DIOHOL, DIOHYS, DULAUR, ELAFIG, FISSPP, GOBKAL, GRABRA, GYMFUN, GYMMIL, GYMMOR, GYMVIC, HAEAUR, 
                                                   HAEATL, HAEPAR, HAEPLU, HALBRA, HALDIM, HALPEN, HALPOE, HALSPP, HETCRU, HOLADS, HOLCIL, HOLTRI, HYPFIS, KYPSPP, LABCRI, 
                                                   LABNUC, LUTALE, LUTJOC, MALDEL, MALMAC, MALZAL, MICCHR, MYRJAC, OCYCHR, ODODEN, OPHTRI, OPIAUR, ORTRUB, PAGPAG, 
                                                   PARFUR, PARLIN, PARMAR, PARMOR, PARPIL, PEMSCH, PSEPER, POMARC, POMPAR, PRIARE, PSEMAC, PTERAN, RYPSAP, SCATRI, 
                                                   SCAZEL, SCOBRA, SCOIST, SCOPLU, SELSET, SERBAL, SERFLA, SPASPP, SPAAMP, SPAAXI, SPAFRO, SPATUI, SPHSPE, SPHSPP, STEFUS)), as.numeric)


###
# TRE
###

bootstrap_data_tre <- subset(bootstrap_data, Island %in% c("TRE"))


species_tre_2023 <- bootstrap_data_tre[, -c(1:2)]
str(species_tre_2023)
summary(species_tre_2023)
head(species_tre_2023)

#Reshape the data

reshaped_species_tre_2023 <- species_tre_2023 %>%
  pivot_longer(
    cols = 1:118,            
    names_to = "Species",    
    values_to = "Abundance" 
  ) %>%
  mutate(
    replicate = rep(1:72, each = 118) # Add replicate column
  ) %>%
  select(replicate, Species, Abundance) # Reorder columns for readability

reshaped_species_tre_2023<-as.data.frame(reshaped_species_tre_2023)
reshaped_species_tre_2023$Time  <- "Current"
reshaped_species_tre_2023$Island  <- "TRE"
head(reshaped_species_tre_2023)




####
#TRE Floeter et al 2001
####

#set seed for reproducibility
set.seed(123)

#read data
tre_2001 <- read.csv("data_2001_fish_Floeter_et_al.csv", header= TRUE, sep=";", dec=",")

#Select just TRE
tre_2001 <- subset(tre_2001, Island %in% c("TRE"))
tre_2001 <- na.omit(tre_2001)
tre_2001$Time <- "Previous"

tre_2001$Mean <- as.numeric(tre_2001$Mean)
tre_2001$SD <- as.numeric(tre_2001$SD)
tre_2001$SE <- as.numeric(tre_2001$SE)

#Select columns
tre_2001 <- tre_2001[,c("Species","Mean","SD","SE","Time")]
head(tre_2001)

#set number of replicates
total_replicates <- 72

#create an empty data frame to store simulated data
data_2001_tre_estimated <- data.frame(replicate = rep(1:total_replicates, each = nrow(tre_2001)),
                                      Species = rep(tre_2001$Species, times = total_replicates),
                                      Abundance = numeric(total_replicates * nrow(tre_2001)))
head(data_2001_tre_estimated)

#generate simulated abundance data for each species in each transect
for (i in 1:nrow(tre_2001)) {
  mean_abundance <- tre_2001$Mean[i]
  sd_abundance <- tre_2001$SD[i]
  Species <- tre_2001$Species[i]
  
  data_2001_tre_estimated[data_2001_tre_estimated$Species == Species, "Abundance"] <- 
    rnorm(total_replicates, mean_abundance, sd_abundance)}

data_2001_tre_estimated$Abundance <- round(data_2001_tre_estimated$Abundance, 0)

data_2001_tre_estimated[data_2001_tre_estimated < 0] <- 0
data_2001_tre_estimated$Time <- "Previous"
data_2001_tre_estimated$Island  <- "TRE"

head(data_2001_tre_estimated)
head(reshaped_species_tre_2023)

table(unique(data_2001_tre_estimated$Species))



###
# ESC
###

bootstrap_data_esc <- subset(bootstrap_data, Island %in% c("ESC"))


species_esc_2023 <- bootstrap_data_esc[, -c(1:2)]
str(species_esc_2023)
summary(species_esc_2023)
head(species_esc_2023)


#Reshape the data

reshaped_species_esc_2023 <- species_esc_2023 %>%
  pivot_longer(
    cols = 1:118,            
    names_to = "Species",    
    values_to = "Abundance" 
  ) %>%
  mutate(
    replicate = rep(1:55, each = 118) # Add replicate column
  ) %>%
  select(replicate, Species, Abundance) # Reorder columns for readability

reshaped_species_esc_2023<-as.data.frame(reshaped_species_esc_2023)
reshaped_species_esc_2023$Time  <- "Current"
reshaped_species_esc_2023$Island  <- "ESC"
head(reshaped_species_esc_2023)




####
#ESC Floeter et al 2001
####

#set seed for reproducibility
set.seed(123)

#read data
esc_2001 <- read.csv("data_2001_fish_Floeter_et_al.csv", header= TRUE, sep=";", dec=",")

#Select just esc
esc_2001 <- subset(esc_2001, Island %in% c("ESC"))
esc_2001 <- na.omit(esc_2001)
esc_2001$Time <- "Previous"

esc_2001$Mean <- as.numeric(esc_2001$Mean)
esc_2001$SD <- as.numeric(esc_2001$SD)
esc_2001$SE <- as.numeric(esc_2001$SE)

#Select columns
esc_2001 <- esc_2001[,c("Species","Mean","SD","SE","Time")]
head(esc_2001)

#set number of replicates
total_replicates <- 55

#create an empty data frame to store simulated data
data_2001_esc_estimated <- data.frame(replicate = rep(1:total_replicates, each = nrow(esc_2001)),
                                      Species = rep(esc_2001$Species, times = total_replicates),
                                      Abundance = numeric(total_replicates * nrow(esc_2001)))
head(data_2001_esc_estimated)

#generate simulated abundance data for each species in each transect
for (i in 1:nrow(esc_2001)) {
  mean_abundance <- esc_2001$Mean[i]
  sd_abundance <- esc_2001$SD[i]
  Species <- esc_2001$Species[i]
  
  data_2001_esc_estimated[data_2001_esc_estimated$Species == Species, "Abundance"] <- 
    rnorm(total_replicates, mean_abundance, sd_abundance)}

data_2001_esc_estimated$Abundance <- round(data_2001_esc_estimated$Abundance, 0)

data_2001_esc_estimated[data_2001_esc_estimated < 0] <- 0
data_2001_esc_estimated$Time <- "Previous"
data_2001_esc_estimated$Island <- "ESC"

head(data_2001_esc_estimated)
head(reshaped_species_esc_2023)

table(unique(data_2001_esc_estimated$Species))


###
#Bind scenarios
###
head(reshaped_species_tre_2023)
head(data_2001_tre_estimated)

obs_fish_species_uvc_tre <- rbind(reshaped_species_tre_2023, data_2001_tre_estimated)
obs_fish_species_uvc_esc <- rbind(reshaped_species_esc_2023, data_2001_esc_estimated)

obs_fish_species_uvc <- rbind(obs_fish_species_uvc_tre, obs_fish_species_uvc_esc)

obs_fish_species_uvc
#write.table(obs_fish_species_uvc,"obs_fish_species_uvc.csv", sep=";", dec=".",row.names = F)



###############
#test-t analysis for species
###############

#clean R environment 
rm(list = ls())

obs_fish_species_uvc <- read.csv("obs_fish_species_uvc.csv", header= TRUE, sep=";", dec=".")
obs_fish_species_uvc$Abundance <- as.numeric(obs_fish_species_uvc$Abundance)


###
#HAEAUR
###

tre_haeaur_tre <- subset(obs_fish_species_uvc, Species == "HAEAUR" & Island == "TRE")
head(tre_haeaur_tre)

t_test_haeaur_tre <- t.test(
  Abundance ~ Time, 
  data = tre_haeaur_tre, 
  var.equal = TRUE)

t_test_haeaur_tre
#t = -1.6121, df = 142, p-value = 0.1092
#Not different

tre_haeaur_esc <- subset(obs_fish_species_uvc, Species == "HAEAUR" & Island == "ESC")
head(tre_haeaur_esc)

t_test_haeaur_esc <- t.test(
  Abundance ~ Time, 
  data = tre_haeaur_esc, 
  var.equal = TRUE)

t_test_haeaur_esc
#t = -5.7986, df = 108, p-value = 6.731e-08
#Different

###
#ACABAH
###

tre_acabah_tre <- subset(obs_fish_species_uvc, Species == "ACABAH" & Island == "TRE")
head(tre_acabah_tre)

t_test_acabah_tre <- t.test(
  Abundance ~ Time, 
  data = tre_acabah_tre, 
  var.equal = TRUE)

t_test_acabah_tre
#t = -2.3257, df = 142, p-value = 0.02145
#Significant different

tre_acabah_esc <- subset(obs_fish_species_uvc, Species == "ACABAH" & Island == "ESC")
head(tre_acabah_esc)

t_test_acabah_esc <- t.test(
  Abundance ~ Time, 
  data = tre_acabah_esc, 
  var.equal = TRUE)

t_test_acabah_esc
#t = -2.4185, df = 108, p-value = 0.01725
#Significant different

###
#HALPOE
###

tre_halpoe_tre <- subset(obs_fish_species_uvc, Species == "HALPOE" & Island == "TRE")
head(tre_halpoe_tre)

t_test_halpoe_tre <- t.test(
  Abundance ~ Time, 
  data = tre_halpoe_tre, 
  var.equal = TRUE)

t_test_halpoe_tre
#t = -8.193, df = 142, p-value = 1.346e-13
#Significant different



###
# TRE
###

# List of unique species
obs_fish_species_uvc_tre <- subset(obs_fish_species_uvc, Island == "TRE")
species_list_tre <- unique(obs_fish_species_uvc_tre$Species)

# Initialize an empty data frame to store t-statistics and p-values
tre_results_test_t_fish_species <- data.frame(Species = character(), t_stat = numeric(), p_value = numeric())

# Loop over each species
for (species in species_list_tre) {
  # Subset the data for the current species
  species_data <- subset(obs_fish_species_uvc_tre, Species == species)
  
  # Check if 'Time' has exactly 2 unique levels
  if (length(unique(species_data$Time)) == 2) {
    # Perform t-test
    t_test_result <- t.test(Abundance ~ Time, data = species_data, var.equal = TRUE)
    
    # Store the t-statistic and p-value in the results data frame
    tre_results_test_t_fish_species <- rbind(tre_results_test_t_fish_species, data.frame(Species = species, t_stat = t_test_result$statistic, p_value = t_test_result$p.value))
  } else {
    # If Time doesn't have exactly 2 levels, store NA for the t-statistic and p-value
    tre_results_test_t_fish_species <- rbind(tre_results_test_t_fish_species, data.frame(Species = species, t_stat = NA, p_value = NA))
  }
}

tre_results_test_t_fish_species$p_value <- ifelse(
  round(tre_results_test_t_fish_species$p_value, 3) < 0.001, 
  "<0.001", 
  round(tre_results_test_t_fish_species$p_value, 3))

tre_results_test_t_fish_species$t_stat <- round(tre_results_test_t_fish_species$t_stat,3)

tre_results_test_t_fish_species$signif_code <- with(tre_results_test_t_fish_species, 
                                               ifelse(p_value < 0.001, "***",
                                                      ifelse(p_value < 0.01, "**",
                                                             ifelse(p_value < 0.05, "*",
                                                                    ifelse(p_value < 0.1, ".", 
                                                                           " ")))))

tre_results_test_t_fish_species <- na.omit(tre_results_test_t_fish_species)
rownames(tre_results_test_t_fish_species) <- NULL
tre_results_test_t_fish_species$Island <- "TRE"

head(tre_results_test_t_fish_species)




###
# ESC
###

# List of unique species
obs_fish_species_uvc_esc <- subset(obs_fish_species_uvc, Island == "ESC")
species_list_esc <- unique(obs_fish_species_uvc_esc$Species)

# Initialize an empty data frame to store t-statistics and p-values
esc_results_test_t_fish_species <- data.frame(Species = character(), t_stat = numeric(), p_value = numeric())

# Loop over each species
for (species in species_list_esc) {
  # Subset the data for the current species
  species_data <- subset(obs_fish_species_uvc_esc, Species == species)
  
  # Check if 'Time' has exactly 2 unique levels
  if (length(unique(species_data$Time)) == 2) {
    # Perform t-test
    t_test_result <- t.test(Abundance ~ Time, data = species_data, var.equal = TRUE)
    
    # Store the t-statistic and p-value in the results data frame
    esc_results_test_t_fish_species <- rbind(esc_results_test_t_fish_species, data.frame(Species = species, t_stat = t_test_result$statistic, p_value = t_test_result$p.value))
  } else {
    # If Time doesn't have exactly 2 levels, store NA for the t-statistic and p-value
    esc_results_test_t_fish_species <- rbind(esc_results_test_t_fish_species, data.frame(Species = species, t_stat = NA, p_value = NA))
  }
}

esc_results_test_t_fish_species$p_value <- ifelse(
  round(esc_results_test_t_fish_species$p_value, 3) < 0.001, 
  "<0.001", 
  round(esc_results_test_t_fish_species$p_value, 3))

esc_results_test_t_fish_species$t_stat <- round(esc_results_test_t_fish_species$t_stat,3)

esc_results_test_t_fish_species$signif_code <- with(esc_results_test_t_fish_species, 
                                               ifelse(p_value < 0.001, "***",
                                                      ifelse(p_value < 0.01, "**",
                                                             ifelse(p_value < 0.05, "*",
                                                                    ifelse(p_value < 0.1, ".", 
                                                                           " ")))))

esc_results_test_t_fish_species <- na.omit(esc_results_test_t_fish_species)
rownames(esc_results_test_t_fish_species) <- NULL
esc_results_test_t_fish_species$Island <- "ESC"

head(esc_results_test_t_fish_species)



###
#Bind scenarios
###
head(tre_results_test_t_fish_species)
head(esc_results_test_t_fish_species)

results_test_t_fish_species <- rbind(tre_results_test_t_fish_species, esc_results_test_t_fish_species)
head(results_test_t_fish_species)
#write.table(results_test_t_fish_species,"results_test_t_fish_species.csv", sep=";", dec=".",row.names = F)









#Top 10, t-stats and p-values
#Filter data
top10_tre_results_test_t_fish_species <- results_test_t_fish_species %>% 
  filter(Species %in% c("HAEAUR", "ACABAH", "HALPOE", "STEFUS", 
                        "PSEMAC", "CHASTR", "ANIVIR", "ACACHI", 
                        "STEVAR", "AZUMUL"),
         Island %in% "TRE")
top10_tre_results_test_t_fish_species <- top10_tre_results_test_t_fish_species %>%
  arrange(Island, p_value)
top10_tre_results_test_t_fish_species

#Filter data
top10_esc_results_test_t_fish_species <- results_test_t_fish_species %>% 
  filter(Species %in% c("AZUMUL", "ACACHI", "ACABAH", "ABUSAX", "DIPARG", 
                        "SPAAXI", "STEPIC", "HAEAUR", "HALPOE", "STEFUS"),
         Island %in% "ESC")
top10_esc_results_test_t_fish_species <- top10_esc_results_test_t_fish_species %>%
  arrange(Island, p_value)
top10_esc_results_test_t_fish_species



###############
#test-t analysis for families
###############

#clean R environment 
rm(list = ls())

obs_fish_species_uvc <- read.csv("obs_fish_species_uvc.csv", header= TRUE, sep=";", dec=".")
info <- read.csv("data_2023_fish_info.csv", header= TRUE, sep=";", dec=".")


obs_fish_species_uvc <- left_join(obs_fish_species_uvc, info %>%
                                                select(Species, Group, Family, Scientific_name), by = "Species")

head(obs_fish_species_uvc)

###
#Acanthuridae          
###

tre_Acanthuridae <- subset(obs_fish_species_uvc, Family == "Acanthuridae" & Island == "TRE")
head(tre_Acanthuridae)

t_test_result <- t.test(
  Abundance ~ Time, 
  data = tre_Acanthuridae, 
  var.equal = TRUE)

t_test_result
#t = -1.3706, df = 430, p-value = 0.1712


###
# TRE
###

# List of unique Family
obs_fish_species_uvc_tre <- subset(obs_fish_species_uvc, Island == "TRE")
species_list_tre <- unique(obs_fish_species_uvc_tre$Family)

# Initialize an empty data frame to store t-statistics and p-values
tre_results_test_t_fish_species <- data.frame(Family = character(), t_stat = numeric(), p_value = numeric())

# Loop over each Family
for (current_family in species_list_tre) {
  # Subset the data for the current Family
  species_data <- subset(obs_fish_species_uvc_tre, Family == current_family)
  
  # Check if 'Time' has exactly 2 unique levels
  if (length(unique(species_data$Time)) == 2) {
    # Perform t-test
    t_test_result <- t.test(Abundance ~ Time, data = species_data, var.equal = TRUE)
    
    # Store the t-statistic and p-value in the results data frame
    tre_results_test_t_fish_species <- rbind(
      tre_results_test_t_fish_species, 
      data.frame(Family = current_family, t_stat = t_test_result$statistic, p_value = t_test_result$p.value)
    )
  } else {
    # If Time doesn't have exactly 2 levels, store NA for the t-statistic and p-value
    tre_results_test_t_fish_species <- rbind(
      tre_results_test_t_fish_species, 
      data.frame(Family = current_family, t_stat = NA, p_value = NA)
    )
  }
}

tre_results_test_t_fish_species

tre_results_test_t_fish_species$p_value <- ifelse(
  round(tre_results_test_t_fish_species$p_value, 3) < 0.001, 
  "<0.001", 
  round(tre_results_test_t_fish_species$p_value, 3))

tre_results_test_t_fish_species$t_stat <- round(tre_results_test_t_fish_species$t_stat,3)

tre_results_test_t_fish_species$signif_code <- with(tre_results_test_t_fish_species, 
                                                    ifelse(p_value < 0.001, "***",
                                                           ifelse(p_value < 0.01, "**",
                                                                  ifelse(p_value < 0.05, "*",
                                                                         ifelse(p_value < 0.1, ".", 
                                                                                " ")))))

tre_results_test_t_fish_species <- na.omit(tre_results_test_t_fish_species)
rownames(tre_results_test_t_fish_species) <- NULL
tre_results_test_t_fish_species$Island <- "TRE"

head(tre_results_test_t_fish_species)


###
# ESC
###

# List of unique Family
obs_fish_species_uvc_esc <- subset(obs_fish_species_uvc, Island == "ESC")
species_list_esc <- unique(obs_fish_species_uvc_esc$Family)

# Initialize an empty data frame to store t-statistics and p-values
esc_results_test_t_fish_species <- data.frame(Family = character(), t_stat = numeric(), p_value = numeric())

# Loop over each Family
for (current_family in species_list_esc) {
  # Subset the data for the current Family
  species_data <- subset(obs_fish_species_uvc_esc, Family == current_family)
  
  # Check if 'Time' has exactly 2 unique levels
  if (length(unique(species_data$Time)) == 2) {
    # Perform t-test
    t_test_result <- t.test(Abundance ~ Time, data = species_data, var.equal = TRUE)
    
    # Store the t-statistic and p-value in the results data frame
    esc_results_test_t_fish_species <- rbind(
      esc_results_test_t_fish_species, 
      data.frame(Family = current_family, t_stat = t_test_result$statistic, p_value = t_test_result$p.value)
    )
  } else {
    # If Time doesn't have exactly 2 levels, store NA for the t-statistic and p-value
    esc_results_test_t_fish_species <- rbind(
      esc_results_test_t_fish_species, 
      data.frame(Family = current_family, t_stat = NA, p_value = NA)
    )
  }
}

esc_results_test_t_fish_species

esc_results_test_t_fish_species$p_value <- ifelse(
  round(esc_results_test_t_fish_species$p_value, 3) < 0.001, 
  "<0.001", 
  round(esc_results_test_t_fish_species$p_value, 3))

esc_results_test_t_fish_species$t_stat <- round(esc_results_test_t_fish_species$t_stat,3)

esc_results_test_t_fish_species$signif_code <- with(esc_results_test_t_fish_species, 
                                                    ifelse(p_value < 0.001, "***",
                                                           ifelse(p_value < 0.01, "**",
                                                                  ifelse(p_value < 0.05, "*",
                                                                         ifelse(p_value < 0.1, ".", 
                                                                                " ")))))

esc_results_test_t_fish_species <- na.omit(esc_results_test_t_fish_species)
rownames(esc_results_test_t_fish_species) <- NULL
esc_results_test_t_fish_species$Island <- "ESC"

head(esc_results_test_t_fish_species)     

###
#Bind scenarios
###
head(tre_results_test_t_fish_species)
head(esc_results_test_t_fish_species)

results_test_t_fish_species <- rbind(tre_results_test_t_fish_species, esc_results_test_t_fish_species)
head(results_test_t_fish_species)
results_test_t_fish_species <- results_test_t_fish_species %>%
  arrange(Family,Island)
results_test_t_fish_family <- results_test_t_fish_species
#write.table(results_test_t_fish_family,"results_test_t_fish_family.csv", sep=";", dec=".",row.names = F)



###############
#test-t analysis for functional groups
###############

library(dplyr)

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Data analysis")

#clean R environment 
rm(list = ls())

obs_fish_species_uvc <- read.csv("obs_fish_species_uvc.csv", header= TRUE, sep=";", dec=".")
info <- read.csv("data_2023_fish_info.csv", header= TRUE, sep=";", dec=".")


obs_fish_species_uvc <- left_join(obs_fish_species_uvc, info %>%
                                    select(Species, Group, Family, Scientific_name), by = "Species")

head(obs_fish_species_uvc)

###
#Acanthuridae          
###

tre_mi <- subset(obs_fish_species_uvc, Group == "MI" & Island == "TRE")
head(tre_mi)

t_test_result <- t.test(
  Abundance ~ Time, 
  data = tre_mi, 
  var.equal = TRUE)

t_test_result
#t = -12.411, df = 3814, p-value < 2.2e-16


###
# TRE
###

# List of unique Group
obs_fish_species_uvc_tre <- subset(obs_fish_species_uvc, Island == "TRE")
species_list_tre <- unique(obs_fish_species_uvc_tre$Group)

# Initialize an empty data frame to store t-statistics and p-values
tre_results_test_t_fish_species <- data.frame(Group = character(), t_stat = numeric(), p_value = numeric())

# Loop over each Group
for (current_Group in species_list_tre) {
  # Subset the data for the current Group
  species_data <- subset(obs_fish_species_uvc_tre, Group == current_Group)
  
  # Check if 'Time' has exactly 2 unique levels
  if (length(unique(species_data$Time)) == 2) {
    # Perform t-test
    t_test_result <- t.test(Abundance ~ Time, data = species_data, var.equal = TRUE)
    
    # Store the t-statistic and p-value in the results data frame
    tre_results_test_t_fish_species <- rbind(
      tre_results_test_t_fish_species, 
      data.frame(Group = current_Group, t_stat = t_test_result$statistic, p_value = t_test_result$p.value)
    )
  } else {
    # If Time doesn't have exactly 2 levels, store NA for the t-statistic and p-value
    tre_results_test_t_fish_species <- rbind(
      tre_results_test_t_fish_species, 
      data.frame(Group = current_Group, t_stat = NA, p_value = NA)
    )
  }
}

tre_results_test_t_fish_species

tre_results_test_t_fish_species$p_value <- ifelse(
  round(tre_results_test_t_fish_species$p_value, 3) < 0.001, 
  "<0.001", 
  round(tre_results_test_t_fish_species$p_value, 3))

tre_results_test_t_fish_species$t_stat <- round(tre_results_test_t_fish_species$t_stat,3)

tre_results_test_t_fish_species$signif_code <- with(tre_results_test_t_fish_species, 
                                                    ifelse(p_value < 0.001, "***",
                                                           ifelse(p_value < 0.01, "**",
                                                                  ifelse(p_value < 0.05, "*",
                                                                         ifelse(p_value < 0.1, ".", 
                                                                                " ")))))

tre_results_test_t_fish_species <- na.omit(tre_results_test_t_fish_species)
rownames(tre_results_test_t_fish_species) <- NULL
tre_results_test_t_fish_species$Island <- "TRE"

head(tre_results_test_t_fish_species)


###
# ESC
###

# List of unique Group
obs_fish_species_uvc_esc <- subset(obs_fish_species_uvc, Island == "ESC")
species_list_esc <- unique(obs_fish_species_uvc_esc$Group)

# Initialize an empty data frame to store t-statistics and p-values
esc_results_test_t_fish_species <- data.frame(Group = character(), t_stat = numeric(), p_value = numeric())

# Loop over each Group
for (current_Group in species_list_esc) {
  # Subset the data for the current Group
  species_data <- subset(obs_fish_species_uvc_esc, Group == current_Group)
  
  # Check if 'Time' has exactly 2 unique levels
  if (length(unique(species_data$Time)) == 2) {
    # Perform t-test
    t_test_result <- t.test(Abundance ~ Time, data = species_data, var.equal = TRUE)
    
    # Store the t-statistic and p-value in the results data frame
    esc_results_test_t_fish_species <- rbind(
      esc_results_test_t_fish_species, 
      data.frame(Group = current_Group, t_stat = t_test_result$statistic, p_value = t_test_result$p.value)
    )
  } else {
    # If Time doesn't have exactly 2 levels, store NA for the t-statistic and p-value
    esc_results_test_t_fish_species <- rbind(
      esc_results_test_t_fish_species, 
      data.frame(Group = current_Group, t_stat = NA, p_value = NA)
    )
  }
}

esc_results_test_t_fish_species

esc_results_test_t_fish_species$p_value <- ifelse(
  round(esc_results_test_t_fish_species$p_value, 3) < 0.001, 
  "<0.001", 
  round(esc_results_test_t_fish_species$p_value, 3))

esc_results_test_t_fish_species$t_stat <- round(esc_results_test_t_fish_species$t_stat,3)

esc_results_test_t_fish_species$signif_code <- with(esc_results_test_t_fish_species, 
                                                    ifelse(p_value < 0.001, "***",
                                                           ifelse(p_value < 0.01, "**",
                                                                  ifelse(p_value < 0.05, "*",
                                                                         ifelse(p_value < 0.1, ".", 
                                                                                " ")))))

esc_results_test_t_fish_species <- na.omit(esc_results_test_t_fish_species)
rownames(esc_results_test_t_fish_species) <- NULL
esc_results_test_t_fish_species$Island <- "ESC"

head(esc_results_test_t_fish_species)     

###
#Bind scenarios
###
head(tre_results_test_t_fish_species)
head(esc_results_test_t_fish_species)

results_test_t_fish_species <- rbind(tre_results_test_t_fish_species, esc_results_test_t_fish_species)
head(results_test_t_fish_species)
results_test_t_fish_species <- results_test_t_fish_species %>%
  arrange(Group,Island)
results_test_t_fish_group <- results_test_t_fish_species
#write.table(results_test_t_fish_group,"results_test_t_fish_group.csv", sep=";", dec=".",row.names = F)





###############
#2.4 Top 10 Fish species
###############

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Data analysis")

bootstrap_data <- read.csv("data_2023_fish_bootstrap.csv", header= TRUE, sep=";", dec=".")
table(unique(bootstrap_data$Island))
data <- data %>%  select(-MUGSPP) #It was not recorded in 2023

bootstrap_data <- mutate_at(bootstrap_data, vars(c(ABUSAX, ACABAH, ACACHI, ACACOE, ACAPOL, ACAQUA, AHLEGM, ALPAFE, 
                                                   AMBPIN, ANISUR, ANIVIR, AULSTR, AZUMUL, BALVET, BODPUL, BODRUF, CALPEN, CANFIG, CANMAC, CANPUL, CARBAR, CARCRY, CARLAT, 
                                                   CARRUB, CEPFUL, CHAFAB, CHASTR, CHASED, CHIRET, CHISPI, CHRJUB, CLEBRA, CORSPP, CORGLA, CORTHR, CRYROS, CTESAE, 
                                                   DACVOL, DERINE, DIPARG, DIPRAD, DIOHOL, DIOHYS, DULAUR, ELAFIG, FISSPP, GOBKAL, GRABRA, GYMFUN, GYMMIL, GYMMOR, GYMVIC, HAEAUR, 
                                                   HAEATL, HAEPAR, HAEPLU, HALBRA, HALDIM, HALPEN, HALPOE, HALSPP, HETCRU, HOLADS, HOLCIL, HOLTRI, HYPFIS, KYPSPP, LABCRI, 
                                                   LABNUC, LUTALE, LUTJOC, MALDEL, MALMAC, MALZAL, MICCHR, MYRJAC, OCYCHR, ODODEN, OPHTRI, OPIAUR, ORTRUB, PAGPAG, 
                                                   PARFUR, PARLIN, PARMAR, PARMOR, PARPIL, PEMSCH, PSEPER, POMARC, POMPAR, PRIARE, PSEMAC, PTERAN, RYPSAP, SCATRI, 
                                                   SCAZEL, SCOBRA, SCOIST, SCOPLU, SELSET, SERBAL, SERFLA, SPASPP, SPAAMP, SPAAXI, SPAFRO, SPATUI, SPHSPE, SPHSPP, STEFUS)), as.numeric)
str(bootstrap_data)
summary(bootstrap_data)



######
#Stats - Top 10 fish for all islands
######


selected_species <- bootstrap_data[, 3:121]
head(selected_species)

species_sums <- colSums(selected_species)
species_sums


#Calculate the mean
species_means <- round(colMeans(selected_species), 2)
species_means

#Calculate the SD
species_sd <- apply(selected_species[, names(species_means)], 2, sd)
species_sd <- round(species_sd, 2)
species_sd

#Calculate the SE
species_se <- apply(selected_species[, names(species_means)], 2, function(x) {
  sd(x) / sqrt(length(na.omit(x)))})
species_se <- round(species_se, 2)
species_se


#####
#Convert the data to a data frame and arrange it by column names
#####

stats_fish_current <- data.frame(Species = names(species_means), Mean = species_means, SD = species_sd, SE = species_se)
stats_fish_current$Time <- "Current"
stats_fish_current

rownames(stats_fish_current) <- NULL
stats_fish_current <- stats_fish_current %>%
  arrange(Species)
stats_fish_current


#Relative abundance for each species

stats_fish_current$Relative_abundance <-  round((stats_fish_current$Mean / sum(stats_fish_current$Mean)) * 100,2)
stats_fish_current <- stats_fish_current %>%
  arrange(desc(Relative_abundance))

head(stats_fish_current,10)

stats_fish_current %>%
  arrange(desc(Relative_abundance)) %>% 
  slice_head(n = 10) %>%                
  summarise(sum_top_10 = sum(Relative_abundance)) %>%  
  pull(sum_top_10) 
#70.35%

##########
#Separetely for TRE and ESC
##########

#Select just TRE and ESC
data_tre <- subset(bootstrap_data, Island %in% c("TRE"))
data_esc <- subset(bootstrap_data, Island %in% c("ESC"))

table(unique(data_tre$Island))
table(unique(data_esc$Island))

#Select species
selected_species_tre <- data_tre[, 3:121]
selected_species_esc <- data_esc[, 3:121]



##########
#Island TRE 2023
##########

#####
#Sum
#####

tre_species_sums <- colSums(selected_species_tre)
tre_species_sums
head(sort(tre_species_sums, decreasing = TRUE), n = 10)

#HAEAUR ACABAH HALPOE STEFUS PSEMAC CHASTR ANIVIR ACACHI STEVAR AZUMUL 
#774    491    425    353    208    126    123     79     71     53 

#####
#Calculate the mean
#####

tre_species_means <- round(colMeans(selected_species_tre), 2)
tre_species_means
top10_tre_mean <- head(sort(tre_species_means, decreasing = TRUE), n = 10)
top10_tre_mean

#HAEAUR ACABAH HALPOE STEFUS PSEMAC CHASTR ANIVIR ACACHI STEVAR AZUMUL 
#10.75   6.82   5.90   4.90   2.89   1.75   1.71   1.10   0.99   0.74  

#####
#Calculate the SD
#####

top10_tre_sd <- apply(selected_species_tre[, names(top10_tre_mean)], 2, sd)
top10_tre_sd <- round(top10_tre_sd, 2)
top10_tre_sd

#HAEAUR ACABAH HALPOE STEFUS PSEMAC CHASTR ANIVIR ACACHI STEVAR AZUMUL 
#17.46   7.57   5.53   6.31   4.38   2.15   4.40   3.22   3.23   2.23

#####
#Calculate the SE
#####

top10_tre_se <- apply(selected_species_tre[, names(top10_tre_mean)], 2, function(x) {
  sd(x) / sqrt(length(na.omit(x)))})
top10_tre_se <- round(top10_tre_se, 2)
top10_tre_se

#HAEAUR ACABAH HALPOE STEFUS PSEMAC CHASTR ANIVIR ACACHI STEVAR AZUMUL 
#2.06   0.89   0.65   0.74   0.52   0.25   0.52   0.38   0.38   0.26

#####
#Convert the data to a data frame and arrange it by column names
#####

top10_tre_current <- data.frame(Species = names(top10_tre_mean), Mean = top10_tre_mean, SD = top10_tre_sd, SE = top10_tre_se)
top10_tre_current$Time <- "Current"

rownames(top10_tre_current) <- NULL
top10_tre_current <- top10_tre_current %>%
  arrange(Species)
top10_tre_current


##########
#Island ESC 2023
##########

#####
#Sum
#####

esc_species_sums <- colSums(selected_species_esc)
esc_species_sums
head(sort(esc_species_sums, decreasing = TRUE), n = 10)

#AZUMUL ACACHI ACABAH ABUSAX DIPARG SPAAXI STEPIC HAEAUR HALPOE STEFUS 
#902    436    342    293    253    192    122     98     94     76

#####
#Calculate the mean
#####

esc_species_means <- round(colMeans(selected_species_esc), 2)
esc_species_means
top10_esc_mean <- head(sort(esc_species_means, decreasing = TRUE), n = 10)
top10_esc_mean

#AZUMUL ACACHI ACABAH ABUSAX DIPARG SPAAXI STEPIC HAEAUR HALPOE STEFUS 
#16.40   7.93   6.22   5.33   4.60   3.49   2.22   1.78   1.71   1.38  

#####
#Calculate the SD
#####

top10_esc_sd <- apply(selected_species_esc[, names(top10_esc_mean)], 2, sd)
top10_esc_sd <- round(top10_esc_sd, 2)
top10_esc_sd

#AZUMUL ACACHI ACABAH ABUSAX DIPARG SPAAXI STEPIC HAEAUR HALPOE STEFUS 
#46.07   9.31   9.83  10.06   8.35   8.01   3.83   6.88   2.27   3.00

#####
#Calculate the SE
#####

top10_esc_se <- apply(selected_species_esc[, names(top10_esc_mean)], 2, function(x) {
  sd(x) / sqrt(length(na.omit(x)))})
top10_esc_se <- round(top10_esc_se, 2)
top10_esc_se

#AZUMUL ACACHI ACABAH ABUSAX DIPARG SPAAXI STEPIC HAEAUR HALPOE STEFUS 
#6.21   1.25   1.32   1.36   1.13   1.08   0.52   0.93   0.31   0.40


#####
#Convert the data to a data frame and arrange it by column names
#####

top10_esc_current <- data.frame(Species = names(top10_esc_mean), Mean = top10_esc_mean, SD = top10_esc_sd, SE = top10_esc_se)
top10_esc_current$Time <- "Current"

rownames(top10_esc_current) <- NULL
top10_esc_current <- top10_esc_current %>%
  arrange(Species)
top10_esc_current



##########
#Data 2001
##########

data_2001 <- read.csv("data_2001_fish_Floeter_et_al.csv", header= TRUE, sep=";", dec=".")
head(data_2001)

#Select just TRE and ESC
data_tre_2001 <- subset(data_2001, Island %in% c("TRE"))
data_esc_2001 <- subset(data_2001, Island %in% c("ESC"))

#Select variable
data_tre_2001 <- data_tre_2001[,c(2,7,9,8)]
data_tre_2001$Time <- "Previous"
head(data_tre_2001)

data_esc_2001 <- data_esc_2001[,c(2,7,9,8)]
data_esc_2001$Time <- "Previous"
head(data_esc_2001)

#Filter data
top10_tre_previous <- data_tre_2001 %>% 
  filter(Species %in% c("HAEAUR", "ACABAH", "HALPOE", "STEFUS", 
                        "PSEMAC", "CHASTR", "ANIVIR", "ACACHI", 
                        "STEVAR", "AZUMUL"))
top10_tre_previous


top10_esc_previous <- data_esc_2001 %>% 
  filter(Species %in% c("AZUMUL", "ACACHI", "ACABAH", "ABUSAX", "DIPARG", 
                        "SPAAXI", "STEPIC", "HAEAUR", "HALPOE", "STEFUS"))
top10_esc_previous

#######
#Bind scenarios
#######

top10_tre_current
top10_tre_previous

top10_tre <- rbind(top10_tre_current, top10_tre_previous)
top10_tre <- top10_tre %>%
  arrange(Species)
top10_tre

top10_esc_current
top10_esc_previous

top10_esc <- rbind(top10_esc_current, top10_esc_previous)
top10_esc <- top10_esc %>%
  arrange(Species)
top10_esc




#######
#Figure 3
#######

top10_tre_results_test_t_fish_species
top10_esc_results_test_t_fish_species

library(forcats)

#Reorder TRE
top10_tre <- top10_tre %>%   group_by(Species) %>%
  mutate(max_mean_current = if_else(Time == "Current", Mean, NA_real_)) %>%
  ungroup() %>%  group_by(Species) %>%  mutate(max_mean_current = max(max_mean_current, na.rm = TRUE)) %>%   ungroup()
top10_tre$Species <- fct_reorder(top10_tre$Species, top10_tre$max_mean_current, .desc = TRUE)

#Reorder ESC
top10_esc <- top10_esc %>%   group_by(Species) %>%
  mutate(max_mean_current = if_else(Time == "Current", Mean, NA_real_)) %>%
  ungroup() %>%  group_by(Species) %>%  mutate(max_mean_current = max(max_mean_current, na.rm = TRUE)) %>%   ungroup()
top10_esc$Species <- fct_reorder(top10_esc$Species, top10_esc$max_mean_current, .desc = TRUE)

legend <- expression(Mean~density~(inds~40~m^-2))

#Creating the figure
plot1<-ggplot(top10_tre, aes(x = Species, y = Mean, fill = Time)) +
  theme_bw(base_size = 20) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                position = position_dodge(width = 0.9), 
                width = 0.2, 
                color = "black") +
  scale_fill_manual(values = alpha(c("blue", "gray"), .3), name=" ")+
  xlab(" ") + ylab(legend) +
  ggtitle("TRE")+
  ylim(0,23)+
  theme(axis.text.x = element_text(size = 20, angle = -50, vjust = 0.1, hjust = 0.25, 
                                   face = ifelse(1:nrow(top10_tre) %in% c(3,6,2,5,10,4), "bold", "plain")),
        legend.text = element_text(size = 20)) + 
  guides(fill = guide_legend(title = "Baseline"))
plot1

plot2<-ggplot(top10_esc, aes(x = Species, y = Mean, fill = Time)) +
  theme_bw(base_size = 20) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                position = position_dodge(width = 0.9), 
                width = 0.2, 
                color = "black") +
  scale_fill_manual(values = alpha(c("blue", "gray"), .3), name=" ")+
  xlab(" ") + ylab("") +
  ggtitle("ESC")+
    ylim(0,23)+
theme(axis.text.x = element_text(size = 20, angle = -50, vjust = 0.1, hjust = 0.25, 
                                   face = ifelse(1:nrow(top10_tre) %in% c(2,5,8,9,6,3,10), "bold", "plain")),
        legend.text = element_text(size = 20)) + 
  guides(fill = guide_legend(title = "Baseline"))
plot2

ggarrange(plot1, plot2, common.legend = TRUE, legend="right", ncol=2, labels = c("A", "B") ,
          font.label=list(color="black",size=20))
#1200 x 600




################
# Table S3. Student’s t-test comparing species abundances between the 2001 baseline (Floeter et al. 2007) and current 2023 scenarios 
################

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Data analysis")

#clean R environment 
rm(list = ls())

bootstrap_data <- read.csv("data_2023_fish_bootstrap.csv", header= TRUE, sep=";", dec=".")
table(unique(bootstrap_data$Island))
bootstrap_data <- bootstrap_data %>%  select(-MUGSPP) #It was not recorded in 2023

bootstrap_data <- mutate_at(bootstrap_data, vars(c(ABUSAX, ACABAH, ACACHI, ACACOE, ACAPOL, ACAQUA, AHLEGM, ALPAFE, 
                                                   AMBPIN, ANISUR, ANIVIR, AULSTR, AZUMUL, BALVET, BODPUL, BODRUF, CALPEN, CANFIG, CANMAC, CANPUL, CARBAR, CARCRY, CARLAT, 
                                                   CARRUB, CEPFUL, CHAFAB, CHASTR, CHASED, CHIRET, CHISPI, CHRJUB, CLEBRA, CORSPP, CORGLA, CORTHR, CRYROS, CTESAE, 
                                                   DACVOL, DERINE, DIPARG, DIPRAD, DIOHOL, DIOHYS, DULAUR, ELAFIG, FISSPP, GOBKAL, GRABRA, GYMFUN, GYMMIL, GYMMOR, GYMVIC, HAEAUR, 
                                                   HAEATL, HAEPAR, HAEPLU, HALBRA, HALDIM, HALPEN, HALPOE, HALSPP, HETCRU, HOLADS, HOLCIL, HOLTRI, HYPFIS, KYPSPP, LABCRI, 
                                                   LABNUC, LUTALE, LUTJOC, MALDEL, MALMAC, MALZAL, MICCHR, MYRJAC, OCYCHR, ODODEN, OPHTRI, OPIAUR, ORTRUB, PAGPAG, 
                                                   PARFUR, PARLIN, PARMAR, PARMOR, PARPIL, PEMSCH, PSEPER, POMARC, POMPAR, PRIARE, PSEMAC, PTERAN, RYPSAP, SCATRI, 
                                                   SCAZEL, SCOBRA, SCOIST, SCOPLU, SELSET, SERBAL, SERFLA, SPASPP, SPAAMP, SPAAXI, SPAFRO, SPATUI, SPHSPE, SPHSPP, STEFUS)), as.numeric)
str(bootstrap_data)
summary(bootstrap_data)


#####
#Island TRE 2023
######

# Select just TRE
data_tre <- subset(bootstrap_data, Island %in% c("TRE"))
data_tre <- data_tre[,c(3:120)]
str(data_tre)

#####
#Calculate the mean, SD, SE
#####

tre_2023_mean <- round(colMeans(data_tre), 2)
tre_2023_mean

tre_2023_sd <- round(apply(data_tre, 2, sd, na.rm = TRUE), 2)
tre_2023_sd

tre_2023_se <- round(tre_2023_sd / sqrt(72), 2)
tre_2023_se

#####
#Convert the data to a data frame and arrange it by column names
#####

tre_2023 <- data.frame(Species = names(tre_2023_mean), Mean = tre_2023_mean, SD = tre_2023_sd, SE = tre_2023_se)
tre_2023$Island <- "TRE"

rownames(tre_2023) <- NULL
tre_2023 <- tre_2023 %>%
  arrange(Species)

#Rename
tre_2023 <- tre_2023 %>%
  rename(
    Mean_2023 = Mean,
    SD_2023 = SD,
    SE_2023 = SE)

head(tre_2023)

#####
#Island ESC 2023
######

# Select just ESC
data_esc <- subset(bootstrap_data, Island %in% c("ESC"))
data_esc <- data_esc[,c(3:120)]
str(data_esc)

#####
#Calculate the mean, SD, SE
#####

esc_2023_mean <- round(colMeans(data_esc), 2)
esc_2023_mean

esc_2023_sd <- round(apply(data_esc, 2, sd, na.rm = TRUE), 2)
esc_2023_sd

esc_2023_se <- round(esc_2023_sd / sqrt(72), 2)
esc_2023_se

#####
#Convert the data to a data frame and arrange it by column names
#####

esc_2023 <- data.frame(Species = names(esc_2023_mean), Mean = esc_2023_mean, SD = esc_2023_sd, SE = esc_2023_se)
esc_2023$Island <- "ESC"

rownames(esc_2023) <- NULL
esc_2023 <- esc_2023 %>%
  arrange(Species)

esc_2023 <- esc_2023 %>%
  rename(
    Mean_2023 = Mean,
    SD_2023 = SD,
    SE_2023 = SE)

head(esc_2023)

##########
#Data 2001
##########

data_2001 <- read.csv("data_2001_fish_Floeter_et_al.csv", header= TRUE, sep=";", dec=".")
head(data_2001)

#Select just TRE and ESC
tre_2001 <- subset(data_2001, Island %in% c("TRE"))
esc_2001 <- subset(data_2001, Island %in% c("ESC"))

#Select variable
tre_2001 <- tre_2001[,c("Species","Mean","SD","SE","Island")] 
tre_2001 <- na.omit(tre_2001)

tre_2001 <- tre_2001 %>%
  rename(
    Mean_2001 = Mean,
    SD_2001 = SD,
    SE_2001 = SE)

head(tre_2001)

#Select variable
esc_2001 <- esc_2001[,c("Species","Mean","SD","SE","Island")]
esc_2001 <- na.omit(esc_2001)

esc_2001 <- esc_2001 %>%
  rename(
    Mean_2001 = Mean,
    SD_2001 = SD,
    SE_2001 = SE)

head(esc_2001)




#####
#Results test-t
#####

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Data analysis")

results_test_t_fish_species <- read.csv("results_test_t_fish_species.csv", header= TRUE, sep=";", dec=".")
head(results_test_t_fish_species)


#Add info to the data
info <- read.csv("data_2023_fish_info.csv", header= TRUE, sep=";", dec=".")
head(info)

results_test_t_fish_species_info <- left_join(results_test_t_fish_species, info %>%
                        select(Species, Group, Family, Scientific_name), by = "Species")

head(results_test_t_fish_species_info)

#subset
results_test_t_fish_species_info_tre <- subset(results_test_t_fish_species_info, Island %in% c("TRE"))
results_test_t_fish_species_info_esc <- subset(results_test_t_fish_species_info, Island %in% c("ESC"))
head(results_test_t_fish_species_info_tre)
head(results_test_t_fish_species_info_esc)

#Abundance (Mean, SD)
head(tre_2023)
head(esc_2023)
head(tre_2001)
head(esc_2001)


# Perform the join
results_tre_2023 <- results_test_t_fish_species_info_tre %>%
  left_join(tre_2023, by = c("Species", "Island"))
head(results_tre_2023)

results_tre_2001 <- results_test_t_fish_species_info_tre %>%
  left_join(tre_2001, by = c("Species", "Island"))
head(results_tre_2001)

results_esc_2023 <- results_test_t_fish_species_info_esc %>%
  left_join(esc_2023, by = c("Species", "Island"))
head(results_esc_2023)

results_esc_2001 <- results_test_t_fish_species_info_esc %>%
  left_join(esc_2001, by = c("Species", "Island"))
head(results_esc_2001)

# Perform the row join
table_s3 <- bind_rows(
  results_tre_2023,
  results_tre_2001,
  results_esc_2023,
  results_esc_2001)
table_s3
table_s3 <- table_s3 %>%
  arrange(Species)

# Combine rows into a single row per Species and Island
table_s3 <- table_s3 %>%
  group_by(Species, Island, t_stat, p_value, Group, Family, Scientific_name) %>%
  summarize(
    Mean_2023 = sum(Mean_2023, na.rm = TRUE),
    SD_2023 = sum(SD_2023, na.rm = TRUE),
    SE_2023 = sum(SE_2023, na.rm = TRUE),
    Mean_2001 = sum(Mean_2001, na.rm = TRUE),
    SD_2001 = sum(SD_2001, na.rm = TRUE),
    SE_2001 = sum(SE_2001, na.rm = TRUE),
    .groups = "drop")

table_s3<-as.data.frame(table_s3)

table_s3 <- table_s3 %>%
  arrange(Family, Species)

#Reorder
table_s3 <- table_s3 %>%
  select(
    Family, Scientific_name, Species, Island, t_stat, p_value, 
    Mean_2001, SD_2001, SE_2001, 
    Mean_2023, SD_2023, SE_2023)
head(table_s3)

table_s3$signif_code <- with(table_s3, 
                             ifelse(p_value < 0.001, "***",
                             ifelse(p_value < 0.01, "**",
                             ifelse(p_value < 0.05, "*",
                             ifelse(p_value < 0.1, ".",
                                    " ")))))
head(table_s3)

#save Table S3
write.table(table_s3, "table_s3.csv", sep=";", dec=".", row.names = F)




###############
#Stats
###############

###
#Spp with info
###
table(unique(table_s3$Species))
length(unique(table_s3$Species))
#64 species with both info between 2001 and 2023


table_s3 %>%
  group_by(Island) %>%
  summarise(unique_species_count = n_distinct(Species))
#TRE: 51 spp with both info between 2001 and 2023
#ESC: 62 spp with both info between 2001 and 2023

###
#Difference
###
table_s3$Difference <- table_s3$Mean_2023 - table_s3$Mean_2001
table_s3$Difference <- as.numeric(table_s3$Difference)
head(table_s3)

table_s3 %>%
  group_by(Island) %>%
  summarise(
    Positive = sum(Difference >= 0.001),
    Negative = sum(Difference <= 0.001))

table_s3 %>%
  group_by(Island) %>%
  summarise(Count = n())

#data were available 
#Island Count
#1 ESC       62
#2 TRE       51

#species p-value <0.05
species_p_value <- table_s3[as.numeric(gsub("<", "", table_s3$p_value)) < 0.051, ]
head(species_p_value)

species_p_value %>%
  group_by(Island) %>%
  summarise(Count = n())

species_p_value %>%
  group_by(Island) %>%
  summarise(
    Positive = sum(Difference > 0),
    Negative = sum(Difference < 0))

#Significant diffenrence
#  Island Positive Negative
#1    ESC        7       31
#2    TRE        8       19

#The abundances (mean ± SD) of 69 species were available at the TRE and ESC islands for both 2001 (Floeter et al. 2007) 
#and 2023 scenarios (Table S3; bootstrapped dataset, n = 127). At the TRE, data were available for 51 species, 
#with 27 significantly changing their abundance (test-t, p-value < 0.05). Among these, 19 species decreased in 
#abundance, while 8 species increased. At the ESC, data were available for 62 species, with 38 showing significant 
#abundance changes (test-t, p-value < 0.05). Of these, 31 species experienced a statistical decline and 7 increased abundance. 


table_s3 <- table_s3[,c(2:13)]
table_s3 %>%
  arrange(Difference)








###############
#2.5 Fish Family
###############

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Data analysis")

#clean R environment 
rm(list = ls())

bootstrap_data <- read.csv("data_2023_fish_bootstrap.csv", header= TRUE, sep=";", dec=".")
table(unique(bootstrap_data$Island))
bootstrap_data <- bootstrap_data %>%  select(-MUGSPP) #It was not recorded in 2023

bootstrap_data <- mutate_at(bootstrap_data, vars(c(ABUSAX, ACABAH, ACACHI, ACACOE, ACAPOL, ACAQUA, AHLEGM, ALPAFE, 
                                                   AMBPIN, ANISUR, ANIVIR, AULSTR, AZUMUL, BALVET, BODPUL, BODRUF, CALPEN, CANFIG, CANMAC, CANPUL, CARBAR, CARCRY, CARLAT, 
                                                   CARRUB, CEPFUL, CHAFAB, CHASTR, CHASED, CHIRET, CHISPI, CHRJUB, CLEBRA, CORSPP, CORGLA, CORTHR, CRYROS, CTESAE, 
                                                   DACVOL, DERINE, DIPARG, DIPRAD, DIOHOL, DIOHYS, DULAUR, ELAFIG, FISSPP, GOBKAL, GRABRA, GYMFUN, GYMMIL, GYMMOR, GYMVIC, HAEAUR, 
                                                   HAEATL, HAEPAR, HAEPLU, HALBRA, HALDIM, HALPEN, HALPOE, HALSPP, HETCRU, HOLADS, HOLCIL, HOLTRI, HYPFIS, KYPSPP, LABCRI, 
                                                   LABNUC, LUTALE, LUTJOC, MALDEL, MALMAC, MALZAL, MICCHR, MYRJAC, OCYCHR, ODODEN, OPHTRI, OPIAUR, ORTRUB, PAGPAG, 
                                                   PARFUR, PARLIN, PARMAR, PARMOR, PARPIL, PEMSCH, PSEPER, POMARC, POMPAR, PRIARE, PSEMAC, PTERAN, RYPSAP, SCATRI, 
                                                   SCAZEL, SCOBRA, SCOIST, SCOPLU, SELSET, SERBAL, SERFLA, SPASPP, SPAAMP, SPAAXI, SPAFRO, SPATUI, SPHSPE, SPHSPP, STEFUS)), as.numeric)
str(bootstrap_data)
summary(bootstrap_data)


#####
#Island TRE 2023
######

# Select just TRE
data_tre <- subset(bootstrap_data, Island %in% c("TRE"))
data_tre <- data_tre[,c(3:120)]
str(data_tre)

#####
#Calculate the mean, SD, SE
#####

tre_2023_mean <- round(colMeans(data_tre), 2)
tre_2023_mean

tre_2023_sd <- round(apply(data_tre, 2, sd, na.rm = TRUE), 2)
tre_2023_sd

tre_2023_se <- round(tre_2023_sd / sqrt(72), 2)
tre_2023_se

#####
#Convert the data to a data frame and arrange it by column names
#####

tre_2023 <- data.frame(Species = names(tre_2023_mean), Mean = tre_2023_mean, SD = tre_2023_sd, SE = tre_2023_se)
tre_2023$Time <- "Current"

rownames(tre_2023) <- NULL
tre_2023 <- tre_2023 %>%
  arrange(Species)
tre_2023

##########
#Add info to the data
##########

info <- read.csv("data_2023_fish_info.csv", header= TRUE, sep=";", dec=".")
head(info)

tre_2023 <- left_join(tre_2023, info %>%
                           select(Species, Group, Family), by = "Species")

head(tre_2023)

#Substitue NA by 0
tre_2023[is.na(tre_2023)] <- 0

#####
#Calculate the Relative abundance, SD and SE
#####

tre_2023_family <- tre_2023 %>%
  group_by(Family) %>%
  summarise(
    Relative_abundance = (sum(Mean) / sum(tre_2023$Mean)) * 100,
    SD = sqrt(sum((SD^2) * (Mean / sum(Mean)))),
    SE = sqrt(sum((SD^2) * (Mean / sum(Mean)))) / sqrt(n()))

tre_2023_family <- as.data.frame(tre_2023_family)
head(tre_2023_family)

#####
#Select 9 top Families
#####

#Arrange 
tre_2023_family <- tre_2023_family %>%
  arrange(desc(Relative_abundance)) %>%
  mutate(Family = ifelse(row_number() > 9, "Other Families", Family))

tre_2023_family


other_families_2023 <- tre_2023_family %>%
  group_by(Family) %>%
  reframe(
    Relative_abundance = ifelse(Family == "Other Families", sum(Relative_abundance), Relative_abundance),
    SD = ifelse(Family == "Other Families", sqrt(sum(SD^2, na.rm = TRUE)), SD),
    SE = ifelse(Family == "Other Families", sqrt(sum(SE^2, na.rm = TRUE)) / sqrt(n()), SE)) %>%
  filter(Family == "Other Families") %>%
  slice_head(n = 1) 

other_families_2023

tre_2023_family <- bind_rows(
  filter(tre_2023_family, Family != "Other Families"), 
  other_families_2023)

tre_2023_family <-as.data.frame(tre_2023_family)
tre_2023_family$Time <- "Current"
tre_2023_family



##########
#Island ESC 2023
##########

# Select just ESC
data_esc <- subset(bootstrap_data, Island %in% c("ESC"))
data_esc <- data_esc[,c(3:120)]
str(data_esc)

#####
#Calculate the mean, SD, SE
#####

esc_2023_mean <- round(colMeans(data_esc), 2)
esc_2023_mean

esc_2023_sd <- round(apply(data_esc, 2, sd, na.rm = TRUE), 2)
esc_2023_sd

esc_2023_se <- round(esc_2023_sd / sqrt(72), 2)
esc_2023_se

#####
#Convert the data to a data frame and arrange it by column names
#####

esc_2023 <- data.frame(Species = names(esc_2023_mean), Mean = esc_2023_mean, SD = esc_2023_sd, SE = esc_2023_se)
esc_2023$Time <- "Current"

rownames(esc_2023) <- NULL
esc_2023 <- esc_2023 %>%
  arrange(Species)
esc_2023

##########
#Add info to the data
##########

info <- read.csv("data_2023_fish_info.csv", header= TRUE, sep=";", dec=".")
head(info)

esc_2023 <- left_join(esc_2023, info %>%
                        select(Species, Group, Family), by = "Species")

head(esc_2023)

#Substitue NA by 0
esc_2023[is.na(esc_2023)] <- 0

#####
#Calculate the Relative abundance, SD and SE
#####

esc_2023_family <- esc_2023 %>%
  group_by(Family) %>%
  summarise(
    Relative_abundance = (sum(Mean) / sum(esc_2023$Mean)) * 100,
    SD = sqrt(sum((SD^2) * (Mean / sum(Mean)))),
    SE = sqrt(sum((SD^2) * (Mean / sum(Mean)))) / sqrt(n()))

esc_2023_family <- as.data.frame(esc_2023_family)
head(esc_2023_family)

#####
#Select 9 top Families
#####

#Arrange 
esc_2023_family <- esc_2023_family %>%
  arrange(desc(Relative_abundance)) %>%
  mutate(Family = ifelse(row_number() > 9, "Other Families", Family))

esc_2023_family


other_families_2023 <- esc_2023_family %>%
  group_by(Family) %>%
  reframe(
    Relative_abundance = ifelse(Family == "Other Families", sum(Relative_abundance), Relative_abundance),
    SD = ifelse(Family == "Other Families", sqrt(sum(SD^2, na.rm = TRUE)), SD),
    SE = ifelse(Family == "Other Families", sqrt(sum(SE^2, na.rm = TRUE)) / sqrt(n()), SE)) %>%
  filter(Family == "Other Families") %>%
  slice_head(n = 1) 

other_families_2023

esc_2023_family <- bind_rows(
  filter(esc_2023_family, Family != "Other Families"), 
  other_families_2023)

esc_2023_family <-as.data.frame(esc_2023_family)
esc_2023_family$Time <- "Current"
esc_2023_family




##########
#Data 2001
##########

data_2001 <- read.csv("data_2001_fish_Floeter_et_al.csv", header= TRUE, sep=";", dec=".")
data_2001$Time <- "Previous"
head(data_2001)

#####
#TRE
######

tre_2023_family

tre_2001 <- subset(data_2001, Island %in% c("TRE"))
tre_2001 <- tre_2001[,c("Species","Mean","SD","SE", "Time","Group", "Family")]

head(tre_2001)

#Substitue NA by 0
tre_2001[is.na(tre_2001)] <- 0

#####
#Calculate the Relative abundance, SD and SE
#####

tre_2001_family <- tre_2001 %>%
  group_by(Family) %>%
  summarise(
    Relative_abundance = (sum(Mean) / sum(tre_2001$Mean)) * 100,
    SD = sqrt(sum((SD^2) * (Mean / sum(Mean)))),
    SE = sqrt(sum((SD^2) * (Mean / sum(Mean)))) / sqrt(n()))

tre_2001_family <- as.data.frame(tre_2001_family)
head(tre_2001_family)

###
#Select same Families as current scenario
###

tre_2023_family

families_to_keep <- c("Haemulidae", "Acanthuridae", "Pomacentridae", 
                      "Labridae", "Mullidae", "Chaetodontidae", 
                      "Labrisomidae", "Pomacanthidae","Holocentridae")

head(tre_2001_family)

tre_2001_family

#Arrange 
tre_2001_family <- tre_2001_family %>%
  arrange(desc(Relative_abundance)) %>%
  mutate(Family = ifelse(Family %in% families_to_keep, Family, "Other Families"))

tre_2001_family

other_families_2001 <- tre_2001_family %>%
  group_by(Family) %>%
  reframe(
    Relative_abundance = ifelse(Family == "Other Families", sum(Relative_abundance), Relative_abundance),
    SD = ifelse(Family == "Other Families", sqrt(sum(SD^2, na.rm = TRUE)), SD),
    SE = ifelse(Family == "Other Families", sqrt(sum(SE^2, na.rm = TRUE)) / sqrt(n()), SE)) %>%
  filter(Family == "Other Families") %>%
  slice_head(n = 1) 

tre_2001_family <- bind_rows(
  filter(tre_2001_family, Family != "Other Families"), 
  other_families_2001)

tre_2001_family$Time <- "Previous"

tre_2001_family
tre_2023_family


#####
#ESC
######

esc_2023_family

esc_2001 <- subset(data_2001, Island %in% c("ESC"))
esc_2001 <- esc_2001[,c("Species","Mean","SD","SE", "Time","Group", "Family")]

head(esc_2001)

#Substitue NA by 0
esc_2001[is.na(esc_2001)] <- 0

#####
#Calculate the Relative abundance, SD and SE
#####

esc_2001_family <- esc_2001 %>%
  group_by(Family) %>%
  summarise(
    Relative_abundance = (sum(Mean) / sum(esc_2001$Mean)) * 100,
    SD = sqrt(sum((SD^2) * (Mean / sum(Mean)))),
    SE = sqrt(sum((SD^2) * (Mean / sum(Mean)))) / sqrt(n()))

esc_2001_family <- as.data.frame(esc_2001_family)
head(esc_2001_family)

###
#Select same Families as current scenario
###

esc_2023_family

families_to_keep <- c("Pomacentridae", "Acanthuridae", "Labridae", 
                      "Sparidae", "Haemulidae", "Pomacanthidae", 
                      "Monacanthidae", "Mullidae","Holocentridae")

head(esc_2001_family)

esc_2001_family

#Arrange 
esc_2001_family <- esc_2001_family %>%
  arrange(desc(Relative_abundance)) %>%
  mutate(Family = ifelse(Family %in% families_to_keep, Family, "Other Families"))

esc_2001_family

other_families_2001 <- esc_2001_family %>%
  group_by(Family) %>%
  reframe(
    Relative_abundance = ifelse(Family == "Other Families", sum(Relative_abundance), Relative_abundance),
    SD = ifelse(Family == "Other Families", sqrt(sum(SD^2, na.rm = TRUE)), SD),
    SE = ifelse(Family == "Other Families", sqrt(sum(SE^2, na.rm = TRUE)) / sqrt(n()), SE)) %>%
  filter(Family == "Other Families") %>%
  slice_head(n = 1) 

esc_2001_family <- bind_rows(
  filter(esc_2001_family, Family != "Other Families"), 
  other_families_2001)

esc_2001_family$Time <- "Previous"

esc_2001_family
esc_2023_family


#######
#Bind scenarios
#######

tre_2001_family
tre_2023_family

tre_family <- rbind(tre_2001_family, tre_2023_family)
tre_family <- tre_family %>%
  arrange(Family)
tre_family



esc_2001_family
esc_2023_family

esc_family <- rbind(esc_2001_family, esc_2023_family)
esc_family <- esc_family %>%
  arrange(Family)
esc_family



#######
#Figure 4
#######

library(forcats)

#Reorder TRE
tre_family <- tre_family %>%   group_by(Family) %>%
  mutate(max_mean_current = if_else(Time == "Current", Relative_abundance, NA_real_)) %>%
  ungroup() %>%  group_by(Family) %>%  mutate(max_mean_current = max(max_mean_current, na.rm = TRUE)) %>%   ungroup()
tre_family$Family <- fct_reorder(tre_family$Family, tre_family$max_mean_current, .desc = TRUE)

#Creating the figure
plot1<-ggplot(tre_family, aes(x = Family , y = Relative_abundance, fill = Time)) +
  theme_bw(base_size = 20) +
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = Relative_abundance - SE, ymax = Relative_abundance + SE), 
                position = position_dodge(width = 0.9), 
                width = 0.2, 
                color = "black") +
  scale_fill_manual(values = alpha(c("blue", "gray"), .3), name=" ")+
  xlab(" ") + ylab("Relative abundance (%)") +
  ggtitle("TRE")+ 
  ylim(0, 55)+
  theme(axis.text.x = element_text(size = 20, angle = -50, vjust = 0.1, hjust = 0.25, 
                                   face = ifelse(1:nrow(tre_family) %in% c(1,4,5,7,8,9,10), "bold", "plain")),
        legend.text = element_text(size = 20)) + 
  guides(fill=guide_legend(title="Baseline"))
plot1

#Reorder ESC
esc_family <- esc_family %>%   group_by(Family) %>%
  mutate(max_mean_current = if_else(Time == "Current", Relative_abundance, NA_real_)) %>%
  ungroup() %>%  group_by(Family) %>%  mutate(max_mean_current = max(max_mean_current, na.rm = TRUE)) %>%   ungroup()
esc_family$Family <- fct_reorder(esc_family$Family, esc_family$max_mean_current, .desc = TRUE)


plot2<-ggplot(esc_family, aes(x = Family, y = Relative_abundance, fill = Time)) +
  theme_bw(base_size = 20) +
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = Relative_abundance - SE, ymax = Relative_abundance + SE), 
                position = position_dodge(width = 0.9), 
                width = 0.2, 
                color = "black") +
  scale_fill_manual(values = alpha(c("blue", "gray"), .3), name=" ")+
  xlab(" ") + ylab("") +
  ggtitle("ESC")+ 
  ylim(0, 55)+
  theme(axis.text.x = element_text(size = 20, angle = -50, vjust = 0.1, hjust = 0.25, 
                                   face = ifelse(1:nrow(tre_family) %in% c(3,5,6,8,10), "bold", "plain")),
        legend.text = element_text(size = 20)) + 
  guides(fill=guide_legend(title="Baseline"))
plot2

ggarrange(plot1, plot2, common.legend = TRUE, legend="right", ncol=2, labels = c("A", "B") ,
          font.label=list(color="black",size=20))

#1200 x 600





###############
#2.6 Fish trophic group
###############

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Data analysis")

#clean R environment 
rm(list = ls())

bootstrap_data <- read.csv("data_2023_fish_bootstrap.csv", header= TRUE, sep=";", dec=".")
table(unique(bootstrap_data$Island))
bootstrap_data <- bootstrap_data %>%  select(-MUGSPP) #It was not recorded in 2023

bootstrap_data <- mutate_at(bootstrap_data, vars(c(ABUSAX, ACABAH, ACACHI, ACACOE, ACAPOL, ACAQUA, AHLEGM, ALPAFE, 
                                                   AMBPIN, ANISUR, ANIVIR, AULSTR, AZUMUL, BALVET, BODPUL, BODRUF, CALPEN, CANFIG, CANMAC, CANPUL, CARBAR, CARCRY, CARLAT, 
                                                   CARRUB, CEPFUL, CHAFAB, CHASTR, CHASED, CHIRET, CHISPI, CHRJUB, CLEBRA, CORSPP, CORGLA, CORTHR, CRYROS, CTESAE, 
                                                   DACVOL, DERINE, DIPARG, DIPRAD, DIOHOL, DIOHYS, DULAUR, ELAFIG, FISSPP, GOBKAL, GRABRA, GYMFUN, GYMMIL, GYMMOR, GYMVIC, HAEAUR, 
                                                   HAEATL, HAEPAR, HAEPLU, HALBRA, HALDIM, HALPEN, HALPOE, HALSPP, HETCRU, HOLADS, HOLCIL, HOLTRI, HYPFIS, KYPSPP, LABCRI, 
                                                   LABNUC, LUTALE, LUTJOC, MALDEL, MALMAC, MALZAL, MICCHR, MYRJAC, OCYCHR, ODODEN, OPHTRI, OPIAUR, ORTRUB, PAGPAG, 
                                                   PARFUR, PARLIN, PARMAR, PARMOR, PARPIL, PEMSCH, PSEPER, POMARC, POMPAR, PRIARE, PSEMAC, PTERAN, RYPSAP, SCATRI, 
                                                   SCAZEL, SCOBRA, SCOIST, SCOPLU, SELSET, SERBAL, SERFLA, SPASPP, SPAAMP, SPAAXI, SPAFRO, SPATUI, SPHSPE, SPHSPP, STEFUS)), as.numeric)
str(bootstrap_data)
summary(bootstrap_data)


#####
#Island TRE 2023
######

# Select just TRE
data_tre <- subset(bootstrap_data, Island %in% c("TRE"))
data_tre <- data_tre[,c(3:120)]
str(data_tre)

#####
#Calculate the mean, SD, SE
#####

tre_2023_mean <- round(colMeans(data_tre), 2)
tre_2023_mean

tre_2023_sd <- round(apply(data_tre, 2, sd, na.rm = TRUE), 2)
tre_2023_sd

tre_2023_se <- round(tre_2023_sd / sqrt(72), 2)
tre_2023_se

#####
#Convert the data to a data frame and arrange it by column names
#####

tre_2023 <- data.frame(Species = names(tre_2023_mean), Mean = tre_2023_mean, SD = tre_2023_sd, SE = tre_2023_se)
tre_2023$Time <- "Current"

rownames(tre_2023) <- NULL
tre_2023 <- tre_2023 %>%
  arrange(Species)
tre_2023

##########
#Add info to the data
##########

info <- read.csv("data_2023_fish_info.csv", header= TRUE, sep=";", dec=".")
head(info)

tre_2023 <- left_join(tre_2023, info %>%
                        select(Species, Group, Family), by = "Species")

head(tre_2023)

#Substitue NA by 0
tre_2023[is.na(tre_2023)] <- 0

#####
#Calculate the Relative abundance, SD and SE
#####

tre_2023_group <- tre_2023 %>%
  group_by(Group) %>%
  summarise(
    Relative_abundance = (sum(Mean) / sum(tre_2023$Mean)) * 100,
    SD = sqrt(sum((SD^2) * (Mean / sum(Mean)))),
    SE = sqrt(sum((SD^2) * (Mean / sum(Mean)))) / sqrt(n()))

tre_2023_group <- as.data.frame(tre_2023_group)
tre_2023_group$Time <- "Current"
tre_2023_group



##########
#Island ESC 2023
##########

# Select just ESC
data_esc <- subset(bootstrap_data, Island %in% c("ESC"))
data_esc <- data_esc[,c(3:120)]
str(data_esc)

#####
#Calculate the mean, SD, SE
#####

esc_2023_mean <- round(colMeans(data_esc), 2)
esc_2023_mean

esc_2023_sd <- round(apply(data_esc, 2, sd, na.rm = TRUE), 2)
esc_2023_sd

esc_2023_se <- round(esc_2023_sd / sqrt(72), 2)
esc_2023_se

#####
#Convert the data to a data frame and arrange it by column names
#####

esc_2023 <- data.frame(Species = names(esc_2023_mean), Mean = esc_2023_mean, SD = esc_2023_sd, SE = esc_2023_se)
esc_2023$Time <- "Current"

rownames(esc_2023) <- NULL
esc_2023 <- esc_2023 %>%
  arrange(Species)
esc_2023

##########
#Add info to the data
##########

info <- read.csv("data_2023_fish_info.csv", header= TRUE, sep=";", dec=".")
head(info)

esc_2023 <- left_join(esc_2023, info %>%
                        select(Species, Group, Family), by = "Species")

head(esc_2023)

#Substitue NA by 0
esc_2023[is.na(esc_2023)] <- 0

#####
#Calculate the Relative abundance, SD and SE
#####

esc_2023_group <- esc_2023 %>%
  group_by(Group) %>%
  summarise(
    Relative_abundance = (sum(Mean) / sum(esc_2023$Mean)) * 100,
    SD = sqrt(sum((SD^2) * (Mean / sum(Mean)))),
    SE = sqrt(sum((SD^2) * (Mean / sum(Mean)))) / sqrt(n()))

esc_2023_group <- as.data.frame(esc_2023_group)
esc_2023_group$Time <- "Current"
esc_2023_group





##########
#Data 2001
##########

data_2001 <- read.csv("data_2001_fish_Floeter_et_al.csv", header= TRUE, sep=";", dec=".")
head(data_2001)

#####
#TRE
######

tre_2001 <- subset(data_2001, Island %in% c("TRE"))
tre_2001 <- tre_2001[,c("Species","Mean","SD","SE", "Time","Group", "Family")]

head(tre_2001)

#Substitue NA by 0
tre_2001[is.na(tre_2001)] <- 0

#####
#Calculate the Relative abundance, SD and SE
#####

tre_2001_group <- tre_2001 %>%
  group_by(Group) %>%
  summarise(
    Relative_abundance = (sum(Mean) / sum(tre_2001$Mean)) * 100,
    SD = sqrt(sum((SD^2) * (Mean / sum(Mean)))),
    SE = sqrt(sum((SD^2) * (Mean / sum(Mean)))) / sqrt(n()))

tre_2001_group <- as.data.frame(tre_2001_group)
tre_2001_group$Time <- "Previous"
tre_2001_group


#####
#ESC
######

esc_2001 <- subset(data_2001, Island %in% c("ESC"))
esc_2001 <- esc_2001[,c("Species","Mean","SD","SE", "Time","Group", "Family")]

head(esc_2001)

#Substitue NA by 0
esc_2001[is.na(esc_2001)] <- 0

#####
#Calculate the Relative abundance, SD and SE
#####

esc_2001_group <- esc_2001 %>%
  group_by(Group) %>%
  summarise(
    Relative_abundance = (sum(Mean) / sum(esc_2001$Mean)) * 100,
    SD = sqrt(sum((SD^2) * (Mean / sum(Mean)))),
    SE = sqrt(sum((SD^2) * (Mean / sum(Mean)))) / sqrt(n()))

esc_2001_group <- as.data.frame(esc_2001_group)
esc_2001_group$Time <- "Previous"
esc_2001_group


#######
#Bind scenarios
#######

tre_2001_group
tre_2023_group

tre_group <- rbind(tre_2001_group, tre_2023_group)
tre_group <- tre_group %>%
  arrange(Group)
tre_group



esc_2001_group
esc_2023_group

esc_group <- rbind(esc_2001_group, esc_2023_group)
esc_group <- esc_group %>%
  arrange(Group)
esc_group



#######
#Figure 5
#######

library(forcats)

#Reorder TRE
tre_group <- tre_group %>%   group_by(Group) %>%
  mutate(max_mean_current = if_else(Time == "Current", Relative_abundance, NA_real_)) %>%
  ungroup() %>%  group_by(Group) %>%  mutate(max_mean_current = max(max_mean_current, na.rm = TRUE)) %>%   ungroup()
tre_group$Group <- fct_reorder(tre_group$Group, tre_group$max_mean_current, .desc = TRUE)

#Creating the figure
plot1<-ggplot(tre_group, aes(x = Group , y = Relative_abundance, fill = Time)) +
  theme_bw(base_size = 20) +
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = Relative_abundance - SE, ymax = Relative_abundance + SE), 
                position = position_dodge(width = 0.9), 
                width = 0.2, 
                color = "black") +
  scale_fill_manual(values = alpha(c("blue", "gray"), .3), name=" ")+
  xlab(" ") + ylab("Relative abundance (%)") +
  ggtitle("TRE")+ 
  ylim(0, 55)+
  theme(axis.text.x = element_text(size = 20, angle = -50, vjust = 0.1, hjust = 0.25, 
                                   face = ifelse(1:nrow(tre_group) %in% c(1,2,3,5,6,7,8), "bold", "plain")),
        legend.text = element_text(size = 20)) + 
  guides(fill=guide_legend(title="Baseline"))
plot1

#Reorder ESC
esc_group <- esc_group %>%   group_by(Group) %>%
  mutate(max_mean_current = if_else(Time == "Current", Relative_abundance, NA_real_)) %>%
  ungroup() %>%  group_by(Group) %>%  mutate(max_mean_current = max(max_mean_current, na.rm = TRUE)) %>%   ungroup()
esc_group$Group <- fct_reorder(esc_group$Group, esc_group$max_mean_current, .desc = TRUE)


plot2<-ggplot(esc_group, aes(x = Group, y = Relative_abundance, fill = Time)) +
  theme_bw(base_size = 20) +
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = Relative_abundance - SE, ymax = Relative_abundance + SE), 
                position = position_dodge(width = 0.9), 
                width = 0.2, 
                color = "black") +
  scale_fill_manual(values = alpha(c("blue", "gray"), .3), name=" ")+
  xlab(" ") + ylab("") +
  ggtitle("ESC")+ 
  ylim(0, 55)+
  theme(axis.text.x = element_text(size = 20, angle = -50, vjust = 0.1, hjust = 0.25, 
                                   face = ifelse(1:nrow(tre_group) %in% c(1,3,4,6,7,8,9,10), "bold", "plain")),
        legend.text = element_text(size = 20)) + 
  guides(fill=guide_legend(title="Baseline"))
plot2

ggarrange(plot1, plot2, common.legend = TRUE, legend="right", ncol=2, labels = c("A", "B") ,
          font.label=list(color="black",size=20))

#1200 x 600



################
# Functional groups
################

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Data analysis")

#clean R environment 
rm(list = ls())

fish_bootstrap <- read.csv("data_2023_fish_bootstrap.csv", header= TRUE, sep=";", dec=".")
table(unique(fish_bootstrap$Island))

fish_bootstrap <- mutate_at(fish_bootstrap, vars(c(ABUSAX, ACABAH, ACACHI, ACACOE, ACAPOL, ACAQUA, AHLEGM, ALPAFE, 
                                         AMBPIN, ANISUR, ANIVIR, AULSTR, AZUMUL, BALVET, BODPUL, BODRUF, CALPEN, CANFIG, CANMAC, CANPUL, CARBAR, CARCRY, CARLAT, 
                                         CARRUB, CEPFUL, CHAFAB, CHASTR, CHASED, CHIRET, CHISPI, CHRJUB, CLEBRA, CORSPP, CORGLA, CORTHR, CRYROS, CTESAE, 
                                         DACVOL, DERINE, DIPARG, DIPRAD, DIOHOL, DIOHYS, DULAUR, ELAFIG, FISSPP, GOBKAL, GRABRA, GYMFUN, GYMMIL, GYMMOR, GYMVIC, HAEAUR, 
                                         HAEATL, HAEPAR, HAEPLU, HALBRA, HALDIM, HALPEN, HALPOE, HALSPP, HETCRU, HOLADS, HOLCIL, HOLTRI, HYPFIS, KYPSPP, LABCRI, 
                                         LABNUC, LUTALE, LUTJOC, MALDEL, MALMAC, MALZAL, MICCHR, MYRJAC, MUGSPP, OCYCHR, ODODEN, OPHTRI, OPIAUR, ORTRUB, PAGPAG, 
                                         PARFUR, PARLIN, PARMAR, PARMOR, PARPIL, PEMSCH, PSEPER, POMARC, POMPAR, PRIARE, PSEMAC, PTERAN, RYPSAP, SCATRI, 
                                         SCAZEL, SCOBRA, SCOIST, SCOPLU, SELSET, SERBAL, SERFLA, SPASPP, SPAAMP, SPAAXI, SPAFRO, SPATUI, SPHSPE, SPHSPP, STEFUS)), as.numeric)
str(fish_bootstrap)
summary(fish_bootstrap)


#####
#Island TRE 2023
######

# Select just TRE
data_tre <- subset(fish_bootstrap, Island %in% c("TRE"))
data_tre <- data_tre[,c(3:121)]
str(data_tre)

#####
#Calculate the mean, SD
#####

tre_2023_mean <- round(colMeans(data_tre), 2)
tre_2023_mean

tre_2023_sd <- round(apply(data_tre, 2, sd, na.rm = TRUE), 2)
tre_2023_sd


#####
#Convert the data to a data frame and arrange it by column names
#####

tre_2023 <- data.frame(Species = names(tre_2023_mean), Mean = tre_2023_mean, SD = tre_2023_sd)
tre_2023$Island <- "TRE"

rownames(tre_2023) <- NULL
tre_2023 <- tre_2023 %>%
  arrange(Species)

#Rename
tre_2023 <- tre_2023 %>%
  rename(
    Mean_2023 = Mean,
    SD_2023 = SD)

tre_2023$Relative_abundance <- round((tre_2023$Mean_2023/sum(tre_2023$Mean_2023))*100,2)

head(tre_2023)

#####
#Island ESC 2023
######

# Select just ESC
data_esc <- subset(fish_bootstrap, Island %in% c("ESC"))
data_esc <- data_esc[,c(3:121)]
str(data_esc)

#####
#Calculate the mean, SD
#####

esc_2023_mean <- round(colMeans(data_esc), 2)
esc_2023_mean

esc_2023_sd <- round(apply(data_esc, 2, sd, na.rm = TRUE), 2)
esc_2023_sd


#####
#Convert the data to a data frame and arrange it by column names
#####

esc_2023 <- data.frame(Species = names(esc_2023_mean), Mean = esc_2023_mean, SD = esc_2023_sd)
esc_2023$Island <- "ESC"

rownames(esc_2023) <- NULL
esc_2023 <- esc_2023 %>%
  arrange(Species)

#Rename
esc_2023 <- esc_2023 %>%
  rename(
    Mean_2023 = Mean,
    SD_2023 = SD)

esc_2023$Relative_abundance <- round((esc_2023$Mean_2023/sum(esc_2023$Mean_2023))*100,2)

head(esc_2023)

#####
#Island RAS 2023
######

# Select just RAS
data_ras <- subset(fish_bootstrap, Island %in% c("RAS"))
data_ras <- data_ras[,c(3:121)]
str(data_ras)

#####
#Calculate the mean, SD
#####

ras_2023_mean <- round(colMeans(data_ras), 2)
ras_2023_mean

ras_2023_sd <- round(apply(data_ras, 2, sd, na.rm = TRUE), 2)
ras_2023_sd


#####
#Convert the data to a data frame and arrange it by column names
#####

ras_2023 <- data.frame(Species = names(ras_2023_mean), Mean = ras_2023_mean, SD = ras_2023_sd)
ras_2023$Island <- "RAS"

rownames(ras_2023) <- NULL
ras_2023 <- ras_2023 %>%
  arrange(Species)

#Rename
ras_2023 <- ras_2023 %>%
  rename(
    Mean_2023 = Mean,
    SD_2023 = SD)

ras_2023$Relative_abundance <- round((ras_2023$Mean_2023/sum(ras_2023$Mean_2023))*100,2)

head(ras_2023)


# Perform the row join
fish_abundance <- bind_rows(
  tre_2023,
  esc_2023,
  ras_2023)

head(fish_abundance)

####
#Add info to the data
####

info <- read.csv("data_2023_fish_info.csv", header= TRUE, sep=";", dec=".")
head(info)


fish_abundance <- left_join(fish_abundance, info %>%  select(Species, Family, Scientific_name, Group, IUCN, ICMBio, INMA), by = "Species")
head(fish_abundance)


sum_groups <- fish_abundance %>%
  group_by(Group,Island) %>%
  summarise(Rel_abund = sum(Relative_abundance, na.rm = TRUE))%>%
  arrange(desc(Rel_abund))

sum_groups <- as.data.frame(sum_groups)
sum_groups

mean_rel_abund <- sum_groups  %>%
  group_by(Group) %>%
  summarise(Mean_Rel_abund = mean(Rel_abund, na.rm = TRUE)) %>%
  arrange(desc(Mean_Rel_abund))

mean_rel_abund <- as.data.frame(mean_rel_abund)
mean_rel_abund$Mean_Rel_abund <-round((mean_rel_abund$Mean_Rel_abund),2)
mean_rel_abund

#Group Mean_Rel_abund
#1    RH          24.47
#2    MI          21.85
#3 MI-PL          15.81
#4    PL          13.13
#5    OM           9.80
#6    TH           7.82
#7    SI           4.45
#8    CA           1.88
#9    PI           0.82







#################################################
#3. Benthos assemblages
#################################################


###############
#3.1 nMDS
###############

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Data analysis")

#packages
library(vegan)
library(dplyr)
library(ggplot2)

#clean R environment 
rm(list = ls())

#read data
data_benthos <- read.csv("data_2023_benthos.csv", header= TRUE, sep=";", dec=".")

str(data_benthos)
summary(data_benthos)

data_var <- data_benthos[, c(3:19)] 
data_var <- sqrt(data_var)
data_var <- as.data.frame(data_var)
str(data_var)

#NMDS with 4 dimensions (k=4) and autotransforming into square root (function default)
set.seed(123)
nmds <- metaMDS(data_var, distance = "bray", k=4, autotransform = TRUE, trymax=100 )
nmds

nmds$stress

#Call:
#metaMDS(comm = data_var, distance = "bray", k = 4, trymax = 100,      autotransform = TRUE) 
#global Multidimensional Scaling using monoMDS
#Data:     wisconsin(data_var) 
#Distance: bray 
#Dimensions: 4 
#Stress:     0.1140393 

species<- envfit(nmds, data_var, permutations = 999, na.rm = TRUE)

#extract NMDS scores (x and y coordinates) for sites from newer versions of vegan package
data_scores <- as.data.frame(scores(nmds)$sites)

#extract coordinates from species
species_coord_cont <- as.data.frame(scores(species, "vectors")) * ordiArrowMul(species)
species_coord_cat <- as.data.frame(scores(species, "factors")) * ordiArrowMul(species)

#add columns to data frame
data_scores$Island = data_benthos$Island
data_scores$Depth_range = data_benthos$Depth_range

library(tibble)

species_coord_cont <- species_coord_cont %>%
  rownames_to_column("Species")

data_scores <- mutate_at(data_scores, vars(c(NMDS1, NMDS2, NMDS3, NMDS4)), as.numeric)

head(data_scores)
head(species_coord_cont,10)
species

#select specis most relevant
selected_rows <- which(species_coord_cont$Species %in% c("Algal_turf","Macroalgae","Enc_calcar","Zoanthids","Mollusc_shells",
                                                         "Ascidians","Crinoids","Milleporids","Urchin","Octocorals"))
species_coord_cont <- species_coord_cont[selected_rows, ]
species_coord_cont <- as.data.frame(species_coord_cont)
head(species_coord_cont)
str(species_coord_cont)
species_coord_cont <- mutate_at(species_coord_cont, vars(c(NMDS1, NMDS2)), as.numeric)
species <- species_coord_cont
head(species)
head(data_scores)

colours <- c("#440154", "#21918C","#D8B100")

fig_nmds_benthos <- ggplot(data_scores, aes(x = NMDS1, y = NMDS2, color=Island))+
                            geom_point(size = 3, alpha=.5)+
                            stat_ellipse(geom = "polygon",
                                         aes(color = Island, fill = Island),
                                         alpha=0.01,
                                         linetype=3,
                                         level=0.75,
                                         type="t")+
                            geom_text(data = species, aes(x = NMDS1+0.1, y = NMDS2+0.1), colour = "grey30", 
                                      fontface = "bold", label = species$Species) +
                            geom_segment(data = species, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
                                         linewidth= 1.2, alpha = 0.5, colour = "grey30") +
                            theme(axis.text.y = element_text(colour = "black"), 
                                  axis.text.x = element_text(colour = "black"), 
                                  legend.text = element_text(colour ="black"), 
                                  legend.position = "right", 
                                  panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA),
                                  legend.key=element_blank()) + 
                            labs(x = "NMDS1", colour = "Island", y = "NMDS2", shape = "Island")+
                            scale_color_manual(values =colours)+
                            scale_fill_manual(values =colours) +
                            xlim(-1.2, 1.1)+
                            scale_y_continuous(breaks = seq(-1.0, 1.5, by = 0.5))

fig_nmds_benthos 


###
#figure 7
###

#packages
library(ggpubr)

ggarrange(fig_nmds_fish, fig_nmds_benthos,
          labels = c("A","B"),
          ncol = 2, nrow =1, legend = "right",
          common.legend = TRUE)






###############
#3.2 PERMANOVA and post hoc adonis
###############

#packages
library(pairwiseAdonis)

# Distance matrix
str(data_benthos)
data_perm <- data_benthos[, c(3:19)]
dist_matrix <- vegdist(data_perm, method = "bray") 

# PERMANOVA
perm_test <- adonis2(dist_matrix ~ Island, data = data_benthos, 
                     permutations = 999, p.adjust.m ='bonferroni')
perm_test

# Perform pairwise PERMANOVA
pairwise_results <- pairwise.adonis2(dist_matrix ~ Island, data = data_benthos, 
                                     nperm = 999, p.adjust.m ='bonferroni')
pairwise_results


# PERMDISP
dispersion <- betadisper(dist_matrix, group = data_benthos$Island)
disp_test <- permutest(dispersion, permutations = 999)
disp_test

#Visualizations
plot(dispersion)
boxplot(dispersion, main = "Multivariate Dispersion (by Island)", ylab = "Distance to Centroid")

###############
#3.3 nicheROVER
###############


#packages
library(nicheROVER)
library(dplyr)
library(ellipse)
library(ggplot2)
library(ggtext)
library(here)
library(purrr)
library(patchwork)
library(readr)
library(stringr)
library(tidyr)

###
#Generate parameters
###

# generate parameter draws from the "default" posteriors of each my_data_benthos
nsamples <- 1e3
system.time({
  my_data_benthos_par <- tapply(1:nrow(data_scores), data_scores$Island ,
                             function(ii) niw.post(nsamples = nsamples, X = data_scores[ii,1:4]))
})

# various parameter plots
clrs <- c("#440154", "#21918C","#D8B100") # colors for each group

# mu1 
par(mar = c(4, 4, .5, .1)+.1, mfrow = c(1,3))
niche.par.plot(my_data_benthos_par, col = clrs, plot.index = 1)
niche.par.plot(my_data_benthos_par, col = clrs, plot.index = 2)
niche.par.plot(my_data_benthos_par, col = clrs, plot.index = 1:2)
legend("topleft", legend = names(my_data_benthos_par), fill = clrs)

# all mu 
niche.par.plot(my_data_benthos_par, col = clrs, plot.mu = TRUE, plot.Sigma = FALSE)
legend("topleft", legend = names(my_data_benthos_par), fill = clrs)

# all mu and Sigma
par(mar = c(4.2, 4.2, 2, 1)+.1)
niche.par.plot(my_data_benthos_par, col = clrs, plot.mu = TRUE, plot.Sigma = TRUE)
legend("topright", legend = names(my_data_benthos_par), fill = clrs)

###
# 2-d projections of 20 niche regions
###

nsamples <- 10
my_data_benthos_par <- tapply(1:nrow(data_scores), data_scores$Island,
                           function(ii) niw.post(nsamples = nsamples, X = data_scores[ii,1:4]))

# format data for plotting function
my_data_benthos_data <- tapply(1:nrow(data_scores), data_scores$Island , function(ii) X = data_scores[ii,1:4])

#####
# Figure S5 - Multivariate diversity space based on the four nMDS dimensions coordinates 
#####

niche.plot(niche.par = my_data_benthos_par, niche.data = my_data_benthos_data, pfrac = .6,
           iso.names = expression("NMDS1", "NMDS2", "NMDS3", "NMDS4"),
           col = clrs, xlab = expression("Multivariate diversity space"))

#750 x 650

# niche overlap plots for 95% niche region sizes
nsamples <- 1000
my_data_benthos_par <- tapply(1:nrow(data_scores), data_scores$Island ,
                           function(ii) niw.post(nsamples = nsamples, X = data_scores[ii,1:4]))

# Overlap calculation.  use nsamples = nprob = 10000 (1e4) for higher accuracy.
# the variable over_stat can be supplied directly to the overlap.plot function

over_stat <- overlap(my_data_benthos_par, nreps = nsamples, nprob = 1e3, alpha = 0.95)

over_stat_df <- over_stat %>% 
  as_tibble(rownames = "species_a") %>% 
  mutate(
    id = 1:nrow(.), 
    species_a = factor(species_a, 
                       level = c("ESC", "RAS", "TRE"))
  ) %>% 
  pivot_longer(cols = -c(id, species_a), 
               names_to = "species_b", 
               values_to = "mc_nr")  %>% 
  separate(species_b, into = c("species_c", "sample_number"), 
           sep = "\\.") %>% 
  select(-id) %>% 
  rename(species_b = species_c) %>% 
  mutate(
    species_b =  factor(species_b, 
                        level = c("ESC", "RAS", "TRE")
    ), 
    mc_nr_perc = mc_nr * 100
  )

#We then are going to take our newly made data frame and extract out 
#the mean percentage of similarities and the 2.5% and 97.5% quarantines. 
#We plot these as lines and dotted lines on our percent similarity density figure.

over_sum <- over_stat_df %>% 
  group_by(species_a, species_b) %>% 
  summarise(
    mean_mc_nr = round(mean(mc_nr_perc), digits = 2),
    qual_2.5 = round(quantile(mc_nr_perc, probs = 0.025, na.rm = TRUE), digits = 2), 
    qual_97.5 = round(quantile(mc_nr_perc, probs = 0.975, na.rm = TRUE), digits = 2)
  ) %>% 
  ungroup() %>% 
  pivot_longer(cols = -c(species_a, species_b, mean_mc_nr), 
               names_to = "percentage", 
               values_to = "mc_nr_qual") %>% 
  mutate(
    percentage = as.numeric(str_remove(percentage, "qual_"))
  ) 

#The mean overlap metrics calculated across iteratations for both niche 
#region sizes (alpha = .95 and alpha = .99) can be calculated and displayed in an array.

over_mean <- apply(over_stat, c(1:2,3), mean)*100
round(over_mean, 2)

over.cred <- apply(over_stat*100, c(1:2,3), quantile, prob = c(.025, .975), na.rm = TRUE)
round(over.cred[,,,1]) # display alpha = .95 niche region


#Overlap plot.Before you run this, make sure that you have chosen your 
#alpha level.

over_stat <- overlap(my_data_benthos_par, nreps = nsamples, nprob = 1e3, alpha = .95)

#####
#Figure 9 a - Histogram of the posterior distribution of overlap probabilities (%) 
#####

overlap.plot(over_stat, col = clrs, mean.cred.col = "black", equal.axis = TRUE,
             xlab = "Overlap Probability (%) in Multivariate diversity spaces (95%)")

#650 x 500

head(data_scores)
# posterior distribution of (mu, Sigma) for each Island 
nsamples <- 1000
my_data_benthos_par <- tapply(1:nrow(data_scores), data_scores$Island ,
                           function(ii) niw.post(nsamples = nsamples, X = data_scores[ii,1:4]))

# posterior distribution of niche size by Island 
data_size_benthos <- sapply(my_data_benthos_par, function(spec) {
  apply(spec$Sigma, 3, niche.size, alpha = .95)
})

# point estimate and standard error
rbind(est = colMeans(data_size_benthos),
      se = apply(data_size_benthos, 2, sd))

#We then need to transform niche_size into a datafame 
#for visualization and summary statistics.
library(dplyr)
niche_size_df <- data_size_benthos %>% 
  as_tibble() %>% 
  mutate(
    id = 1:nrow(.)
  ) %>% 
  pivot_longer(
    cols = -id, 
    names_to = "Islands", 
    values_to = "diversity_size"
  ) %>% 
  mutate(
    id = 1:nrow(.), 
    Islands = factor(Islands,
                     level = c("ESC", "RAS", 
                               "TRE"))
  )

#We can calculate the mean niche size, standard deviation, and standard error.
niche_size_mean <- niche_size_df %>% 
  group_by(Islands) %>% 
  summarise(
    mean_niche = round(mean(diversity_size), digits = 2), 
    sd_niche = round(sd(diversity_size), digits = 2), 
    sem_niche = round(sd(diversity_size) / sqrt(n()), digits = 2)
  )

niche_size_mean

#Islands mean_niche sd_niche sem_niche
#1 ESC          10.5      2.53      0.08
#2 RAS           4.64     1.11      0.04
#3 TRE          10.4      2.42      0.08


# boxplots

boxplot(data_size_benthos, col = clrs,
        ylab = "Multivariate diversity space size", xlab = "Island ")


head(data_size_benthos)
size_benthos <- as.data.frame(data_size_benthos)

size_benthos <- size_benthos %>%
  pivot_longer(
    cols = everything(),
    names_to = "Islands",
    values_to = "Size")

size_benthos <- as.data.frame(size_benthos)
size_benthos$Size <- as.numeric(size_benthos$Size)

head(size_benthos)
str(size_benthos)

######
#Figure 8 b
######

library(ggstatsplot)

ggbetweenstats(data = size_benthos,
               x = Islands,
               y = Size,
               plot.type = "box",
               xlab= "Islands",
               ylab= "Multivariate diversity size",
               type = "nonparametric", # ANOVA or Kruskal-Wallis
               pairwise.comparisons = TRUE,
               pairwise.display = "significant",
               centrality.plotting = FALSE,
               bf.message = FALSE)+
  ggplot2::scale_color_manual(values = c("#440154", "#21918C","#D8B100"))


colours <- c("#440154", "#21918C","#D8B100")

Fig8b <- ggplot(data = size_benthos, aes(x = Islands, y = Size, fill = Islands)) +
  theme_bw(base_size = 20) +
  geom_boxplot(alpha = 0.7, width = 0.3) +
  ylab(expression("Multivariate diversity space")) +
  xlab(expression("Islands")) +
  scale_fill_manual(values = colours) +
  theme(legend.position="none",
        axis.text = element_text(size = 20),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 20))

Fig8b
#650 x 500










###############
#3.4 Benthos cover rate
###############

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Data analysis")

#clean workspace
rm(list = ls())

#read data
benthos_2001 <- read.csv("data_2001_benthos_Floeter_et_al.csv", header= TRUE, sep=";", dec=".")
benthos_2023 <- read.csv("data_2023_benthos.csv", header= TRUE, sep=";", dec=".")

benthos_2001
benthos_2023

#####
#Island TRE
######

# Select just TRE
tre_benthos_2001 <- subset(benthos_2001, Island %in% c("TRE"))
tre_benthos_2001 <- tre_benthos_2001[,c(1,3,4)]
n <- as.numeric(120)
tre_benthos_2001$SD <- round(tre_benthos_2001$SE * sqrt(n), 2)
tre_benthos_2001 <- tre_benthos_2001[,c(1,2,4,3)]
tre_benthos_2001

tre_benthos_2023 <- subset(benthos_2023, Island %in% c("TRE"))
tre_benthos_2023 <- tre_benthos_2023[,c(3:19)]
str(tre_benthos_2023)

#####
#Calculate the mean, SE
#####

tre_2023_mean <- round(colMeans(tre_benthos_2023), 2)
tre_2023_mean

tre_2023_sd <- round(apply(tre_benthos_2023, 2, sd, na.rm = TRUE), 2)
tre_2023_sd

tre_2023_se <- round(tre_2023_mean / sqrt(36), 2)
tre_2023_se

#####
#Convert the data to a data frame and arrange it by column names
#####

tre_benthos_2023 <- data.frame(Group = names(tre_2023_mean), Mean = tre_2023_mean, SD = tre_2023_sd, SE = tre_2023_se)
tre_benthos_2023$Time <- "Current"
tre_benthos_2023$Island <- "TRE"

rownames(tre_benthos_2023) <- NULL
tre_benthos_2023 <- tre_benthos_2023 %>%
  arrange(desc(Mean))
tre_benthos_2023

#####
#Island ESC 
######

# Select just ESC
esc_benthos_2001 <- subset(benthos_2001, Island %in% c("ESC"))
esc_benthos_2001 <- esc_benthos_2001[,c(1,3,4)]
n <- as.numeric(59)
esc_benthos_2001$SD <- round(esc_benthos_2001$SE * sqrt(n), 2)
esc_benthos_2001 <- esc_benthos_2001[,c(1,2,4,3)]
esc_benthos_2001

esc_benthos_2001

esc_benthos_2023 <- subset(benthos_2023, Island %in% c("ESC"))
esc_benthos_2023 <- esc_benthos_2023[,c(3:19)]
str(esc_benthos_2023)

#####
#Calculate the mean, SE
#####

esc_2023_mean <- round(colMeans(esc_benthos_2023), 2)
esc_2023_mean

esc_2023_sd <- round(apply(esc_benthos_2023, 2, sd, na.rm = TRUE), 2)
esc_2023_sd

esc_2023_se <- round(esc_2023_mean / sqrt(36), 2)
esc_2023_se

#####
#Convert the data to a data frame and arrange it by column names
#####

esc_benthos_2023 <- data.frame(Group = names(esc_2023_mean), Mean = esc_2023_mean, SD = esc_2023_sd, SE = esc_2023_se)
esc_benthos_2023$Time <- "Current"

rownames(esc_benthos_2023) <- NULL
esc_benthos_2023 <- esc_benthos_2023 %>%
  arrange(desc(Mean))
esc_benthos_2023


#####
#Island RAS 
######

ras_benthos_2023 <- subset(benthos_2023, Island %in% c("RAS"))
ras_benthos_2023 <- ras_benthos_2023[,c(3:19)]
str(ras_benthos_2023)

#####
#Calculate the mean, SE
#####

ras_2023_mean <- round(colMeans(ras_benthos_2023), 2)
ras_2023_mean

ras_2023_sd <- round(apply(ras_benthos_2023, 2, sd, na.rm = TRUE), 2)
ras_2023_sd

ras_2023_se <- round(ras_2023_mean / sqrt(36), 2)
ras_2023_se

#####
#Convert the data to a data frame and arrange it by column names
#####

ras_benthos_2023 <- data.frame(Group = names(ras_2023_mean), Mean = ras_2023_mean, SD = ras_2023_sd, SE = ras_2023_se)
ras_benthos_2023$Time <- "Current"

rownames(ras_benthos_2023) <- NULL
ras_benthos_2023 <- ras_benthos_2023 %>%
  arrange(dras(Mean))
ras_benthos_2023

#######
#Bind scenarios
#######

#Table 1
tre_benthos_2023$Island <- "TRE"
tre_benthos_2023

esc_benthos_2023$Island <- "ESC"
esc_benthos_2023

ras_benthos_2023$Island <- "RAS"
ras_benthos_2023

tre_benthos_2023
esc_benthos_2023
ras_benthos_2023
table(unique(tre_benthos_2023$Group))
table(unique(esc_benthos_2023$Group))
table(unique(ras_benthos_2023$Group))

table_1 <- rbind(tre_benthos_2023, esc_benthos_2023,ras_benthos_2023)
table(unique(table_1$Group))

# Define the desired order for the Group column
group_order <- c("Algal_turf",	"Macroalgae",	"Enc_calcar",	"Sponges",
                 "Milleporids",	"Mass_corals",	"Zoanthids",	"Octocorals",
                 "Ascidians",	"Polychaeta",	"Crinoids",	"Urchin",
                 "Mollusc_shells",	"Bryozoa",	"Hydrozoa",	"Anthozoa",	"NI")

# Convert Group to a factor with the defined order
table_1 <- table_1 %>%
  mutate(Group = factor(Group, levels = group_order))

# Arrange the dataset by Island and Group
table_1 <- table_1 %>%
  arrange(Island, Group)

table_1
#write.table(table_1,"table_1.csv", sep=";", dec=".",row.names = F)

#TRE

tre_benthos_2001
tre_benthos_2001$Time <- "Previous"
tre_benthos_2001$Island <- "TRE"
tre_benthos_2001 <- tre_benthos_2001 %>%
  arrange(tre_benthos_2001(Mean))
tre_benthos_2001



tre_benthos_2023

tre_benthos <- rbind(tre_benthos_2023, tre_benthos_2001)
tre_benthos <- tre_benthos %>%
  arrange(Group)
tre_benthos

#ESC

esc_benthos_2001
esc_benthos_2001$Time <- "Previous"
esc_benthos_2001$Island <- "ESC"

esc_benthos_2001 <- esc_benthos_2001 %>%
  arrange(desc(Mean))
esc_benthos_2001



esc_benthos_2023

esc_benthos <- rbind(esc_benthos_2023, esc_benthos_2001)
esc_benthos <- esc_benthos %>%
  arrange(Group)
esc_benthos




#####################
#3 t-test for benthos
#####################

#2001
head(tre_benthos_2001)
head(esc_benthos_2001)

#2023
head(tre_benthos_2023)
head(esc_benthos_2023)

#2001 and 2023
head(tre_benthos)
head(esc_benthos)


#####
#Floeter et al 2001
#####

####
#TRE 2001
####

#set seed for reproducibility
set.seed(123)

head(tre_benthos_2001)

#set number of replicates
total_replicates <- 120

#create an empty data frame to store simulated data
data_2001_tre_estimated <- data.frame(replicate = rep(1:total_replicates, each = nrow(tre_benthos_2001)),
                                      Group = rep(tre_benthos_2001$Group, times = total_replicates),
                                      Relative_abundance = numeric(total_replicates * nrow(tre_benthos_2001)))
head(data_2001_tre_estimated)


# Simulate abundance data and ensure relative abundance sums to 100% per replicate
for (i in 1:nrow(tre_benthos_2001)) {
  mean_abundance <- tre_benthos_2001$Mean[i]
  sd_abundance <- tre_benthos_2001$SD[i]
  Group <- tre_benthos_2001$Group[i]
  
  # Simulate abundance values for each replicate
  data_2001_tre_estimated[data_2001_tre_estimated$Group == Group, "Abundance"] <- 
    rnorm(total_replicates, mean_abundance, sd_abundance)
}

# Ensure Abundance values are non-negative
data_2001_tre_estimated$Abundance[data_2001_tre_estimated$Abundance < 0] <- 0

# Normalize to ensure the sum of relative abundance equals 100% for each replicate
data_2001_tre_estimated <- data_2001_tre_estimated %>%
  group_by(replicate) %>%
  mutate(
    Total_abundance = sum(Abundance),
    Relative_abundance = round((Abundance / Total_abundance) * 100, 2)
  ) %>%
  ungroup()

# Adjust to make sure the sum is exactly 100% per replicate
data_2001_tre_estimated <- data_2001_tre_estimated %>%
  group_by(replicate) %>%
  mutate(
    Cumulative_abundance = cumsum(Relative_abundance),
    Relative_abundance = ifelse(row_number() == n(), 
                                100 - sum(Relative_abundance[-n()]), 
                                Relative_abundance)
  ) %>%
  select(replicate, Group, Relative_abundance) %>%
  ungroup()


head(data_2001_tre_estimated)
data_2001_tre_estimated <- as.data.frame(data_2001_tre_estimated)
data_2001_tre_estimated$Time  <- "Previous"
data_2001_tre_estimated$Island  <- "TRE"
head(data_2001_tre_estimated)



####
#ESC 2001
####

#set seed for reproducibility
set.seed(123)

head(esc_benthos_2001)

#set number of replicates
total_replicates <- 59

#create an empty data frame to store simulated data
data_2001_esc_estimated <- data.frame(replicate = rep(1:total_replicates, each = nrow(esc_benthos_2001)),
                                      Group = rep(esc_benthos_2001$Group, times = total_replicates),
                                      Relative_abundance = numeric(total_replicates * nrow(esc_benthos_2001)))
head(data_2001_esc_estimated)


# Simulate abundance data and ensure relative abundance sums to 100% per replicate
for (i in 1:nrow(esc_benthos_2001)) {
  mean_abundance <- esc_benthos_2001$Mean[i]
  sd_abundance <- esc_benthos_2001$SD[i]
  Group <- esc_benthos_2001$Group[i]
  
  # Simulate abundance values for each replicate
  data_2001_esc_estimated[data_2001_esc_estimated$Group == Group, "Abundance"] <- 
    rnorm(total_replicates, mean_abundance, sd_abundance)
}

# Ensure Abundance values are non-negative
data_2001_esc_estimated$Abundance[data_2001_esc_estimated$Abundance < 0] <- 0

# Normalize to ensure the sum of relative abundance equals 100% for each replicate
data_2001_esc_estimated <- data_2001_esc_estimated %>%
  group_by(replicate) %>%
  mutate(
    Total_abundance = sum(Abundance),
    Relative_abundance = round((Abundance / Total_abundance) * 100, 2)
  ) %>%
  ungroup()

# Adjust to make sure the sum is exactly 100% per replicate
data_2001_esc_estimated <- data_2001_esc_estimated %>%
  group_by(replicate) %>%
  mutate(
    Cumulative_abundance = cumsum(Relative_abundance),
    Relative_abundance = ifelse(row_number() == n(), 
                                100 - sum(Relative_abundance[-n()]), 
                                Relative_abundance)
  ) %>%
  select(replicate, Group, Relative_abundance) %>%
  ungroup()


head(data_2001_esc_estimated)
data_2001_esc_estimated <- as.data.frame(data_2001_esc_estimated)
data_2001_esc_estimated$Time  <- "Previous"
data_2001_esc_estimated$Island  <- "ESC"
head(data_2001_esc_estimated)

#####
#2023
#####

#TRE

tre_benthos_2023_b <- subset(benthos_2023, Island %in% c("TRE"))
tre_benthos_2023_b$Anthozoa <- as.numeric(tre_benthos_2023_b$Anthozoa)
head(tre_benthos_2023_b)

#Reshape the data
groups_tre_2023 <- tre_benthos_2023_b[, -c(1:2)]
groups_tre_2023

reshaped_benthos_tre_2023 <- groups_tre_2023 %>%
  pivot_longer(
    cols = 1:17,            
    names_to = "Group",    
    values_to = "Relative_abundance" 
  ) %>%
  mutate(
    replicate = rep(1:36, each = 17) # Add replicate column
  ) %>%
  select(replicate, Group, Relative_abundance) # Reorder columns for readability

reshaped_benthos_tre_2023<-as.data.frame(reshaped_benthos_tre_2023)
reshaped_benthos_tre_2023$Time  <- "Current"
reshaped_benthos_tre_2023$Island  <- "TRE"
head(reshaped_benthos_tre_2023)


#ESC

esc_benthos_2023_b <- subset(benthos_2023, Island %in% c("ESC"))
esc_benthos_2023_b$Anthozoa <- as.numeric(esc_benthos_2023_b$Anthozoa)
head(esc_benthos_2023_b)

#Reshape the data
groups_esc_2023 <- esc_benthos_2023_b[, -c(1:2)]
groups_esc_2023

reshaped_benthos_esc_2023 <- groups_esc_2023 %>%
  pivot_longer(
    cols = 1:17,            
    names_to = "Group",    
    values_to = "Relative_abundance" 
  ) %>%
  mutate(
    replicate = rep(1:36, each = 17) # Add replicate column
  ) %>%
  select(replicate, Group, Relative_abundance) # Reorder columns for readability

reshaped_benthos_esc_2023<-as.data.frame(reshaped_benthos_esc_2023)
reshaped_benthos_esc_2023$Time  <- "Current"
reshaped_benthos_esc_2023$Island  <- "ESC"
head(reshaped_benthos_esc_2023)



#######
#Datasets
#######

#2001
head(data_2001_tre_estimated)
head(data_2001_esc_estimated)

#2023
head(reshaped_benthos_tre_2023)
head(reshaped_benthos_esc_2023)


###
#Bind scenarios
###
head(reshaped_species_tre_2023)
head(data_2001_tre_estimated)

obs_benthos_photo_tre <- rbind(reshaped_benthos_tre_2023, data_2001_tre_estimated)
obs_benthos_photo_esc <- rbind(reshaped_benthos_esc_2023, data_2001_esc_estimated)

obs_benthos_photo <- rbind(obs_benthos_photo_tre, obs_benthos_photo_esc)
obs_benthos_photo
#write.table(obs_benthos_photo,"obs_benthos_photo.csv", sep=";", dec=".",row.names = F)



###############
#test-t analysis for benthos groups
###############

obs_benthos_photo <- read.csv("obs_benthos_photo.csv", header= TRUE, sep=";", dec=".")
obs_benthos_photo$Relative_abundance <- as.numeric(obs_benthos_photo$Relative_abundance)
obs_benthos_photo



###
# TRE
###

# List of unique species
obs_benthos_photo_tre <- subset(obs_benthos_photo, Island == "TRE")
group_list_tre <- unique(obs_benthos_photo$Group)

# Initialize an empty data frame to store t-statistics and p-values
tre_results_test_t_benthos <- data.frame(Group = character(), t_stat = numeric(), p_value = numeric())

# Loop over each species
for (species in group_list_tre) {
  # Subset the data for the current species
  species_data <- subset(obs_benthos_photo_tre, Group == species)
  
  # Check if 'Time' has exactly 2 unique levels
  if (length(unique(species_data$Time)) == 2) {
    # Perform t-test
    t_test_result <- t.test(Relative_abundance ~ Time, data = species_data, var.equal = TRUE)
    
    # Store the t-statistic and p-value in the results data frame
    tre_results_test_t_benthos <- rbind(tre_results_test_t_benthos, data.frame(Group = species, t_stat = t_test_result$statistic, p_value = t_test_result$p.value))
  } else {
    # If Time doesn't have exactly 2 levels, store NA for the t-statistic and p-value
    tre_results_test_t_benthos <- rbind(tre_results_test_t_benthos, data.frame(Group = species, t_stat = NA, p_value = NA))
  }
}

tre_results_test_t_benthos$p_value <- ifelse(
  round(tre_results_test_t_benthos$p_value, 3) < 0.001, 
  "<0.001", 
  round(tre_results_test_t_benthos$p_value, 3))

tre_results_test_t_benthos$t_stat <- round(tre_results_test_t_benthos$t_stat,3)

tre_results_test_t_benthos$signif_code <- with(tre_results_test_t_benthos, 
                                                    ifelse(p_value < 0.001, "***",
                                                           ifelse(p_value < 0.01, "**",
                                                                  ifelse(p_value < 0.05, "*",
                                                                         ifelse(p_value < 0.1, ".", 
                                                                                " ")))))

tre_results_test_t_benthos <- na.omit(tre_results_test_t_benthos)
rownames(tre_results_test_t_benthos) <- NULL
tre_results_test_t_benthos$Island <- "TRE"

head(tre_results_test_t_benthos)



###
# ESC
###

# List of unique species
obs_benthos_photo_esc <- subset(obs_benthos_photo, Island == "ESC")
group_list_esc <- unique(obs_benthos_photo$Group)

# Initialize an empty data frame to store t-statistics and p-values
esc_results_test_t_benthos <- data.frame(Group = character(), t_stat = numeric(), p_value = numeric())

# Loop over each species
for (species in group_list_esc) {
  # Subset the data for the current species
  species_data <- subset(obs_benthos_photo_esc, Group == species)
  
  # Check if 'Time' has exactly 2 unique levels
  if (length(unique(species_data$Time)) == 2) {
    # Perform t-test
    t_test_result <- t.test(Relative_abundance ~ Time, data = species_data, var.equal = TRUE)
    
    # Store the t-statistic and p-value in the results data frame
    esc_results_test_t_benthos <- rbind(esc_results_test_t_benthos, data.frame(Group = species, t_stat = t_test_result$statistic, p_value = t_test_result$p.value))
  } else {
    # If Time doesn't have exactly 2 levels, store NA for the t-statistic and p-value
    esc_results_test_t_benthos <- rbind(esc_results_test_t_benthos, data.frame(Group = species, t_stat = NA, p_value = NA))
  }
}

esc_results_test_t_benthos$p_value <- ifelse(
  round(esc_results_test_t_benthos$p_value, 3) < 0.001, 
  "<0.001", 
  round(esc_results_test_t_benthos$p_value, 3))

esc_results_test_t_benthos$t_stat <- round(esc_results_test_t_benthos$t_stat,3)

esc_results_test_t_benthos$signif_code <- with(esc_results_test_t_benthos, 
                                               ifelse(p_value < 0.001, "***",
                                                      ifelse(p_value < 0.01, "**",
                                                             ifelse(p_value < 0.05, "*",
                                                                    ifelse(p_value < 0.1, ".", 
                                                                           " ")))))

esc_results_test_t_benthos <- na.omit(esc_results_test_t_benthos)
rownames(esc_results_test_t_benthos) <- NULL
esc_results_test_t_benthos$Island <- "ESC"

head(esc_results_test_t_benthos)

###
#Bind scenarios
###
head(tre_results_test_t_benthos)
head(esc_results_test_t_benthos)

results_test_t_benthos <- rbind(tre_results_test_t_benthos, esc_results_test_t_benthos)
head(results_test_t_benthos)


tre_benthos_2001$Island <- "TRE"
tre_benthos_2023$Island <- "TRE"
esc_benthos_2001$Island <- "ESC"
esc_benthos_2023$Island <- "ESC"

tre_benthos_2001 <- tre_benthos_2001 %>%
  arrange(Group)
esc_benthos_2001 <- esc_benthos_2001 %>%
  arrange(Group)
tre_benthos_2023 <- tre_benthos_2023 %>%
  arrange(Group)
esc_benthos_2023 <- esc_benthos_2023 %>%
  arrange(Group)

head(tre_results_test_t_benthos)
head(esc_results_test_t_benthos)

head(tre_benthos_2001)
head(tre_benthos_2023)

head(esc_benthos_2001)
head(esc_benthos_2023)

tre_results_test_t_benthos <- tre_results_test_t_benthos %>%
  arrange(Group)
esc_results_test_t_benthos <- esc_results_test_t_benthos %>%
  arrange(Group)

unique(tre_results_test_t_benthos$Group)
unique(tre_benthos_2023$Group)


# Perform the join
results_tre_2023 <- tre_results_test_t_benthos %>%
  left_join(tre_benthos_2023, by = c("Group", "Island"))
head(results_tre_2023)

results_tre_2001 <- tre_results_test_t_benthos %>%
  left_join(tre_benthos_2001, by = c("Group", "Island"))
head(results_tre_2001)

results_esc_2023 <- esc_results_test_t_benthos %>%
  left_join(esc_benthos_2023, by = c("Group", "Island"))
head(results_esc_2023)

results_esc_2001 <- esc_results_test_t_benthos %>%
  left_join(esc_benthos_2001, by = c("Group", "Island"))
head(results_esc_2001)

# Perform the row join
benthos_t_test_sig <- bind_rows(
  results_tre_2023,
  results_tre_2001,
  results_esc_2023,
  results_esc_2001)
benthos_t_test_sig

results_test_t_benthos <- benthos_t_test_sig %>%
  arrange(Group,Island,Time)

results_test_t_benthos
#write.table(results_test_t_benthos, "results_test_t_benthos.csv", sep=";", dec=".", row.names = F)




tre_benthos

#######
#Figure 6
#######

library(forcats)
library(dplyr)

#Reorder TRE
tre_benthos <- tre_benthos %>%   group_by(Group) %>%
  mutate(max_mean_current = if_else(Time == "Current", Mean, NA_real_)) %>%
  ungroup() %>%  group_by(Group) %>%  mutate(max_mean_current = max(max_mean_current, na.rm = TRUE)) %>%   ungroup()
tre_benthos$Group <- fct_reorder(tre_benthos$Group, tre_benthos$max_mean_current, .desc = TRUE)
tre_benthos <- tre_benthos %>% 
  filter(Group %in% c("Algal_turf", "Macroalgae", "Enc_calcar", "Octocorals", 
                        "Zoanthids", "Sponges", "Mass_corals", "Milleporids"))
         
plot1<-ggplot(tre_benthos, aes(x = Group , y = Mean, fill = Time)) +
  theme_bw(base_size = 20) +
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                position = position_dodge(width = 0.9), 
                width = 0.2, 
                color = "black") +
  scale_fill_manual(values = alpha(c("blue", "gray"), .3), name=" ")+
  xlab(" ") + ylab("Cover rate (%)") +
  ggtitle("TRE")+ 
  ylim(0, 50)+
  theme(axis.text.x = element_text(size = 20, angle = -50, vjust = 0.1, hjust = 0.25, 
                                   face = ifelse(1:nrow(tre_benthos) %in% c(2,4,5,6,7,8), "bold", "plain")),
        legend.text = element_text(size = 20)) + 
  guides(fill = guide_legend(title = "Baseline"))
plot1

#Reorder ESC
esc_benthos <- esc_benthos %>%   group_by(Group) %>%
  mutate(max_mean_current = if_else(Time == "Current", Mean, NA_real_)) %>%
  ungroup() %>%  group_by(Group) %>%  mutate(max_mean_current = max(max_mean_current, na.rm = TRUE)) %>%   ungroup()
esc_benthos$Group <- fct_reorder(esc_benthos$Group, esc_benthos$max_mean_current, .desc = TRUE)
esc_benthos <- esc_benthos %>% 
  filter(Group %in% c("Algal_turf", "Macroalgae", "Enc_calcar", "Octocorals", 
                      "Zoanthids", "Sponges", "Mass_corals", "Milleporids"))

plot2<-ggplot(esc_benthos, aes(x = Group , y = Mean, fill = Time)) +
  theme_bw(base_size = 20) +
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                position = position_dodge(width = 0.9), 
                width = 0.2, 
                color = "black") +
  scale_fill_manual(values = alpha(c("blue", "gray"), .3), name=" ")+
  xlab(" ") + ylab(" ") +
  ggtitle("ESC")+ 
  ylim(0, 50)+
  theme(axis.text.x = element_text(size = 20, angle = -50, vjust = 0.1, hjust = 0.25, 
                                   face = ifelse(1:nrow(tre_benthos) %in% c(4,6,8), "bold", "plain")),
        legend.text = element_text(size = 20)) + 
  guides(fill = guide_legend(title = "Baseline"))
plot2

###
#Figure 6
###

#packages
library(ggpubr)

ggarrange(plot1, plot2,
          ncol = 2, nrow =1, legend = "right",
          common.legend = TRUE)

#1200 x 600





#################################################
#4. Supplementary material
#################################################

#################
#4.1 Habitat complexity
#################

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Data analysis")

#packages
library(vegan)
library(dplyr)
library(ggplot2)

#Clean R environment 
rm(list = ls())

#Read our data
data_complex <- read.csv("data_2023_fish_abun_per_trans.csv", header= TRUE, sep=";", dec=".")

data_complex <- data_complex %>%
  select(ID, Cmean, Island)
data_complex

head(data_complex)

habitat_complexity <- data_complex

head(habitat_complexity)

#Habitat complexity

habitat_complexity$Cmean <- as.numeric(habitat_complexity$Cmean)
summary(habitat_complexity$Cmean)



#Figure S3

colours <- c("#440154", "#21918C","#D8B100")

ggplot(habitat_complexity, aes(x=Island, y=Cmean, fill = Island)) +
  geom_boxplot(alpha = 0.5,width=0.5) +
  theme_light(base_size=20) +
  scale_fill_manual(values=colours) +
  theme(legend.position="none",
        axis.text = element_text(size = 20),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 20)) +
  xlab("") + ylab("Habitat complexity")  +ylim(c(1,5.5))

#500 x 500

mean <- tapply(habitat_complexity$Cmean, habitat_complexity$Island, mean, na.rm = TRUE)
sd <- tapply(habitat_complexity$Cmean, habitat_complexity$Island, sd, na.rm = TRUE)

round((mean),2)
round((sd),2)


model<-aov(Cmean ~ Island, habitat_complexity)
summary(model) #Difference, p value =  0.0651 .

#Tests

library(car)
leveneTest(model) #p-value =  0.07, we can assume the homogeneity of variance
shapiro.test(model$residuals) #p-value < 0.05, we can NOT assume the normality
TukeyHSD(model, conf.level=.95)

#Non-parametric 
kruskal.test(Cmean ~ Island, data = habitat_complexity) #Difference
library(FSA)

#Perform Dunn's Test with Bonferroni correction for p-values
dunnTest(Cmean ~ Island, data = habitat_complexity,method="bonferroni")  #Difference


#Habitat complexity exhibited significant differences among islands (Kruskal-Wallis, p < .001),
#being highest in RAS (3.26 ± 0.87; mean ± SD), followed by ESC (2.98 ± 0.84) and TRE (2.76 ± 0.88; Fig. S4). 


#################################################
#4.2 Length frequency
#################################################

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Data analysis")

#packages
library(vegan)
library(dplyr)
library(ggplot2)

#Clean R environment 
rm(list = ls())

#Read our data
data <- read.csv("fish_length.csv", header= TRUE, sep=";", dec=".")


data <- mutate_at(data, vars(c(ABUSAX, ACABAH, ACACHI, ACACOE, ACAPOL, ACAQUA, AHLEGM, ALUSCR, ALPAFE, 
                               AMBPIN, ANISUR, ANIVIR, AULSTR, AZUMUL, BALVET, BODPUL, BODRUF, CALPEN, CANFIG, CANMAC, CANPUL, CARBAR, CARCRY, CARLAT, 
                               CARRUB, CEPFUL, CHAFAB, CHASTR, CHASED, CHIRET, CHISPI, CHRJUB, CLEBRA, CORSPP, CORGLA, CORTHR, CRYROS, CTESAE, 
                               DACVOL, DERINE, DIPARG, DIPRAD, DIOHOL, DIOHYS, DULAUR, ELAFIG, FISSPP, GOBKAL, GRABRA, GYMFUN, GYMMIL, GYMMOR, GYMVIC, HAEAUR, 
                               HAEATL, HAEPAR, HAEPLU, HALBRA, HALDIM, HALPEN, HALPOE, HALSPP, HETCRU, HOLADS, HOLCIL, HOLTRI, HYPFIS, KYPSPP, LABCRI, 
                               LABNUC, LUTALE, LUTJOC, MALDEL, MALZAL, MICCHR, MUGSPP, MYRJAC, OCYCHR, ODODEN, OPHTRI, OPIAUR, ORTRUB, PAGPAG, 
                               PARFUR, PARLIN, PARMAR, PARMOR, PARPIL, PEMSCH, PSEPER, POMARC, POMPAR, PRIARE, PSEMAC, PTERAN, RYPSAP, SCATRI, 
                               SCAZEL, SCOBRA, SCOIST, SCOPLU, SELSET, SERBAL, SERFLA, SPASPP, SPAAMP, SPAAXI, SPAFRO, SPATUI, SPHSPE, SPHSPP, STEFUS)), as.numeric)

str(data)


# Load the necessary library
library(tidyr)
library(dplyr)
library(hrbrthemes)

# Transform columns G to ED into rows
melted_df <- data %>%
  pivot_longer(
    cols = ABUSAX:THANOR,  # Adjust if your column range differs
    names_to = "Species",
    values_to = "Individuals"
  )

# Summing the values for each group
summed_df <- melted_df %>%
  group_by(ID, Island, Site, Date, Class_size, Class, Species) %>%
  summarise(Individuals = sum(Individuals, na.rm = TRUE), .groups = "drop")

summed_df

summed_df <- summed_df %>%
  filter(Individuals != 0)

summed_df


##########
#Add info to the data
##########

info <- read.csv("data_2023_fish_info.csv", header= TRUE, sep=";", dec=".")
head(info)

length <- left_join(summed_df, info %>%
                        select(Species, Group, Family), by = "Species")

head(length)


length <- length %>%
  group_by(Island, Family, Class) %>%
  summarise(
    Individuals = sum(Individuals))

length <- as.data.frame(length)
length


#########
#Group by Family
#########

#Acanthuridae
acanthuridae_data <- length %>%
  filter(Family == "Acanthuridae")

acanthuridae_data <- as.data.frame(acanthuridae_data)
str(acanthuridae_data)

#Epinephelidae
epinephelidae_data <- length %>%
  filter(Family == "Epinephelidae")

epinephelidae_data <- as.data.frame(epinephelidae_data)
str(epinephelidae_data)

#Haemulidae
haemulidae_data <- length %>%
  filter(Family == "Haemulidae")

haemulidae_data <- as.data.frame(haemulidae_data)
str(haemulidae_data)

#Holocentridae
holocentridae_data <- length %>%
  filter(Family == "Holocentridae")

holocentridae_data <- as.data.frame(holocentridae_data)
str(holocentridae_data)

#Labridae
labridae_data <- length %>%
  filter(Family == "Labridae")

labridae_data <- as.data.frame(labridae_data)
str(labridae_data)

#Lutjanidae
lutjanidae_data <- length %>%
  filter(Family == "Lutjanidae")

lutjanidae_data <- as.data.frame(lutjanidae_data)
str(lutjanidae_data)

#Monacanthidae
monacanthidae_data <- length %>%
  filter(Family == "Monacanthidae")

monacanthidae_data <- as.data.frame(monacanthidae_data)
str(monacanthidae_data)

#Pomacentridae
pomacentridae_data <- length %>%
  filter(Family == "Pomacentridae")

pomacentridae_data <- as.data.frame(pomacentridae_data)
str(pomacentridae_data)

#Sparidae
sparidae_data <- length %>%
  filter(Family == "Sparidae")

sparidae_data <- as.data.frame(sparidae_data)
str(sparidae_data)


#########
#Replicate rows based on Individuals
#########

#Acanthuridae
data_aca <- acanthuridae_data[rep(seq_len(nrow(acanthuridae_data)), acanthuridae_data$Individuals), ]
str(data_aca)
data_aca$Class<-as.numeric(data_aca$Class)

#Epinephelidae
data_epi <- epinephelidae_data[rep(seq_len(nrow(epinephelidae_data)), epinephelidae_data$Individuals), ]
str(data_epi)
data_epi$Class<-as.numeric(data_epi$Class)

#Haemulidae 
data_hae <- haemulidae_data[rep(seq_len(nrow(haemulidae_data)), haemulidae_data$Individuals), ]
str(data_hae)
data_hae$Class<-as.numeric(data_hae$Class)

#Holocentridae
data_hol <- holocentridae_data[rep(seq_len(nrow(holocentridae_data)), holocentridae_data$Individuals), ]
str(data_hol)
data_hol$Class<-as.numeric(data_hol$Class)

#Labridae
data_lab <- labridae_data[rep(seq_len(nrow(labridae_data)), labridae_data$Individuals), ]
str(data_lab)
data_lab$Class<-as.numeric(data_lab$Class)

#Lutjanidae
data_lut <- lutjanidae_data[rep(seq_len(nrow(lutjanidae_data)), lutjanidae_data$Individuals), ]
str(data_lut)
data_lut$Class<-as.numeric(data_lut$Class)
                           
#Monacanthidae
data_mon <- monacanthidae_data[rep(seq_len(nrow(monacanthidae_data)), monacanthidae_data$Individuals), ]
str(data_mon)
data_mon$Class<-as.numeric(data_mon$Class)

#Pomacentridae
data_pom <- pomacentridae_data[rep(seq_len(nrow(pomacentridae_data)), pomacentridae_data$Individuals), ]
str(data_pom)
data_pom$Class<-as.numeric(data_pom$Class)

#Sparidae
data_spa <- sparidae_data[rep(seq_len(nrow(sparidae_data)), sparidae_data$Individuals), ]
str(data_spa)
data_mon$Class<-as.numeric(data_mon$Class)

#########
#Figure S2
#########

####
#Acanthuridae
####

aca <- ggplot(data_aca, aes(x=Class, fill=Island)) +
                    geom_histogram(binwidth = 5, color="#e9ecef", alpha=0.6, position = 'identity') +
                    scale_fill_manual(values=c("#440154", "#21918C","#D8B100")) +
                    facet_wrap(~ Island, ncol = 1) +
                    theme_ipsum() +
                    labs(x = "Size", y = "Frequency", title = "Acanthuridae") +
                    theme(plot.title = element_text(size=12),)+
                    theme(legend.position = "none")+
                    theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),)+ 
                    xlim(c(0, 30))
aca
#400 x 600

####
#Epinephelidae
####

epi <- ggplot(data_epi, aes(x=Class, fill=Island)) +
                geom_histogram(binwidth = 5, color="#e9ecef", alpha=0.6, position = 'identity') +
                scale_fill_manual(values=c("#440154", "#21918C","#D8B100")) +
                facet_wrap(~ Island, ncol = 1) +
                theme_ipsum() +
                labs(x = "Size", y = "Frequency", title = "Epinephelidae") +
                theme(plot.title = element_text(size=12),)+
                theme(legend.position = "none")+
                theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),)

epi
#400 x 600

####
#Haemulidae
####

hae <- ggplot(data_hae, aes(x=Class, fill=Island)) +
                    geom_histogram(binwidth = 5, color="#e9ecef", alpha=0.6, position = 'identity') +
                    scale_fill_manual(values=c("#440154", "#21918C","#D8B100")) +
                    facet_wrap(~ Island, ncol = 1) +
                    theme_ipsum() +
                    labs(x = "Size", y = "Frequency", title = "Haemulidae") +
                    theme(plot.title = element_text(size=12),)+
                    theme(legend.position = "none")+
                    theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),)+ 
                    xlim(c(0, 30))


hae
#400 x 600

####
#Holocentridae
####

hol <- ggplot(data_hol, aes(x=Class, fill=Island)) +
                geom_histogram(binwidth = 5, color="#e9ecef", alpha=0.6, position = 'identity') +
                scale_fill_manual(values=c("#440154", "#21918C","#D8B100")) +
                facet_wrap(~ Island, ncol = 1) +
                theme_ipsum() +
                labs(x = "Size", y = "Frequency", title = "Holocentridae") +
                theme(plot.title = element_text(size=12),)+
                theme(legend.position = "none")+
                theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),)

hol
#400 x 600

####
#Labridae
####

lab <- ggplot(data_lab, aes(x=Class, fill=Island)) +
                geom_histogram(binwidth = 5, color="#e9ecef", alpha=0.6, position = 'identity') +
                scale_fill_manual(values=c("#440154", "#21918C","#D8B100")) +
                facet_wrap(~ Island, ncol = 1) +
                theme_ipsum() +
                labs(x = "Size", y = "Frequency", title = "Labridae") +
                theme(plot.title = element_text(size=12),)+
                theme(legend.position = "none")+
                theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),)

lab
#400 x 600

####
#Lutjanidae
####

lut <- ggplot(data_lut, aes(x=Class, fill=Island)) +
              geom_histogram(binwidth = 5, color="#e9ecef", alpha=0.6, position = 'identity') +
              scale_fill_manual(values=c("#440154", "#21918C","#D8B100")) +
              facet_wrap(~ Island, ncol = 1) +
              theme_ipsum() +
              labs(x = "Size", y = "Frequency", title = "Lutjanidae") +
              theme(plot.title = element_text(size=12),)+
              theme(legend.position = "none")+
              theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),)

lut
#400 x 600

####
#Monacanthidae
####

mon <- ggplot(data_mon, aes(x=Class, fill=Island)) +
              geom_histogram(binwidth = 5, color="#e9ecef", alpha=0.6, position = 'identity') +
              scale_fill_manual(values=c("#440154", "#21918C","#D8B100")) +
              facet_wrap(~ Island, ncol = 1) +
              theme_ipsum() +
              labs(x = "Size", y = "Frequency", title = "Monacanthidae") +
              theme(plot.title = element_text(size=12),)+
              theme(legend.position = "none")+
              theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),)

mon
#400 x 600

####
#Pomacentridae
####

pom <- ggplot(data_pom, aes(x=Class, fill=Island)) +
                geom_histogram(binwidth = 5, color="#e9ecef", alpha=0.6, position = 'identity') +
                scale_fill_manual(values=c("#440154", "#21918C","#D8B100")) +
                facet_wrap(~ Island, ncol = 1) +
                theme_ipsum() +
                labs(x = "Size", y = "Frequency", title = "Pomacentridae") +
                theme(plot.title = element_text(size=12),)+
                theme(legend.position = "none")+
                theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),)+ 
                xlim(c(0, 25))

pom
#400 x 600

####
#Sparidae
####

spa <- ggplot(data_spa, aes(x=Class, fill=Island)) +
                geom_histogram(binwidth = 5, color="#e9ecef", alpha=0.6, position = 'identity') +
                scale_fill_manual(values=c("#440154", "#21918C","#D8B100")) +
                facet_wrap(~ Island, ncol = 1) +
                theme_ipsum() +
                labs(x = "Size", y = "Frequency", title = "Sparidae") +
                theme(plot.title = element_text(size=12),)+
                theme(legend.position = "none")+
                theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),)+
                xlim(c(0,25))


spa
#400 x 600


#END
