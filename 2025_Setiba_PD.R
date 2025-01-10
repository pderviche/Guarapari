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
#Summary
#################################################

#1. Balancing N samples
######1.1 Rarefaction to define the smallest sampling effort  ###NOT DONE
######1.2 Filtering the data
######1.3 Resample the data randomly by bootstrapping
######1.4 Save dataset

#2. Fish assemblages
######2.1 nMDS
######2.2 PERMANOVA and post hoc adonis
######2.3 nicheROVER
######2.4 Top 10 Fish species
######2.5 Fish Family
######2.6 Fish trophic group

#3. Benthos assemblages
######3.1 nMDS
######3.2 PERMANOVA
######3.3 nicheROVER
######3.4 Benthos cover rate

#4. Supplementary material
######4.1 Habitat complexity
######4.1 Length frequency ###NOT DONE

#################################################
#1. Balancing N samples
#################################################

###############
#1.1 Rarefaction to define the smallest sampling effort 
###############

#Clean R environment 
rm(list = ls())

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Review/Data analysis")

#packages
library("iNEXT")
library(ggplot2)
library(gridExtra)
library(grid)
library(ggpubr)

#load
esc <-read.csv2("rarefaction_islands_esc.csv",head = TRUE)
ras <-read.csv2("rarefaction_islands_ras.csv",head = TRUE)
tre <-read.csv2("rarefaction_islands_tre.csv",head = TRUE)

#transform
esc_matrix<-as.matrix(apply(esc[,-1],2,as.integer))
row.names(esc_matrix) <- esc[,1]
ras_matrix<-as.matrix(apply(ras[,-1],2,as.integer))
row.names(ras_matrix) <- ras[,1]
tre_matrix<-as.matrix(apply(tre[,-1],2,as.integer))
row.names(tre_matrix) <- tre[,1]

#list
setiba = list(ESC = esc_matrix, RAS = ras_matrix, TRE = tre_matrix)
show(setiba)

colours <- c("#440154", "#21918C","#FDE725")

#per unit
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


#per individual
ind <-read.csv2("individuals.csv",head = TRUE)
ind$spp<-NULL
fig_ind <- iNEXT(ind, datatype="abundance")
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

###############
#1.2 Filtering the data
###############

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Review/Data analysis")

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
#1.3 Resample the data randomly by bootstrapping
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
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Review/Data analysis")

#packages
library(vegan)
library(dplyr)
library(ggplot2)

#clean R environment 
rm(list = ls())

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

data_var <- bootstrap_data[, c(3:121)] 
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

#save 
#write.table(data_scores, "nmds_scores_fish.csv", sep=";", dec=".", row.names = FALSE)
#write.table(species_coord_cont, "nmds_species_coord_cont_fish.csv", sep=";", dec=".", row.names = T)

#read data
data_scores <- read.csv("nmds_scores_fish.csv", header= TRUE, sep=";", dec=",")
head(data_scores)
str(data_scores)
data_scores <- mutate_at(data_scores, vars(c(NMDS1, NMDS2, NMDS3, NMDS4)), as.numeric)

#read data
species_coord_cont <- read.csv("nmds_species_coord_cont_fish.csv", header= TRUE, sep=";", dec=".")
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
  geom_text(data = species, aes(x = NMDS1*30-0.1, y = NMDS2*30+0.1), colour = "grey30", 
            fontface = "bold", label = species$Species) +
  geom_segment(data = species, aes(x = 0, y = 0, xend = NMDS1*30, yend = NMDS2*30), 
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

# Distance matrix (replace `your_data` with your actual dataset)
str(bootstrap_data)
data_perm <- bootstrap_data[, c(3:121)]
dist_matrix <- vegdist(data_perm, method = "bray") 

# PERMANOVA
perm_test <- adonis2(dist_matrix ~ Island, data = bootstrap_data, 
                     permutations = 999, p.adjust.m ='bonferroni')
perm_test

# Perform pairwise PERMANOVA
pairwise_results <- pairwise.adonis2(dist_matrix ~ Island, data = bootstrap_data, 
                                     nperm = 999, p.adjust.m ='bonferroni')
pairwise_results


###############
#2.3 nicheROVER
###############


















###############
#2.4 Top 10 Fish species
###############

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Review/Data analysis")

#clean R environment 
rm(list = ls())

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
                color = "darkgray") +
  scale_fill_manual(values = alpha(c("blue", "gray"), .3), name=" ")+
  xlab(" ") + ylab(legend) +
  ggtitle("TRE")+ 
  ylim(0, 23)+
  theme(axis.text.x = element_text(size = 20, angle = -50, vjust = 0.1, hjust=0.25), 
        legend.text=element_text(size=20)) + guides(fill=guide_legend(title="Scenario"))
plot1

plot2<-ggplot(top10_esc, aes(x = Species, y = Mean, fill = Time)) +
  theme_bw(base_size = 20) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                position = position_dodge(width = 0.9), 
                width = 0.2, 
                color = "darkgray") +
  scale_fill_manual(values = alpha(c("blue", "gray"), .3), name=" ")+
  xlab(" ") + ylab("") +
  ggtitle("ESC")+ 
  ylim(0, 23)+
  theme(axis.text.x = element_text(size = 20, angle = -50, vjust = 0.1, hjust=0.25)) + guides(fill=guide_legend(title="Scenario"))
plot2

ggarrange(plot1, plot2, common.legend = TRUE, legend="right", ncol=2, labels = c("A", "B") ,
          font.label=list(color="black",size=20))
#1200 x 600




###############
#2.5 Fish Family
###############

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Review/Data analysis")

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
                color = "darkgray") +
  scale_fill_manual(values = alpha(c("blue", "gray"), .3), name=" ")+
  xlab(" ") + ylab("Relative abundance (%)") +
  ggtitle("TRE")+ 
  ylim(0, 55)+
  theme(axis.text.x = element_text(size = 20, angle = -50, vjust = 0.1, hjust=0.25), 
        legend.text=element_text(size=20))+ guides(fill=guide_legend(title="Scenario"))
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
                color = "darkgray") +
  scale_fill_manual(values = alpha(c("blue", "gray"), .3), name=" ")+
  xlab(" ") + ylab("") +
  ggtitle("ESC")+ 
  ylim(0, 55)+
  theme(axis.text.x = element_text(size = 20, angle = -50, vjust = 0.1, hjust=0.25), 
        legend.text=element_text(size=20))+ guides(fill=guide_legend(title="Scenario"))
plot2

ggarrange(plot1, plot2, common.legend = TRUE, legend="right", ncol=2, labels = c("A", "B") ,
          font.label=list(color="black",size=20))

#1200 x 600





###############
#2.6 Fish trophic group
###############

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Review/Data analysis")

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
                color = "darkgray") +
  scale_fill_manual(values = alpha(c("blue", "gray"), .3), name=" ")+
  xlab(" ") + ylab("Relative abundance (%)") +
  ggtitle("TRE")+ 
  ylim(0, 55)+
  theme(axis.text.x = element_text(size = 20, angle = -50, vjust = 0.1, hjust=0.25), 
        legend.text=element_text(size=20))+ guides(fill=guide_legend(title="Scenario"))
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
                color = "darkgray") +
  scale_fill_manual(values = alpha(c("blue", "gray"), .3), name=" ")+
  xlab(" ") + ylab("") +
  ggtitle("ESC")+ 
  ylim(0, 55)+
  theme(axis.text.x = element_text(size = 20, angle = -50, vjust = 0.1, hjust=0.25), 
        legend.text=element_text(size=20))+ guides(fill=guide_legend(title="Scenario"))
plot2

ggarrange(plot1, plot2, common.legend = TRUE, legend="right", ncol=2, labels = c("A", "B") ,
          font.label=list(color="black",size=20))

#1200 x 600
























#################################################
#3. Benthos assemblages
#################################################


###############
#3.1 nMDS
###############

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Review/Data analysis")

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

species<- envfit(nmds, data_var, permutations = 999, na.rm = TRUE)

species

#extract NMDS scores (x and y coordinates) for sites from newer versions of vegan package
data_scores <- as.data.frame(scores(nmds)$sites)

#extract coordinates from species
species_coord_cont <- as.data.frame(scores(species, "vectors")) * ordiArrowMul(species)
species_coord_cat <- as.data.frame(scores(species, "factors")) * ordiArrowMul(species)

#add columns to data frame
data_scores$Island = data_benthos$Island
data_scores$Depth_range = data_benthos$Depth_range

#save 
#write.table(data_scores, "nmds_scores_benthos.csv", sep=";", dec=".", row.names = FALSE)
#write.table(species_coord_cont, "nmds_species_coord_cont_benthos.csv", sep=";", dec=".", row.names = T)

#read data
data_scores <- read.csv("nmds_scores_benthos.csv", header= TRUE, sep=";", dec=",")
head(data_scores)
str(data_scores)
data_scores <- mutate_at(data_scores, vars(c(NMDS1, NMDS2, NMDS3, NMDS4)), as.numeric)

#read data
species_coord_cont <- read.csv("nmds_species_coord_cont_benthos.csv", header= TRUE, sep=";", dec=".")
head(species_coord_cont)

#select specis most relevant
selected_rows <- which(species_coord_cont$Species %in% c("Turf_matrix","Macroalgae","Enc_calcar","Zoanthids","Molluscs",
                                                         "Ascidians","Crinoids","Millepores","Urchin","Octocorals"))
species_coord_cont <- species_coord_cont[selected_rows, ]
species_coord_cont <- as.data.frame(species_coord_cont)
head(species_coord_cont)
str(species_coord_cont)
species_coord_cont <- mutate_at(species_coord_cont, vars(c(NMDS1, NMDS2)), as.numeric)
species <- species_coord_cont
str(species)
str(data_scores)

colours <- c("#440154", "#21918C","#D8B100")

fig_nmds_benthos <- ggplot(data_scores, aes(x = NMDS1, y = NMDS2, color=Island))+
                            geom_point(size = 3, alpha=.5)+
                            stat_ellipse(geom = "polygon",
                                         aes(color = Island, fill = Island),
                                         alpha=0.01,
                                         linetype=3,
                                         level=0.75,
                                         type="t")+
                            geom_text(data = species, aes(x = NMDS1*30-0.1, y = NMDS2*30+0.1), colour = "grey30", 
                                      fontface = "bold", label = species$Species) +
                            geom_segment(data = species, aes(x = 0, y = 0, xend = NMDS1*30, yend = NMDS2*30), 
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


###############
#3.3 nicheROVER
###############







###############
#3.4 Benthos cover rate
###############

rm(list = ls())

#read data
benthos_2001 <- read.csv("data_2001_benthos_Floeter_et_al.csv", header= TRUE, sep=";", dec=".")
benthos_2023 <- read.csv("data_2023_benthos.csv", header= TRUE, sep=";", dec=".")

benthos_2001
benthos_2023

#####
#Island TRE 2023
######

# Select just TRE
tre_benthos_2001 <- subset(benthos_2001, Island %in% c("TRE"))
tre_benthos_2001 <- tre_benthos_2001[,c(1,3,4)]
tre_benthos_2001

tre_benthos_2023 <- subset(benthos_2023, Island %in% c("TRE"))
tre_benthos_2023 <- tre_benthos_2023[,c(3:19)]
str(tre_benthos_2023)

#####
#Calculate the mean, SE
#####

tre_2023_mean <- round(colMeans(tre_benthos_2023), 2)
tre_2023_mean

tre_2023_se <- round(tre_2023_mean / sqrt(36), 2)
tre_2023_se

#####
#Convert the data to a data frame and arrange it by column names
#####

tre_benthos_2023 <- data.frame(Group = names(tre_2023_mean), Mean = tre_2023_mean, SE = tre_2023_se)
tre_benthos_2023$Time <- "Current"

rownames(tre_benthos_2023) <- NULL
tre_benthos_2023 <- tre_benthos_2023 %>%
  arrange(desc(Mean))
tre_benthos_2023

#####
#Island ESC 2023
######

# Select just ESC
esc_benthos_2001 <- subset(benthos_2001, Island %in% c("ESC"))
esc_benthos_2001 <- esc_benthos_2001[,c(1,3,4)]
esc_benthos_2001

esc_benthos_2023 <- subset(benthos_2023, Island %in% c("ESC"))
esc_benthos_2023 <- esc_benthos_2023[,c(3:19)]
str(esc_benthos_2023)

#####
#Calculate the mean, SE
#####

esc_2023_mean <- round(colMeans(esc_benthos_2023), 2)
esc_2023_mean

esc_2023_se <- round(esc_2023_mean / sqrt(36), 2)
esc_2023_se

#####
#Convert the data to a data frame and arrange it by column names
#####

esc_benthos_2023 <- data.frame(Group = names(esc_2023_mean), Mean = esc_2023_mean, SE = esc_2023_se)
esc_benthos_2023$Time <- "Current"

rownames(esc_benthos_2023) <- NULL
esc_benthos_2023 <- esc_benthos_2023 %>%
  arrange(desc(Mean))
esc_benthos_2023


#######
#Bind scenarios
#######

#TRE

tre_benthos_2001
tre_benthos_2001$Time <- "Previous"
tre_benthos_2001 <- tre_benthos_2001 %>%
  arrange(dtre(Mean))
tre_benthos_2001



tre_benthos_2023 <- tre_benthos_2023 %>%
  filter(Group %in% tre_benthos_2001$Group)
tre_benthos_2023

tre_benthos <- rbind(tre_benthos_2023, tre_benthos_2001)
tre_benthos <- tre_benthos %>%
  arrange(Group)
tre_benthos

#ESC

esc_benthos_2001
esc_benthos_2001$Time <- "Previous"
esc_benthos_2001 <- esc_benthos_2001 %>%
  arrange(desc(Mean))
esc_benthos_2001



esc_benthos_2023 <- esc_benthos_2023 %>%
                filter(Group %in% esc_benthos_2001$Group)
esc_benthos_2023

esc_benthos <- rbind(esc_benthos_2023, esc_benthos_2001)
esc_benthos <- esc_benthos %>%
  arrange(Group)
esc_benthos


#######
#Figure 6
#######

library(forcats)

#Reorder TRE
tre_benthos <- tre_benthos %>%   group_by(Group) %>%
  mutate(max_mean_current = if_else(Time == "Current", Mean, NA_real_)) %>%
  ungroup() %>%  group_by(Group) %>%  mutate(max_mean_current = max(max_mean_current, na.rm = TRUE)) %>%   ungroup()
tre_benthos$Group <- fct_reorder(tre_benthos$Group, tre_benthos$max_mean_current, .desc = TRUE)

plot1<-ggplot(tre_benthos, aes(x = Group , y = Mean, fill = Time)) +
  theme_bw(base_size = 20) +
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                position = position_dodge(width = 0.9), 
                width = 0.2, 
                color = "darkgray") +
  scale_fill_manual(values = alpha(c("blue", "gray"), .3), name=" ")+
  xlab(" ") + ylab("Cover rate (%)") +
  ggtitle("TRE")+ 
  ylim(0, 50)+
  theme(axis.text.x = element_text(size = 20, angle = -50, vjust = 0.1, hjust=0.25), 
        legend.text=element_text(size=20))+ guides(fill=guide_legend(title="Scenario"))
plot1

#Reorder ESC
esc_benthos <- esc_benthos %>%   group_by(Group) %>%
  mutate(max_mean_current = if_else(Time == "Current", Mean, NA_real_)) %>%
  ungroup() %>%  group_by(Group) %>%  mutate(max_mean_current = max(max_mean_current, na.rm = TRUE)) %>%   ungroup()
esc_benthos$Group <- fct_reorder(esc_benthos$Group, esc_benthos$max_mean_current, .desc = TRUE)

plot2<-ggplot(esc_benthos, aes(x = Group , y = Mean, fill = Time)) +
  theme_bw(base_size = 20) +
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                position = position_dodge(width = 0.9), 
                width = 0.2, 
                color = "darkgray") +
  scale_fill_manual(values = alpha(c("blue", "gray"), .3), name=" ")+
  xlab(" ") + ylab(" ") +
  ggtitle("ESC")+ 
  ylim(0, 50)+
  theme(axis.text.x = element_text(size = 20, angle = -50, vjust = 0.1, hjust=0.25), 
        legend.text=element_text(size=20))+ guides(fill=guide_legend(title="Scenario"))
plot2

###
#Figure 6
###

#packages
library(ggpubr)

ggarrange(plot1, plot2,
          labels = c("A","B"),
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
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Review/Data analysis")

#packages
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


data$Cmean<-as.numeric(data$Cmean)
data <- data[!is.na(data$Cmean), ]
summary(data$Cmean)

#Figure S3

colours <- c("#440154", "#21918C","#FDE725")

ggplot(data, aes(x=Island, y=Cmean, fill = Island)) +
  geom_boxplot(alpha = 0.5,width=0.5) +
  theme_light(base_size=20) +
  scale_fill_manual(values=colours) +
  theme(legend.position="none",
        axis.text = element_text(size = 20),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 20)) +
  xlab("") + ylab("Habitat complexity")  +ylim(c(1,5.5))

mean <- tapply(data$Cmean, data$Island, mean)
sd <- tapply(data$Cmean, data$Island, sd)

mean
sd

model<-aov(Cmean ~ Island, data)
summary(model) #Difference, p value = 3.36e-09 ***
#Tests
shapiro.test(model$residuals) #p-value < 0.05, we can NOT assume the normality
library(car)
leveneTest(model) #p-value = 0.96, we can assume the homogeneity of variance
shapiro.test(data$Cmean) #p-value < 0.05, we can NOT assume the normality
TukeyHSD(model, conf.level=.95)


#Non-parametric 
kruskal.test(Cmean ~ Island, data = data) #Difference
library(FSA)
#Perform Dunn's Test with Bonferroni correction for p-values
dunnTest(Cmean ~ Island, data=data,method="bonferroni")  #Difference





#################################################
#4.2 Length frequency
#################################################

#set working directory
setwd("C:/Users/patri/OneDrive/Documentos/APA Setiba - PADI/Biodiversity and conservation/Review/Data analysis")

#packages
library(vegan)
library(dplyr)
library(ggplot2)

#Clean R environment 
rm(list = ls())

#Read our data
data <- read.csv("fish_length.csv", header= TRUE, sep=";", dec=".")

data$Date <- dmy(data$Date)
str(data)
data <- subset(data, year(Date) == 2023)
data <- subset(data, year(Date) == 2023 & Date < as.Date("2023-04-30"))


data <- mutate_at(data, vars(c(ABUSAX, ACABAH, ACACHI, ACACOE, ACAPOL, ACAQUA, AHLEGM, ALPAFE, 
                               AMBPIN, ANISUR, ANIVIR, AULSTR, AZUMUL, BALVET, BODPUL, BODRUF, CALPEN, CANFIG, CANMAC, CANPUL, CARBAR, CARCRY, CARLAT, 
                               CARRUB, CEPFUL, CHAFAB, CHASTR, CHASED, CHIRET, CHISPI, CHRJUB, CLEBRA, CORSPP, CORGLA, CORTHR, CRYROS, CTESAE, 
                               DACVOL, DERINE, DIPARG, DIPRAD, DIOHOL, DIOHYS, DULAUR, ELAFIG, FISSPP, GOBKAL, GRABRA, GYMFUN, GYMMIL, GYMMOR, GYMVIC, HAEAUR, 
                               HAEATL, HAEPAR, HAEPLU, HALBRA, HALDIM, HALPEN, HALPOE, HALSPP, HETCRU, HOLADS, HOLCIL, HOLTRI, HYPFIS, KYPSPP, LABCRI, 
                               LABNUC, LUTALE, LUTJOC, MALDEL, MALZAL, MICCHR, MYRJAC, OCYCHR, ODODEN, OPHTRI, OPIAUR, ORTRUB, PAGPAG, 
                               PARFUR, PARLIN, PARMAR, PARMOR, PARPIL, PEMSCH, PSEPER, POMARC, POMPAR, PRIARE, PSEMAC, PTERAN, RYPSAP, SCATRI, 
                               SCAZEL, SCOBRA, SCOIST, SCOPLU, SELSET, SERBAL, SERFLA, SPASPP, SPAAMP, SPAAXI, SPAFRO, SPATUI, SPHSPE, SPHSPP, STEFUS)), as.numeric)

str(data)


# Load the necessary library
library(tidyr)
library(dplyr)

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
                    scale_fill_manual(values=c("#440154", "#21918C","#FDE725")) +
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
                scale_fill_manual(values=c("#440154", "#21918C","#FDE725")) +
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
                    scale_fill_manual(values=c("#440154", "#21918C","#FDE725")) +
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
                scale_fill_manual(values=c("#440154", "#21918C","#FDE725")) +
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
                scale_fill_manual(values=c("#440154", "#21918C","#FDE725")) +
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
              scale_fill_manual(values=c("#440154", "#21918C","#FDE725")) +
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
              scale_fill_manual(values=c("#440154", "#21918C","#FDE725")) +
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
                scale_fill_manual(values=c("#440154", "#21918C","#FDE725")) +
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
                scale_fill_manual(values=c("#440154", "#21918C","#FDE725")) +
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