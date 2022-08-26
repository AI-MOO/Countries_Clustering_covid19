install.packages("readr")
library(readr)
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
install.packages("tidylog")
library(tidylog)
install.packages("janitor")
library(janitor)
library(readr)
#----------------------------------
install.packages("countrycode")
library(countrycode)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("ggdendro")
install.packages("dtw")
library(ggdendro)
library(dtw)
install.packages("scales")
library(scales)
install.packages("factoextra")
install.packages("cluster")
install.packages("magrittr")
library("cluster")
library("factoextra")
library("magrittr")
install.packages("NbClust")
library("NbClust")





raw_NPI <-read_csv("https://raw.githubusercontent.com/amel-github/covid19-interventionmeasures/master/COVID19_non-pharmaceutical-interventions_version2_utf8.csv") 
                  
raw_NPI <- as.data.frame(raw_NPI)

## save a copy:
raw_NPI_copy<- raw_NPI


## List the Distinct of Measure_L2 column:
raw_NPI %>% distinct(Measure_L2)  %>% View()
l2 <- raw_NPI %>% distinct(Measure_L2)


## Remove from Measure_L1 "Risk communication" and "Returning to normal life":
raw_NPI <- raw_NPI[!( raw_NPI$Measure_L1 == "Risk communication"|
                      raw_NPI$Measure_L1 == "Returning to normal life" ) ,  ]

##Select only the interested columns 
raw_NPI<- raw_NPI [,c(2,6,8)]

# Extract data frame subset from January2020 to December 2020 
raw_NPI_12 <- raw_NPI[raw_NPI$Date >= "2020-01-01" &    
                      raw_NPI$Date <= "2020-12-31", ]

## Remove duplicated rows
raw_NPI_12<-raw_NPI_12 [!duplicated(raw_NPI_12), ] 


## Remove countries that have only few NPLs data 
NPI_12 <- raw_NPI_12[!(raw_NPI_12$Country== "Belgium"|raw_NPI_12$Country== "Bosnia and Herzegovina"|
                       raw_NPI_12$Country=="Czech Republic"|raw_NPI_12$Country=="Diamond Princess"|
                       raw_NPI_12$Country=="Syria"| raw_NPI_12$Country=="Iceland"| raw_NPI_12$Country=="Taiwan" |
                       raw_NPI_12$Country=="Kuwait"| raw_NPI_12$Country== "Slovenia"), ]


NPI_12_copy <-NPI_12



## Add continent column
NPI_12$continent <- countrycode(sourcevar = NPI_12[, "Country"],
                                origin = "country.name",
                                destination = "continent")
## Kosovo
NPI_12$continent<- NPI_12$continent %>% replace_na("Europe")



########## Descriptive Analysis ############


####################################


## Re-code Measure_L2 column to numerical representation using mutate() and recode()
NPI_12.recode <- NPI_12 %>% mutate(Measure_L2_recode =recode(Measure_L2,
                                                         
                                                         "Activate case notification"= 1,
                                                         "Airport health check"= 2,
                                                         "Border health check" = 3,
                                                         "Enhance detection system"= 4,
                                                         "Isolation of cases"= 5,
                                                         "Quarantine"= 6,
                                                         "Restricted testing"= 7,
                                                         "Surveillance"= 8,
                                                         "Tracing and tracking"=9,
                                                         
                                                         
                                                         "Enhance hygiene conditions"= 10,
                                                         "Environmental cleaning and disinfection"= 11,
                                                         
                                                         
                                                         "Adapt procedures for patient management"= 12,
                                                         "Enhance laboratory testing capacity"= 13,
                                                         "Increase availability of PPE"= 14,
                                                         "Increase healthcare workforce"= 15,
                                                         "Increase in medical supplies and equipment"= 16,
                                                         "Increase isolation and quarantine facilities"= 17,
                                                         "Increase patient capacity"= 18,
                                                         "Personal protective measures"= 19,
                                                         "Repurpose hospitals"= 20,
                                                         "Research"= 21,
                                                         
                                                         
                                                         "Closure of educational institutions"=22,
                                                         "Measures for public transport"= 23,
                                                         "Public transport restriction"=24,
                                                         "Measures for special populations"=25,
                                                         "Special measures for certain establishments"= 26,
                                                         "Work safety protocols"= 27,
                                                         "Indoor and outdoor gathering restriction"=28,
                                                         "Indoor gathering restriction"=29,
                                                         "Outdoor gathering restriction"=30,
                                                         
                                                         
                                                         "Actively communicate with healthcare professionals"= 31,
                                                         "Actively communicate with managers"= 32,
                                                         "Educate and actively communicate with the public"=33,
                                                         "Travel alert and warning"= 34,
                                                         
                                                         
                                                         "Activate or establish emergency response"=35,
                                                         "Crisis management plans"= 36,
                                                         "Economic measure to stimulate consumption"=37,
                                                         "Measures to ensure security of supply"= 38,
                                                         "Police and army interventions"=39,
                                                         "Provide international help"=40,
                                                         "Receive international help"= 41,
                                                         "The government provide assistance to vulnerable populations"= 42,
                                                         
                                                         
                                                         "Airport restriction"= 43,
                                                         "Border restriction"=44,
                                                         "Cordon sanitaire"= 45,
                                                         "Individual movement restrictions"=46,
                                                         "National lockdown"=70,
                                                         "Port and ship restriction"= 48,
                                                         
                                                         
                                                         "Access to non-essential/critical healthcare services"= 49,
                                                         "Exemption of quarantine"= 50,
                                                         "Re-opening educational institutions"= 51,
                                                         "Lift airport restrictions"= 52,
                                                         "Lift personal protective measures"= 53,
                                                         "Lift restriction on individual movements"=54,
                                                         "Lift restriction on public transports"= 55,
                                                         "Lift travel restriction"= 56,
                                                         "Phase out emergency management"= 57,
                                                         "Re-opening of educational institutions"= 58,
                                                         "Lift measures to protect vulnerable populations"= 59,
                                                         "Lift restriction on indoor gatherings"=60,
                                                         "Lift border restrictions"= 61,
                                                         "Lift restriction on outdoor gatherings"= 62,
                                                         "Secure future access to anti-Covid19 medication and vaccine"= 63,
                                                         "Special measures for certain establishments"= 64,
                                                         "Lift restriction on indoor and outdoor gatherings" = 65,
                                                         "Lift restriction on indoor-gatherings"= 66,
                                                         "Relax detection system"= 67,
                                                         "Lift restrictions on outdoor gatherings"= 68,
                                                         "Lift restrictions on indoor and outdoor gatherings"= 69,
                                                         "Lift restricton on outdoor gatherings"= 70,
                                                         "Resume export of medical and personal protective equipment"= 71 )) 
## Remove Measure_L2 and continent columns:  
NPI_12.recode <-NPI_12.recode [,c(1,2,5)] 


## Set date from January 2020 to May 2020
# To include all Countries, 
#because some Country contain data until May 2020;
NPI_May.recode <- NPI_12.recode[NPI_12.recode$Date >= "2020-01-01" &    # Extract data frame subset
                              NPI_12.recode$Date <= "2020-05-31", ]

NPI_May.recode.g <- NPI_May.recode %>% group_by(Country,Date)  %>% 
  dplyr::summarise(Measure_L2_recode = sum(Measure_L2_recode))
NPI_May.recode.g <- NPI_May.recode.g %>% as.data.frame()


## pivot_wider Country >> column
NPI_May.wider <- NPI_May.recode.g %>% pivot_wider(names_from ="Country",
                                         values_from="Measure_L2_recode")
## Replace missing value NA of NPI with 0:   
NPI_May.wider <- mutate_at(NPI_May.wider, c(2:50), ~replace(., is.na(.), 0))

##############################################
###### Data Analysis Phase - Clustering ######
##############################################

##### 1 - create scaled data  #####
NPI_May.scaled <- scale(NPI_May.wider[,-1]) # fitter-out  Date column


##### 2 - Calculate the optimal number of clusters  ##### 
# NbClust 
NbClust(data = NPI_May.scaled, diss = NULL, distance = "maximum", 
        min.nc = 2, max.nc = 10, 
        method = "complete", index = "all", alphaBeale = 0.1)


# Elbow method
fviz_nbclust(NPI_May.scaled, hcut, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(NPI_May.scaled, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
set.seed(123)
fviz_nbclust(NPI_May.scaled, hcut, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")



##### 4  - Change to data frame #####
NPI_May.scaled_new <- as.data.frame(t(NPI_May.scaled))



######### DTW method #############

##### 5 - calculate distance using DTW method ########
## dist()
NPI_May.dtw <- dist(NPI_May.scaled_new, method = "dtw")

##### 6 - clustering data using "hclust()" for hierarchical clustering #####
## hclust
NPI_May.hc <- hclust(NPI_May.dtw)


###############################################
############### Visualization #################
###############################################

##### 6 - Hierarchical Clustering Dendrogram  ########

# Visualize using factoextra 
# Cut in ....  groups K= ? and color by groups
fviz_dend(NPI_May.hc, k = 4, 
          cex = 0.5, 
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07","#0522fc","#f805fc","#1fb58d"),
          color_labels_by_k = TRUE, 
          rect = FALSE ,
          horiz = TRUE,
          main = " Hierarchical Clustering Dendrogram \n      using DTW distance")

##### 7 - Distance Heatmap of DTW #####
## convert to matrix data type
NPI_May.matrix <- as.matrix(NPI_May.dtw)
row <- NPI_May.hc$order
column <- as.integer(t(row))
NPI <- as.numeric(as.factor(substr(rownames(NPI_May.matrix), 1, 1)))
colSide <- brewer.pal(8, "Set1")[NPI]
col_bounds <- colorRampPalette(brewer.pal(8, "Blues"))(30)
heatmap(NPI_May.matrix[row, column], 
        Colv = NA, Rowv = NA, scale = "none",
        col=col_bounds, 
        main = "DTW Distance Countries Matrix")
legend("bottomright",
       legend = c("Min Distance", "Max Distance"),
       fill = c("#FFFFFF","#084599"), bty = "n")






















#################################################


### split data 

countries_May_2020 <- c("Albania", "Brazil","Hungary","Lithuania","South Korea",
                     "Netherlands","New Zealand","North Macedonia","Portugal","Republic of Ireland",
                     "Slovakia","Canada","El Salvador","Honduras","Mexico","Indonesia","Japan",
                     "Kazakhstan","Malaysia","Thailand","Romania", "United States of America")

NPI_12.filterd <- NPI_12.recode %>%
  filter(!(Country %in% countries_May_2020)) 
## 
#NPI_12.filterd <- NPI_12.recode[NPI_12.recode$Date >= "2020-07-01" &    # Extract data frame subset
 #                                 NPI_12.recode$Date <= "2020-12-31", ]

NPI_Dec.recode.g <- NPI_12.filterd %>% group_by(Country,Date)  %>% 
                    dplyr::summarise(Measure_L2_recode = sum(Measure_L2_recode))
NPI_Dec.recode.g <- NPI_Dec.recode.g %>% as.data.frame()


## pivot_wider Country >> column
NPI_Dec.wider <- NPI_Dec.recode.g %>% pivot_wider(names_from ="Country",
                                                  values_from="Measure_L2_recode")
## Replace missing value NA of NPI with 0:   
NPI_Dec.wider <- mutate_at(NPI_Dec.wider, c(2:28), ~replace(., is.na(.), 0))

##############################################
###### Data Analysis Phase - Clustering ######
##############################################

##### 1 - create scaled data  #####
NPI_Dec.scaled <- scale(NPI_Dec.wider[,-1])


##### 2 - Calculate the optimal number of clusters  ##### 
# NbClust 
NbClust(data = NPI_Dec.scaled, diss = NULL, distance = "minkowski", 
        min.nc = 2, max.nc = 10, 
        method = "complete", index = "all", alphaBeale = 0.1)


# Elbow method
fviz_nbclust(NPI_Dec.scaled, hcut, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(NPI_Dec.scaled, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
set.seed(123)
fviz_nbclust(NPI_Dec.scaled, hcut, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")


##### 4  - Change to data frame #####
NPI_Dec.scaled_new <- as.data.frame(t(NPI_Dec.scaled))



######### DTW method #############

##### 5 - calculate distance using DTW method ########

NPI_Dec.dtw <- dist(NPI_Dec.scaled_new, method = "dtw")

##### 6 - clustering data using "hclust()" for hierarchical clustering #####
NPI_Dec.hc <- hclust(NPI_Dec.dtw)


###############################################
############### Visualization #################
###############################################

##### 6 - Hierarchical Clustering Dendrogram  ########

# Visualize using factoextra 
# Cut in ....  groups K= ? and color by groups
fviz_dend(NPI_Dec.hc, k = 4, 
          cex = 0.5, 
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07","#0522fc","#f805fc","#1fb58d"),
          color_labels_by_k = TRUE, 
          rect = FALSE ,
          horiz = TRUE,
          main = " Hierarchical Clustering Dendrogram \n      using DTW distance")

##### 7 - Distance Heatmap of DTW #####
## convert to matrix data type
NPI_Dec.matrix <- as.matrix(NPI_Dec.dtw)
row <- NPI_Dec.hc$order
column <- as.integer(t(row))
NPI <- as.numeric(as.factor(substr(rownames(NPI_Dec.matrix), 1, 1)))
#colSide <- brewer.pal(8, "Set1")[NPI]
col_bounds <- colorRampPalette(brewer.pal(8, "Blues"))(30)
heatmap(NPI_Dec.matrix[row, column], 
        Colv = NA, Rowv = NA, scale = "none",
        col=col_bounds, 
        main = "DTW Distance Countries Matrix")
legend("bottomright",
       legend = c("Min Distance", "Max Distance"),
       fill = c("#FFFFFF","#084599"), bty = "n")
