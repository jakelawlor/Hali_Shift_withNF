# Manipulate data For POC  

•	reverse the distance to border values so that they make a little more sense
•	Make a shapefile for the new border, it will be drawn as the border between NS and NF NAFO divisions 


library(ggplot2)
library(patchwork)
library(here)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
#library(rnaturalearthhires)
library(dplyr)
library(geosphere)
library(ggpmisc)

POC_COG<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_COG.csv"))
str(POC_COG)

#1: move the centroids, put Canada in 4Xw, and USA in 3ps, 
# move COG for USA up into Newfoundland (based it off of a leading edge poing that is relatively west)
# move COG for Canada over into 4Xw
library(dplyr)

POC_COG <- POC_COG %>%
  mutate(
    centroid_longitude = case_when(
      Region == "USA"    ~ centroid_longitude + 12.28,
      Region == "Canada" ~ centroid_longitude - 2.84,
      TRUE ~ centroid_longitude
    ),
    centroid_latitude = case_when(
      Region == "USA"    ~ centroid_latitude + 2.81,
      Region == "Canada" ~ centroid_latitude - 1.40,
      TRUE ~ centroid_latitude
    )
  )
#2: replace Canada/USA with Region 1 and 2
POC_COG <- POC_COG %>%
  mutate(
    Region = ifelse(Region == "Canada", "Region1", Region),
    Region = ifelse(Region == "USA", "Region2", Region)
  )
#he is only using the longitude and latitude for the POC so i will take the others out 
POC_COG <- POC_COG[, 1:4]
write.csv(POC_COG,(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_COG_manipulated.csv")), row.names = FALSE)

#all data need to have country names removed 
#Abundance
POC_Abundance<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_Abundance.csv"))
POC_Abundance <- POC_Abundance %>%
  mutate(
    Region = ifelse(Region == "Canada", "Region1", Region),
    Region = ifelse(Region == "USA", "Region2", Region)
  )
write.csv(POC_Abundance,(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_Abundance_manipulated.csv")), row.names = FALSE)

#Area Occupied
POC_AO<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_AreaOccupied.csv"))
POC_AO <- POC_AO %>%
  mutate(
    Region = ifelse(Region == "Canada", "Region1", Region),
    Region = ifelse(Region == "USA", "Region2", Region)
  )
write.csv(POC_AO,(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_AreaOccupied_manipulated.csv")), row.names = FALSE)

#Abundance weighted depth 
POC_AWD<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_AWD.csv"))
POC_AWD <- POC_AWD %>%
  mutate(
    Region = ifelse(Region == "Canada", "Region1", Region),
    Region = ifelse(Region == "USA", "Region2", Region)
  )
write.csv(POC_AWD,(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_AWD_manipulated.csv")), row.names = FALSE)


#distance to border
POC_DtoB<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_DtoB.csv"))
POC_DtoB <- POC_DtoB %>%
  mutate(
    Region = ifelse(Region == "Canada", "Region1", Region),
    Region = ifelse(Region == "USA", "Region2", Region)
  )

write.csv(POC_DtoB,(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_DtoB_manipulated.csv")), row.names = FALSE)
