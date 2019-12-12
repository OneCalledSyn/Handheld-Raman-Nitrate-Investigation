library(tidyverse)
library(ggplot2)
library(plyr)
library(data.table)
library(readr)
library(dplyr)
library(ggfortify)
library(factoextra)


#Grab the file names of the 5 spectra for 1064nm measured Calcium Nitrate
myfiles <- list.files(path = "C:/Users/jays/Desktop/Converted_Spectra/1064/Ca_Nitrate", 
                      pattern = "*.txt", full.names = TRUE)

#Instantiate an empty data frame to jam the raw data into
ca_nitrate_1064 <- data.frame()

#Loop through the files to build one dataframe
for (lambda in c(1:5)) {
  location <- myfiles[lambda]
  temp <- fread(file = location, 
                sep = ";", fill = TRUE, header = TRUE) %>% select(`Dark Subtracted`)
  
  temp <- transpose(temp)
  
  ca_nitrate_1064[lambda, 1:582] <- temp
}

#Repeat above steps for the 5 spectra of 1064nm measured Magnesium Nitrate
myfiles <- list.files(path = "C:/Users/jays/Desktop/Converted_Spectra/1064/Mg_Nitrate", 
                      pattern = "*.txt", full.names = TRUE)

#Instantiate an empty data frame to jam the raw data into
mg_nitrate_1064 <- data.frame()

#Loop through the files
for (lambda in c(1:5)) {
  location <- myfiles[lambda]
  temp <- fread(file = location, 
                sep = ";", fill = TRUE, header = TRUE) %>% select(`Dark Subtracted`)
  
  temp <- transpose(temp)
  
  mg_nitrate_1064[lambda, 1:582] <- temp
}

#Slap on an identification column
ca_nitrate_1064 <- ca_nitrate_1064 %>% mutate(compound = "ca")
mg_nitrate_1064 <- mg_nitrate_1064 %>% mutate(compound = "mg")

#Create a master dataframe of all 10 spectra
all_1064_spectra <- rbind(ca_nitrate_1064, mg_nitrate_1064)

simple_pca <- prcomp(all_1064_spectra[ , 1:582],
                     center = TRUE,
                     scale = FALSE,
                     rank. = 5)

#Check out statistics describing how important each PC is                       
summary(simple_pca)

#Graphical depiction of how the data looks on the PC axes
autoplot(simple_pca, data = all_1064_spectra, colour = "compound")

#Variance of each principal component
screeplot(simple_pca, npcs = 5)

#Explanatory power of each principal component
fviz_eig(simple_pca)

fviz_pca_ind(simple_pca,
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE,
             col.ind = all_1064_spectra$compound,
             ellipse.type = "confidence",
             ellipse.level = 0.95,
             legend.title = "Groups",
             repel = TRUE)
