library(tidyverse)
library(ggplot2)
library(plotly)
library(plyr)
library(data.table)
library(readr)
library(dplyr)
library(ggfortify)
library(factoextra)



#Grab the file names of the 20 spectra for 785nm measured Calcium Nitrate
myfiles <- list.files(path = "C:/Users/jays/Desktop/Converted_Spectra/785/Ca_Nitrate", 
                        pattern = "*.txt", full.names = TRUE)
myfiles

#Instantiate an empty data frame to jam the raw data into
ca_nitrate_785 <- data.frame()

#Loop through the files to build one dataframe
for (lambda in c(1:20)) {
  location <- myfiles[lambda]
  temp <- fread(file = location, 
                sep = ";", fill = TRUE, header = TRUE) %>% select(`Dark Subtracted`)
  
  temp <- transpose(temp)
  
  ca_nitrate_785[lambda, 1:582] <- temp
}


#Repeat above steps for the 20 spectra of 785nm measured Magnesium Nitrate
myfiles <- list.files(path = "C:/Users/jays/Desktop/Converted_Spectra/785/Mg_Nitrate", 
                      pattern = "*.txt", full.names = TRUE)

#Instantiate an empty data frame to jam the raw data into
mg_nitrate_785 <- data.frame()

#Loop through the files
for (lambda in c(1:20)) {
  location <- myfiles[lambda]
  temp <- fread(file = location, 
                sep = ";", fill = TRUE, header = TRUE) %>% select(`Dark Subtracted`)
  
  temp <- transpose(temp)
  
  mg_nitrate_785[lambda, 1:582] <- temp
}

#Slap on an identification column
ca_nitrate_785 <- ca_nitrate_785 %>% mutate(compound = "ca")
mg_nitrate_785 <- mg_nitrate_785 %>% mutate(compound = "mg")

#Create a master dataframe of all 40 spectra
all_785_spectra <- rbind(ca_nitrate_785, mg_nitrate_785)

#Loop for testing different number of PCs in the PCA model
#for (lambda in c(2:10)) {
#  simple_pca <- prcomp(all_785_spectra,
#                       center = TRUE,
#                       scale = TRUE,
#                       rank. = lambda)
#  
#  summary(simple_pca)
#  autoplot(simple_pca)
#}

#Creating the PCA model using SVD method
simple_pca <- prcomp(all_785_spectra[ , 1:582],
                      center = TRUE,
                      scale = TRUE,
                      rank. = 5)

#Check out statistics describing how important each PC is                       
summary(simple_pca)

#Graphical depiction of how the data looks on the PC axes
autoplot(simple_pca, data = all_785_spectra, colour = "compound")

#Variance of each principal component
screeplot(simple_pca, npcs = 5)

#Explanatory power of each principal component
fviz_eig(simple_pca)

#Confidence ellipses: Related to unobserved population parameters
#A 95% confidence ellipse for the mean = Replicate sampling from the underlying distribution
#many times, and each time calculate a confidence ellipse, 95% of the ellipses would contain
#the underlying mean
fviz_pca_ind(simple_pca,
              palette = c("#00AFBB",  "#FC4E07"),
              addEllipses = TRUE,
              col.ind = all_785_spectra$compound,
              ellipse.type = "confidence",
              ellipse.level = 0.95,
              legend.title = "Groups",
              repel = TRUE)

#Retrying PCA model with Mg Nitrate scan #12 outlier removed
all_785_spectra_adjusted <- all_785_spectra[-c(24),]

#Creating the PCA model using SVD method
simple_pca_adjusted <- prcomp(all_785_spectra_adjusted[ , 1:582],
                     center = TRUE,
                     scale = FALSE,
                     rank. = 5)

#Check out statistics describing how important each PC is                       
summary(simple_pca_adjusted)

#Graphical depiction of how the data looks on the PC axes
autoplot(simple_pca_adjusted, data = all_785_spectra_adjusted, colour = "compound")

#Variance of each principal component
screeplot(simple_pca_adjusted, npcs = 5)

#Explanatory power of each principal component
fviz_eig(simple_pca_adjusted)

fviz_pca_ind(simple_pca_adjusted,
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE,
             col.ind = all_785_spectra_adjusted$compound,
             ellipse.type = "confidence",
             ellipse.level = 0.95,
             legend.title = "Groups",
             repel = TRUE)


scores_785 <- data.frame(simple_pca_adjusted$x)  

p <- plot_ly(scores_785, x = ~PC1, y = ~PC2, z = ~PC3, color = ~all_785_spectra_adjusted$compound, 
             colors = c("#00AFBB",  "#FC4E07")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')))

p
