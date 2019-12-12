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
myfiles <- list.files(path = "C:/Users/jays/Desktop/potassium_carbonate/methods/785/txt_files/KCO3", 
                      pattern = "*.txt", full.names = TRUE)
myfiles

#Instantiate an empty data frame to jam the raw data into
k_carbonate_785 <- data.frame()

#Loop through the files to build one dataframe
for (lambda in c(1:20)) {
  location <- myfiles[lambda]
  temp <- fread(file = location, 
                sep = ";", fill = TRUE, header = TRUE) %>% select(`Dark Subtracted`)
  
  temp <- transpose(temp)
  
  k_carbonate_785[lambda, 1:582] <- temp
}


#Repeat above steps for the 20 spectra of 785nm measured Magnesium Nitrate
myfiles <- list.files(path = "C:/Users/jays/Desktop/potassium_carbonate/methods/785/txt_files/KCO3_ses", 
                      pattern = "*.txt", full.names = TRUE)

#Instantiate an empty data frame to jam the raw data into
k_carbonate_ses_785 <- data.frame()

#Loop through the files
for (lambda in c(1:20)) {
  location <- myfiles[lambda]
  temp <- fread(file = location, 
                sep = ";", fill = TRUE, header = TRUE) %>% select(`Dark Subtracted`)
  
  temp <- transpose(temp)
  
  k_carbonate_ses_785[lambda, 1:582] <- temp
}

#Slap on an identification column
k_carbonate_785 <- k_carbonate_785 %>% mutate(compound = "kco3")
k_carbonate_ses_785 <- k_carbonate_ses_785 %>% mutate(compound = "kco3_ses")

all_k_carbonates_785 <- rbind(k_carbonate_785, k_carbonate_ses_785)

#Creating the PCA model using SVD method
k_carbs_pca <- prcomp(all_k_carbonates_785[ , 1:582],
                     center = TRUE,
                     scale = FALSE,
                     rank. = 5)

#Check out statistics describing how important each PC is                       
summary(k_carbs_pca)

#Graphical depiction of how the data looks on the PC axes
autoplot(k_carbs_pca, data = all_k_carbonates_785, colour = "compound")

#Variance of each principal component
screeplot(k_carbs_pca, npcs = 5)

#Explanatory power of each principal component
fviz_eig(k_carbs_pca)

#Confidence ellipses: Related to unobserved population parameters
#A 95% confidence ellipse for the mean = Replicate sampling from the underlying distribution
#many times, and each time calculate a confidence ellipse, 95% of the ellipses would contain
#the underlying mean
fviz_pca_ind(k_carbs_pca,
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE,
             col.ind = all_k_carbonates_785$compound,
             ellipse.type = "confidence",
             ellipse.level = 0.95,
             legend.title = "Groups",
             repel = TRUE)

scores_kcarb_785 <- data.frame(k_carbs_pca$x)  

p <- plot_ly(scores_kcarb_785, x = ~PC1, y = ~PC2, z = ~PC3, color = ~all_k_carbonates_785$compound, 
             colors = c("#00AFBB",  "#FC4E07")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')))

p
