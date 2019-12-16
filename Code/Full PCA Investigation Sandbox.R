library(tidyverse)
library(ggplot2)
library(plotly)
library(plyr)
library(data.table)
library(readr)
library(dplyr)
library(ggfortify)
library(factoextra)



#Grab the file names of all the spectra for 785nm measured samples
myfiles <- list.files(path = "C:/Users/jays/Desktop/master_spectra_dump/785", 
                      pattern = "*.txt", full.names = TRUE)

#Instantiate an empty data frame to jam the raw data into
all_785 <- data.frame()

#Loop through the files
for (lambda in c(1:80)) {
  location <- myfiles[lambda]
  temp <- fread(file = location, 
                sep = ";", fill = TRUE, header = TRUE) %>% select(`Dark Subtracted`)
  
  temp <- transpose(temp)
  
  all_785[lambda, 1:582] <- temp
}

#Creating the PCA model using SVD method
all_785_pca <- prcomp(all_785[ , 1:582],
                      center = TRUE,
                      scale = FALSE,
                      rank. = 5)

#Check out statistics describing how important each PC is                       
summary(all_785_pca)

#Graphical depiction of how the data looks on the PC axes
autoplot(all_785_pca, data = all_785)

#Variance of each principal component
screeplot(all_785_pca, npcs = 5)

#Explanatory power of each principal component
fviz_eig(all_785_pca)

#Confidence Ellipses
fviz_pca_ind(all_785_pca,
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = FALSE,
             ellipse.type = "confidence",
             ellipse.level = 0.95,
             legend.title = "Groups",
             repel = TRUE)

all_785 <- mutate(all_785, compound = "ca")
all_785[21:40, 583] <- "kses"
all_785[41:60, 583] <- "k"
all_785[61:80, 583] <- "mg"

#Scan 64 looks like total sh- uh, I mean it's an outlier. Time to remove it
all_785 <- all_785[-c(64), ]

is.null(all_785)

str(all_785)

