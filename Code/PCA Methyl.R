library(tidyverse)
library(ggplot2)
library(plotly)
library(plyr)
library(data.table)
library(readr)
library(dplyr)
library(ggfortify)
library(factoextra)




myfiles <- list.files(path = "C:/Users/jays/Desktop/wtf", 
                      pattern = "*.txt", full.names = TRUE)
myfiles

#Instantiate an empty data frame to jam the raw data into
methyl_cellulose_1064 <- data.frame()

#Loop through the files to build one dataframe
for (lambda in c(1:12)) {
  location <- myfiles[lambda]
  temp <- fread(file = location, 
                sep = ";", fill = TRUE, header = TRUE) %>% select(`Dark Subtracted`)
  
  temp <- transpose(temp)
  
  methyl_cellulose_1064[lambda, 1:582] <- temp
}

methyl_cellulose_1064 <- methyl_cellulose_1064 %>% mutate(compound = "hpmc")
methyl_cellulose_1064[6, 583] <- "hpmc val"
methyl_cellulose_1064[7:11, 583] <- "mc"
methyl_cellulose_1064[12, 583] <- "mc val"

meth_cell_pca <- prcomp(methyl_cellulose_1064[ , 1:582],
                     center = TRUE,
                     scale = FALSE,
                     rank. = 5)

summary(meth_cell_pca)

#Graphical depiction of how the data looks on the PC axes
autoplot(meth_cell_pca, data = methyl_cellulose_1064, colour = "compound")

#Variance of each principal component
screeplot(meth_cell_pca, npcs = 5)

#Explanatory power of each principal component
fviz_eig(meth_cell_pca)


fviz_pca_ind(meth_cell_pca,
             palette = c("#00AFBB",  "#FC4E07", "#3bd95d", "#e3121d"),
             addEllipses = TRUE,
             col.ind = methyl_cellulose_1064$compound,
             ellipse.type = "confidence",
             ellipse.level = 0.95,
             legend.title = "Groups",
             repel = TRUE)

scores_meth_cell_1064 <- data.frame(meth_cell_pca$x)  

p <- plot_ly(scores_meth_cell_1064, x = ~PC1, y = ~PC2, z = ~PC3, color = ~methyl_cellulose_1064$compound, 
             colors = c("#00AFBB",  "#FC4E07", "#3bd95d", "#e3121d")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')))

p
