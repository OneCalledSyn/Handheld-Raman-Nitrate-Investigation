library(tidyverse)
library(ggplot2)
library(plotly)
library(plyr)
library(data.table)
library(readr)
library(dplyr)
library(ggfortify)
library(factoextra)
library(randomForest)
library(caret)
library(prospectr)


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

all_785 <- mutate(all_785, compound = "ca")
all_785[21:40, 583] <- "kses"
all_785[41:60, 583] <- "k"
all_785[61:80, 583] <- "mg"

#Scan 64 looks like total sh- uh, I mean it's an outlier. Time to remove it
all_785 <- all_785[-c(64), ]

set.seed(123)

partition <- sample(nrow(all_785), 0.9*nrow(all_785), replace = FALSE)

training <- all_785[partition, ]
validation <- all_785[-partition, ]

wimpy_forest <- randomForest(factor(compound) ~ ., data = training, importance = TRUE, type = classification)
print(wimpy_forest)

varImpPlot(wimpy_forest)

predict_training <- predict(wimpy_forest, training, type = "class")

table(predict_training, training$compound)

#validation$predicted <- predict(wimpy_forest, data = validation, type = "class")

sprouting_forest <- randomForest(factor(compound) ~ V222 + V219 + V221 + V218 + V128 + V217 + V220 + V224 + V223 + V129,
                                 data = training, importance = TRUE, type = classification)

varImpPlot(sprouting_forest)
print(sprouting_forest)

scorched_forest <- randomForest(factor(compound) ~ V222 + V219 + V221 + V218 + V128 + V217 + V220 + V224 + V223 + V129, data = training, importance = TRUE, type = classification)
print(scorched_forest)
varImpPlot(scorched_forest)

all_785 <- savitzkyGolay(all_785[ , 1:582], m = 1, p = 3, w = 5)


