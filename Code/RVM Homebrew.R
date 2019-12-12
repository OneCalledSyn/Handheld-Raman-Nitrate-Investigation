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

#Import the validation files
ca_validation <- fread(file = "C:/Users/jays/Desktop/Converted_Spectra/1064/Validation/Ca_Nitrate_Validation.txt", 
                       sep = ";", fill = TRUE, header = TRUE) %>% select(`Dark Subtracted`) %>% transpose()

mg_validation <- fread(file = "C:/Users/jays/Desktop/Converted_Spectra/1064/Validation/Mg_Nitrate_Validation.txt",
                       sep = ";", fill = TRUE, header = TRUE) %>% select(`Dark Subtracted`) %>% transpose()

#Add identifier to each observation
ca_validation <- ca_validation %>% mutate(compound = "ca_val")
mg_validation <- mg_validation %>% mutate(compound = "mg_val")

#Attach validation observations to master dataset
all_1064_spectra <- rbind(all_1064_spectra, ca_validation, mg_validation)

all_1064_spectra_var1 <- all_1064_spectra[ , 1]

RVM_df <- apply(all_1064_spectra_var1[ , 1], 2, sd)

RVM_df
