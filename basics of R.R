# we need to have the environment clearing code on standby

install.packages("installr")
rm(list=ls()) # to clear the environment

#  loading the libraries that will be used
library(dplyr)
library(readr)
library(tidyverse)
library(readxl)
library(learnr)

# to import files quickly
# go to files> import dataset> from Excel......> browse> import
# this is the code used to import the dataset
Project_Data_final <- read_excel("~/STATISTIX/IYANU/Project Data final.xlsx")
View(Project_Data_final)

# OR
# Set work directory, click session, then set working directory, browse, then use the read.csv() command
# this is the code for setting the work directory
setwd("~/STATISTIX/IYANU")
Dataset = read_excel("Project Data final.xlsx")
view(Dataset)

# Exploring the dataset
head(Dataset) # gives the first 6 rows
head(Dataset, 3)   # gives the first 3 rows
head(Dataset. 3)  # returns an error because of . instead of ,

str(Dataset)      # Describes the structure of data

nrow(Dataset)     # Number of rows
ncol(Dataset)     # Number of columns
dim(Dataset)      # Describes the dimension, that is, row and column

class(Dataset)    # Nature of data


#structure of data, chr= character, num= numerical. character data type must be changed to factor data type

as_factor()
#categorical variables
Dataset$SEX <- as.factor(Dataset$SEX)

# lets check for corrections on the structure
str(Dataset)

# A quick summary descriptive or univariate analysis using the summary function
summary(Dataset)
