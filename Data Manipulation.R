# we need to have the environment clearing code on standby

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
Dataset <- read_excel("~/STATISTIX/IYANU/Project Data final.xlsx")
View(Dataset)

head(Dataset)


# data manipulation 1: creating new variables
Dataset$PCV <- (Dataset$HGB)*3
view(Dataset)

# data manipulation 2: Recording new variable, lets categorize our PCV into High and low
summary(Dataset$PCV)
# the mean is 35.72, now lets categorize
Dataset$PCVcat <- ifelse(Dataset$PCV < 35.72, c("low"), c("high")) 
 # that is,  if value in the variable PCV is < 35.72, categorise as low, else categorise as high in another variable PCVcat

# data manipulation 3: Rename columns
Dataset= rename(Dataset, Hb.conc = HGB)
colnames(Dataset) # see the changed name

# Data manipulation 4: Subset rows/column of a data frame, note, data frame are accessed by "[]"

Dataset[3,2] # in Dataset file, row 3, column 2: returns SEX, F
DatasetMale <- subset(Dataset, SEX=="M") #data frame for only Male

# Data manipulation 4: remove/delete a column in a dataframe
str(Dataset)
Newdataset <- subset(Dataset, select=-c(FERRITIN)) # removing one variable
Newdataset <- subset(Dataset, select=-c(FERRITIN, PCVcat)) # removing multiple variables

# Data manipulation 5: Decsriptive statistics
# central tendencies: mean, median , mode
# variability; standard deviation, vairance etc
# HGB is a continuous vaiable/scale so we run a summary statistics

summary(Dataset) # summarises the whole dataset at a go
min(Dataset$`WBC (10^9)`); max(Dataset$`WBC (10^9)`); mean(Dataset$`WBC (10^9)`); var(Dataset$`WBC (10^9)`) # this will run in a single line

# for categorical variables. we use table, this is to show the frequency table
# Sex is a categorical variable

table(Dataset$SEX)

# sometimes it is important to report in percentage, so, let's go
ranks<- table(Dataset$SEX) # stores in object ranks
ranks

prop.table(ranks) # gives percentages

round(prop.table(ranks),2)*100 # rounding off at 2 d.p

#Alternatively

round(prop.table(table(Dataset$SEX)),2)*100



####### DATA VISUALISATION
#graphics.off()
#since sex is categorical so we run a bar chart or a pie chart
barplot(ranks)
# or
barplot(table(Dataset$SEX))

dev.off()  # removes current figure from the plot window


barplot(ranks, horiz = TRUE) # gives a horizontal figure

barplot(ranks, horiz = FALSE) # gives a vertical figure

### PIE CHART!!!

pie(ranks)
  col = c("black","blue")

# To plot a continuous variable, we run scatter plot or histogram
  plot(Dataset$`WBC (10^9)`)

# Graph of WBC using blue points overlayed by a line
plot(Dataset$`WBC (10^9)`, type="o", col="blue")

# create a title with a red, bold/italic font
title(main="WBC level across the population", col.main="blue")

# for the histogram

hist(Dataset$`WBC (10^9)` )
title(main="WBC level across the population", col.main="blue")

# option 2: use the add on package readxl to read in the excel file
 


# the data exporting functions include readr() etc

#write data to csv files
#decimal point = " . " and value separators = comma ","
write.csv(Dataset, file = " new dataset.csv") # dataset is the name of the current file, whereas new dataset will be the name of the new one
