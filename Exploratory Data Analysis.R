# First install some packages

install.packages("learnr")
install.packages("plotrix")
install.packages("epiDisplay")
install.packages("ggpubr")
install.packages("foreign")
install.packages("survival")
install.packages("MASS")
install.packages("nnet")
install.packages("tidyverse")
install.packages("patchwork")
install.packages("Hmisc")
install.packages("descr")

# Now, we load our libraries for this project

library(tidyverse)  #data manipulation
library(learnr)  # exports data from r to excel
library(dplyr)   #compute summary statistics
library(readxl)  # read excel files
library(readr)  # read CSV files
library(plotrix)  # plot 3D graphs
library(epiDisplay)  # present graph and table in ho
library(ggplot2)  # data visualisation
library(ggpubr) # used to create graphs


# setting the working directory
setwd("~/STATISTIX/IYANU")
#---------------------------------------------------------------------

Iyanu <- read_xlsx("Project Data final.xlsx") # read xlsx files
head(Iyanu) # view first 6 rows
 head(Iyanu, 10) # view first 10 rows

str(Iyanu)   # descrives the structure of data

class(Iyanu)   # nature of dataset

nrow(Iyanu)  # number of rows
ncol(Iyanu)   # number of columns


### DESCRIPTIVE STATISTICS
# Centrl tendency: mean, median and mode
# variability: range, interquartile variance etc
# frequency tables

summary(Iyanu) #shows the desriptive statistice

### To edit tables/data frame, use the edit() function
IyanuTable <- edit(Iyanu)

### To do a crosstab, load the library(descr)
library(descr) ### this is used for descriptive statistics
CrossTable(SEX, MP2, prop.r = FALSE, prop.t = FALSE, prop.c = FALSE, chisq = TRUE, prop.chisq = TRUE)
### where prop.r = TRUE/FALSE means adding the percentage by row
### where prop.t = TRUE/FALSE means adding the percentage by table
### where prop.c = TRUE/FALSE means adding the percentage by column
### where prop.chisq = TRUE/FALSE means adding the percentage of the contributing Chi-square
### where chisq = TRUE/FALSE means adding the chisquare test

Iyanu3<- subset(Iyanu, select = c(SEX, MP2))
freq(Iyanu$SEX, y.axis = "percent") +### gives frequency table and plot a barchart
    title(main = "barchart of the sex frequency", xlab = "sex demographics", ylab = "Number of participants") #### adding some extras
freq(Iyanu$SEX, y.axis = "percent")
freq(Iyanu3, y.axis = "percent") ### lets see how it outputs in the case of missing responses
      ### it actually showed the missing entry as NA's, interesting
f<- freq(Iyanu$SEX, user.missing = "No answer", plot = FALSE)
plot(f)

descr(Iyanu)
####### DATA VISUALISATION
#graphics.off()
#since sex is categorical so we run a bar chart or a pie chart

sex<- table(Iyanu$SEX)
barplot(sex)
# or
barplot(table(Iyanu$SEX))
barplot(prop.table(sex)*100)

dev.off()  # removes current figure from the plot window


barplot(sex, horiz = TRUE) # gives a horizontal figure

barplot(sex, horiz = FALSE) # gives a vertical figure

### PIE CHART!!!

pie(sex)
col = c("black","blue")

# To plot a continuous variable, we run scatter plot or histogram
plot(Dataset$`WBC (10^9)`)

# Graph of WBC using blue points overlayed by a line
plot(Iyanu$`WBC (10^9)`, type="o", col="blue")

# create a title with a red, bold/italic font
title(main="WBC level across the population", col.main="blue", font.main=4)

# for the histogram

hist(Iyanu$`WBC (10^9)` )
title(main="WBC level across the population", col.main="blue")


# Relationship between two variables, either qualitative or quantitative
# 1. Boxplot
FERRITIN <- Iyanu$FERRITIN
SEX <- Iyanu$SEX
boxplot(FERRITIN~SEX, data = Iyanu, main="Ferritin level by Sex",
        xlab = SEX, ylab = FERRITIN)
# Building a plot with ggplot2 commandds
# constructing a scatter plot

ggplot(Iyanu) + geom_point(aes(x = FERRITIN, y = Iyanu$`HCT (%)`))


# converting to a factor
str(Iyanu)
AGE<-as.numeric(Iyanu$AGE)
MP2<-as.factor(Iyanu$MP2)
SEX<-as.factor(Iyanu$SEX)
str(Iyanu)
class(MP2)
class(SEX)

###########################################################
#Descriptive statistics by groups

Iyanu %>%
  group_by(SEX) %>%
  dplyr::summarise(
          count = n(),
          mean = mean(FERRITIN, na.rm = TRUE), # for mean across the group, where na.rm means remove missing values
          sd = sd(FERRITIN, na.rm = TRUE)) # for standard deviation across the group, where na.rm means remove missing values

# using ggplot to plot boxplot
ggplot(Iyanu, aes(x = SEX, y = FERRITIN))+geom_boxplot()

# for boxplot using the palette function
ggboxplot (Iyanu, x = "SEX", y = "FERRITIN",
          color = "SEX",
          palette = c("#E69F00", "#009E73"))
?palette  
graphics.off()


# vislisation: visualise your data using scatter plot
colnames(Iyanu)

ggscatter(Iyanu, x = "FERRITIN", y = "Iyanu$`HCT (%)",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "ferritin level", ylab = "HCT level")
###### Oops! an error was returned, what can i do, i think the error is in the y =...........lemme try one of my presaved values, i'll try using age instead of HCT
rlang::last_trace()   # this is used to traceback an error
ggscatter(Iyanu, x = "FERRITIN", y = "AGE",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "ferritin level", ylab = "Age")
######### waow it did work, the plot came out very fine
# Interpretaion of results
# The p-value is 0.44, which is not statistically significant
# The significance level alpha = 0.05, we can conclude that there is sufficient evidence to accept the null hypothesis
# Age and Ferritin are not correlated with a coefficient level of -0.082

### CORRELATION MATRIX
# I will need to remove the categorical variable and leave the continous variables for my correlation matrix
# Dropping a column from my dataset
# use the command subset()
str(Iyanu) #to check which ones are categorical varibles and continous variables
# so we are removing SEX,MP and MP2
# lets name the new dataset Iyanu2
Iyanu2<- subset(Iyanu, select = -c(SEX, MP, MP2))
View(Iyanu2)

res <- cor(Iyanu2)  # runs the correlation matrix and create the object 'res'
res # opens'res' in the console
round(res, 2) #rounds off the correlation matrix to 2. decimal places


#### I want to plot multiple graphs and then patch them on a page

###  1. working with two quantitative variable
# loading the necssary libraries
library(ggplot2)
library(tidyverse)
library(patchwork)

First <- ggplot(data = Iyanu, aes(AGE, FERRITIN)) +
  geom_point() +  # geom_point is used because we are doing a scatterplot
  geom_smooth() + # to make the plot more clear by adding the line of origin and area of concentration, so to say
  labs(title = "Correlation between Age and Ferritin level",
       y = 'Ferritin level',
       x = 'Age') + # this is where i write the label
  theme(text = element_text(size = 12)) # this is where i specify the text size
### 2. Two qualitative variables (one plotted, the other one will be used as fill)
 Second <- ggplot(data = Iyanu, aes(x = MP2, fill = sex)) +
   geom_bar() +  # we are using bar chart
   labs(title = "Malaria prevalence based on gender",
        y = 'Number of participants',
        x = 'Presence of Malaria') +
   theme(text = element_text(size = 12))
 ########### Oops, this line of codes did not work because my 'sex' is a table variable so i need to use the 'SEX' which i already made a factor
 
 ### we go again
 Second <- ggplot(data = Iyanu, aes(x = MP2, fill = SEX)) +
   geom_bar() +  # we are using bar chart
   labs(title = "Malaria prevalence based on gender",
        y = 'Number of participants',
        x = 'Presence of Malaria') +
   theme(text = element_text(size = 12))
 
### 3. A quantitative and qualitative variable
 Third <- ggplot(data = Iyanu, aes(x =reorder(SEX,log(Iyanu$PLATELET)), 
                                   y = log(Iyanu$PLATELET))) +
          geom_boxplot() +
          labs(title ="The distribution of Platelet according to Gender",
           y = 'platelet (log)',
           x = 'gender') +
         theme(text = element_text(size = 12))
 
### Time to patch the plots
 patch <- First/(Second + Third)
 patch + plot_annotation(tag_levels = 'A') # tag level can be alphabetical use A, numerical use 1, roman numeral use I
 
 ggsave(filename = 'output/three_plots.png', width = 10, height = 5)
 
