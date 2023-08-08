# now, lets look at correlation matrix with significance level (P-value)
# the function rcorr(), in the Hmisc package
# can be used to compute the P-values
# it is used for pearson and spearman correlations

library(Hmisc)
res2 <- rcorr(as.matrix (Iyanu2)) # it will present both the correlation and the significance level
res2

#### extracting the correlation coefficient
res2$r
### extracting the P-values
res2$P

########## Costructing multiple graph on a page
par(mfrow = c(4,4))
plot(Iyanu2)  

### cross tabultion
crosstab <- table(SEX,MP2)
crosstab
round(prop.table(crosstab)*100, 2)


#### to clear the environment 
rm(list=ls())
### to clear console type ctrl+l

cat("\014")  #to clear all the plot
