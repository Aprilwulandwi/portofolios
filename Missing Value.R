library(missForest)
library(mice)
library(Hmisc)
library(VIM)

library(readxl)
melb_data <- read.csv("C:/melb_data.csv")
View(melb_data)

DataNew=data.frame(data1=c(melb_data$BuildingArea),data2=c(melb_data$YearBuilt))
DataNew

#MISSING VALUE
miss.val = as_tibble(DataNew)
miss.val
is.na(miss.val)
summary(miss.val)
miss.val=prodNA(miss.val,noNA=0.1)
summary(miss.val)

#IMPUTED DATA DENGAN MEDIAN
DataNew
DataNew$data1<- with(DataNew, impute(data1, median))
DataNew$data2<- with(DataNew, impute(data2, median))
DataNew
summary(DataNew)

#ANALISIS PATTERN
mice_plot <- aggr(DataNew, col=c('maroon','olivedrab3'),                
numbers=TRUE, sortVars=TRUE,
labels=names(DataNew), cex.axis=.7,
gap=3, ylab=c("Missing data","Pattern"))


#MEMBUAT HISTOGRAM,BOXPLOT,SCATTERPLOT
library(gridExtra)
x = c(DataNew$data1)
x
#Histogram, Q-Q Plot, and Boxplot
par(mfrow = c(1, 3))
hist(x, main = "Histogram")
boxplot(x, main = "Boxplot")
qqnorm(x, main = "Normal Q-Q plot")

#MENCARI NILAI Q1, Q3, IQR
summary(x)
#Mencari Nilai Q1
Q1 = quantile(x, probs = 0.25) 
Q1
#Mencari Nilai Q3
Q3 = quantile(x, probs = 0.75) 
Q3
#Mencari Interquartile
IQR = Q3 - Q1 
IQR(x)

#Threshold
Tmin = Q1-(1.5*IQR)

Tmin
Tmax = Q3+(1.5*IQR)
Tmax
x[which(x < Tmin | x > Tmax)]

#HANDLING OUTLIERS
#1. Log Transformation
x = c(DataNew$data1)
log_x <- log10(x)
log_x
hist(x, col='steelblue', main='Original')
hist(log_x, col='coral2', main='Log Transformed')

#Data 2
#Membuat Library(GridExtra)
library(gridExtra)
x2 = c(DataNew$data2)
x2
#Histogram, Q-Q plot, and Boxplot
par(mfrow = c(1, 3))
hist(x2, main = "Histogram")
boxplot(x2, main = "Boxplot")
qqnorm(x2, main = "Normal Q-Q plot")

#MENCARI NILAI Q1, Q3, IQR
summary(x2)
#Mencari Nilai Q1
Q1 = quantile(x2, probs = 0.25) 
Q1
#Mencari Nilai Q3
Q3 = quantile(x2, probs = 0.75) 
Q3
#Mencari Interquartile
IQR = Q3 - Q1 
IQR(x2)

#Threshold
Tmin = Q1-(1.5*IQR)
Tmin
Tmax = Q3+(1.5*IQR)
Tmax
x2[which(x2 < Tmin | x2 > Tmax)]

#HANDLING OUTLIERS
#1. Log Transformation
x2 = c(DataNew$data2)
log_x2 <- log10(x2)
log_x2
hist(x2, col='palevioletred1', main='Original')
hist(log_x2, col='plum2', main='Log Transformed')