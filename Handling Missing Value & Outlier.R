Data_Melb = read.csv("C:/Users/user/Downloads/melb_data.csv")
Data_Melb

#Data Kuantitatif : rooms, price, distance, bedroom2, bathroom, car, landsize, building area, yearbuilt (9)
#Data Kualitatif : suburb, address, type, method, sellerg, councilarea, Region name (7)

#------------------IDENTIFIKASI MISSING VALUE DAN OUTLIER---------------------------------------------

#1.) Identifikasi Missing Value Untuk Data Kuantitatif
summary(Data_Melb) 
#masih terdapat variabel yang missing value "NA" (3 variabel) yaitu car,building area, yearbuilt 


#2.) Identifikasi Missing Value Untuk Data Kualitatif / kategori
Data_Melb[Data_Melb == ''] <- NA

#Masukkan variabel kategori
sum(is.na(Data_Melb$Suburb))
sum(is.na(Data_Melb$Address))
sum(is.na(Data_Melb$Type))
sum(is.na(Data_Melb$Method))
sum(is.na(Data_Melb$SellerG))
sum(is.na(Data_Melb$CouncilArea))
sum(is.na(Data_Melb$Regionname))


#Ternyata Masih terdapat missing value pada council area sebanyak 1369

#3.) Identifikasi outlier untuk semua variabel (kuantitatif)
par(mfrow = c(1,4))
boxplot(Data_Melb$Rooms,main="Rooms")
boxplot(Data_Melb$Price, main="Price")
boxplot(Data_Melb$Distance, main="Distance")
boxplot(Data_Melb$Bedroom2, main="Bedroom")

par(mfrow = c(1,5))
boxplot(Data_Melb$Bathroom, main="Bathroom")
boxplot(Data_Melb$Car, main="Car")
boxplot(Data_Melb$Landsize, main="Landsize")
boxplot(Data_Melb$BuildingArea, main="Building Area")
boxplot(Data_Melb$YearBuilt, main="Year built")


#------------------HANDLING MISSING VALUE DAN OUTLIER---------------------------------------------

#1.) Membuat Histogram => Data missing value Kuantitatif ==> Untuk mengetahui metode imputasi yang cocok digunakan
par(mfrow = c(1,3))
hist(Data_Melb$Car, col='yellow', main='Car') 
hist(Data_Melb$BuildingArea, col='purple', main='Building Area') 
hist(Data_Melb$YearBuilt, col='blue', main='Year Built')

#Semua histogram mempunyai outlier ==> Skewness ==> Sehingga digunakan metode imputasi median

#metode imputasi median karena plot histogram membentuk skewness
library(Hmisc)
Data_Melb$Car <- with(Data_Melb, impute(Car, median))
Data_Melb$BuildingArea <- with(Data_Melb, impute(BuildingArea, median))
Data_Melb$YearBuilt  <- with(Data_Melb, impute(YearBuilt , median))
summary(Data_Melb)


#2.) Data kualitatif / kategori ==> Menggunakan metode modus
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(Data_Melb$CouncilArea)

library(tidyr)
library(dplyr)
library(tidyverse)
fill = fill(Data_Melb,CouncilArea)
fill
sum(is.na(fill$CouncilArea))
#Sudah terisi variabale yang missing value ==> Council Area "NA" = 0

#HANDLING OUTLIER
par(mfrow = c(1,2))
log_Lattitude <- log10(Data_Melb$Rooms)
hist(Data_Melb$Rooms, col ='Purple', main= 'Original')
hist(log_Lattitude, col = 'yellow', main = 'Log Transformed')

par(mfrow = c(1,2))
log_Lattitude <- log10(Data_Melb$Price)
hist(Data_Melb$Price, col ='Purple', main= 'Original')
hist(log_Lattitude, col = 'yellow', main = 'Log Transformed')

par(mfrow = c(1,2))
log_Lattitude <- log10(Data_Melb$Distance)
hist(Data_Melb$Distance, col ='Purple', main= 'Original')
hist(log_Lattitude, col = 'yellow', main = 'Log Transformed')

par(mfrow = c(1,2))
log_Lattitude <- log10(Data_Melb$Bedroom2)
hist(Data_Melb$Bedroom2, col ='Purple', main= 'Original')
hist(log_Lattitude, col = 'yellow', main = 'Log Transformed')

par(mfrow = c(1,2))
log_Lattitude <- log10(Data_Melb$Bathroom)
hist(Data_Melb$Bathroom, col ='Purple', main= 'Original')
hist(log_Lattitude, col = 'yellow', main = 'Log Transformed')

par(mfrow = c(1,2))
log_Lattitude <- log10(Data_Melb$Car)
hist(Data_Melb$Car, col ='Purple', main= 'Original')
hist(log_Lattitude, col = 'yellow', main = 'Log Transformed')

par(mfrow = c(1,2))
log_Lattitude <- log10(Data_Melb$Landsize)
hist(Data_Melb$Landsize, col ='Purple', main= 'Original')
hist(log_Lattitude, col = 'yellow', main = 'Log Transformed')

par(mfrow = c(1,2))
log_Lattitude <- log10(Data_Melb$BuildingArea)
hist(Data_Melb$BuildingArea, col ='Purple', main= 'Original')
hist(log_Lattitude, col = 'yellow', main = 'Log Transformed')

par(mfrow = c(1,2))
log_Lattitude <- log10(Data_Melb$YearBuilt)
hist(Data_Melb$YearBuilt, col ='Purple', main= 'Original')
hist(log_Lattitude, col = 'yellow', main = 'Log Transformed')