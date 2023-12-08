#to clear the environment
rm(list = ls())
library(fpp2)
library(readxl)
library(lmtest)
library(forecast)
library(writexl)
library(tseries)
library(MASS)
library(EnvStats)

#Membaca Data
df<- read_excel("C:/Users/wulandari/Downloads/70.30.xlsx", sheet = "20-22")
df

# Time Series Plot
date<-df$Date
TS<- ggplot(data =df, aes(x =date, y=Y)) +
  geom_line() +
  labs(title = "Plot Deret Waktu", x = "Tanggal", y = "Nilai")

# Missing Value
is.na(Y)
sum(is.na(Y))

#  Mengecek outlier
df$baris<- row.names(df)
Q1 <- quantile(Y, 0.25)
Q3 <- quantile(Y, 0.75)
IQR <- Q3 - Q1
upper_bound <- Q3 + 1.5 * IQR
lower_bound <- Q1 - 1.5 * IQR

outliers <- Y[Y > upper_bound | Y < lower_bound]
print(outliers)

#Identifikasi plot ACF dan PACF
par(mfrow=c(2,1))
acf(df[,"Y"])
pacf(df[,"Y"])

#Melakukan differencing data yg belum stationer
diff<- diff(Y,lag=1)
plot(diff, main="differencing TS")


#UJI STATIONERITAS MEAN
adf.test(diff)

#new ACF PACF
par(mfrow=c(2,1))
acf(diff)
pacf(diff)

#permodelan ARIMA 
estimate1= Arima(Y, order=c(2,1,0)); estimate1; coeftest(estimate1)
accuracy(estimate1)
testing<- ts(testing)

# ljung box test ARIMA
residual1=estimate1$residuals
Box.test(residual1, lag=6)
Box.test(residual1, lag=12)
Box.test(residual1, lag=18)
Box.test(residual1, lag=24)
Box.test(residual1, lag=30)
Box.test(residual1, lag=36)
Box.test(residual1, lag=42)
Box.test(residual1, lag=48)

RESI1<-residuals(estimate1)
print(RESI)

# UJI NORMALITAS ARIMA
qqnorm(RESI)
qqline(RESI)
ks_result<- ks.test(RESI, "pnorm")
print(ks_result)

#FORECAST ARIMA
prediksi=forecast(estimate1, h=181); prediksi
data <- data.frame(prediksi)
#write_xlsx(data, "hasil_aprilarima.xlsx")

#permodelan ARIMAX
Y<-matrix(RESI1, ncol=1)
X1<-matrix(df$LEBARAN, ncol=1)
X2<-matrix(df$`maulid`, ncol=1)
X3<-matrix(df$`idul adha`, ncol=1)
#dim(X)

estimateARIMAX= Arima(Y, order=c(2,1,0), xreg=cbind(X1,X2,X3))
estimateARIMAX; coeftest(estimateARIMAX)
accuracy(estimateARIMAX)

# ljung box test ARIMAX
residual2=estimateARIMAX$residuals
Box.test(residual2, lag=6)
Box.test(residual2, lag=12)
Box.test(residual2, lag=18)
Box.test(residual2, lag=24)
Box.test(residual2, lag=30)
Box.test(residual2, lag=36)
Box.test(residual2, lag=42)
Box.test(residual2, lag=48)

RESIX<-residuals(estimateARIMAX)
print(RESIX)

# UJi normalitas ARIMAX
qqnorm(RESIX)
qqline(RESIX)
ks_result<- ks.test(RESIX, "pnorm")
print(ks_result)
RESIX<-data.frame(RESIX)
write_xlsx(RESIX, "C:/Users/wulandari/Downloads/RESID.xlsx")

# RAMALAN ARIMAX
forecast_horizon <- 181
mean_x1 <- mean(X1)
mean_x1
mean_x2 <- mean(X2)
mean_x2
mean_x3 <-mean(X3)
xreg_matrix<-cbind(rep(mean_x1,forecast_horizon), rep(mean_x2,forecast_horizon), rep(mean_x3,forecast_horizon))
xreg_matrix
forecast_hasil <- forecast(estimateARIMAX, xreg=xreg_matrix, h = forecast_horizon)
print(forecast_hasil)
data <- data.frame(forecast_hasil)
write_xlsx(data, "LAST.xlsx")