---
title: "Series Temporales"
author: "Alejandro Mora"
date: "19/1/2020"
output: html_document
---

# Apartado 1 --------------------------------------------------------------

# Graficar la serie sales_ts (n�mero de tractores vendidos) y hacer el gr�fico de boxplot. 
#�Presenta estacionalidad la serie?. Descomponer la serie temporal.
  
rm(list=ls())
setwd(choose.dir())


is.installed <- function(paquete) is.element(paquete, installed.packages())

paquetes<-c("ggfortify","tseries","forecast","zoo","fpp2","ggplot2")
for(i in 1:length(paquetes)){
  if(!is.installed(paquetes[i])){
    install.packages(paquetes[i])}
  library(package=paquetes[i],character.only = TRUE)}

sum(is.na(sales_ts))
summary(sales_ts)
?sales_ts


autoplot(sales_ts) + labs(x ="Fecha", y = "N�mero de tractores vendidos", title="N�mero de tractores vendidos entre 2003 y 2014") 

boxplot(sales_ts ~ cycle(sales_ts),xlab="Mes", ylab = "Numero de tractores vendidos",main ="N�mero de tractores vendidos entre 2003 y 2014")

#La serie claramente presenta estacionalidad. Adem�s, se trata de un modelo multiplicativo. Por ello, vamos a aplicar la descomposici�n de la misma 
decompose_sales_ts <- decompose(sales_ts, "multiplicative")
autoplot(decompose_sales_ts)

# Apartado 2 ----------------------------------------------------------------------

# Aplicar diferencias regulares y estacionales para que la serie sea estacionaria y 
#utilizar la transformaciones de Box Cox en caso que sea necesario. Hacer el gr�fico de la serie diferenciada.

# Eliminaci�n de la tendencia
autoplot(diff(sales_ts,lag=1)) + labs(x ="Fecha", y = "Diferencia de orden 1", title="Serie con 1 Diferencia") 

autoplot(diff(diff(sales_ts, lag=1), lag=1)) + labs(x ="Fecha", y = "Diferencia de orden 2", title="Serie con 2 Diferencias") 

#con una diferencia nos vale para eliminar la tendencia

# Eliminaci�n de la estacionalidad
autoplot(diff(diff(sales_ts, lag=1), lag=12)) + labs(x ="Fecha", y = "Diferencia de orden 1 para la tendencia y de orden 12 para la estacionalidad", title="Serie con 1 diferencia de orden 1 y una diferencia de orden 12") 


# Apartado 3 --------------------------------------------------------------

#Graficar las funciones de autocorrelaci�n simple (FAS) y la de Autocorrelaci�n Parcial (FAP). �Qu� modelos sugieren estas dos funciones?.

#FAS
autoplot(acf(diff(diff(sales_ts, lag=1), lag=12 ), plot = FALSE)) + labs(title="FAS de las ventas con una diferencia para la tendencia y otra para la parte estacional")

#FAP
autoplot(pacf(diff(diff(sales_ts, lag=1), lag=12 ), plot = FALSE)) + labs(title="FAP de las ventas con una diferencia para la tendencia y otra para la parte estacional")

#a la vista de los resultados obtenidos, aplicamos ARIMA.


# Apartado 4 --------------------------------------------------------------

# Ajustar varios modelos y comparar el AIC.

# ARIMA011X010
tsARIMA011x010<-arima(sales_ts, order = c(0,1,1), 
                       seasonal = list(order = c(0,1,0)))
autoplot(acf(tsARIMA011x010$residuals))

# ARIMA 110X010
tsARIMA110x010<-arima(sales_ts, order = c(1,1,0), 
                      seasonal = list(order = c(0,1,0)))
autoplot(acf(tsARIMA110x010$residuals))

# ARIMA 111X010
tsARIMA111x010<-arima(sales_ts, order = c(1,1,1), 
                      seasonal = list(order = c(0,1,0)))
autoplot(acf(tsARIMA111x010$residuals))

#AIC

AIC(tsARIMA011x010,tsARIMA110x010, tsARIMA111x010)
#segun Akaike el mejor modelo es ARIMA110X010


# Apartado 5 --------------------------------------------------------------

#Con el modelo final hacer predicciones dos a�os adelante.

forecasttsARIMA110x010 <- forecast(tsARIMA110x010, level = c(95), h = 24)
autoplot(forecasttsARIMA110x010)
