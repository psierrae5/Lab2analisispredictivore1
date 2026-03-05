#1. Configurar el entorno de trabajo
install.packages("forecast")
install.packages("tseries")
install.packages("ggplot2")  

library(forecast)
library(tseries)
library(ggplot2)

data(gas)

#2. Explorar los datos

str(gas)
frequency(gas)
summary(gas)

plot(gas, main="Producción de Gas en Australia", ylab="Millones de pies cúbicos", xlab="Tiempo")

#3. Transformar la serie temporal

## 3.1. Comprobamos si es estacionaria con el Test Augmented Dickey-Fuller
adf.test(gas)
### Con un p valor de 0,2764>0,05 no es estacionaria
acf(gas, main="ACF Serie Original")
pacf(gas, main="PACF Serie Original")
### 3.2. Como la serie no es estacionaria, aplicamos una diferenciación eliminando la  
### tendencia o estacionalidad
gas_diff <- diff(gas)
plot(gas_diff, main="Serie Diferenciada")
### Observamos graficamente que la serie diferenciada no tiene varianza constante
gas_log <- log(gas)
gas_log_diff <- diff(gas_log)
plot(gas_log_diff, main="Serie Log-Diferenciada")
adf.test(gas_log_diff)
### Esta nueva serie si es estacionaria y por lo tanto podemos aplicar ARIMA

#4. Ajustar un modelo ARIMA

###Buscamos los parametros optimos (p,d,q) del metodo ARIMA
arima_model <- auto.arima(gas)
summary(arima_model)
###Comprobamos las métricas AIC y BIC
arima_model$aic
arima_model$bic
### Revisamos los residuales del modelo
checkresiduals(arima_model)  

# 5. Predicciones del modelo

forecasted <- forecast(arima_model, h=12)

plot(forecasted, main="Pronóstico Producción de Gas - 12 meses")
# Visualización con ggplot2
autoplot(forecasted) + autolayer(fitted(arima_model), series="Ajustado") +
  ggtitle("Producción de Gas: Históricos + Pronóstico") +
  ylab("Millones de pies cúbicos") + xlab("Tiempo")


#6. Evaluación del modelo

fitted_values <- fitted(arima_model)
###Métrica RMSE
rmse <- sqrt(mean((gas - fitted_values)^2))
###Métrica MAE
mae <- mean(abs(gas - fitted_values))

rmse
mae