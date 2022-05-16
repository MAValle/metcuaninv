# Test de raiz unitaria
# apoyo a unit_root_test.pdf y original_unitroot.mp4


# Se descargan precios de amazon entre 01-ago-2008 y 27-mar-2020
# 


rm(list = ls())
library(quantmod)
#library(tidyquant)
library(xts)
#library(rvest)
library(tidyverse)
#library(stringr)
#library(forcats)
#library(lubridate)
#library(plotly)
library(dplyr)
#library(PerformanceAnalytics)


getSymbols("AMZN",from="2008-08-01",to="2020-03-27")
head(AMZN)
Y <- as.numeric(AMZN$AMZN.Adjusted)
head(Y)
plot(Y)


# computo de diferencias a lag
lags = 1
DY = diff(Y)  # computo de  delta Y y_{t+1} - t_{t}
#DY <- c(NA, DY)
head(DY) # notar que la primera fila se pierde porque no tiene y_{t-1}
length(DY) # 2932   Dw = w_t - w_{t-1}  o      w_{t+1} - w_{t}
length(Y)  # 2933

# Ahora tenemos que fomar una dataframe que tenga
# DY = Y_{t} - Y_{t-1} |   Y_{t-1}    
# -0.040001            |   75.75
# 3.400002             |   75.71
# .
# .

n=length(Y)
Ylag=Y[1:n-1]
#Ylag=Y[(lags+1):n]
#Ylag <- c(NA, Ylag)
#time <- seq_along(1:n)
df <- data.frame(DY = DY, Ylag = Ylag)


# estimacion econometrica
summary(lm(DY ~ 0 + Ylag, data = df ))  # Dw = theta*w_{t} + e_{t}
#summary(lm(DY ~ time + Ylag ))  # Dw = theta*w_{t} + e_{t}    con linear drift.

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -145.488   -3.197    0.074    3.981  136.768 
# 
# Coefficients:
#       Estimate   Std.Error  t-value  Pr(>|t|)  
# Ylag 0.0006799  0.0003509   1.938   0.0528 .
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 16.53 on 2931 degrees of freedom
# Multiple R-squared:  0.001279,	Adjusted R-squared:  0.0009385 
# F-statistic: 3.754 on 1 and 2931 DF,  p-value: 0.05277


# Conclusion
# recordemos H0: theta = 0 (hay raiz unitaria, el proceso no es estacionario)
#            H1: theta < 0
# estacionario significa que las propiedades estadisticas del proceso
# se mantienen en el tiempo. 

# Resultados
# ***el coeficiente dio positivo igual a 0.0006799, aunque es bastante pequeno
# cercano a cero. 

# ***El valor t del coeficiente es de 1.938.
# Test al 5% de significancia, el valor critico es de 2.86 (asumiendo simetria).
# Si asumimmos normalidad, el valor critico seria de 1.96.
# De todas formas, el valor t es 1.938 < 2.86 y menor a 1.96, es decir,
# no podemos rechazar H0: theta = 0.
# O sea, no podemos rechazar que rho = 1, en consecuencia, los datos
# indican que tenemos evidencia de un proceso unitario (no es estacionario)
# En conclusion, tenemos un RW, acorde la la EMH.
# Sera lo mismo para una accion chilena?
qnorm(c(.01,.05,.1)/2)

