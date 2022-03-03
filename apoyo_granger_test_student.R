# Granger causality Test


rm(list = ls())
#library(quantmod)
#library(tidyquant)
#library(xts)
#library(rvest)
library(tidyverse)
#library(stringr)
#library(forcats)
#library(lubridate)
#library(plotly)
library(dplyr)
library(Hmisc)
#library(PerformanceAnalytics)
setwd("xyz")


# carga de datos y visualizacion
data <- read.csv("data.csv", header = TRUE)
data$date <-  as.Date( data$date, format = "%m/%d/%Y")

str(data)
head(data)

plot(data$date, data$gdp, main="gdp", type='l')
plot(data$date, data$sp_adj, main="Inflation adjusted SP500", type='l')
# claramente las series no son estacionarias!


# series de tiempo en variacion porcentual Xt = (Xt - Xt-1)/Xt
Ylag <- Lag(data$gdp, shift=1); lag <- diff(data$gdp); lag <- c(NA, lag)/Ylag
data$Dgdp <- lag 

Ylag <- Lag(data$sp_adj, shift=1); lag <- diff(data$sp_adj); lag <- c(NA, lag)/Ylag
data$Dsp_adj <- lag 

head(data)

plot(data$date, data$Dgdp, main="Variacion gdp", type='l')
plot(data$date, data$Dsp_adj, main=" Variacion Inflation adjusted SP500", type='l')





# Estimacion M --> I:  con 2 rezagos
# vamos a crear los rezagos
Dgdp_1 <- Lag(data$Dgdp, shift=1)
Dgdp_2 <- Lag(data$Dgdp, shift=2)
Dsp_adj_1 <- Lag(data$Dsp_adj, shift=1)
Dsp_adj_2 <- Lag(data$Dsp_adj, shift=2)

# MODELO RESTRINGIDO   I = b1*I(-1) + b2*I(-2) 
r_model <- lm(Dgdp ~ Dgdp_1  +  Dgdp_2  , data=data)

# MODELO NO RESTRINGIDO     I = a1*M(-1) + a2*M(-2)  + b1*I(-1) + b2*I(-2)
ur_model <- lm(Dgdp ~ Dsp_adj_1 + Dsp_adj_2 +  Dgdp_1  +  Dgdp_2 , data=data )



# test F
anova(r_model, ur_model)
# F(2,4) = 2.60, p=0.08.  : Los rezagos de M causan (a lo granger) I, pero solo al 10% de sig.







# Estimacion I --> M:  con 2 rezagos
# vamos a crear los rezagos
Dgdp_1 <- Lag(data$Dgdp, shift=1)
Dgdp_2 <- Lag(data$Dgdp, shift=2)
Dsp_adj_1 <- Lag(data$Dsp_adj, shift=1)
Dsp_adj_2 <- Lag(data$Dsp_adj, shift=2)

# MODELO RESTRINGIDO   M = g1*M(-1) + g2*M(-2)  
r_model <- lm(Dsp_adj ~   Dsp_adj_1 + Dsp_adj_2 , data=data )

# MODELO NO RESTRINGIDO   M = l1*I(-1) + l2*I(-2)  +  g1*M(-1) + g2*M(-2)  
ur_model <- lm(Dsp_adj ~ Dgdp_1  +  Dgdp_2  + Dsp_adj_1 + Dsp_adj_2   , data=data)

# test F
anova(r_model, ur_model)
# F(2,4) = 5.33, p=0.007**  : Los rezagos de I causan (a lo granger) M.




# conclusion:
# CAUSALIDAD bilateral M --> I (utilIzando 2 rezagos), I --> M 

