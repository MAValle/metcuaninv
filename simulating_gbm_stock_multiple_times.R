
# GBM
# Simulation de un stock con GMB
# Apoyo video simulating_gbm_stock_multiple_times

# lista de tikers:
# https://finance.yahoo.com/lookup?s=+LIST

# recursos:
# https://blog.quantinsti.com/random-walk-geometric-brownian-motion/
# S_t es el valor del activo en el tiempo t, mu es el % de drift y sigma es
# el % de volatilidad, y W_t es el Brownian motion.

# Con los datos, mu serie el promedio diario de rentabilidad y sigma
# seria el promedio de volatilidad diaria del stock.
# La idea es simular los precios del stock utilizando valores
# empiricos de mu y de sigma.
# El modelo es:
# S_t/S_{t-1} = exp( (mu - (2*sigma/2)*dt + sigma*W_t) )

# Vamos a descargar los precios de AMAZON
# recurso:
# https://towardsdatascience.com/analyzing-stocks-using-r-550be7f5f20d
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

AMZN_log_returns <- AMZN %>% 
  Ad() %>% # nos rescata directamente el precio de cierre ajustado.
    dailyReturn(type='log')
# lo anterior es lo mismo que: AMZN_log_returns <- dailyReturn(Ad(AMZN), type="log")


head(AMZN_log_returns)
length(AMZN_log_returns) # 2933 retornos diarios en log (natural log)

# veamos los precios:
AMZN %>% 
  Ad() %>% 
    chartSeries()
# lo anterior lo mismo que chartSeries(Ad(AMZN))

# grafica de la serie de rentabilidades
AMZN_log_returns %>% 
  chartSeries()

# # 
# calculemos mu y sigma
mu <- mean(AMZN_log_returns)
sig <- sd(AMZN_log_returns)



# Simular los valores o precios para todas las simulaciones
# desde el mismo precio de partida. En este caso, consideramos
# el precio de cierre del stock hace 1 year atras.
# vamos a hacer simulacion de amazon DESDE 1 YEAR ATRAS CON GBM.
periodos <- 252
periodos <- periodos - 1
# recurso:
# https://www.codingfinance.com/post/2018-03-27-download-price/
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
getSymbols("AMZN", from = '2008-08-01', to = "2020-03-09", warnings = FALSE, auto.assign = TRUE)
#amazon_close <- AMZN$AMZN.Close
amazon_adj <- AMZN$AMZN.Adjusted
length(amazon_adj)
#amazon_start <- amazon_close 


#sim_actual_values <- amazon_close[nrow(AMZN)-periodos]
sim_actual_values <- amazon_adj[nrow(AMZN)-periodos] # precio de inicio.
S <-  vector(mode="numeric", length=periodos) # aqui vamos guardando los valores
S[1] <- sim_actual_values
head(S)
for (t in c(2:periodos) ) {
  new_S <- S[t-1] * exp( (mu-(0.5*sig^2)) + sig * rnorm(1)   )
  S[t] <- new_S
} 
head(S)
# Plot
plot(1:length(S), S, 
     main="GBM AMAZON", xlab="tiempo", ylab="S(t)",
     type="l")
# ahora en rojo agregamos la original
fin <- nrow(AMZN)
ini <- fin - periodos
#amazon_real <- amazon_close[fin:ini]
amazon_real <- amazon_adj[ini:fin]
lines(1:length(amazon_real), amazon_real, type="l", col="red")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Ahora bien, lo mismo podemos hacer, pero ahora simulando
# muchos GBMs!!
# Vamos a definir una funcion que genere un GBM
# los inputs seran las rentabilidades, los precios de cierre y el numero
# de periodos
# INPUT
# periodos: periodos a simular: se toman los ultimos (length(vector_returns) - periodos) para hacer la simulacion
# closes_prices: vector de los precios de cierre del stock
# vector_returns: vector de los retornos del stock
### IMPORTANTE: el numero de periodos a simular debe ser menor que el tamano de la muestra
# OUTPUT
# S = vector con los valores simulados del precio del stock
gbm_sim <- function(periodos, close_prices, vector_returns) {
  # estimadores de mu y sigma
  mu <- mean(vector_returns)
  sig <- sd(vector_returns)
  fin <- length(vector_returns)
  ini <- fin - periodos
  
  # simulacion
  sim_actual_values <- AMZN$AMZN.Close[ini]
  S <-  vector(mode="numeric", length=periodos) # aqui vamos guardando los valores
  S[1] <- sim_actual_values
  for (t in c(2:periodos) ) {
    new_S <- S[t-1] * exp( (mu-(0.5*sig^2)) + sig * rnorm(1)   )
    S[t] <- new_S
  } 
  return(S)
}

# ejemplo
S <- gbm_sim(periodos = 252, close_prices =  AMZN$AMZN.Close, vector_returns = AMZN_log_returns)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



# AHora podemos simular varias veces y plotear:
n_sims = 500
n_periods = 252
sims <- matrix(NA, ncol=n_sims+1, nrow=n_periods)

for (i in 1:n_sims) {
  values <-  gbm_sim(periodos = 252, close_prices =  AMZN$AMZN.Adjusted, vector_returns = AMZN_log_returns)
  sims[, i] <- values
}
sims[, n_sims+1] <- 1:n_periods

# Seteo del plot
plot(sims[, n_sims+1], sims[,1], type="l",
     xlab="tiempo", ylab="W(t)", ylim=c(800, 7000))
colors = rainbow(n_sims)

# add lines
for ( i in 1:n_sims) {
  lines(sims[, n_sims+1], sims[,i], type="l", col=colors[i])
}


# Ahora seleccionamos pro ejemplo, el ultimo periodo simulado, y obtenemos su distribucion
# y estadisticos.
# extraer ultimo periodo
ww <- sims[nrow(sims), 1:n_sims]
hist(ww, 30)
mean(ww)
sd(ww)
# recordemos que el valor inicial era:
sim_actual_values

# calculemos la probabilidad de que P(W < sim_actual_values)
# contamos el numero de veces que el vector ww posee valores menores que sim_actual_values.
sum(ww <= as.numeric(sim_actual_values))

#probabilidad:
sum(ww <= as.numeric(sim_actual_values))/n_sims # equivale a prob de tener perdidas.
