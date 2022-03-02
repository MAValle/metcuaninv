# simulacion GBM simple  y calculo de retornos.


rm(list = ls())
library(quantmod)
#library(xts)
#library(rvest)
library(tidyverse)

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
  sim_actual_values <- as.numeric(close_prices[1])
  S <-  vector(mode="numeric", length=periodos) # aqui vamos guardando los valores
  S[1] <- sim_actual_values
  for (t in c(2:periodos) ) {
    new_S <- S[t-1] * exp( (mu-(0.5*sig^2)) + sig * rnorm(1)   )
    S[t] <- new_S
  } 
  return(S)
}

# ejemplo
#S <- gbm_sim(periodos = 252, close_prices =  AMZN$AMZN.Close, vector_returns = AMZN_log_returns)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
getSymbols("AMZN", from = '2021-01-01', to = "2021-07-11", warnings = FALSE, auto.assign = TRUE)


AMZN_log_returns <- AMZN %>% 
  Ad() %>% # nos rescata directamente el precio de cierre ajustado.
  dailyReturn(type='log')

amazon_adj <- AMZN$AMZN.Adjusted
length(amazon_adj)


periodos <- length(amazon_adj) + 40
sim_actual_values <- as.numeric(amazon_adj[1])
#sim_actual_values <- amazon_adj[nrow(amazon_adj )] # precio de inicio.
#S <-  vector(mode="numeric", length=periodos) # aqui vamos guardando los valores
#S[1] <- sim_actual_values
#head(S)

# AHora podemos simular varias veces y plotear:
n_sims = 500
sims <- matrix(NA, ncol=n_sims+1, nrow=periodos)

for (i in 1:n_sims) {
  values <-  gbm_sim(periodos = periodos, close_prices = amazon_adj, vector_returns = AMZN_log_returns)
  sims[, i] <- values
}
sims[, n_sims+1] <- 1:periodos

# Seteo del plot
plot(sims[, n_sims+1], sims[,1], type="l",
     xlab="tiempo", ylab="W(t)", ylim=c(800, 7000))
colors = rainbow(n_sims)

# add lines
for ( i in 1:n_sims) {
  lines(sims[, n_sims+1], sims[,i], type="l", col=colors[i])
}


# promedio de S(t=129) (11 de julio)
media <- mean(sims[129, c(1:500)]) 
# rentabilidad
100*(media - sim_actual_values )/sim_actual_values






# IMPORTANTE:
# es importante recordar en que simulacion de montecarlo:
# E[g(x)]  NO ES LO MISMO QUE   g(E[x])  ////    E[g(x)]  <>    g(E[x]) 

# es decir, la forma de calcular el retorno medio es equivale a g(E[x]), es decir, 
# aplicamos operador de retorno (g) sobre el valor medio de los valores.

# Lo correcto es encontrar E[g(x)], es decir, la medioa de retornos de cada simulacion!

retornos <- 100*(sims[129, c(1:500)] - sim_actual_values)/sim_actual_values
mean(retornos)
