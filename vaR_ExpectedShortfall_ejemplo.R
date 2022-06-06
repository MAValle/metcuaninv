# VaR y expected shorfall
# ejemplos 


# COMPUTAR VaR y ES considerando una distribucion normal.
# la funcion qnorm calcula los cuantiles de una pdf normal considerando
# la probabilidad p, la media y la desv estandar, y podemos usarla
# para calcular el VaR.

# La funcion ESnorm() del paquete QRM calcula el ES para una pdf normal.
# uso:

#qnorm(p, mean = 0, sd = 1)
#ESnorm(p, mu = 0, sd = 1)


# vamos a computar el VaR y el ES par ana pdf normal N(mu, sigma^2).
# Los valores de mu y sigma se pueden obtener del los precios
# del activo de iteres para un cierto periodo.

library(PerformanceAnalytics)
mu = -0.000444099
sigma = 0.02001809
# generamos 100 valors de -4*sigma to 4*sigma
xvals <- seq(from = -4*sigma, to = 4*sigma, length.out = 100)

# computar la densidad de la pdf
ndens <- dnorm(xvals, mean = mu, sd = sigma )
# valores de retornos
ret <- rnorm(100, mean = mu, sd = sigma)

# Plot ndens contra xvals
plot(xvals, ndens, type='l')

# computar VaR y ES al 99%
VaR99 <- -qnorm(0.99, mean = mu, sd = sigma )
#ES99 <- ESnorm(0.99, mu = mu, sd = sigma )
ES99 <- ES(R=as.vector(ret), p=0.99,  method="historical" )



# Draw vertical lines at VaR99 and ES99 in red and green
abline(v = VaR99 , col = "red")
abline(v = ES99, col = "green")

# En este caso ES99 es solo 14.7% mas grande que el VaR. Para 
# colas pesadas, la diferencia puede ser mucho mas grande.
