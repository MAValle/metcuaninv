# Script apoyo para *funcion de autocorrelacion: una introduccion*

rm(list = ls())
library(quantmod)
library(tidyverse)


# get GE y sp500 diario desde el 25 de marzo 2020 hasta 10 de agosto.
getSymbols("GE" , src="yahoo", from="2020-03-25", to="2020-08-10")
getSymbols("^GSPC" , src="yahoo", from="2020-03-25", to="2020-08-10")

# trabajaremos con los precios ajustados
ge = GE$GE.Adjusted
sp = GSPC$GSPC.Adjusted


# o mejor de una vez, creamos un tibble
df <- tibble(ge = as.numeric(GE$GE.Adjusted), sp = as.numeric(GSPC$GSPC.Adjusted))




# # # # # # # # # # # # # # # PARTE I - calculo de retornos
# calculamos los retornos diarios
df2 <- df %>% mutate(ge_r = (ge - lag(ge,1)) / lag(ge,1),
              sp_r = (sp - lag(sp,1)) / lag(sp,1) )

# opcion: calcular los log returns:
# https://stackoverflow.com/questions/37847441/add-columns-to-data-frame-to-calculate-log-return
# https://stackoverflow.com/questions/27027347/mutate-each-summarise-each-in-dplyr-how-do-i-select-certain-columns-and-give
df2 <- df %>% mutate_each(funs(c(NA,diff(log(.)))), ge_logr = ge, sp_logr = sp)



# Pongamos las fechas al tibble y quitamos los precios
df2$dates <- index(GE)
df2 <- select(df2, -c(ge, sp))
str(df2)



# Ahora ploteamos RETORNOS DIARIOS
N = nrow(df2)
df3 <- tibble(date = c(df2$dates, df2$dates), returns = c(df2$ge_r, df2$sp_r), 
              stock = c(rep("GE", N), rep("SP500", N))  )
ggplot(df3, aes(x = date, y = returns, color = stock)) + 
    geom_line(size=1) + theme(legend.position="bottom") +
  labs(title = "Retornos",
       subtitle = "GE y SP500",
       caption = "Data source: Yahoo")



# # # # # # # # # # # # # # # PARTE II
# La autocorrelacion nos sirve para testear la aleatoridad en un datset.
# Esta aleatoridad se evalua calculando las autocorrelaciones para los
# valores de una serie de tiempo en distintos lags. Si es aleatorio, estas
# correlaciones debieran ser cercanas a cero para todos los lags.
# Si no lo son, tendriamos un proceso dirigido por un driver no aleatorio.
# Ojo que, uncorrelated data no signigica que sea necesariamente aleatorio.
# Datos no correlacionados puede exhibir no-aleatoridad.

# la autocorrelacion de los precios SP500
rez <- 50
autc <- acf(df$sp, lag.max = rez, type = 'correlation', plot=FALSE)
autc
x <- seq(from=0, to=rez)
plot(x, autc$acf, type='l', main = 'ACF sp500', ylab = 'afc', xlab = 'lag'  )
acf(df$sp, lag.max = 25, type = 'correlation', plot=TRUE)
# las lineas punteadas corresponde a un IC al 95% de una correlacion nula
# (o sea es el error estandar)
# Cualquier correlacion que este fuera de la banda seria significativa al 5%.
# Cabe aclarar que el IC o este error estandar de una correlacion se puede aproximar 
# -1/n +- 2/sqrt(n).
# Cabe considerar que aunque la IC se plotea como una linea horizontal
# el IC en realidad va creciendo a medida que el lag aumenta por que 
# el numero de puntos n para estimar la correlacion va disminuyendo.



# la autocorrelacion de los retornos sp500
rez <- 25
df2 <- df2[-1, ] # eliminamos la primera fila
autc <- acf(df2$sp_r, lag.max = rez, type = 'correlation', plot=FALSE)
autc
x <- seq(from=0, to=rez)
plot(x, autc$acf, type='l', main = 'ACF sp500 returns', ylab = 'afc', xlab = 'lag'  )
acf(df2$sp_r, lag.max = 25, type = 'correlation', plot=TRUE)
# Vemos que las autocorrelaciones (salvo la con lag=0), son practicamente nulas
# y no significativas. Si puesen significativa y/o elevadas, entonces las serie
# de tiempo de rentabilidades seria predecible, y en consecuencia, los inversores
# podrian sacar ventaja de esto y lograr ganancias. 








# # # # # # # # # # # # # # # PARTE III
# La correlacion cruzada.
# La cross correlation nos interesa cuando queremos mirar dos series 
# en tiempos distintos.

# cross-correlation de los precios entre GE y sp500  
# (ojo que no es lo mismo entre sp500 y GE)
rez <- 35
crossc <- ccf(df$ge, df$sp, lag.max = rez, type = 'correlation', plot=FALSE)
crossc

corrs <- as.numeric(crossc$acf)
corrs <- corrs[-(1:rez)] # cross-correlation a partir de lag=0

x <- seq(from=0, to=rez)
plot(x, corrs, type='l', main = 'CCF GE-sp500', ylab = 'ccf', xlab = 'lag'  )
ccf(df$ge, df$sp, lag.max = rez, type = 'correlation', plot=TRUE)
# vemos que GE posee correlaciones bajas, y no significativas en rezagos
# de tiempo de incluso 1 mes (hay algunas significativas). Despues de eso,
# ya practicamente las correlaciones son nulas.

# notemos que al hacer al reves, vemos por ejemplo que el sp500 no esta
# signigicativamente correlacionada con otros rezagos de GE.
ccf(df$sp, df$ge, lag.max = rez, type = 'correlation', plot=TRUE)


# con los retornos entre GE y SP500 
rez <- 35
crossc <- ccf(df2$ge_r, df2$sp_r, lag.max = rez, type = 'correlation', plot=FALSE)
crossc
corrs <- as.numeric(crossc$acf)
corrs <- corrs[-(1:rez)] # cross-correlation a partir de lag=0
x <- seq(from=0, to=rez)
plot(x, corrs, type='l', main = 'CCF GE-sp500', ylab = 'ccf', xlab = 'lag'  )
ccf(df2$ge_r, df2$sp_r, lag.max = rez, type = 'correlation', plot=TRUE)
# como era de esperar, no existe correlaciones significativas para lags > 1
# Vemos solo una correlacion de app 0.6 entre GE y SP500 en lag=0, lo cual 
# es bastante esperable. 

