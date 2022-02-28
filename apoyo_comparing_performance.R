# Comparing performance
# Vamos a cargar SP500 y GE y calculamos sus
# rentabilidades ambas a partir de una fecha establecida
# graficamos las rentabilidades de ambas a partir de la fecha establecida.


rm(list = ls())
library(quantmod) # https://cran.r-project.org/web/packages/quantmod/quantmod.pdf
library(dplyr)
library(ggplot2)
library(tidyverse)



# ticker list yahoo: https://finance.yahoo.com/lookup?s=+LIST

# get GE y sp500 diario desde el 25 de marzo 2020 hasta 10 de agosto.
getSymbols("GE" , src="yahoo", from="2020-03-25", to="2020-08-10")
getSymbols("^GSPC" , src="yahoo", from="2020-03-25", to="2020-08-10")


# trabajaremos con los precios ajustados
ge = GE$GE.Adjusted
sp = GSPC$GSPC.Adjusted


# o mejor de una vez, creamos un tibble
df <- tibble(ge = as.numeric(GE$GE.Adjusted), sp = as.numeric(GSPC$GSPC.Adjusted))


# # # # # # # # # # # # # # # PARTE I
# calculamos los retornos diarios
df2 <- df %>% mutate(ge_r = (ge - lag(ge,1)) / lag(ge,1),
              sp_r = (sp - lag(sp,1)) / lag(sp,1) )



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
# Pero en realidad lo que nosotros queremos es el retorno
# acumulado para poder comparar.
head(df2)
df2 <- df2[-1,]
df4 <- df2 %>% mutate (ge_ar = cumsum(ge_r), sp_ar = cumsum(sp_r))
head(df4)
df4 <- select(df4, -c(ge_r, sp_r))
head(df4)

# ploteo
N = nrow(df4)
df5 <- tibble(date = c(df4$dates, df4$dates), returns = c(df4$ge_ar, df4$sp_ar), 
              stock = c(rep("GE", N), rep("SP500", N))  )
head(df5)
ggplot(df5, aes(x = date, y = returns, color = stock)) + 
  geom_line(size=1) + theme(legend.position="bottom") +
  labs(title = "Retornos Acumulados",
       subtitle = "GE y SP500",
       caption = "Data source: Yahoo")






# # # # # # # # # # # # # # # PARTE III
# Pero la verdad de las cosas, es que tambien podemos evaluar 
# el performance utilizando indices, normalizando el precio del stock 
# en t0 igual a 100 para todos. 
# Notemos que la rentabilidad acumulada dia a dia no es la misma que 
# al calcularla entre el inicio y al final.
# Sea St el precio del activo en t. Sea rt la rentababilidad = (r_t/r_{t-1})-1
# luego el precio estandarizado partiendo de 100 seria Pt = P_{t-1}*(1+r_t)

# repitamos el proceso para no perdernos:
# get GE y sp500 diario desde el 25 de marzo 2020 hasta 10 de agosto.
getSymbols("GE" , src="yahoo", from="2020-03-25", to="2020-08-10")
getSymbols("^GSPC" , src="yahoo", from="2020-03-25", to="2020-08-10")

# trabajaremos con los precios ajustados
ge = GE$GE.Adjusted
sp = GSPC$GSPC.Adjusted

# o mejor de una vez, creamos un tibble
df <- tibble(ge = as.numeric(GE$GE.Adjusted), sp = as.numeric(GSPC$GSPC.Adjusted))

# calculamos los retornos diarios
df2 <- df %>% mutate(ge_r = (ge - lag(ge,1)) / lag(ge,1),
                     sp_r = (sp - lag(sp,1)) / lag(sp,1) )
# Pongamos las fechas al tibble y quitamos los precios
df2$dates <- index(GE) 
head(df2)

df2 %>% add_column(se_stand = NA)
df2[1, "ge_stand"] <- 100
df2 %>% add_column(sp_stand = NA)
df2[1, "sp_stand"] <- 100
head(df2)

# Sea St el precio del activo en t. Sea rt la rentababilidad = (r_t/r_{t-1})-1
# luego el precio estandarizado partiendo de 100 seria Pt = P_{t-1}*(1+r_t)

for ( i in c(2:nrow(df2)) ) {
  df2[i, "sp_stand"] <- df2[i-1, "sp_stand"]*(1+df2[i, "sp_r"])
  df2[i, "ge_stand"] <- df2[i-1, "ge_stand"]*(1+df2[i, "ge_r"])
}
head(df2)
# dejamos solo los precios indices
df6 <- select(df2, -c(ge, sp, ge_r, sp_r))
head(df6)

# ploteo
N = nrow(df6)
df7 <- tibble(date = c(df6$dates, df6$dates), standarized_prices = c(df6$ge_stand, df6$sp_stand), 
              stock = c(rep("GE", N), rep("SP500", N))  )
head(df7)
tail(df7)
ggplot(df7, aes(x = date, y = standarized_prices, color = stock)) + 
  geom_line(size=1) + theme(legend.position="bottom") +
  labs(title = "Precios indices",
       subtitle = "GE y SP500",
       caption = "Data source: Yahoo")


