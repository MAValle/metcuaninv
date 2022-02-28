# Apoyo video de calculo de volatilidad de series de retornos

rm(list = ls())
library(quantmod)
library(dplyr)
library(ggplot2)
library(tidyverse)
#library(qrmdata)

# get AAPL y sp500 diario desde el 25 de marzo 2020 hasta 10 de agosto.
getSymbols("AAPL" , src="yahoo", from="2020-01-20", to="2020-08-10")


# trabajaremos con los precios ajustados
#aapl = AAPL$AAPL.Adjusted

# o mejor de una vez, creamos un tibble
df <- tibble(aapl = as.numeric(AAPL$AAPL.Adjusted))
head(df)

# incorporamos el retorno en logaritmo
df <- df %>% mutate(log_ret =  log(aapl) - log(lag(aapl) ) )
df$dates <- index(AAPL) 
head(df)
str(df)



# # # # # # # # # #  FUNCION PARA CALCULAR LA VOLATILIDAD
#https://www.youtube.com/watch?v=rf-cSBtSBEk
# inputs:
# roll_window = 22 # tomamos ventanas de 22 dias, con un avance o desfase de 1 dia (23 dias de traslape)
# retornos = vector con valores de retornos
# output:
# vector con las volatilidades (sd de las varianzas en una ventana de roll_window)
compute_volat <- function(roll_window = 22, retornos, annualized = FALSE, h=252) {
  if (annualized == FALSE) {
    final = length(retornos) - roll_window + 1     #264
    sdvalues <- vector(mode="numeric", length=final )
    for (ti in c(1:final)) {
      te=ti + roll_window-1
      sampl <- retornos[c(ti:te)]
      vlue <- sd(sampl)*100
      sdvalues[ti] <- vlue
    }
  } else {
    final = length(retornos) - roll_window + 1     #264
    sdvalues <- vector(mode="numeric", length=final )
    for (ti in c(1:final)) {
      te=ti + roll_window-1
      sampl <- retornos[c(ti:te)]
      vlue <- sum(sampl^2)*sqrt(h)
      sdvalues[ti] <- vlue
    }
  }
  return(sdvalues)
}

# # # # # # # # # # 
df <- df[-1,]
volatilidad_aapl <- compute_volat(roll_window = 22, retornos=as.numeric(df$log_ret), annualized = FALSE, h=252)

df_vol <- as.data.frame(volatilidad_aapl)
head(df_vol)


volatilidad_aapl_an <- compute_volat(roll_window = 22, retornos=as.numeric(df$log_ret), annualized = TRUE, h=252)
df_vol_an <- as.data.frame(volatilidad_aapl_an )
head(df_vol_an)


# grafica retornos
pl <- ggplot(df, aes(dates, log_ret) ) +  
  geom_line(aes(), color="black", size=0.5) 
  labs(x = "Dates", y = "Log-returns" ) 
pl  
  
# grafica vol 
plot(df_vol$volatilidad_aapl, type='l')

# grafica vol anualizada
plot(df_vol_an$volatilidad_aapl_an, type='l')
