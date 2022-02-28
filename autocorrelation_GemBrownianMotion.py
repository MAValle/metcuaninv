#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Oct 15 14:24:35 2020

@author: MAValle
"""
from IPython import get_ipython
get_ipython().magic('reset -sf')
import numpy as np
import matplotlib.pyplot as plt


# Vamos a definir una funcion que genere un GBM
# los inputs seran_
    # mu = media de la rentabilidad
    # sigma = volatilidad (des. estandar de las rentabilidades)
    # periodos = numero de periodos a simular
# salida:
    # S = los precios del activo segun GBM

def geom_bm_sim(mu, sigma, periodos):
    S0 = 1 # precio inicial del stock
    S = [None] * (periodos+1)
    S[0] = S0
    for t in range(1,len(S)):
        S[t] = S[t-1] * np.exp(mu - (0.5*sigma**2) ) + sigma * np.random.normal()
    return(S)

serie = geom_bm_sim(mu = 0.08, sigma=0.5, periodos=350)

# plot de la serie
# https://matplotlib.org/3.1.1/gallery/lines_bars_and_markers/nan_test.html#sphx-glr-gallery-lines-bars-and-markers-nan-test-py
time = list(range(0, len(serie)) )

plt.plot(time, serie, label='GBM simulation')
plt.xlabel('time')
plt.ylabel('Prices - S')
plt.title('A GBM simulation')
plt.grid(True)



import pandas as pd
# calcular las log returns
# logR = np.diff(np.log(serie)) 
# calcular los retornos algeibraicos
serie_df = pd.DataFrame(serie) # transformar a dataframe
rtornos = serie_df / serie_df.shift(1) - 1
# rtornos = serie_df.pct_change(1) # es lo mismo
rtornos = rtornos.dropna() # primera fila se elimina
rtornos = rtornos.to_numpy() # vuelta a transformar a array

# plot de los retornos
# https://matplotlib.org/3.1.1/gallery/lines_bars_and_markers/nan_test.html#sphx-glr-gallery-lines-bars-and-markers-nan-test-py
time = list(range(1, len(rtornos)+1) )

plt.plot(time, rtornos, label='GBM simulation returns')
plt.xlabel('time')
plt.ylabel('Returns - r')
plt.title('A GBM simulation')
plt.grid(True)




# funcion de autocorrelacion de los precios
from statsmodels.graphics.tsaplots import plot_acf
from statsmodels.graphics.tsaplots import acf
plot_acf(serie, lags=50)
funcor = acf(serie, nlags=5)




# funcion de autocorrelacion de los retornos
from statsmodels.graphics.tsaplots import plot_acf
from statsmodels.graphics.tsaplots import acf
plot_acf(rtornos, lags=50)
funcor = acf(rtornos, nlags=5)



# Tarea: Simular 100 GBMs para un mismo mu y sigma, 
# y promediar las funciones de correlaciones y plotearlas.
