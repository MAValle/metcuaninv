# pdf de los retornos diarios, semanales y mensuales
# es el comportamiento de los retornos el mismo en distintas frecuencias?



rm(list = ls())
library(quantmod)
library(tidyverse)


# get DJIA from the FED https://fred.stlouisfed.org/   / YEAR-month-day
# max desde 2012-06-04
getSymbols("DJIA" , src="FRED", from="2012-006-04", to="2022-06-1", warnings = FALSE, auto.assign = TRUE)
prices <- DJIA
length(prices) # 2610

# plot
plot(DJIA, main="DJIA")


# returns: 
d_ret = periodReturn(prices, period="daily") # 2518
w_ret = periodReturn(prices, period="weekly") # 522
m_ret = periodReturn(prices, period="monthly") # 121

#plot de los retornos
par(mfrow=c(3,1))
plot(d_ret); plot(w_ret); plot(m_ret)




# plot pdf de los returns en semilog plane. La curva azul representa pdf de una normal con mu y sigma empiricos
par(mfrow=c(3,1))
# daily
min(d_ret); max(d_ret)
#rb <- c(-0.15, -0.10,-0.05, 0 , 0.05, 0.10, 0.15)
#rb <- seq(from=min(d_ret), to=max(d_ret), by=0.005)
rb <- seq(from=mean(d_ret)-20*sd(d_ret), to=mean(d_ret)+20*sd(d_ret), by=0.005)
mydata_hist <- hist(d_ret, breaks=rb, plot=FALSE)
plot(mydata_hist$mids, mydata_hist$count/sum(mydata_hist$count), log="y", type='l', lwd=1, ylab='P(r)', xlab='d_returns', main='daily')
abline(v=mean(d_ret), col="red", lwd=1, lty=2)
sim <- rnorm(200, mean=mean(d_ret), sd=sd(d_ret))
sim_hist <- hist(sim, breaks=rb, plot=FALSE)
lines(sim_hist$mids, sim_hist$count/sum(sim_hist$count), col = "blue", lwd = 1, lty = 3)

# weekly
min(w_ret); max(w_ret)
rb <- seq(from=mean(w_ret)-8*sd(w_ret), to=mean(w_ret)+8*sd(w_ret), by=0.005)
mydata_hist <- hist(w_ret, breaks=rb, plot=FALSE)
plot(mydata_hist$mids, mydata_hist$count/sum(mydata_hist$count), log="y", type='l', lwd=1, ylab='P(r)', xlab='m_returns', main='weekly')
abline(v=mean(w_ret), col="red", lwd=1, lty=2)
sim <- rnorm(200, mean=mean(w_ret), sd=sd(w_ret))
sim_hist <- hist(sim, breaks=rb, plot=FALSE)
lines(sim_hist$mids, sim_hist$count/sum(sim_hist$count), col = "blue", lwd = 1, lty = 3)

# monthly
min(m_ret); max(m_ret)
rb <- seq(from=mean(m_ret)-4*sd(m_ret), to=mean(m_ret)+4*sd(m_ret), by=0.005)
mydata_hist <- hist(m_ret, breaks=rb, plot=FALSE)
plot(mydata_hist$mids, mydata_hist$count/sum(mydata_hist$count), log="y", type='l', lwd=1, ylab='P(r)', xlab='m_returns', main='monthly')
abline(v=mean(m_ret), col="red", lwd=1, lty=2)
sim <- rnorm(200, mean=mean(m_ret), sd=sd(m_ret))
sim_hist <- hist(sim, breaks=rb, plot=FALSE)
lines(sim_hist$mids, sim_hist$count/sum(sim_hist$count), col = "blue", lwd = 1, lty = 3)

# como se puede ver, las pdf son un poco sesgadas, con valores inusualmente bajos.
