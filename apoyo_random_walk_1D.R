
# Random Walk
# Simulation de random walk

# teoria:
# si d_N es la posicion neta despues de N movimientos, con n_1 movimientos
# y con n_2 movimientos a la izquierda, entonces la distrobucion de d_N = n_1 - n_2 es:

# P(d_N ) = N!/(n1!)(n2!) *  p^(n1) * q^(n2)

# y que E(d_N) = N(p-q)
# y Var(d_N) = 4Npq.


# Vamos a simular camino aleatorio 1D, para N=10.


rm(list = ls())
# numero de periodos
n_periods = 11
p = 0.5
q = 1- p

# FUNDAMENTO
# W(t) sera el valor del movimiento en el tiempo t.
# En cada momento t, se genera un estado. El estado s 
# puede ser 1 o -1 con probabilidad 0.5
# Asi, W(t) = W(t-1) + s



y <- w <- vector()
for (t in 1:n_periods ) {
  #y <- floor(runif(1, min=-1, max=1)) # crea 1 o -1 con igual probabilidad
  y <- sample(x = c(-1, 1), 1, replace = T, prob = c(q,p))
  w <- c(w, y)
  w_new <- cumsum(w)
}
plot(1:n_periods, w_new, 
     main="Random Walk", xlab="tiempo", ylab="W(t)",
     type="l")


# Convirtamos lo anterior en una funcion:
create_rw <- function(n_periods, p) {
  y <- w <- vector()
  for (t in 1:n_periods ) {
    y <- sample(x = c(-1, 1), 1, replace = T, prob = c(1-p,p))
    w <- c(w, y)
    w_new <- cumsum(w)
  }
  return(w_new)
}
simu1 <- create_rw(n_periods = 10, p=0.5)
plot(1:10, simu1, 
     main="Mov Browniano", xlab="tiempo", ylab="W(t)",
     type="l")


# Podemos hacer lo mismo pero para varios movimientos.
# numero de simulacions
n_sims = 10
n_periods = 10
sims <- matrix(NA, ncol=n_sims+1, nrow=n_periods)

for (i in 1:n_sims) {
  values <-  create_rw(n_periods = n_periods, p=0.5)
  sims[, i] <- values
}
sims[, n_sims+1] <- 1:n_periods

# Seteo del plot
plot(sims[, n_sims+1], sims[,1], type="l",
     xlab="tiempo", ylab="W(t)", ylim=c(-10, 10))
colors = rainbow(n_sims)
# add lines
for ( i in 1:n_sims) {
  lines(sims[, n_sims+1], sims[,i], type="l", col=colors[i])
}




# veamos la distrobucion de d_N
# tenemos que ver numero de veces en que d_N = -10, -9, ...., 9, 10
# para esto, es solo contar en la columna penultima de sims.
tfinal <- sims[n_periods, ]
hist(tfinal, 50)  # parece una distribucion binomial!


# y si ahora hacemos la simulacion con N=10000
hist(tfinal, 20)

# vemos que la distribucion de d_N comienza a parecer una normal 
mean(tfinal)   # da igual a 0.00509949 , mientras que valor teorico es: 10000(0.5-0.5)=0
var(tfinal) # da 10.90087, mientras que valor teorico es 4*10000*0.5*0.5 = 10,000

# BASTANTE SIMILAR!!!
