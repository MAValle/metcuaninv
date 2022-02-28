
# Random Walk
# Simulation de random walk
# Apoyo video random_walk




rm(list = ls())
# numero de periodos
n_periods = 10

# FUNDAMENTO
# W(t) sera el valor del movimiento en el tiempo t.
# En cada momento t, se genera un estado. El estado s 
# puede ser 1 o -1 con probabilidad 0.5
# Asi, W(t) = W(t-1) + s



y <- w <- vector()
for (t in 1:n_periods ) {
  #y <- floor(runif(1, min=-1, max=1)) # crea 1 o -1 con igual probabilidad
  y <- sample(x = c(-1, 1), 1, replace = T, prob = c(0.5,0.5))
  w <- c(w, y)
  w_new <- cumsum(w)
}
plot(1:n_periods, w_new, 
     main="Random Walk", xlab="tiempo", ylab="W(t)",
     type="l")


# Convirtamos lo anterior en una funcion:
create_rw <- function(n_periods) {
  y <- w <- vector()
  for (t in 1:n_periods ) {
    y <- sample(x = c(-1, 1), 1, replace = T, prob = c(0.8,0.2))
    w <- c(w, y)
    w_new <- cumsum(w)
  }
  return(w_new)
}
simu1 <- create_rw(n_periods = 1000)
plot(1:n_periods, simu1, 
     main="Mov Browniano", xlab="tiempo", ylab="W(t)",
     type="l")


# Podemos hacer lo mismo pero para varios movimientos.
# numero de simulacions
n_sims = 10
n_periods = 1000
sims <- matrix(NA, ncol=n_sims+1, nrow=n_periods)

for (i in 1:n_sims) {
  values <-  create_rw(n_periods = n_periods)
  sims[, i] <- values
}
sims[, n_sims+1] <- 1:n_periods


# Seteo del plot
plot(sims[, n_sims+1], sims[,1], type="l",
     xlab="tiempo", ylab="W(t)", ylim=c(-100, 100))
colors = rainbow(n_sims)

# add lines
for ( i in 1:n_sims) {
  lines(sims[, n_sims+1], sims[,i], type="l", col=colors[i])
