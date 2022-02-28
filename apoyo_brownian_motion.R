
# Brownian motion
# Simulation de movimiento Browniano
# Apoyo video brownian_motion




rm(list = ls())
# numero de periodos
n_periods = 1000

# FUNDAMENTO
# Sabemos que W(t)-W(s) ~ N(0,t-s).
# Si s-t = 1, entonces W(t)-W(s) N(0,1).
# Lo que hacemos es simular muchos y = W(t)-W(s).
# Por lo tanto si tenemos el valor de W(t), 
# entonces, el valor W(s) = W(t) + y.
# Lo que hacemos es simular los incrementos de W basado
# en la propiedad del movimiento Browniano.

y <- w <- vector()
for (t in 1:n_periods ) {
  y <- rnorm(1) #normal N(0,1) simula W(t)-W(s)
  w <- c(w, y)
  w_new <- cumsum(w)
}
plot(1:n_periods, w_new, 
     main="Mov Browniano", xlab="tiempo", ylab="W(t)",
     type="l")


# Convirtamos lo anterior en una funcion:
create_brownian <- function(n_periods) {
  y <- w <- vector()
  for (t in 1:n_periods ) {
    y <- rnorm(1) #normal N(0,1) simula W(t)-W(s)
    w <- c(w, y)
    w_new <- cumsum(w)
  }
  return(w_new)
}
simu1 <- create_brownian(n_periods = 1000)
n_periods = 1000
plot(1:n_periods, simu1, 
     main="Mov Browniano", xlab="tiempo", ylab="W(t)",
     type="l")


# Tarea adicional opcional: simular en R 10 realizaciones de random walk 
# generalizado con drift: mu = 1, sigma = 10.

# ahora le pongo drift (generalized random walk or generalized brownian motion)
create_brownian_drift <- function(n_periods, sigma, mu) {
  y <- w <- vector()
  for (t in 1:n_periods ) {
    y <- rnorm(1) #normal N(0,1) simula W(t)-W(s)
    w <- c(w, y)
    w_new <- cumsum(w)
  }
  drift = mu*c(1:n_periods)
  B <- drift + sigma*w_new
  return(B)
}
# ejemplo:
simu2 <- create_brownian_drift(n_periods = 1000, sigma=10, mu=1)
n_periods = 1000
plot(1:n_periods, simu2, 
     main="Mov Browniano", xlab="tiempo", ylab="B(t)",
     type="l")






# Podemos hacer lo mismo pero para varios movimientos.
# numero de simulacions
n_sims = 10
n_periods = 1000
sims <- matrix(NA, ncol=n_sims+1, nrow=n_periods)

for (i in 1:n_sims) {
  values <-  create_brownian_drift(n_periods = 1000, sigma=10, mu=1)
  sims[ , i] <- values
}
sims[, n_sims+1] <- 1:n_periods

          
# Seteo del plot
plot(sims[, n_sims+1], sims[,1], type="l",
     xlab="tiempo", ylab="B(t)", ylim=c(-100, 2000))
colors = rainbow(n_sims)

# add lines
for ( i in 1:n_sims) {
  lines(sims[, n_sims+1], sims[,i], type="l", col=colors[i])
}
