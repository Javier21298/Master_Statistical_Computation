alpha <- 0.05
p <- 0.4
n_values <- c(10, 50, 100, 250, 500,600,700, 1000, 2500, 5000, 10000)

num_simulations <- 1000  

cobertura <- numeric(length(n_values))

for (i in seq_along(n_values)) #Inicio de la simulación
{
  n <- n_values[i]
  epsilon_n <- sqrt((1 / (2 * n)) * log(2 / alpha))
  
  contains_p <- numeric(num_simulations)
  for (j in 1:num_simulations) {
    x <- rbinom(n, 1, p)  
    p_estimada <- mean(x)  
    ci_low <- max(0, p_estimada - epsilon_n) 
    ci_high <- min(1, p_estimada + epsilon_n) 
    contains_p[j] <- (p >= ci_low & p <= ci_high)  
  }
  
  cobertura[i] <- mean(contains_p)
}

# Plot results
plot(n_values, cobertura, type = "b", pch = 19, col = "blue", xlab = "Tamaño de la muestra (n)", ylab = "Cobertura",
     main = paste0("Cobertura del intervalo \n p = ", p, " (alpha = ", alpha, ")"))

legend("topright", legend = c("Cobertura Simulada"), col = c("blue"), lty = c(1))



