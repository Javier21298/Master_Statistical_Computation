alpha <- 0.05
p <- 0.4
n_values <- c(10, 50, 100, 250, 500, 1000, 2500, 5000, 10000)  

interval_lengths <- numeric(length(n_values))

for (i in seq_along(n_values)) 
{
  n <- n_values[i]
  epsilon_n <- sqrt((1 / (2 * n)) * log(2 / alpha))
  interval_length <- 2 * epsilon_n  #Tamaño del intervalo
  interval_lengths[i] <- interval_length
}

plot(n_values, interval_lengths, type = "b", pch = 19, col = "blue", xlab = "Tamaño de la muestra (n)", ylab = "Tamaño del intervalo",
     main = "Tamaño de intervalo vs tamaño de muestra", log = "x")
abline(h = 0.05, col = "red", lty = 2)  
legend("topright", legend = c("Tamaño del intervalo", "tamaño objetivo (0.05)"), col = c("blue", "red"), lty = c(1, 2))

min_n <- min(n_values[interval_lengths < 0.05])
min_n
