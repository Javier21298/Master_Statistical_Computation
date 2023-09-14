lambda <- 0.5
T_ <- 1
n_simulaciones <- 10^4
eventos <- c()
resultados <- list()

for (i in 1:n_simulaciones) 
{
  sim <- simPoisson(T_, lambda)
  eventos <- c(eventos, max(sim$ve))
}

eventos <- subset(eventos, eventos != 0)

tabla <- table(eventos)

tablaN <- prop.table(tabla)
teorica <- dpois(0:6, lambda = 0.5)

plot(tablaN, xlab = "Eventos", 
     ylab = "Frecuencia normalizada",
     col = "black",
     main = "Frecuencias de eventos normalizada
     N = 10^4, lamda = 0.5, T = 1")
points(teorica, type = "o", col = "green")

legend("topright", legend = c("Simulación","Poisson"),
       col = c("black","green"), lty = 1, cex = 0.8)
