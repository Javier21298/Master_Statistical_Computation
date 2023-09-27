pp_plot <- function(data) {
  # Ordenar datos
  datos_ordenados <- sort(data)
  
  n <- length(data)
  probabilidades_empiricas <- (1:n) / (n + 1)
  
  probabilidades_teoricas <- pnorm(datos_ordenados, mean(data), sd(data))
  
  plot(probabilidades_teoricas, probabilidades_empiricas,
       xlab = "Probabilidades Teóricas (Normal)",
       ylab = "Probabilidades Empíricas",
       main = "PP-Plot",
       pch = 19, col = "blue", xlim = c(0, 1), ylim = c(0, 1))
  abline(0, 1, col = "red") 
  
  legend("topleft", legend = c("Datos", "Línea teórica"), 
         col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1))
}

# Datos
datos <- c(23.37, 21.87, 24.41, 21.27, 23.33, 15.20, 24.21, 27.52, 15.48, 27.19,
           25.05, 20.40, 21.05, 28.83, 22.90, 18.00, 17.55, 25.92, 23.64, 28.96,
           23.02, 17.32, 30.74, 26.73, 17.22, 22.81, 20.78, 23.17, 21.60, 22.37)


hist(datos, freq=FALSE, breaks=10, xlim=c(10,35), main="Histograma con densidad normal superpuesta", xlab="Valor")

x <- seq(10, 35, length=100)
y <- dnorm(x, mean(datos), sd(datos))

lines(x, y, col="red", lwd=2)

pp_plot(datos)
