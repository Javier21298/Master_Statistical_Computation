qq_normal <- function(datos, alpha) 
{
  # Ordenamos los datos
  datos_ordenados <- sort(datos)
  s <- sd(datos)        # Obtenemos desviación y media para escalar y trasladar
  x_bar <- mean(datos)
  
  datosNormalizados <- (datos_ordenados - x_bar) / s # Escalamos y trasladamos los datos
  
  n <- length(datos)
  # Calcular los cuantiles observados
  cuantiles_observados <- (1:n) / (n + 1)
  
  # Predicción cuantil de la normal (inversa de la acumulada):
  cuantiles_esperados <- qnorm(cuantiles_observados)
  
  D <- sqrt(-log(alpha / 2) / (2 * n)) #Estadístico de kolmogorov-Smirnov
  
  upper_band <- qnorm(pnorm(datosNormalizados) + D)*s + x_bar #Multiplicamos para reescalar
  lower_band <- qnorm(pnorm(datosNormalizados) - D)*s + x_bar # y trasladar las bandas
  
  plot(cuantiles_esperados, datos_ordenados, 
       xlab = "Cuantiles teóricos",
       ylab = "Datos",
       main = "Gráfico Q-Q Normal",
       col = "red",
       lty = 1)
  
  lines(cuantiles_esperados, upper_band, col = "orange", lty = 2, lwd = 2)
  lines(cuantiles_esperados, lower_band, col = "orange", lty = 2, lwd = 2)
  
  abline(a = x_bar, b = s, col = "royalblue", lty = 1) # juste
  
  legend("topleft", legend = c("Recta de ajuste", "Cuantiles de datos","Bandas de confianza"), 
         col = c("royalblue","red","orange"), pch = c(NA, 1, NA), lty = c(1,0,2), lwd = c(1,1,2))
  
  return(NULL)
}

datos <- c(23.37, 21.87, 24.41, 21.27, 23.33, 15.20, 24.21, 27.52, 15.48, 27.19,
           25.05, 20.40, 21.05, 28.83, 22.90, 18.00, 17.55, 25.92, 23.64, 28.96,
           23.02, 17.32, 30.74, 26.73, 17.22, 22.81, 20.78, 23.17, 21.60, 22.37)

qq_normal(datos, 0.05)
qq_normal(datos, 0.01)
