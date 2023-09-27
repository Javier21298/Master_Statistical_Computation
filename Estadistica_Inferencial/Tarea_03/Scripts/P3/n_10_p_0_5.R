# Probabilidad y valor de n
p <- 0.5
q <- 1-p
n <- 10

# Cálculo de medias y varianzas
media <- n * p
desv <- sqrt(n * p * q)

# Valores de x para graficar
x <- seq(0, n, by = 1)

# Calcular las distribuciones
binomial <- dbinom(x, n, p)
normal <- dnorm(x, media, desv)

# Crear una matriz con los valores de ambas distribuciones
matriz_datos <- cbind(binomial, normal)

# Crear el gráfico utilizando matplot
matplot(x, matriz_datos, type = c("p","b"), col = c("blue", "red"), pch = 19, lwd = 2,
        main = "Distribución Binomial vs. Distribución Normal 
                p = 0.5 y n = 10",
        xlab = "Valores de x", ylab = "Densidad de Probabilidad")

# Agregar una leyenda
legend("topright", legend = c("Binomial", "Normal"), col = c("blue", "red"), lwd = 2)