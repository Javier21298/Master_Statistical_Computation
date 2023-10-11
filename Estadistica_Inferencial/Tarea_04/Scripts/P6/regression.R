regresion_minimos <- function(x_data, y_data) {
  n <- length(x_data) 
  numerador_alpha <- sum(x_data * y_data) -  sum(x_data)*sum(y_data) / n
  denominador_alpha <- sum(x_data^2) - (sum(x_data))^2 / n
  alpha <- numerador_alpha / denominador_alpha
  
  beta <- (1/n) * sum(y_data) -  alpha * (1/n) * sum(x_data)
  
  result <- list(alpha = alpha, beta = beta)
  return(result)
}

gaussian_kernel <- function(u) 
{
  return((1/sqrt(2*pi)) * exp(-0.5 * u^2))
}
uniform_kernel <- function(u) 
{
  if (abs(u) <= 1) 
  {
    return(0.5)
  }else
  {
    return(0)
  }
}

kernel_regression <- function(x ,x_data, y_data, h, kernel, data)
{
  n <- length(x_data)
  
  numerador <- sum(y_data * sapply(x_data, function(x_i) kernel((x-x_i)/h)) )
  
  denominador <- sum(sapply(x_data, function(x_i) kernel((x-x_i)/h)))
  
  y_n <- numerador/denominador
  
  return(y_n)
}

data <- read.csv("Maiz.csv")
x <- datos$P_tonelada_maiz
y <- datos$P_tonelada_tortilla
h <- 3

regresion_2 <- numeric()

regresion_1 <- regresion_minimos(datos$P_tonelada_maiz, datos$P_tonelada_tortilla)

for(i in seq_along(x))
{
  regresion_2[i] <- kernel_regression(x[i],x,y,h, gaussian_kernel,data)
}

plot(x,regresion_2)


plot(datos$P_tonelada_maiz, datos$P_tonelada_tortilla,
     main = "Regresiones",
     xlab = "P_tonelada_maiz",
     ylab = "P_tonelada_tortilla")

abline(a = regresion_1$beta, b = regresion_1$alpha, col = "red")

points(x,regresion_2, col = "green", pch = 16)

legend("topleft", 
       legend = c("Datos", "Mínimos cuadrados", "Regresión por kernel"),
       col = c("black", "red", "green"),
       pch = c(1, NA, 16),  
       lty = c(NA, 1, NA))  
    
print(regresion_1)

