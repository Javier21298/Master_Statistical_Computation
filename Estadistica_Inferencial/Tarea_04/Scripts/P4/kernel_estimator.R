gaussian_kernel <- function(u) 
{
  return((1/sqrt(2*pi)) * exp(-0.5 * u^2))
}
kernel_estimator <- function(x,h,kernel,data)
{
  n <- length(data)
  estimated <- sum(sapply(data, function(xi) kernel((x - xi)/h))) 
  return((1/(n*h)) * estimated)
}

data <- read.csv("Tratamiento.csv")
tabla <- table(data)
tabla_prop <- prop.table(tabla)
plot(tabla_prop)

#Para h = 20

h <- 0.025
x <- data$X1
estimacion <- c()

for (i in seq_along(x)) 
{
  estimacion[i] <- kernel_estimator(x[i], h, gaussian_kernel, data$X1)
}

plot(x,estimacion)
hist(x, probability = T ,xlab = "Días de tratamiento", ylab = "Proporción",  main = "Comparación Kernel-estimator en el histograma \n h = 20")
points(x,estimacion, col= "green")




#Para h = 30


h <- 30
x <- data$X1
estimacion <- c()

for (i in seq_along(x)) 
{
  estimacion[i] <- kernel_estimator(x[i], h, gaussian_kernel, data$X1)
}

hist(x, probability = T ,xlab = "Días de tratamiento", ylab = "Proporción",  main = "Comparación Kernel-estimator en el histograma \n h = 30")
points(x,estimacion, col = "green")


#Para h = 60


h <- 60
x <- data$X1
estimacion <- c()

for (i in seq_along(x)) 
{
  estimacion[i] <- kernel_estimator(x[i], h, gaussian_kernel, data$X1)
}

hist(x, probability = T ,xlab = "Días de tratamiento", ylab = "Proporción", main = "Comparación Kernel-estimator en el histograma \n h = 60")
points(x,estimacion, col = "green")


#Optimizado

h <- 14.7194
x <- data$X1
estimacion <- c()

for (i in seq_along(x)) 
{
  estimacion[i] <- kernel_estimator(x[i], h, gaussian_kernel, data$X1)
}

hist(x, probability = T, breaks = 12 ,xlab = "Días de tratamiento", ylab = "Proporción", main = "Comparación Kernel-estimator en el histograma \n h = 14.71")
points(x,estimacion, col = "blue")

legend("topright", 
       legend = c("Estimador Kernel"),
       col = c("blue"),
       pch = c(1, NA, 16),  
       lty = c(NA, 1, NA))  
