gaussian_kernel <- function(u) 
{
  return((1/sqrt(2*pi)) * exp(-0.5 * u^2))
}

leave_one_out_kde <- function(x, left_out_index, h, kernel_function, data) 
{
  data <- data[-left_out_index]  
  return(kernel_estimator(x, h, kernel_function, data))
}

kernel_estimator <- function(x,h,kernel,data)
{
  n <- length(data)
  estimated <- sum(sapply(data, function(xi) kernel((x - xi)/h))) 
  return((1/(n*h)) * estimated)
}
ucv <- function(h, kernel_function, data) 
{
  n <- length(data)
  termino1 <- sum(sapply(data, function(x) kernel_estimator(x, h, kernel_function, data)^2)) / n
  termino2 <- mean(sapply(1:n, function(i) leave_one_out_kde(data[i], i, h, kernel_function, data)))
  return(termino1 - 2/n * termino2)
}

data <- read.csv("Tratamiento.csv")

print(data)

h_values <- seq(5, 30, length.out = 500)  

ucv_values <- sapply(h_values, ucv, kernel_function = gaussian_kernel, data = data$X1)

optimal_h <- h_values[which.min(ucv_values)]

print(optimal_h)

plot(h_values, ucv_values, type = "l", main = "UCV vs Bandwidth h", xlab = "h", ylab = "UCV")
abline(v = optimal_h, col = "red", lty = 2)
