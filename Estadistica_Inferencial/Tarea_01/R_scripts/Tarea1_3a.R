
uniformP <- function(x, min = 1, max = 10)
{
  ifelse(x>=min & x<=max & round(x) == x,1/(max-min+1), 0 )
}

uniformC <-function(x,min = 1, max = 10)
{
  ifelse(x<min,0,ifelse(x<=max,floor(x)/(max-min+1),1))
}

par(mfrow = c(1, 2))


min <- 1
max <- 10

x <- 1:10
y1 <- sapply(x,uniformP)
y2 <- sapply(x, uniformC)


plot(x, y1, xlab = "x", ylab = "PMF", main = "Uniform PDF")
axis(1, at = x, labels = x)
plot(ecdf(x), xlab = "x", ylab = "CMF", main = "Uniform CDF")
axis(1, at = x, labels = x)






