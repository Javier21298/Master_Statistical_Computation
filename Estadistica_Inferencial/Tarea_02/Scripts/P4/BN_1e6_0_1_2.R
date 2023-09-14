results2 <- read.csv("BN_1e6_0_1_2.csv") # p=0.1, r=2

tabla1 <- table(results2[,2])
tabla1N <- prop.table(tabla1) # Normalizamos las frecuencias para comparar con BN

plot(tabla1N, col = "royalblue",
     main = "Frecuencias del intento 'x' hasta obtener la 2da águila 
              p=0.1, N = 1e6, r = 2",
     type = "h",
     xlab = "Intento ",
     ylab = "Frecuencia de éxitos en el 'x' intento")

BN <- dnbinom(0:max(results2), 2 ,0.1)

lines(BN, col = "red2", type = "o", lty = 2)

legend("topright", legend = c("Simulación","Binom Neg."),
       col = c("royalblue","red2"), lty=c(1,2),lwd = c(3,1) , cex=0.9)