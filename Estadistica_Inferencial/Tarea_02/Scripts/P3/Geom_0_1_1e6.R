results2 = read.csv("datos_1e6.csv")

tablaF1 <- table(results2[,2])   #Probabilidad = 0.1
tableF1 <- prop.table(tablaF1) #Normalizado
plot(tableF1, col = "royalblue", 
     main = "Frecuencias del intento 'x' hasta obtener águila \n p=0.1, N = 1e6",
     type = "h", 
     xlab = "Intento ", 
     ylab = "Frecuencia de éxitos en el 'x' intento")

muestra_geom <- dgeom(0:max(results2[,2]),0.1)

n <- 0:max(results2[,2])

lines(n+1,muestra_geom, col = "red2", type = "o", lty = 2)

legend("topright", legend = c("Simulación","Geométrica"), 
       col = c("royalblue","red2"), lty=c(1,2),lwd = c(2,1) , cex=0.9)
