results = read.csv("C:/Users/ASUS/Downloads/datos_1e4.csv")

tablaF2 <- table(results[,2])   #Probabilidad = 0.1
tableF2 <- prop.table(tablaF2) #Normalizado
plot(tableF2, col = "royalblue", 
     main = "Frecuencias del intento 'x' hasta obtener águila \n p=0.1, N = 1e4",
     type = "h", 
     xlab = "Intento ", 
     ylab = "Frecuencia de éxitos en el 'x' intento")

muestra_geom <- dgeom(0:max(results[,2]),0.1)

n <- 0:max(results[,2])

lines(n+1,muestra_geom, col = "red2", type = "o", lty = 2)

legend("topright", legend = c("Simulación","Geométrica"), 
       col = c("royalblue","red2"), lty=c(1,2),lwd = c(3,1) , cex=0.9)