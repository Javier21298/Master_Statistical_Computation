results = read.csv("C:/Users/ASUS/Downloads/datos_1e4.csv")

tablaF1 <- table(results[,1])   #Probabilidad = 0.5
tableF1 <- prop.table(tablaF1) #Normalizado
plot(tableF1, col = "royalblue", 
     main = "Frecuencias del intento 'x' hasta obtener águila \n p=0.5, N = 1e4",
     type = "h", 
     xlab = "Intento ", 
     ylab = "Frecuencia de éxitos en el 'x' intento")

muestra_geom <- dgeom(0:max(results[,1]),0.5)

n <- 0:max(results[,1])

lines(n+1,muestra_geom, col = "red2", type = "o", lty = 2)

legend("topright", legend = c("Simulación","Geométrica"), 
       col = c("royalblue","red2"), lty=c(1,2),lwd = c(3,1) , cex=0.9)
