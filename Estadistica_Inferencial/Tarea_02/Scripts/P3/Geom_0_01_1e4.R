results = read.csv("C:/Users/ASUS/Downloads/datos_1e4.csv")

tablaF3 <- table(results[,3])   #Probabilidad = 0.01
tableF3 <- prop.table(tablaF3) #Normalizado
plot(tableF3, col = "royalblue", 
     main = "Frecuencias del intento 'x' hasta obtener águila
             p=0.01, N = 1e4",
     type = "h", 
     xlab = "Intento ", 
     ylab = "Frecuencia de éxitos en el 'x' intento")

muestra_geom <- dgeom(0:max(results[,3]),0.01)

n <- 0:max(results[,3])

lines(n+1,muestra_geom, col = "red2", type = "o", lty = 2)

legend("topright", legend = c("Simulación","Geométrica"), 
       col = c("royalblue","red2"), lty=c(1,2),lwd = c(3,1) , cex=1)
