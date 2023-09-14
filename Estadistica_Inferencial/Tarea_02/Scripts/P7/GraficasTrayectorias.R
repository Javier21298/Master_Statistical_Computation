experimento1 <- simPoisson(10,2)
experimento2 <- simPoisson(10,2)
experimento3 <- simPoisson(10,2)

#Alternativa para x = seq(0,T,dt)
matplot(experimento1$vt, cbind(experimento1$ve,experimento2$ve,experimento3$ve),
        type = "l", xlab = "Tiempo", ylab = "Acumulado de eventos",
        main = "Trayectorias \n T = 10, lamda = 2 ", pch = 19, lwd = 5, lty = 1) 

legend("topleft", legend = c("Experimento 1", "Experimento 2", "Experimento 3"),
       col = 1:3, lty = 1, lwd = 5, cex = 0.8)
