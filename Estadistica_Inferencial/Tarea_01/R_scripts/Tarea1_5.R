bolas <- c(rep("gris",46),rep("blanca",49))

experimento <- sample(bolas, 20, replace = FALSE)

print(experimento)

numGris <- sum(experimento == "gris")

cat("Numero de bolas grises: ", numGris)

resultados <- matrix(nrow = 10000, ncol = 20)

for(i in 1:10000)
{
  experimento <- experimento <- sample(bolas, 20, replace = FALSE)
  resultados[i, ] <- experimento 
}

freqGrises <- rowSums(resultados == "gris")
freqGrises <- table(freqGrises)
freqP <- prop.table(freqGrises)

plot(freqGrises, xlab = "Numero de bolas grises extraidas p/ evento",
     ylab = "frecuencia del evento", main = "Experimento de 10,000 extracciones", col = "cornsilk4")

plot(freqP, xlab = "Numero de bolas grises extraidas p/ evento", 
     ylab = "frecuencia del evento", main = "Experimento de 10,000 extracciones", col = "black")

lines(0:20, dhyper(0:20,46,49,20), col = "palegreen3")

legend("topleft", legend=c("Experimento", "Hipergeometrica"), 
       fill = c("black", "palegreen3"))





