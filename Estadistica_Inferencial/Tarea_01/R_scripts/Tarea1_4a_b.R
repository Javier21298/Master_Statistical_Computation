
set.seed(13)

posiblesResultados <- c("T","H")


experimento <- sample(posiblesResultados, size = 10, replace = TRUE)

print(experimento)

resultados <- matrix(nrow = 10000, ncol = 10)

print("Primeros 3 tres resultados: ")

for(i in 1:10000)
{
  experimento <- sample(posiblesResultados, size = 10, replace = TRUE)
  resultados[i, ] <- experimento
  
  if(i<=3)
  {
    print(resultados[i, ])
  }
  
}

sumaCaras <- rowSums(resultados[1:3, ] == "H")

print(sumaCaras)

par(mfrow = c(2, 2))

freCaras <- rowSums(resultados == "H")
freCarasF <- table(freCaras)
freCarasP <- prop.table(freCarasF) #Datos de manera proporcional

binomial <- dbinom(0:10,10,0.5)

plot(freCarasF, main = "Frecuencia de caras", xlab = "Numero de caras", 
     ylab = "Frecuencia de resultado", col = "#EE6A50")

plot(freCarasP, main = "Proporción de Caras", xlab = "Numero de caras", 
     ylab = "Proporción", col = "royalblue", type ="h")

lines(0:10,binomial, type="o", col = "black")

legend("topleft", legend=c("Experimento", "Binomial"), 
       fill = c("royalblue", "black"))









