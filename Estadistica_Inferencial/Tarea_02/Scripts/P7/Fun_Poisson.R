simPoisson <- function(T_, lambda)
{
  No_Intervalos = 1000 #Divisiones del intervalo 
  dt <- T_/No_Intervalos #Intervalos pequeños que subdividen [0,T]
  p <- lambda * dt + 10^(-6)
  probabilidades <- c(1-p,p) 
  vectorEventos <- c()
  vectorTiempo <- c()
  suma <- 0
  
  resultado <- 1
  for(k in 1:No_Intervalos-1)
  {
    if (resultado == 1)
    {
      suma <- suma + resultado
      vectorTiempo <- c(vectorTiempo, k*dt)
      vectorEventos <- c(vectorEventos,suma)
    }
    else
    {
      vectorTiempo <- c(vectorTiempo, k*dt)
      vectorEventos <- c(vectorEventos,suma)
    }
    
    resultado <- sample(0:1, size = 1 ,prob = probabilidades,
                        replace = T)  
    
  }
  return(list(vt = vectorTiempo, ve = vectorEventos))
}
