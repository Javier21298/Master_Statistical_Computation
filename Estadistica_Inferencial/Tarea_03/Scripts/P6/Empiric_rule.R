k <- c(1,2,3)

# Definición de las funciones
prob_uniform <- function(media, var, k, a, b) 
{
  return(punif(media + k*sqrt(var), a, b) - punif(media - k*sqrt(var), a, b))
}

prob_normal <- function(media, var, k) 
{
  return(pnorm(media + k*sqrt(var)) - pnorm(media - k*sqrt(var)))
}

prob_exponencial <- function(media, var, k) 
{
  lamda <- 1/media
  return(pexp(media + k*sqrt(var), rate=lamda) - pexp(media - k*sqrt(var), rate=lamda))
}

prob_gamma <- function(media, var, k, alpha, beta) 
{
  return(pgamma(media + k*sqrt(var), shape=alpha, scale=1/beta) - pgamma(media - k*sqrt(var), shape=alpha, scale=1/beta))
}

prob_beta <- function(media, var, k, alpha, beta) 
{
  return(pbeta(media + k*sqrt(var), shape1=alpha, shape2=beta) - pbeta(media - k*sqrt(var), shape1=alpha, shape2=beta))
}

prob_weibull <- function(media, var, k, alpha, beta) 
{
  return(pweibull(media + k*sqrt(var), shape=alpha, scale=beta) - pweibull(media - k*sqrt(var), shape=alpha, scale=beta))
}

prob_lognormal <- function(media, var, k, mu, sigma) 
{
  return(plnorm(media + k*sqrt(var), meanlog=mu, sdlog=sigma) - plnorm(media - k*sqrt(var), meanlog=mu, sdlog=sigma))
}

# Cálculo de las probabilidades para k=1,2,3
distribuciones <- c("Uniforme", "Normal", "Exponencial", "Gamma1", "Gamma2", "Beta", "Weibull", "Log-normal")

valores_k <- list()
for (val in k) {
  valores_k[[as.character(val)]] <- c(
    prob_uniform((3+(-3))/2, ((3-(-3))^2)/12, val, -3, 3),
    prob_normal(0, 1, val),
    prob_exponencial(1/2, 1/4, val),
    prob_gamma(2, 2, val, 2, 1),
    prob_gamma(3, 3, val, 3, 1),
    prob_beta(2/(2+2), (2*2)/((2+2+1)*(2+2)^2), val, 2, 2),
    prob_weibull(1*gamma(1+1/4), 1^2*(gamma(1+2/4) - gamma(1+1/4)^2), val, 4, 1),
    prob_lognormal(exp(3 + (2^2)/2), (exp(2^2) - 1)*exp(2*3 + 2^2), val, 3, 2)
  )
}

# Creando el dataframe
tabla1 <- data.frame(Distribucion = distribuciones,
                    'k = 1' = valores_k[['1']],
                    'k = 2' = valores_k[['2']],
                    'k = 3' = valores_k[['3']])
print(tabla1)

write.csv(tabla1, file = "Tabla_teoricos.csv")






#Segunda parte: simulación de cada distribución



sim_unif <- runif(1000,-3,3)
sim_norm <- rnorm(1000,0,1)
sim_exp <- rexp(1000,2)
sim_gamma1 <- rgamma(1000, 2, 1)
sim_gamma2 <- rgamma(1000, 3, 1)
sim_beta <- rbeta(1000,2,2)
sim_weibull <- rweibull(1000,4,1)
sim_logrnorm <- rlnorm(1000,3,2)

print(mean(sim_gamma1))
print(sd(sim_gamma1))


medias <- c(mean(sim_unif), 
            mean(sim_norm), 
            mean(sim_exp), 
            mean(sim_gamma1), 
            mean(sim_gamma2), 
            mean(sim_beta), 
            mean(sim_weibull), 
            mean(sim_logrnorm))

desv <- c(sd(sim_unif), 
          sd(sim_norm), 
          sd(sim_exp), 
          sd(sim_gamma1), 
          sd(sim_gamma2), 
          sd(sim_beta), 
          sd(sim_weibull), 
          sd(sim_logrnorm))


valores_k_emp <- list()
for (val in k) 
{
  valores_k_emp[[as.character(val)]] <- c(
    prob_uniform(medias[1],desv[1]^2, val, -3, 3),
    prob_normal(medias[2],desv[2]^2 , val),
    prob_exponencial(medias[3], desv[3]^2, val),
    prob_gamma(medias[4], desv[4]^2, val, 2, 1),
    prob_gamma(medias[5], desv[5]^2, val, 3, 1),
    prob_beta(medias[6], desv[6]^2, val, 2, 2),
    prob_weibull( medias[7], desv[7]^2, val, 4, 1),
    prob_lognormal( medias[8], desv[8]^2, val, 3, 2)
  )
}

print(medias[8])
print(desv[8])

hist(sim_unif)

tabla2 <- data.frame(Distribucion = distribuciones,
                    'k1' = valores_k_emp[['1']],
                    'k2' = valores_k_emp[['2']],
                    'k3' = valores_k_emp[['3']])

print(tabla2)
write.csv(tabla2, file = "Tabla_simulados.csv")

tablaComparativa <- data.frame(Distribucion = distribuciones,
                               'k1_teo' = valores_k[['1']],
                               'k1_sim' = valores_k_emp[['1']],
                               'k2_teo' = valores_k[['2']],
                               'k2_sim' = valores_k_emp[['2']],
                               'k3_teo' = valores_k[['3']],
                               'k3_sim' = valores_k_emp[['3']])

print(tablaComparativa)
write.csv(tablaComparativa, file = "TablaComparativa.csv")

