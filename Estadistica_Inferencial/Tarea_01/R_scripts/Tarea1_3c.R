
set.seed(13)

sample_data <- sample(1:10, size = 10000, replace = TRUE)
freq_table <- table(sample_data)

write.csv(freq_table, file = "tabla_frecuencias.csv")

mean_value <- mean(sample_data)
variance_value <- var(sample_data)

print(freq_table)
cat("\n")
cat("Media:", mean_value, "\n")
cat("Varianza:", variance_value, "\n")

barplot(freq_table, main="Frecuencias absolutas", x = c(1:10) , xlab = "números", ylab = "Frecuencias")



