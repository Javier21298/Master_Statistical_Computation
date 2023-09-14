library(ggplot2)  
library(dplyr)    


delitos <- read.csv("Delitos.csv")
datos_delitos <- dplyr::select(delitos, TIPO, SEMANA)


# Create a box plot
ggplot(datos_delitos, aes(x = TIPO, y = SEMANA)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Comportamiento semanal de los delitos",
    x = "Tipo de delito",
    y = "Semana del año"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
