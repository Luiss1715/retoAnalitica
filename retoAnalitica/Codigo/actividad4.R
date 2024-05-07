
getwd()

data <- read.csv("./GlobalLandTemperaturesByCountry.csv")


#limpiamos data

newData <- data[!is.na(data$AverageTemperature) & !is.na(data$AverageTemperatureUncertainty), ]
#newData <- data[!is.na(data$AverageTemperatureUncertainty),]
#View(newData)

#sapply(newData,function(x)sum(is.na(x)))

#una vez limipio, hacemos el analisis de temperatura promedio por
#cada pais por cada año 

# Supongamos que tu dataframe se llama 'data' y tiene las columnas: fecha, temperatura, temperaturaIncertidumbre y pais
library(ggplot2)

# Primero, agrupamos los datos por país y calculamos la temperatura promedio para cada país
avg_temp_by_country <- aggregate(AverageTemperature ~ Country, data = newData, FUN = mean, na.rm = TRUE)

paises_a_graficar <- c("Slovenia", "Tanzania", "Turkey")  # Reemplaza 'Pais1', 'Pais2' y 'Pais3' con los nombres de los países que desees graficar

avg_temp_by_country_filtrado <- avg_temp_by_country[avg_temp_by_country$AverageTemperature %in% paises_a_graficar, ]

# Ahora creamos el gráfico de barras
ggplot(avg_temp_by_country_filtrado, aes(x = Country, y = AverageTemperature)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Temperatura Promedio por País",
       x = "País",
       y = "Temperatura Promedio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

