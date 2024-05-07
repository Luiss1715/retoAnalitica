
getwd()
data <- read.csv("./GlobalLandTemperaturesByCountry.csv")
#limpiamos data
newData <- data[!is.na(data$AverageTemperature) & !is.na(data$AverageTemperatureUncertainty), ]


library(ggplot2)

avg_temp_by_country <- aggregate(AverageTemperature ~ Country, data = newData, FUN = mean, na.rm = TRUE)

paises_a_graficar <- c("Slovenia", "Tanzania", "Turkey")


avg_temp_by_country_filtrado <- subset(avg_temp_by_country, Country %in% paises_a_graficar)

# Ahora creamos el gráfico de barras
ggplot(avg_temp_by_country_filtrado, aes(x = Country, y = AverageTemperature)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Temperatura Promedio por País",
       x = "País",
       y = "Temperatura Promedio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#grafica del año 2013
data_2013 <- subset(newData, substr(dt, 1, 4) == "2013" & Country %in% c("Slovenia", "Tanzania", "Turkey"))

# Ahora agrupamos los datos por país y calculamos la temperatura promedio para cada país
avg_temp_by_country_2013 <- aggregate(AverageTemperature ~ Country, data = data_2013, FUN = mean, na.rm = TRUE)

# Ahora creamos el gráfico de barras
ggplot(avg_temp_by_country_2013, aes(x = Country, y = AverageTemperature)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Temperatura Promedio por País en 2013",
       x = "País",
       y = "Temperatura Promedio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#graficar todos los años
data_selected_countries <- subset(newData, Country %in% c("Slovenia", "Tanzania", "Turkey"))
data_selected_countries$Year <- format(as.Date(data_selected_countries$dt), "%Y")
data_selected_countries$Year <- as.numeric(data_selected_countries$Year)
avg_temp_by_year <- aggregate(AverageTemperature ~ Country + Year, data = data_selected_countries, FUN = mean, na.rm = TRUE)
ggplot(avg_temp_by_year, aes(x = Year, y = AverageTemperature, color = Country)) +
  geom_line(color = "red") +
  facet_wrap(~ Country, scales = "free_y") +
  labs(title = "Temperatura Promedio por Año",
       x = "Año",
       y = "Temperatura Promedio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

