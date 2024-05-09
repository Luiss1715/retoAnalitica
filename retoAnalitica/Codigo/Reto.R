
setwd("D:/4 Semestre ITESM/SemanaTec Github/retoAnalitica/retoAnalitica/Codigo")
getwd()
data <- read.csv("./CasoHuracanesCSV.csv")


#limpiamos data

#quitar columna filas donde Name = "UNNAMED"
data <- data[data$Name != "            UNNAMED", ]
#quitar filas donde presion = -999
data <- data[data$Pressure != -999, ]
#quitar filas donde population = 0
data <- data[data$Population != 0, ]
data <-data[data$Wind > 0,]

data$Fecha = NULL
data$Month = NULL

View(data)



# punto4 ------------------------------------------------------------------

# Calcular la media y el promedio de las variables
media_wind <- mean(data$Wind)
media_pressure <- mean(data$Pressure)
media_co2 <- mean(data$CO2)
media_population <- mean(data$Population)

promedio_wind <- median(data$Wind)
promedio_pressure <- median(data$Pressure)
promedio_co2 <- median(data$CO2)
promedio_population <- median(data$Population)

# Mostrar los resultados utilizando sprintf
cat("Media de Wind: ", sprintf("%.2f", media_wind), "\n")
cat("Media de Pressure: ", sprintf("%.2f", media_pressure), "\n")
cat("Media de CO2: ", sprintf("%.2f", media_co2), "\n")
cat("Media de Population: ", sprintf("%.2f", media_population), "\n")

cat("Promedio de Wind: ", sprintf("%.2f", promedio_wind), "\n")
cat("Promedio de Pressure: ", sprintf("%.2f", promedio_pressure), "\n")
cat("Promedio de CO2: ", sprintf("%.2f", promedio_co2), "\n")
cat("Promedio de Population: ", sprintf("%.2f", promedio_population), "\n")

# paso 6 ------------------------------------------------------------------

#graficamos la correlacion
#limpiamos columnas no numericas 
dataCorrelation <- data
dataCorrelation$ID <- NULL
dataCorrelation$Ocean <- NULL
dataCorrelation$Name <- NULL
dataCorrelation$Clave <- NULL
dataCorrelation$Status <- NULL
dataCorrelation$year <- NULL
View(dataCorrelation)
corrmpg <- cor(dataCorrelation)
corrplot(corrmpg,methos = "elipse")


#regresion lineal
regC02Popu <- lm(dataCorrelation$Population ~ dataCorrelation$CO2)
summary(regC02Popu)
plot(dataCorrelation$Population,dataCorrelation$CO2,col="blue")
abline(regC02Popu,col="red")

#escalamos datos
HurEscalado <- scale(as.matrix(dataCorrelation))
kM2 <- kmeans(HurEscalado,2)
fviz_cluster(kM2,dataCorrelation)



#Cuartil del viento 
summary(data$Wind)
boxplot(data$Wind, main= "Cuartiles del viento")




#Cuartil de la presión 
summary(data$Pressure)
boxplot(data$Pressure, main= "Cuartiles de la presión")


#Cuartil del CO2 
summary(data$CO2)
boxplot(data$CO2, main= "Cuartiles del CO2")

#Cuartil de la población 
summary(data$Population)
boxplot(data$Population, main="Cuartiles de la población")


#Graficar la variable WIND 
plot(data$Wind, main= "Gráfica del viento", col= "blue")

data <- data[order(data$year), ]
plot(data$year, data$Wind, 
     main = "Año vs. Viento", 
     xlab = "Año", 
     ylab = "Viento",
     col = "blue") 

# Año vs presión 
plot(data$year, data$Pressure, 
     main = "Año vs. Presión", 
     xlab = "Año", 
     ylab = "Presión",
     col = "red") 

#Correlación entre presión y viento
cor(data$Wind, data$Pressure)
corGE <- data.frame(data$Wind, data$Pressure)
chart.Correlation(corGE, main= "Viento vs. Presión", col="red", lwd=15)

# Población vs C02 
plot(data$Population, data$CO2, 
     main = "Población vs. CO2", 
     xlab = "Población", 
     ylab = "CO2",
     col = "orange") 


# Lugares donde se producen más huracanes 
#Para visualizar la gráfica, se volvió a colocar la columna de océano
ggplot(data, aes(x = Ocean, fill = Ocean)) +
  geom_bar() +
  labs(title = "Frecuencia de huracanes por océano") +
  scale_fill_manual(values = c("pink", "violet")) 

#Temporada de huracanes
#Para visualizar la gráfica, se volvió a colocar la columna de mes. 

data$Month <- factor(data$Month, levels = c("January", "February", "March", "April", "May", "June", 
                                            "July", "August", "September", "October", "November", "December"))
ggplot(data, aes(x = Month)) +
  geom_bar() +
  labs(title = "Frecuencia de observaciones por mes")




#
