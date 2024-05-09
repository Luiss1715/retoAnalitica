
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

