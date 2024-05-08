
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


