library(corrplot)
library(factoextra)
data("mtcars")
mtcars.vs = NULL
mtcars.am = NULL
View(mtcars)


#graficamos la correlacion
corrmpg <- cor(mtcars)
corrplot(corrmpg,methos = "elipse")

# k means 
kM <- kmeans(mtcars,4)
kM
fviz_cluster(kM,mtcars)

#escalamos datos
carsEscalado <- scale(as.matrix(mtcars))
kM2 <- kmeans(carsEscalado,4)
fviz_cluster(kM2,mtcars)


# Graficos de correlacion 
#Correlación entre millas por galón y desplazamiento
cor(mtcars$mpg, mtcars$disp)
corGE <- data.frame(mtcars$mpg, mtcars$disp)
chart.Correlation(corGE)


#Correlación entre millas por galón y caballos
cor(mtcars$mpg, mtcars$hp)
corGE <- data.frame(mtcars$mpg, mtcars$hp)
chart.Correlation(corGE)


#Correlación entre millas por galón y relación del eje trasero
cor(mtcars$mpg, mtcars$drat)
corGE <- data.frame(mtcars$mpg, mtcars$drat)
chart.Correlation(corGE)



#Correlación entre millas por galón y peso
cor(mtcars$mpg, mtcars$wt)
corGE <- data.frame(mtcars$mpg, mtcars$wt)
chart.Correlation(corGE)
