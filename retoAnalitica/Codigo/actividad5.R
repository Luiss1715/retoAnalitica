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