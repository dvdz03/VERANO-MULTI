#clase 1JULIO
# DATOS
data(iris)
str(iris)
iris#especies de iris, especies y variables morfométricas
library(tidyverse)
view(iris)
#empezar a ver con un análisis univariado de cada variable morfométrica
#LONGITUD DEL SEPALO
tapply(iris$Sepal.Length,iris$Species,summary)#la longitud del sepalo de setosa es más chica que las otras dos

boxplot(iris$Sepal.Length~iris$Species)#setosa con virginica si son más diferentes porque los rangos no se sobrelapan mucho.
tapply(iris$Sepal.Length,iris$Species,shapiro.test)#cada una se distribuye de manera normal, para la variable del sepalo
bartlett.test(iris$Sepal.Length~iris$Species)#no hay homogeneidad de varianza
#ANCHO SEPALO
tapply(iris$Sepal.Width,iris$Species,summary)
boxplot(iris$Sepal.Width~iris$Species)#aquí setosa es más diferente que versicolor y virginica
tapply(iris$Sepal.Width,iris$Species,shapiro.test)#todos son normales
bartlett.test(iris$Sepal.Width~iris$Species)#aquí si hay homogeneidad de varianzas
#como componer la varianza, VERSIÓN NO PARAMÉTRICA DE LA PRUEBA DE T, U DE mann-whitney/wilcoxon. wilcox.test
#son robustas a las desviaciones o algo así
#kruskal-wallis es anova no paramétrico

#LONGITUD PETALO
tapply(iris$Petal.Length,iris$Species,summary)
boxplot(iris$Petal.Length~iris$Species)
tapply(petalolargo,iris$Species,shapiro.test)
bartlett.test(petalolargo~iris$Species)#mp hay varianzas homogéneas
petalolargo<-(iris$Petal.Length)^0.1414
boxCox(iris$Petal.Length~iris$Species)

#esto es de google
library(MASS)
juanis<-lm(iris$Petal.Length^0.2~iris$Species)#esto se hace con el boxcox
boxCox(juanis)
shapiro.test(juanis$residuals)
leveneTest(iris$Petal.Length^0.2~iris$Species)

pechuga<-juanis2$x[which.max(juanis2$y)]
pechuga




#ANCHO PETALO
tapply(iris$Petal.Width,iris$Species,summary)
boxplot(iris$Petal.Width~iris$Species)
tapply(iris$Petal.Width,iris$Species,shapiro.test)
bartlett.test(iris$Petal.Width~iris$Species)
leveneTest(iris$Petal.Width~iris$Species)
#estos datos no son ni normales ni homogeneos
#ahora tenemos que intentar componer haciendo transformaciones. 
#estas son las transformaciones jeje
pechuga<-lm(iris$Petal.Width^0.6~iris$Species)
boxCox(pechuga)
shapiro.test(pechuga$residuals)
leveneTest(iris$Petal.Width^0.6~iris$Species)

#METODOS DE ORDENACION 
#2 FUNCIONES prcomp y princomp, ver que te da cada uno
#prcomp te da la varianza, cargas 
#princomp nos da las calificaciones, las variables artificiales, las cargas (correlación del componente ocon cada variable, la contribución de cada variable al componente)
sepalongitud<-scale(iris$Sepal.Length^-1)
sepancho<-scale(iris$Sepal.Width)
petalolargo<-scale(iris$Petal.Length^0.2)
petancho<-scale(iris$Petal.Width^0.6)

morfos<-data.frame(sepalongitud,sepancho,petalolargo,petancho)
morfos
compos<-princomp(morfos)
library(tidyverse)
#poner el comopnente 1 es la primera columna de la matriz de calificaciones
C1<-compos$scores[,1]#componente 1, todas las observaciones de la primera columna
C2<-compos$scores[,2]
#gráfico de dispersión
espes<-iris$Species
plot(C1,C2,pch=20)[iris$Species]
elpca<-data.frame(C1,C2,espes)
ggplot(elpca,aes(C1,C2,col=espes))+
  geom_point()
#ya eso nos da el gráfico del PCA, graficamos los componentes, lo que hace es acomodar la varianza común en cada componente, cada uno de esos es ortogonal al otro, diferente al MDS
#ahora para las elipses
ggplot(elpca,aes(C1,C2,col=espes))+
  geom_point()+
  stat_ellipse()
#PCA nos ilustra perfectamente la situación mijito

#MÉTODO QUE SE BASA EN LA MATRIZ DE DISTANCIAS

# MDS 
mds_iris <- cmdscale(dist(iris[, 1:4]))#te pide matriz de distnacias, cuantas dimensiones quieres, si quieres eigenvvalores de cada dimensión
#te devuelve una serie de puntos por dimensión, lo que le pone ahí es que calcule las distsancias de eso. pero ya hicimos un data frame con las variables morfológicas ya transformadas y escaladas.
#para hacerlo de la cosa esa uqe hicimos
mds_iris1<-cmdscale(dist(morfos))#por defecto te da 2 dimensiones
#dist te calcula la euclidiana por defecto, si es bray es otra cosa mijo

# GRÁFICO
plot(mds_iris1[, 1], mds_iris1[, 2], 
     type = "n", xlab = "Dimensión 1", 
     ylab = "Dimensión 2")
points(mds_iris1[, 1], mds_iris1[, 2], 
       pch = 21, bg = "lightblue")
text(mds_iris1[, 1], mds_iris1[, 2], 
     labels = substr(iris$Species, 1, 2), 
     pos = 3, cex = 0.8)

#por disimilitud o algo así dijo. 

#EJEMPLO 2
# ESCALAMIENTO MULTIDIMENSIONAL MÉTRICO
library(tidyverse)
install.packages("magrittr")
library(magrittr)
install.packages("ggpubr")
library(ggpubr)

data("swiss")
view(swiss)


#la cosa esa es para concatenar o unir las instrucciones, para que no tengas que escribir más. es con tidyverse
#también hay otro pipe |>, encadena las instrucciones
mds <- swiss %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")
# Plot MDS
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(swiss),
          size = 1,
          repel = TRUE)

#EJERCICIO BASE DE DATOS CALIDAD VINO

library(MASS)
library(tidyverse)
library(magrittr)
library(ggpubr)

url<-"https://archive.ics.uci.edu/static/public/186/wine+quality.zip"
destfile<-"C:/Users/100032608/Downloads/wine+quality.zip"
download.file(url, destfile)
unzip("C:/Users/100032608/Downloads/wine+quality.zip")

vinos<-read.csv("winequality-white.csv", header = TRUE, sep = ";")
head(vinos)
vinos$quality<-factor(vinos$quality)
view(vinos)
ene<-1:4898
muestra<-sample(ene,100)
vinos2<-vinos[muestra,]
str(vinos2)
# MDS 
mds_vinos <- cmdscale(dist(vinos2[, 1:11]),k=3)

# GRÁFICO

df<-data.frame(mds_vinos[,1],mds_vinos[,2],vinos2$quality)
df
noms<-c("D1", "D2", "Calidad")
colnames(df)<-noms

ggplot(df, aes(x=D1, y=D2, color= Calidad)) +
  geom_point()
view(vinos)
head(vinos)

#Resumen numérico
#ACIDEZ
tapply(vinos$fixed.acidity,vinos$quality,summary)
boxplot(vinos$fixed.acidity~vinos$quality)
tapply(vinos$fixed.acidity,vinos$quality,shapiro.test)
bartlett.test(vinos$fixed.acidity~vinos$quality)#no homogeneidad
#volatil
tapply(vinos$volatile.acidity,vinos$quality,summary)
boxplot(vinos$volatile.acidity~vinos$quality)
tapply(vinos$volatile.acidity,vinos$quality,shapiro.test)
bartlett.test(vinos$volatile.acidity~vinos$quality)#no homogeneidad
#acido citrico
tapply(vinos$citric.acid,vinos$quality,summary)
boxplot(vinos$citric.acid~vinos$quality)
tapply(vinos$citric.acid,vinos$quality,shapiro.test)
bartlett.test(vinos$citric.acid~vinos$quality)
#azucar residual
tapply(vinos$residual.sugar,vinos$quality,summary)
boxplot(vinos$residual.sugar~vinos$quality)
tapply(vinos$residual.sugar,vinos$quality,shapiro.test)
bartlett.test(vinos$residual.sugar~vinos$quality)
#cloro
tapply(vinos$chlorides,vinos$quality,summary)
boxplot(vinos$chlorides~vinos$quality)
tapply(vinos$chlorides,vinos$quality,shapiro.test)
bartlett.test(vinos$chlorides~vinos$quality)
#dioxido de azufre libre
#dioxido de azufre total
#densidad
#ph
#sulfatos
#alcohol
