#CLASE 30 JUN
#ANÁLISIS DE CONGLOMERADOS
hclust()#matriz de disimilitud, método, necesitamos una matriz de distancias
dist()#esto es para la matriz de distancias, paquete vegan 
install.packages("vegan")
library(vegan)
#base de datos, calcular matriz de distancias, eso a hclust y luego seleccionar el método que quieras utiliza
#para tomar decisiones ajustar diferentes modelos, promedio mediana o así y determinar cual queda mejor, más congruente con tu teoría. 
#cargar vegan y luego ver los datos que tienes, datos numéricos, si tenemos una variable nominal para pasarla a cuantitativa tienes que hacerla una variable dummy.
#ya que lo calcula lo guardas en objeto y hclust lo hace arbol, ahí decides como agrupar. 
library(factoextra)
datos30JUN<-read.table("C:/Users/100032608/Downloads/datos30JUN.txt", header=TRUE)
datos30JUN
str(datos30JUN)#COMO ESTÁN RELACIONADAS LAS VARIABLES, ANÁLISIS EXPLORATORIO UNIVARIADO. 
#si hay varios datos, una correlación no paramétrica, kendall o spearman 
#2 variables numéricas y una agrupación. Análisis bivariado
#ver que tan asociadas están las cuantitativas continuas, hacer lo de siempre, un análisis gráfico y numérico. uni y bivariado
summary(datos30JUN)
library(Rmisc)
summarySE(datos30JUN,measurevar="x")
summarySE(datos30JUN,measurevar="y")
library(car)
#función dist, se pueden calcular la matriz de distancias.
shapiro.test(datos30JUN$y)
shapiro.test(datos30JUN$x)
#ninguno son normales
#ahora ver las gráficas individuales
hist(datos30JUN$x)#HAY DOS MODAS, entonces va a haber por lo menos 2 grupos ahí
plot(density(datos30JUN$x))
boxplot(datos30JUN)

tapply(datos30JUN$x,datos30JUN$group,shapiro.test)#son varias distribuciones
plot(density(datos30JUN$y))#HAY POR LO MENOS 3 CHIPOTES ENTONCES 3 GRUPOS TAL VEZ


tapply(datos30JUN$y,datos30JUN$group,shapiro.test)
#esto se puede hacer solo cuando todo es normal
tapply(datos30JUN$y,datos30JUN$x,cor.test)
numericas<-cbind(datos30JUN$x,datos30JUN$y)
distas<-dist(numericas)
distas
grupos<-hclust(distas)
plot(grupos)

library(vegan)
library(tidyverse)
library(factoextra)
library(cluster)
install.packages("NbClust")
library(NbClust)#prueba para determinar que tantos grupos valen la pena
install.packages("dendroextras")
library(dendroextras)

data("dune")
view(dune)


#
library(vegan)
library(factoextra)
library(car)

#Matriz de distancias con vegan
#Funcion h-cloust para comparar y luego qu emetodo 
#Objeto en un arbol y tu decides como agrupar

#Cargar base de datos 
base <- read.table("C:/Users/tmock/Downloads/VERANO MULTIVARIADA/VERANO MULTI/datos2.txt", header = T)
View(base)
#Son variables numéricas
#Función "dist" es de base y se puede calcular la matriz de distancia 

#Como se ven los datos
boxplot(base$x)
hist(base$x)
#Se pueden ver diferentes distribuciones
plot(density(base$x))
boxplot(base$x ~ base$group) #Para ver por grupo 


#Son normales?
#NO SON NOMALES PORQUE SON MENORES A 0.05 
#Por eso en los garficos se ven dos grupos (en hist)
shapiro.test(base$x)
shapiro.test(base$y)

#Para calcular diferentes funciones 
tapply(base$x, base$group, shapiro.test)
#Los valores son no significativos por lo tanto cada grupoes normal 

shapiro.test(base$y)
tapply(base$y, base$group, shapiro.test)


#Prueba de correlacion
cor.test(base$x, base$y)
#vARIBALES ALTAMENTE CORRELACIONADAS 
numericas <- cbind(base$x, base$y)

distas <- dist(numericas)
distas

grupos <- hclust(distas)
plot(grupos)




#ESTO NO
# k medias 2

setwd("C:/Users/Monica Figueroa/Desktop/CONGLOMERADOS")

library(tidyverse)
library(factoextra)

taxa<-read.table("taxon.txt",header=T)

view(taxa)

attach(taxa)
names(taxa)

pairs(taxa)

kmd2<-kmeans(taxa,2)
grupos2<-kmd2$cluster
plot(taxa$Petals, taxa$Internode, col=grupos2,pch=16)
plot(taxa$Petals, taxa$Bract, col=grupos2,pch=16)

fviz_cluster(kmd2, data=taxa)

kmd3<-kmeans(taxa,3)
fviz_cluster(kmd3, data=taxa)

grupos3<-kmd3$cluster
plot(taxa$Petals, taxa$Internode, col=grupos3,pch=16)
plot(taxa$Petals, taxa$Sepal, col=grupos3,pch=16)
plot(taxa$Sepal, taxa$Petiole, col = grupos3,pch=16)
plot(taxa$Sepal, taxa$Leaf, col = grupos3,pch=16)

kmd4<-kmeans(taxa,4)
fviz_cluster(kmd4, data=taxa)
grupos4<-kmd4$cluster
plot(taxa$Petals, taxa$Internode, col=grupos4,pch=16)
plot(taxa$Petals, taxa$Sepal, col=grupos4,pch=16)

kmd5<-kmeans(taxa,5)
fviz_cluster(kmd5, data=taxa)
grupos5<-kmd5$cluster
plot(taxa$Petals, taxa$Internode, col=grupos5,pch=16)

kmd6<-kmeans(taxa,6)
fviz_cluster(kmd6, data=taxa)

fviz_nbclust(taxa, kmeans, method = "wss")

detach(taxa)

############

# Análisis de componentes
view(taxa)
cor(taxa)
# distancias<-dist(taxa)
# distancias
compos<-princomp(taxa)
compos$sdev
compos$loadings
cp1<-compos$scores[,1]
cp2<-compos$scores[,2]

plot(cp1, cp2)

taxa2<-data.frame(cp1, cp2)
kmd4b<-kmeans(taxa2,4)
grupos4<-kmd4b$cluster
plot(taxa2$cp1, taxa2$cp2, col=grupos4,pch=16)

detach(taxa)