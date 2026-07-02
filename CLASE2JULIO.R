#CORRELACIÓN CANÓNICA EJERCICIOS 02JULIO

library(tidyverse)
install.packages("xlsx",dependencies = T)
library(xlsx)
install.packages("CCA")
library(CCA)
library(vegan)
library(GGally)
library(fields)
install.packages("openxlsx")
library(openxlsx)

# tenemos 10 estados de México donde se observa la presencia 
# de 5 especies endémicas y 5 variables climaticas medidas en cada sitio. 

bichos<-read.csv("C:/Users/100032608/Downloads/bichos.xlsx", header = T)
bichos<-read.xlsx("C:/Users/100032608/Downloads/bichos.xlsx", sheetIndex = 1)
view(bichos)

especies<-bichos[,2:6]
clima<-bichos[,7:11]
ggpairs(especies, title = "Especies endemicas")#son conteos, esperamos que no en todos los lugares estén las mismas condiciones climáticas
#no es para ver como se distribuyen, ya sabemos que no va a ser lineal, lo que estmaos viendo es quien vive en donde.
#las que se relacionan de manera significaitva es porque tienen preferencias amientales parecidas, ver quien está junto con quien. 
ggpairs(clima, title = "Variables climaticas")
#no hay una tendencia muy definida, hay gráficos, hay al menos 2 modas, por ejemplo en la temperatura. que hay 2 grupos. 2 poblaciones, 2 condiciones de temperatura. 
#si hay escarabajos y esponjas y corales entonces la temperatura si va a variar. la mayor parte de estas variables no se distribuye de manera normal y mo hay un solo lugar con la misma temperatura
#podríamos ver que temperatura están en localidad, un tapply para determinar que localidades tienen temperaturas o así con las demás.
rownames(bichos)
#un vector con las localidades
climas<-data.frame(bichos$sitio,clima)
tapply(climas$temperatura,climas$bichos.sitio,summary)
medianas <- tapply(climas$temperatura, climas$bichos.sitio, median)
barplot(sort(medianas),las=2)
humedad<-tapply(climas$humedad,climas$bichos.sitio,median)
barplot(sort(humedad),las=2)
#necesito tener la variable de las localidades en el mismo dataframe.

#ANALISIS DE COMPONENTES PRINCIPALES 
view(clima)
view(climas)
#5 variables cuantitativas, y 1 cualitativa
#necesita un dataframe 
jul02<-princomp(climas[,-1])
str(jul02)#para hacer el PCA se usan los scores
jul02$scores#tantos componentes como variables pongo, utilizar las primeras 2
jul02$sdev#aquí vemos como las primeras 2 son las que tienen la mayor cantidad de información
jul02$loadings#esto nos dice de que están hechos los componentes. 

Co1<-jul02$loadings[,1]
Co2<-jul02$loadings[,2]#este no porque es igual al componente 1
Co3<-jul02$loadings[,3]
pgr<-cbind.data.frame(Co1,Co3)
pgr
plot(Co1,Co3,pch=20)#pareciera que hay 3 grupos climáticos, pero no es suficiente, no se puede poner a los estados.
## QUE OTRA TÉCNICA PODEMOS USAR PARA PONER LOS ESTADOS 
#ANÁLISIS DE CONGLOMERADOS
hclust()#variables continuas etiquetadas
#necesitamos construir una base de datos que no tenga una columna cualitativa, necesitamos etiquetar nuestras filas porque necesitamos que la base de datos sea solo numérica
climas<-data.frame(clima)#separar solo las variables climáticas
rownames(climas)<-bichos$sitio#que los rownames fuera la variable sitio del bojeto bichos
view(climas)
#ahora si ya a hacer el modelo
arbol<-hclust(dist(climas))
plot(arbol)#aquí el grupo de sonora, zacatecas y sinaloa era el punto aislado en el pca, 
#justo com ose vio en el pca, hay 3 climas 

#ahora explicar la abundancia en razón de los climas. 























































sitios <-cbind(especies,clima)

ggduo(sitios,columnsX = 1:5,columnsY = 6:10,
      types = list(continuous = "smooth_lm"),
      title = "Correlación entre variables Especies y  climaticas",
      xlab = "Especies endemicas",
      ylab = "Clima"
)

mat_cor  <- matcor(clima, especies)
mat_cor#da las correlaciones de las climáticas, las especies y luego las que son de temperatura y humedad 
cor.test()#hacer las pruebas de las correlaciones para ver si son significativas.


cca1<- cc(clima,especies)#canónicas
cca1#es como una regresión, te calcula ecuaciones.
#lo de coef son sus betas, luego te pone las calificaciones para las explicativas, las de abundancia y luego las correlaciones de las x:X y:Y y las x:y
img.matcor(mat_cor, type = 2)
#aquí se puede ver como están asociadas las destas, correlaciones cruzadas
#se concluye que las variables seleccionadas no explican la abundancia, no son suficientes tal vez, aparte del clima hay otras cosas que explican la abundancia


plt.cc(cca1,var.label=T)#este gráfico te pone tus variables u observaciones, las especies y como están cercanas a determinados valores. Que variables están asociadas, aunque ya sabemos que no hay asociaciones importantes, significativas.
#graficando las primeras 2 dimensiones, para las especies y variables climáticas. 

cca2<- cca(clima,especies)
cca2

plot(cca2)#este te grafica todo, una regla para seleccionar los destos componentes , era que los eigenvalores sean 1 o más grande que 1


#EJERCICIO 2 VARIABLES CORPORALES, PARA VER SI PODEMOS EXPLICAR EL DESEMPEÑO DE LAS PERSONAS EN TÉRMINOS DE EJERCICIO
#Supongamos que tenemos un total de 20 personas sobre las cuales hemos medido 
# 6 variables que se pueden agrupar en dos grupos claramente diferenciados, 
# por un lado variables relacionadas con características fisiológicas 
# como el peso (medido en libras), la longitud de la cintura (en pulgadas) y 
# las pulsaciones por minuto en reposo y por otro lado variables relacionadas 
# con rendimiento deportivo como número de dominadas, sentadillas y saltos.

ej22jul<-read.csv("C:/Users/100032608/Downloads/datos_cca.csv", header = T)
view(ej22jul)

desempeño<-ej22jul[,5:7]
biometricas<-ej22jul[,2:4]
ggpairs(desempeño, title = "Desempeño deportistas")
ggpairs(biometricas, title = "Variables biométricas")

ej22jul2 <-cbind(desempeño,biometricas)
view(ej22jul2)



ggduo(ej22jul2,columnsX = 1:3,columnsY = 4:6,
      types = list(continuous = "smooth_lm"),
      title = "Correlación entre variables Desempeño y  Biométricas",
      xlab = "Desempeño",
      ylab = "Biométricas"
)

mat_cor  <- matcor(biometricas, desempeño)
mat_cor

cca1<- cc(biometricas, desempeño)
cca1
img.matcor(mat_cor, type = 2)#las asociaciones son débiles

plt.cc(cca1,var.label=T)
#correlaciones menores a 0.5, correlaciones más pequeñas, como con pulso#en las dos dimensiones 