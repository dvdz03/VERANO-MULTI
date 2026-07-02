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



#MANOVA
library(tidyverse)
library(car)
library(Rmisc)

# MANOVA IRIS
data("iris")

view(iris)

# LONGITUD DEL PÉTALO
# Resumen numérico

tapply(iris$Petal.Length, iris$Species, summary)
tapply(iris$Petal.Length, iris$Species, shapiro.test)
res_PL<-summarySE(data = iris, measurevar = "Petal.Length", groupvars = "Species")
print(res_PL, digits = 2)

# Resumen gráfico
boxplot(iris$Petal.Length~iris$Species)

mod1<-aov(iris$Petal.Length~iris$Species)
# summary(mod1)

shapiro.test(mod1$residuals)
leveneTest(iris$Petal.Length~iris$Species)
mod1b<-lm(iris$Petal.Length~iris$Species)
durbinWatsonTest(mod1b)
boxCox(mod1b)

mod2<-aov(iris$Petal.Length^0.1 ~ iris$Species)
shapiro.test(mod2$residuals)
leveneTest(iris$Petal.Length^0.1 ~ iris$Species)
mod2b<-lm(iris$Petal.Length^0.1 ~ iris$Species)
durbinWatsonTest(mod2b)
summary(mod2)
TukeyHSD(mod2)

#####
tapply(iris$Sepal.Length, iris$Species, summary)
tapply(iris$Sepal.Length, iris$Species, shapiro.test)
res_PL<-summarySE(data = iris, measurevar = "Sepal.Length", groupvars = "Species")
print(res_PL, digits = 2)

# Resumen gráfico
boxplot(iris$Sepal.Length~iris$Species)

mod1<-aov(iris$Sepal.Length~iris$Species)
shapiro.test(mod1$residuals)
bartlett.test(iris$Sepal.Length~iris$Species)
mod1b<-lm(iris$Sepal.Length~iris$Species)
durbinWatsonTest(mod1b)

boxCox(mod1b)

mod2<-aov(iris$Sepal.Length^-1 ~ iris$Species)
shapiro.test(mod2$residuals)
bartlett.test(iris$Sepal.Length^-1~iris$Species)
mod2b<-lm(iris$Sepal.Length^-1~iris$Species)
durbinWatsonTest(mod2b)

cor.test(iris$Sepal.Length, iris$Petal.Length)

####

sepl <- iris$Sepal.Length^-1
petl <- iris$Petal.Length^0.1
respuesta<-cbind(sepl, petl)

sepalongitud<-scale(iris$Sepal.Length^-1)
sepancho<-scale(iris$Sepal.Width)
petalolargo<-scale(iris$Petal.Length^0.2)
petancho<-scale(iris$Petal.Width^0.6)
respuesta1<-cbind(sepalongitud,petalolargo)#ponerlas en una matriz que se llamará respuesta, MANOVA variable explicativa las especies.

# MANOVA test
res.man <- manova(respuesta ~ iris$Species)
res.man1<-manova(respuesta1~iris$Species)
summary(res.man1)#salen casi igual 
summary(res.man) #Pillai, puedes poner más de 1,hacíamos prueba de F, pero pues hay bien hartas 
str(res.man)
shapiro.test(res.man$residuals)
#si tienen distribución normal, después de haber ajustado los problemas de heterostacididad o eso 

summary.aov(res.man)#esto te da el informe de tu anova por cada una de tus variables respuesta
proeba1<-aov(sepl~iris$Species)
TukeyHSD(proeba1)
proeba2<-aov(petl~iris$Species)
TukeyHSD(proeba2)
#como el mancova ya salió significativo los resumenes pues si mijo salen igual
#puedes hacer los anovas para ver específicamente esa diferencia y así es mijo todos ason diferentes a todos.


install.packages("MVN")
library(MVN)

mvn(iris[,c(1,3)],mvnTest = "hz")

install.packages("biotools")
library(biotools)

# Homogeneidad varianzas y covarianzas
biotools::boxM(iris[,c(1,3)],grouping = iris[,c(5)])


# PosHoc

library(MASS)
post_hoc <- lda(iris$Species~respuesta, CV=F)#análisis de discriminantes
post_hoc



##################################################
################### EJERCICIO 2 ##################
##################################################
library(tidyverse)
library(MVN)
library(biotools)
library(car)

# En un experimento para inhibir un tumor, 
# se quiere investigar el efecto del sexo (S) y de la temperatura ambiental (T). 
# Se consideran las variables: Y1 =peso inicial, Y2 =peso final, Y3 =peso del tumor.

pesos <-c (18.15,16.51, 0.24, 19.15, 19.49, 0.16, 18.68,
           19.50, 0.32, 18.35, 19.81, 0.17, 19.54, 19.84,
           0.20, 20.58, 19.44, 0.22,21.27, 23.30, 0.33, 
           18.87, 22.00, 0.25, 19.57, 22.30, 0.45, 20.66,
           21.08, 0.20, 20.15, 18.95, 0.35, 21.56, 20.34, 
           0.20,20.74, 16.69, 0.31, 20.22, 19.00, 0.18, 
           20.02, 19.26, 0.41, 18.38, 17.92, 0.30, 17.20,
           15.90, 0.28, 20.85, 19.90, 0.17)
y1<-pesos[c(T,F,F)]
y2<-pesos[c(F,T,F)]
y3<-pesos[c(F,F,T)]
temp <-factor(c(rep(4,6),rep(20,6),rep(34,6)))
sex<-factor(rep(c(rep("M",3),rep("H",3)),3))
ratas<-data.frame(y1,y2,y3,temp,sex)
view(ratas)
head(ratas)
#VOLVER LA TEMPERATURA COMO FACTOR
ratas$temp<-factor(ratas$temp)

# Normalidad univariada, todo lo de siempre, verificar normalidad y homoestacidad para cada una de las variables
shapiro.test(ratas$y1)
shapiro.test(ratas$y2)
shapiro.test(ratas$y3)
#todas son normales 
#luego verificar la normalidad por grupo
tapply(ratas$y1,ratas$sex,shapiro.test)
tapply(ratas$y1,ratas$temp,shapiro.test)
#no hay problemas con normalidad
tapply(ratas$y2,ratas$sex,shapiro.test)
tapply(ratas$y2,ratas$temp,shapiro.test)
tapply(ratas$y3,ratas$sex,shapiro.test)
tapply(ratas$y3,ratas$temp,shapiro.test)
#ahora prueba de bartlett porque todo toditititititito es normal
bartlett.test(ratas$y1~ratas$sex)#sin problema
bartlett.test(ratas$y2~ratas$sex)
bartlett.test(ratas$y3~ratas$sex)
#todas OK
bartlett.test(ratas$y1~ratas$temp)
bartlett.test(ratas$y2~ratas$temp)
bartlett.test(ratas$y3~ratas$temp)
#no problema con homogeneidad de varianzas. 

#PRUEBA NORMALIDAD MÚLTIPLE
library(MVN)
# Normalidad múltiple
mvn(ratas[,c(1,2,3)],mvnTest = "hz")

# Homogeneidad varianzas y covarianzas
biotools::boxM(ratas[,c(1,2,3)],grouping = ratas[,c(4)])#lo que hace esa función es probar la matriz de varianzas y covarianzas.
biotools::boxM(ratas[,c(1,2,3)],grouping = ratas[,c(5)])

cor(ratas[,c(1,2,3)])#esto es para comprobar que no haya multicolinearidad
#aquí la y1 y y2 tienen una significativa lo que puede producir inflación en la varianza 


modA<-lm(ratas$y1~ratas$temp)
plot(modA)

# MANOVA
mod1<-manova(cbind(y1,y2,y3)~ sex + temp, data=ratas)
summary(mod1)#solo la temperatura es significativa. para hacer el de sexo si ya sabemos que no va a salir significativa

summary.aov(mod1)#solo para la respuesta 2 temperatura es significativa, no pasa con ninguna de las demás. 
#ahora correr el modelo para saber quien es diferente a quien y así
m2<-aov(y2~temp,data=ratas)
TukeyHSD(m2)#la más significativa es la de 34 vs 20 y la que apenas es es la de 20vs4.
boxplot(y2~temp)#la temperatura 20 si es más diferente que las otras dos entonces por eso sale así mijito






library(MASS)

respuesta<-cbind(ratas$y1, ratas$y2, ratas$y3)
post_hoc <- lda(ratas$temp~respuesta, CV=F)
post_hoc
