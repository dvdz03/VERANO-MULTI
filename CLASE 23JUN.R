#CLASE 23 JUN
#regresión multiple
install.packages("MPV")
library(MPV)
data()
data("faithful")
mod1<-lm(waiting~eruptions,data=faithful)
mod1
lm(formula=waiting~eruptions,data=faithful)
summary(mod1)
#te da la ordenada al origen que es lo de 33.47, la pendiente que es 10.79, el error estandar
#el valor de t para las pruebas de hipótesis y el valor de p
#aquí los dos son significativos entonces significa que si hay relación en las dos vairables.
# también nos da cuanto vale el error, la R2 ajustada y el valor de p del anova, que lo que más te interesa es si salió significativo
plot(mod1)

#el q-q es para ver la normalidad normal de las normales. 
#residuals vs leverage ahí marca si hay como datos influenciables o algo asíx
library(car)
library(car)
shapiro.test(mod1$residuals)
#no es una desviación muy importante, no son normales
ncvTest(mod1)
#los datos tienen una varianza constante, estos datos tiene sus residuales homocedásticos.
durbinWatsonTest(mod1)
#esto es para ver si los residuales son independientes.
#parece que los datos no son independientes.
plot(waiting~eruptions)
attach(faithful)
#no se distribuyen de manera homogenea, de 50-60 es un grupo y luego hay otro grupo de 80-90. 
#uso de modelo es limitado porque no son independientes, nuestro modelo explica el comporatmiento pero algo pasa que hay una parte vacía bien asuichis
boxCox(mod1)

#2do modelo
mod2<-lm(waiting^1.5~eruptions,data=faithful)
summary(mod2)
scatterplot(waiting~eruptions)
detach(faithful)

#método de pearson
#método de spearman es para no normales, lo adecuado es no usar la de pearson que asume la normalidad de los datos. 



library(tidyverse)
install.packages("mice")
install.packages("GGally")
install.packages("plotly")
library(car)
library(mice)
library(GGally)
library(plotly)

datos23JUN<-table.b3
attach(datos23JUN)
str(datos23JUN)
view(datos23JUN)
#la regresión es un glm pero con no se que gaussiano.

reporte5<-table.b2
attach(reporte5)
str(reporte5)
view(reporte5)
#analisis numerico y grafico
corres<-cor(datos23JUN,use="pairwise.complete.obs")
print(corres,digits=2)#LA Y tiene una relación inversamente proporcional, sus valores de la correlación va de -1 a 1. -1 es inversamente proporcional, 1 es directamente proporcional. mientras más cerca esté del 0 más debil es la relación entre las dos variables
ggpairs(datos23JUN)
cor.test(y,x4)
###############
corres1<-cor(reporte5,use="pairwise.complete.obs")
print(corres1)

ggpairs(reporte5)
cor.test(y,x2)


#apenas se rechaza la hipótesis nula
#revisar la correlación entre las variables explicativas, si están altamente correlacionadas estamos teniendo información redundante, está pasando la sobrecarga del modelo, sobreajuste.
#multicolinealidad
#probar que tanto infla la varianza cada variable explicativa porque la medai en que está más correlacinoado va inflando la varianza
#indice de inflación de la varianza o algo así,

#análisis componentes principales= algo
#ir eliminando variables o hacer un pca y trabajar con pocos componentes, cada uno se va quedadndo con un poco de varianza total. 

modelo<-lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11)
shapiro.test(modelo$residuals)
ncvTest(modelo)
durbinWatsonTest(modelo)
summary(modelo)
vif(modelo)#con esto el nivel de como aportación, es otra palabra, que usé atrás pero no me acuerdo cual mijo.
step(modelo)

#imputación de los datos con la mediana
x3b<-x3
x3b[23]<-median(x3,na.rm=T)
x3b[25]<-median(x3,na.rm=T)
x3b

modelo1<-lm(y~x1+x3b+x5+x8+x9+x10)

shapiro.test(modelo1$residuals)
ncvTest(modelo1)
durbinWatsonTest(modelo1)
summary(modelo1)
vif(modelo1)#todavía hay unos muy 
step(modelo1)


library(mice)
datos23JUN_1<- mice(datos23JUN)
str(datos23JUN_1)
datos23JUN_1.1<-complete(datos23JUN_1)
view(datos23JUN_1.1)

detach(datos23JUN)
attach(datos23JUN)
#COMPONENTES PRINCIPALES
#toda la variabilidad en un sentido en el primer componente
#el segundo componente es ortogonal al primero(¿?)
#ortgonal~perpendicular, sería como eso, no comparte información

numericas <- cbind(x1,x2,x3b,x4,x5,x6,x7,x8,x9,x10,x11)
#los scores son las contribuciones de cada variable a cada componente y cada una le contribuye de manera diferente
numericas
compos<-princomp(numericas,cor=T)#aquí le digo que use la matrizde correlaciones en lugar la de varianzas
plot(compos)#solo grafica las 2 primeras, te grafica la cantidad de varianza contenida en cada una de las variables
#vemos como el primer componente tiene casi toda la información y pues si porque si se correlacionaban así bien harto.
summary(compos)
str(compos)
#los loadings son las contribuciones de cada variable
#los valores de los componentes son los scores
compos$sdev

compos$loadings#todas las variables contribuyen al componente 1
#ahora cuanta varianza hay en cada una que está abajo


#SCOREs(variables sintéticas)
c1<-compos$scores[,1]
c2<-compos$scores[,2]
c3<-compos$scores[,3]

y2<-y
modeloc<-lm(y2~c1+c2)
shapiro.test(modeloc$residuals)
ncvTest(modeloc)
durbinWatsonTest(modeloc)
boxCox(modeloc)

modelod<-lm(y2^-.1~c1+c2)
shapiro.test(modelod$residuals)
ncvTest(modelod)
durbinWatsonTest(modelod)
boxCox(modelod)
summary(modelod)
barplot(compos$loadings[,1])#interpretar el componente 1, hablar de cada una de las variables
#la x4 5 y 5 tienen una relación inversamente proporcional
