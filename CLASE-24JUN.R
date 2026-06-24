#CLASE 24 JUN
library(MPV)
install.packages("AICcmodavg")
library(AICcmodavg)
datos0<-data.frame(x =c(1,1.5,2,3,4,4.5,5,5.5,6,6.5,7,8,9,10,11,12,13,14,15), 
                   y=c(6.3,11.1,20,24,26.1,30,33.8,34,38.1,39.9,42,46.1,53.1,
                       52,52.5,48,42,27.8,21.9))
plot(datos0$x, datos0$y)
#MEJORAR CON UNA REGRESIÓN LINEAL EN DONDE A LA Y LE SACAMOS RAÍZ CUADRADA. 
#PRIMERO INTENTAR HACER OTRO MODELO TRANSFORMANDO LOS DATOS ANTES DE HACER UNO POLINOMIAL. 

modelo0<-lm(data=datos0,y~x)
plot(modelo0$residuals)#para ver como se ven los residuales 
plot(modelo0)
#se despega bastante en la parte baja del gráfico
#no hay datos influenciables, seguimos con el proceso de probar los supuestos

shapiro.test(modelo0$residuals)
library(car)
ncvTest(modelo0)
#si se cumple
durbinWatsonTest(modelo0)#este no se cumple

#el modelo no es adecuado porque no es una recta, es una curva
#podemos empezar a construir nuestro modelo polinomial

#Transformar a logaritmo
y2<-log(datos0$y)
plot(datos0$x,y2)
y2<-(datos0$y)^0.5#raíz 
plot(datos0$x,y2)

#tratar de componerlo 
boxCox(modelo0,lambda=seq(-3,3))#está incluido la raíz cuadrada, elevarla al cuadrado
#modelo polinomial
x1<-datos0$x-mean(datos0$x)
summary(x1)
modelo00<-lm(datos0$y~x1)
summary(modelo00)
plot(modelo00)
anova(modelo00)
AICc(modelo00)
shapiro.test(modelo00$residuals)
ncvTest(modelo00)
plot(modelo00,which=1)
plot(modelo00,which=2)
plot(modelo00,which=3)
plot(modelo00,which=4)
boxplot(datos0$y)

#hacer una nueva serie de datos, regresión para predecir datos
juana<-seq(-6.2632,7.7368, length.out=100)
new<-data.frame(x1=juana)
yes<-predict(modelo00,newdata=new,interval="confidence")
plot(x1,datos$y)
lines(new$x1,yes[,1],col="purple",lwd=3)
lines(new$x1,yes[,2],col="orange",lwd=1)
lines(new$x1,yes[,3],col="orange",lwd=1)
scatterplot(datos0$x,datos0$y)


#modelo 2
x1<-datos0$x-mean(datos0$x)
x2<-x1^2
modepoli<-lm(datos0$y ~ poly(x1,2))
summary(modepoli)
#te da las pendientes y todo eso, ahora ver los supuestos de una regresión.
AICc(modepoli)#es más chiquito que el otro modelo
shapiro.test(modepoli$residuals)
ncvTest(modepoli)
plot(modepoli,which=1)
plot(modepoli,which=2)
plot(modepoli,which=3)
plot(modepoli,which=4)

yes<-predict(modepoli,newdata=new,interval="prediction")
plot(x1,datos0$y)
lines(new$x1,yes[,1],col="green",lwd=3)
lines(new$x1,yes[,2],col="blue",lwd=1)
lines(new$x1,yes[,3],col="blue",lwd=1)


#modelo 3
x3<-x1^3
modeloordentres<-lm(datos0$y ~ poly(x1, 3))
summary(modeloordentres)
anova(modeloordentres)
AICc(modeloordentres)
shapiro.test(modeloordentres$residuals)
ncvTest(modeloordentres)
plot(modeloordentres)
yes<-predict(modeloordentres,newdata=new,interval="prediction")
plot(x1,datos0$y,ylim=c(0,70))
lines(new$x1,yes[,1],col="green",lwd=3)
lines(new$x1,yes[,2],col="blue",lwd=1)
lines(new$x1,yes[,3],col="blue",lwd=1)
