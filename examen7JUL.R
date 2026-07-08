#EXAMEN A CASA
library(MVA)
library(car)
library(tidyverse)
install.carinstall.packages("MVA")
#hacer análisis exploratorios, si vemos que los supuestos no se cumplen hacer modificaciones
# TODO JUSTIFICADO
#CONTESTAR LA PREGUNTA U OBJETIVO QUE CREAN QUE ES MÁS ADECUADO



#EJERCICIO 1
data("USairpollution")
view(USairpollution)
aire<-USairpollution
#saber cuales son los mejores indicadores o predictores de contaminación atmos medido según el contenido de SO2 en aire
tapply(aire$temp,aire$SO2,summary)
aire$SO2
#esta es una regresión linear múltiple
head(USairpollution)
ejercicio1<-lm(SO2~temp+manu+popul+wind+precip+predays,data=aire)
summary(ejercicio1)
ejercicio1.1<-step(ejercicio1)
summary(ejercicio1.1)#62% de ajuste
shapiro.test(ejercicio1.1$residuals)#no hay distribución normal 
leveneTest(aire$SO2,data=aire)
plot(ejercicio1.1)
library(car)
vif(ejercicio1.1)#manufactura y población están altamente correlacionados
boxCox(ejercicio1.1)
bc<-boxCox(ejercicio1)
lambda_optimo <- bc$x[which.max(bc$y)]
print(lambda_optimo)


ejercicio1.2<-lm(log(SO2)~temp+manu+wind+precip,data=aire)
summary(ejercicio1.2)#aquí la R ajustada es del 41% es menor, pero con las otras subió a 58%
vif(ejercicio1.2)
shapiro.test(ejercicio1.2$residuals)#si son normales
ncvTest(ejercicio1.2)#varianzas constantes 
plot(ejercicio1.2)
durbinWatsonTest(ejercicio1.2)#no hay correlación 
aire$SO2
barplot(aire$SO2)

ejercicio1.3<-lm(SO2^-0.3~temp+manu+wind+precip,data=aire)
summary(ejercicio1.3)#40% de ajuste, si pongo las otras sube al 60%
vif(ejercicio1.3)#no hay correlaciones
shapiro.test(ejercicio1.3$residuals)
ncvTest(ejercicio1.3)
durbinWatsonTest(ejercicio1.3)
#Análisis numérico 
head(aire)
aire
summary(aire)
view(aire)
library(Rmisc)
summarySE(aire,measurevar="SO2",groupvars="lugares")
summarySE(aire,measurevar="temp")
summarySE(aire,measurevar="manu")
summarySE(aire,measurevar="popul")
summarySE(aire,measurevar="wind")
summarySE(aire,measurevar="precip")
summarySE(aire,measurevar="predays")
tapply(aire,aire$manu,summary)
boxplot(aire$SO2)
boxplot(scale(aire))


#EJERCICIO 2
#quiere identificar tipos de mujer
#ANALISIS DE CONGLOMERADOS

data("CHFLS")
view(CHFLS)
ejercicio2<-CHFLS
head(ejercicio2)
str(ejercicio2)
summary(ejercicio2)
#distancia de gower porque hay variables de todos los tipos y así 
library(cluster)
df_limpio<-ejercicio2 %>%
  na.omit() %>%
  mutate(across(c(R_health,R_happy,R_region,R_edu)))
dis_gow<-daisy(df_limpio,metric="gower")
hc_arbol<-hclust(dis_gow,method="ward.D2")

plot(hc_arbol,main="tipos de mujeres",labels=F)
rect.hclust(hc_arbol, k=5, border="red")
grupitos<-cutree(hc_arbol, k=5)
ej4final<- cbind(df_limpio, Cluster=as.factor(grupitos))

#perfilitos<-ej4final %>%
#  group_by(Cluster) %>%
#  summarise(
#    años=mean(R_age),
#    dinero=mean(R_income),
#    Saludes=mean(as.numeric(R_health)),
#    .groups="drop"
#  )


perfil_prom<-aggregate(cbind(R_age, R_income, as.numeric(R_health)) ~ Cluster, 
                       data = ej4final, 
                       FUN = mean, na.rm = TRUE)
conteos<-table(ej4final$Cluster)
perfil_prom
conteos
class(ej4final)

library(dplyr)
perfilitos
library(Rmisc)
library(tidyverse)
library(CCA)
library(vegan)
library(GGally)
library(fields)



nonummuj<-ejercicio2[,c(1,3,5,7)]
numuj<-ejercicio2[,c(2,4,6)]
numuj1<-scale(numuj)
distancias<-dist(numuj1)
arbol<-hclust(distancias,method="ward.D2")
plot(arbol,
     main="tipos de mujeres",
     xlab="encuestadas",
     ylab="distancia",
     sub="",
     labels=F)
tipos<-cutree(arbol,k=3)
perfiles<-data.frame(tipologia=tipos,nonummuj)
perfiles1<-aggregate(numuj,by=list(Tipo=tipos),FUN=mean)
perfiles1
table(perfiles$tipologia,perfiles[,1])
#todos estos son observaciones de mujeres nada más
ggpairs(nonummuj)
ggpairs(numuj)
regiones<-data.frame(nonummuj$R_region,numuj)
regiones
edades<-tapply(regiones$R_age,regiones$nonummuj.R_region,median)
ingresos<-tapply(regiones$R_income,regiones$nonummuj.R_region,median)
alturas<-tapply(regiones$R_height,regiones$nonummuj.R_region,median)
alturas
barplot(sort(edades),las=2)
barplot(sort(ingresos),las=2)
barplot(sort(alturas),las=2)

morfos<-data.frame(numuj)
morfos1<-scale(morfos)
dist_ancias<-dist(morfos1,method="euclidean")
arbol1<-hclust(morfos1,method="ward.D2")

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


#### otro método
ks<-kmeans(numuj1,centers=3,nstart=25)
table(ks$cluster)
aggregate(numuj,by=list(tipo=ks$cluster),FUN=mean)
perfiles2<-data.frame(tipologia=ks$cluster,nonummuj)
table(perfiles2$tipologia,perfiles2$R_region)

#EJERCICIO 2
as.factor(c(ejercicio2$R_region,ejercicio2$R_edu,ejercicio2$R_health,ejercicio2$R_happy))
str(ejercicio2)
library(cluster)
dis_tan<-daisy(ejercicio2,metric="gower")
hc<-hclust(dis_tan,method="ward.D2")
plot(hc,main="holiwis")
rect.hclust(hc,k=6,border="blue")
grupos<-cutree(hc,k=6)
oksi<-cbind(ejercicio2,tipomujer=as.factor(grupos))
per_fil<-oksi %>%
  group_by(tipomujer) %>%
  summarise(
    años=mean(R_age,na.rm=T),
    ingreso=mean(R_income,na.rm=T),
    salud=mean(as.numeric(R_health),na.rm=T),
    feliz=mean(as.numeric(R_happy),na.rm=T)
  )
per_fil
#EJERCICIO 3
data("skulls")
view(skulls)
calaveras<-skulls
#las mediciones se mantienen cercanas o no
#resumen numérico
tapply(calaveras$mb,calaveras$epoch,summary)
tapply(calaveras$bh,calaveras$epoch,summary)
tapply(calaveras$bl,calaveras$epoch,summary)
tapply(calaveras$nh,calaveras$epoch,summary)
#normalidad
tapply(calaveras$mb,calaveras$epoch,shapiro.test)#el único donde hay 2 no normales
tapply(calaveras$bh,calaveras$epoch,shapiro.test)
tapply(calaveras$bl,calaveras$epoch,shapiro.test)
tapply(calaveras$nh,calaveras$epoch,shapiro.test)
#summarySE
library(Rmisc)
summarySE(calaveras,measurevar="mb",groupvars="epoch")
summarySE(calaveras,measurevar="bh",groupvars="epoch")
summarySE(calaveras,measurevar="bl",groupvars="epoch")
summarySE(calaveras,measurevar="nh",groupvars="epoch")

#resumen gráfico
boxplot(calaveras$bh~calaveras$epoch)
boxplot(calaveras$mb~calaveras$epoch)
boxplot(calaveras$bl~calaveras$epoch)
boxplot(calaveras$nh~calaveras$epoch)
#no hay agrupaciones al parecer, los rangos se sobrelapan al igual que las medianas y medias. 
view(skulls)
mode1<-aov(calaveras$mb~calaveras$epoch)
summary(mode1)
shapiro.test(mode1$residuals)
bartlett.test(calaveras$mb~calaveras$epoch)#si todo bien
TukeyHSD(mode1)
respuestas<-cbind(calaveras$mb,calaveras$bh,calaveras$bl,calaveras$nh)
mano_va<-manova(respuestas~epoch,data=calaveras)
summary(mano_va)#si hay diferencias entre las épocas
shapiro.test(mano_va$residuals)#si hay normalidad
summary.aov(mano_va)
res1<-aov(calaveras$mb~calaveras$epoch)
res2<-aov(calaveras$bh~calaveras$epoch)
res3<-aov(calaveras$bl~calaveras$epoch)
TukeyHSD(res1)
TukeyHSD(res2)
TukeyHSD(res3)

library(MASS)
mod_lda<-lda(epoch~mb+bh+bl+nh,data=calaveras)
mod_lda
plot(mod_lda,dimen=1)
post_hocs <- lda(calaveras$epoch~respuestas, CV=F)#análisis de discriminantes
post_hocs
#en conclusión si hubo mestizaje

#EJERCICIO 4
data("watervoles")
view(watervoles)
watervoles1<-decostand(watervoles,method="hellinger")
matridist<-vegdist(watervoles1,method="euclidean")
ahorasi<-metaMDS(matridist,k=2,trymax=100)
#escalamiento multidimensional no métrico
library(vegan)
ejercicio4<-metaMDS(watervoles,k=2,trymax=100)
grups<-c(rep("Reino Unido",6),rep("Europa",8))
plot(ahorasi,type="n",main="NMDS")
points(ahorasi,display="sites",col=ifelse(grups=="Reino Unido","red","blue"),pch=19)
text(ahorasi,labels=rownames(watervoles))
legend("topright",legend=c("Reino Unido","Europa"),col=c("red","blue"),pch=19)
