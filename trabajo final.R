# TRABAJO FINAL
library(xlsx)
garbanzo<-read.xlsx("C:/Users/100032608/Documents/veranomulti_exp.1.xlsx", sheetIndex = 1)
garbanzo
view(garbanzo)
library(tidyverse)
head(garbanzo)
#######
# exploración de los datos
#######
summary(garbanzo)
tapply(garbanzo,garbanzo$agua,summary)
tapply(garbanzosvivos,garbanzo$agua,summary)

summary(garbanzosvivos)

tapply(garbanzo,garbanzo$fertilizante,summary)
tapply(garbanzo,garbanzo$luz,summary)
library(Rmisc)
summarySE(garbanzosvivos,measurevar="tallo")
summarySE(garbanzo,measurevar="tallo")
summarySE(garbanzo,measurevar="tallo",groupvars=c("luz","agua","fertilizante"))
summarySE(garbanzo,measurevar="hojas")
summarySE(garbanzosvivos,measurevar="hojas")
summarySE(garbanzo,measurevar="raiz")
summarySE(garbanzosvivos,measurevar="raiz")

#tallo
par(mfrow=c(3,3))
boxplot(garbanzo$tallo~garbanzo$luz,main="tallo y luz")
boxplot(garbanzo$tallo~garbanzo$agua,main="tallo y agua")
boxplot(garbanzo$tallo~garbanzo$fertilizante,main="talloy fertilizante")

boxplot(garbanzosvivos$tallo~garbanzosvivos$luz,main="tallo y luz")
boxplot(garbanzosvivos$tallo~garbanzosvivos$agua,main="tallo y agua")
boxplot(garbanzosvivos$tallo~garbanzosvivos$fertilizante,main="talloy fertilizante")
#hojas
boxplot(garbanzo$hojas~garbanzo$luz,main="hoja y luz")
boxplot(garbanzo$hojas~garbanzo$agua,main="hoja y agua")
boxplot(garbanzo$hojas~garbanzo$fertilizante,main="hoja y fertilizante")

boxplot(garbanzosvivos$hojas~garbanzosvivos$luz,main="tallo y luz")
boxplot(garbanzosvivos$hojas~garbanzosvivos$agua,main="tallo y agua")
boxplot(garbanzosvivos$hojas~garbanzosvivos$fertilizante,main="talloy fertilizante")

#raices
boxplot(garbanzo$raiz~garbanzo$luz,main="raiz y luz")
boxplot(garbanzo$raiz~garbanzo$agua,main="raíz y agua")
boxplot(garbanzo$raiz~garbanzo$fertilizante,main="raiz y fertilizante") 

boxplot(garbanzosvivos$raiz~garbanzosvivos$luz,main="tallo y luz")
boxplot(garbanzosvivos$raiz~garbanzosvivos$agua,main="tallo y agua")
boxplot(garbanzosvivos$raiz~garbanzosvivos$fertilizante,main="talloy fertilizante")

ggpairs(garbanzo)

shapiro.test(garbanzo$tallo)
shapiro.test(garbanzo$hojas)
shapiro.test(garbanzo$raiz)
#ninguno tiene distribución normal

shapiro.test(garbanzosvivos$tallo)#sinormal
shapiro.test(garbanzosvivos$hojas)#nonormal

shapiro.test(garbanzosvivos$raiz)#sinormal




#si hay homogeneidad de varianzas
leveneTest(garbanzo$tallo~garbanzo$fertilizante)
leveneTest(garbanzo$tallo~garbanzo$luz)

garbanzosvivos<-subset(garbanzo,tallo>0)

#GERMINACIÓN
garbanzo$estado <- ifelse(garbanzo$tallo == 0, "Muerta", "Viva")
tasa_agua <- aggregate(estado == "Muerta" ~ agua, data = garbanzo, FUN = mean)
tasa_luz <- aggregate(estado == "Muerta" ~ luz, data = garbanzo, FUN = mean)
tasa_fert <- aggregate(estado == "Muerta" ~ fertilizante, data = garbanzo, FUN = mean)

tasa_agua
tasa_fert
tasa_luz

par(mfrow=c(1,3))
barplot(tasa_agua[,2], names.arg=tasa_agua[,1], main="Mortalidad por Agua", col="red")
barplot(tasa_luz[,2], names.arg=tasa_luz[,1], main="Mortalidad por Luz", col="red")
barplot(tasa_fert[,2], names.arg=tasa_fert[,1], main="Mortalidad por Fertilizante", col="red")

head(garbanzo)
cor(garbanzosvivos[,c(4,5,6)])#hay multicolinearidad 
head(garbanzosvivos)
garbanzosvivos$hojas<-garbanzosvivos$hojas^0.5
#MANOVA
hojas_si<-garbanzosvivos$hojas^0.5
mod_man0<-manova(cbind(tallo,hojas,raiz)~agua*luz*fertilizante,data=garbanzo)
summary(mod_man0)
summary.aov(mod_man0)
shapiro.test(mod_man0$residuals)
mod_man<-manova(cbind(tallo,hojas_si,raiz)~agua*luz*fertilizante,data=garbanzosvivos)
mod_man1<-manova(cbind(tallo,hojas,raiz)~agua+luz+fertilizante+agua*luz*fertilizante,data=garbanzosvivos)
summary(mod_man)
summary.aov(mod_man)
summary(mod_man1)
shapiro.test(mod_man$residuals)#no distribución normal
shapiro.test(mod_man1$residuals)
library(vegan)
#TRANSFORMACIÓN DE LOS DATOS
mod_norm<- manova(cbind(log1p(tallo), log1p(hojas), log1p(raiz)) ~ agua * luz * fertilizante, data = garbanzosvivos)
summary(mod_norm)
shapiro.test(mod_norm$residuals)
cosa<-summary.aov(mod_norm)
cosa1<-summary.aov(mod_man)
summary.aov(mod_man1)
summary.aov(mod_man)
cosa  



#anovas
tallos<-aov(tallo~agua*luz*fertilizante,data=garbanzosvivos)
summary(tallos)
hojasz<-aov(hojas_si~agua*luz*fertilizante,data=garbanzosvivos)
summary(hojasz)
raices<-aov(raiz~agua*luz*fertilizante,data=garbanzosvivos)
summary(raices)

shapiro.test(tallos$residuals)
shapiro.test(hojasz$residuals)#esta no es normal
hojasz1<-lm(hojas_si~agua+luz+fertilizante,data=garbanzosvivos)
boxcox(hojasz1)
shapiro.test(raices$residuals)

durbinWatsonTest(tallos)
durbinWatsonTest(hojasz)
durbinWatsonTest(raices)

garbanzosvivos$agua <- as.factor(garbanzosvivos$agua)
garbanzosvivos$fertilizante <- as.factor(garbanzosvivos$fertilizante)
garbanzosvivos$luz <- as.factor(garbanzosvivos$luz)

library(car)
leveneTest(tallo ~luz * fertilizante, data = garbanzosvivos)
leveneTest(hojas_si ~luz * fertilizante, data = garbanzosvivos)
leveneTest(raiz ~ agua * luz * fertilizante, data = garbanzosvivos)

#LDA
mod_lda1<-lda(agua~tallo+raiz+hojas,data=garbanzosvivos)
mod_lda1
garbanzosvivos$tratamientos<- interaction(garbanzosvivos$agua,garbanzosvivos$luz,garbanzosvivos$fertilizante)
mod_lda1.2<-lda(tratamientos~tallo+raiz+hojas,data=garbanzosvivos)
plot(mod_lda1)
par(mfrow=c(1,1))
class(garbanzosvivos$agua)
library(MASS)
mod_lda2<-lda(agua~tallo+raiz+fertilizante,data=garbanzo)
mod_lda2
plot(mod_lda2,dimen=1)
mod_lda3<-lda(luz~tallo+raiz+hojas,data=garbanzosvivos)
mod_lda3
plot(mod_lda3)

mod_lda4<-lda(fertilizante~tallo+raiz+hojas,data=garbanzosvivos)
mod_lda4
plot(mod_lda4)

library(ggplot2)
library(Rmisc)

resumen_tallo <- summarySE(garbanzosvivos, measurevar="tallo", groupvars=c("agua"))

ggplot(resumen_tallo, aes(x=agua, y=tallo)) +
  geom_bar(stat="identity", fill="steelblue", width=0.6) +
  geom_errorbar(aes(ymin=tallo-se, ymax=tallo+se), width=.2) +
  theme_classic() +
  labs(title="Media de crecimiento del tallo por cantidad de agua",
       x="Cantidad de Agua", y="Tallo (cm)")

ggplot(garbanzosvivos, aes(x = agua, y = tallo, color = fertilizante)) +
  # Puntos individuales
  geom_jitter(width = 0.2, size = 3) +
  # Esto crea una gráfica para cada nivel de Luz
  facet_grid(~luz) +
  theme_bw() +
  labs(title = "Efecto de Luz, Agua y Fertilizante",
       x = "Agua", y = "Tallo (cm)")


