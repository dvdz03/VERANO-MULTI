#examen 2do parcial 
library(tidyverse)
library(MASS) 
library(car)  
library(GGally)
library(biotools)
library(Rmisc)
library(plyr)
library(dplyr)
wu<-read.csv("C:/Users/100032608/Downloads/wu.csv")
head(wu)
summary(wu)
tapply(wu,wu$landuse,summary)#para verlo por tipo de suelo 

ggpairs(wu)
wu %>%
  pivot_longer(cols = ca:p, names_to = "variable", values_to = "valor") %>%
  ggplot(aes(x = landuse, y = valor, fill = group)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribución de variables químicas por tipo de sitio")
boxplot(wu$ca~wu$landuse,las=2)
boxplot(wu$cr~wu$landuse,las=2)
vari_resp<-cbind(wu$ca,wu$cr,wu$cu,wu$fe,wu$mn,wu$pb,wu$zn,wu$cd,wu$mg,wu$ni,wu$soc,wu$ph,wu$n,wu$k,wu$p)
boxplot(vari_resp~wu$landuse)
#boxplots quedan pendientes


#SUPUESTOS
boxM(wu_log[, 1:15],wu_log$landuse)#no hay homogeneidad de varianza, usar Pillai
wu_log<-wu%>%
  mutate(across(ca:p,~log1p(.)))

cor(wu_log[,1:15])
cor.test(wu$k,wu$mg)
#ca y cr relación significativa 0.5
#hay varias correlaciones muy altas, y parece ser que todas son significativas 
vifi<-lm(as.matrix(wu_log[,1:15])~1,data=wu_log)
vif(lm(ca~cd+cr+cu+fe+mg+mn+ni+pb+zn+ph+n+p+k+soc,data=wu_log))# si hay multicolinealidad 
#eliminar el fe, mg, zn,ni

mano_vita<-manova(cbind(ca,cr,cu,fe,mn,pb,zn,cd,mg,ni,soc,ph,n,k,p)~landuse,data=wu_log)
summary(mano_vita,test="Pillai")#p=2.2e-16 
summary.aov(mano_vita)
shapiro.test(mano_vita$residuals)#residuales no normales
lda_ita<-lda(landuse~cbind(ca,cr,cu,fe,mn,pb,zn,cd,mg,ni,soc,ph,n,k,p),data=wu_log)
lda_ita
#con los 2 ejes se explica el 90% de la variación
#EL lda1 diferencia más por contenido en magnesio y plomo (9.6 y 4.96), después por niquel
#Los sitios con valores altos aquí son suelos ricos en magnesio mientras que los bajos tienen más cobre y potasio

#El LDA2 aquí también el mg es muy alto, pero lo contrasta con potasio y soc, o sea que tal vez el LDA2 diferencia ferilitidad orgánica por el contenido de carbono

#el mg es la variable dominante en todo, no solo es un indicador de fertilidad pero actua ccomo el factor principal para diferenciar entre los suelos

plot(lda_ita)

# 1. Obtener las predicciones del modelo
# Esto asigna a cada sitio (de los 300) el grupo que el modelo cree que es
predicciones <- predict(lda_ita, wu_log)

# 2. Matriz de Confusión (para ver qué tan bueno es tu modelo)
# Comparamos el uso de suelo real (landuse) con el predicho por el LDA
tabla_confusion <- table(Real = wu_log$landuse, Predicho = predicciones$class)
print(tabla_confusion)

# 3. Calcular el Porcentaje de Acierto (Eficiencia del modelo)
accuracy <- sum(diag(tabla_confusion)) / sum(tabla_confusion)
cat("Precisión global del modelo: ", round(accuracy * 100, 2), "%\n")

# 4. Validación Cruzada (Leave-One-Out - el método "Pro")
# Esto evita el sobreajuste (overfitting) y da una medida real de precisión
lda_cv <- lda(landuse ~ cbind(ca,cr,cu,fe,mn,pb,zn,cd,mg,ni,soc,ph,n,k,p), data = wu_log, CV = TRUE)
tabla_cv <- table(Real = wu_log$landuse, Predicho = lda_cv$class)
accuracy_cv <- sum(diag(tabla_cv)) / sum(tabla_cv)
cat("Precisión tras Validación Cruzada: ", round(accuracy_cv * 100, 2), "%\n")

# 5. Visualización Final (el "mapa" químico que mencionamos)
plot_data <- data.frame(LD1 = predicciones$x[,1], 
                        LD2 = predicciones$x[,2], 
                        landuse = wu_log$landuse)

library(ggplot2)
ggplot(plot_data, aes(x = LD1, y = LD2, color = landuse)) +
  geom_point(size = 3, alpha = 0.6) +
  stat_ellipse(level = 0.95) + # Dibuja elipses de confianza
  theme_minimal() +
  labs(title = "Patrón Químico: Espacio Discriminante",
       subtitle = paste("Precisión del modelo:", round(accuracy * 100, 1), "%"),
       x = "LD1 (50% de la varianza)",
       y = "LD2 (40% de la varianza)")





#
lemin<-read.csv("C:/Users/100032608/Downloads/lemminvert2.csv")
lemin
library(vegan)
view(lemin)
length(lemin)
abundancias<-lemin[,4:32]
view(abundancias)
#resumen
head(lemin)
summary(lemin)
tapply(lemin,lemin$manag,summary)
lemin$TotalAbund <- rowSums(abundancias)
boxplot(TotalAbund ~ manag, data = lemin, 
        main = "Distribución de Abundancia Total por Gestión",
        xlab = "Tipo de Gestión", ylab = "Abundancia Total",
        col = c("steelblue", "darkcyan", "seagreen", "yellowgreen"))


#canónico 
canon<-cca(abundancias~manag,data=lemin)
summary(canon)
#aquí en lo de constrained inertia, en lo de proportion nos indica que el 24% de las diferencias en las comunidades son por el tipo de manejo
#unconstrained es lo que no podemos explicar, como el ruido de fondo, la variación natural
#
#
#con el otro, el CCA1 y CCA2 son los ejes principales, el 1 es más fuerte porque el eigen es más alto, o sea captura mejor la variación
#los CA1... son como el ruido de fondo, lo que está de forma independiente al modelo. 
###
#lo último de accumulated explica que el CCA1 captura 66% de la variación 
#el CCA2 captura otro 20% y el cumulativo nos dice que solo con el 1 y 2 se está explicando el 86% de la influencia del manejo sobre las especies


anova(canon)
plot(canon, display = c("species", "sites"), type = "text", 
     main = "CCA: Comunidad de macroinvertebrados vs Gestión")
#aquí los números son los estanques, si están cerca significa que sus composiciones de macroinvertebrados son casi iguales, mientras más lejos más diferentes
#parece haber grupos como el 8-11-4-7-10-6
#9-23-2-20-5-14
#21-3-22
#15-1-13-18
#12-16
#las letras rojas son las especies.

#como el CCA1 explica casi todo alv si el estanque está muy a la derecha o izquierda es porque el manejo lo empuja a uno de esos extremos
# 1. Graficar sin nada primero
plot(canon, type = "n", main = "Relación: Gestión vs Comunidad")

# 2. Agregar los puntos de los sitios (estanques)
points(canon, display = "sites", pch = 21, bg = "gray")

# 3. Agregar los nombres de las especies en rojo
text(canon, display = "species", col = "red", cex = 0.7)

# 4. ¡LO MÁS IMPORTANTE! Agregar los centroides de gestión
# Esto dibujará un punto para cada tipo de manejo (yf, nm, li, nf)
text(canon, display = "bp", col = "blue", cex = 1.2, font = 2) # Si fueran vectores
# O mejor, agregar los centroides:
with(data, ordispider(canon, groups = manag, col = "blue", label = TRUE))

#los estanques no están agrupados al azar, gradientes bióticos. En la misma región tienen una composición similar




#####
SEM


#resumen
summary(SEM)
library(lavaan)
sem1<-read.table("C:/Users/100032608/Downloads/SEM_1.txt",header=TRUE)
sem1

tpb_model <- '
  # Definición de Variables Latentes
  Actitud =~ attitude1 + attitude2 + attitude3 + attitude4
  SN      =~ SN1 + SN2 + SN3 + SN4
  PBC     =~ PBC1 + PBC2 + PBC3 + PBC4
  Int     =~ int1 + int2 + int3 + int4 + int5
  # Usamos beh4 tal cual viene en la base
  Conducta =~ beh1 + beh2 + beh3 + beh4

  # Relaciones Estructurales
  Int ~ Actitud + SN + PBC
  Conducta ~ Int + PBC
'
fit <- sem(tpb_model, data = sem1)
summary(fit, standardized = TRUE, fit.measures = TRUE)

library(MVN)
mvn(sem1)#son normales
cor(sem1)
